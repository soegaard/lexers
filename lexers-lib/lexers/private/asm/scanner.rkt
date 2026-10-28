#lang racket/base

;;;
;;; Assembly Profile Scanner
;;;

;; Bounded, byte-preserving lexical scanning for the assembly profiles exposed
;; by lexers/asm.  Profile rules govern comments, directives, labels, strings,
;; registers, and lexical state changes; unknown words remain valid symbols.

;; make-profiled-asm-derived-lexer : keyword-arguments -> (input-port? -> (or/c asm-derived-token? 'eof))
;;   Construct one bounded derived-token reader for a selected profile.

(provide make-profiled-asm-derived-lexer)

(require racket/bytes
         racket/list
         racket/string
         "../source.rkt"
         "profiles.rkt")

;; The implementation caps macro and conditional bookkeeping.  It does not
;; expand source or visit includes, so these are lexical-context bounds only.
(define maximum-context-depth
  32)

;; scanner-state : symbol? ... -> scanner-state?
;;   Mutable state that affects how later source fragments are interpreted.
(struct scanner-state (mode string-delimiter c-comments? syntax cpu-mode
                            macro-depth conditional-depth pending-directive
                            line-start? word-continuation-id script-depth
                            namespace-depth emitted-encoding?)
  #:mutable
  #:transparent)

;; base-word-character? : char? -> boolean?
;;   Recognize a conservative identifier/literal character shared by profiles.
(define (base-word-character? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (memv ch '(#\_ #\. #\$ #\?))))

;; source-unit-character : source-unit? -> char?
;;   Extract the single decoded scalar carried by one source unit.
(define (source-unit-character unit)
  (string-ref (source-unit-text unit) 0))

;; profile-tag : asm-dialect-spec? string? -> symbol?
;;   Construct a stable, profile-specific syntax-role tag.
(define (profile-tag spec suffix)
  (string->symbol
   (string-append (symbol->string (asm-dialect-spec-dialect spec))
                  "-"
                  suffix)))

;; architecture-tag : asm-dialect-spec? -> symbol?
;;   Construct the broad architecture tag carried by profile tokens.
(define (architecture-tag spec)
  (string->symbol
   (string-append "asm-architecture-"
                  (symbol->string (or (asm-dialect-spec-architecture spec)
                                      'unknown)))))

;; profile-state-tags : asm-dialect-spec? scanner-state? -> (listof symbol?)
;;   Return profile and selected mode tags without making modes presentation.
(define (profile-state-tags spec state)
  (append (list (profile-tag spec "source")
                (architecture-tag spec))
          (cond
            [(scanner-state-syntax state)
             (list (profile-tag spec
                                (string-append "syntax-"
                                               (symbol->string
                                                (scanner-state-syntax state)))))]
            [else '()])
          (cond
            [(scanner-state-cpu-mode state)
             (list (profile-tag spec
                                (string-append "mode-"
                                               (symbol->string
                                                (scanner-state-cpu-mode state)))))]
            [else '()])
          (cond
            [(positive? (scanner-state-macro-depth state))
             '(asm-macro-body)]
            [else '()])
          (cond
            [(positive? (scanner-state-conditional-depth state))
             '(asm-conditional-region)]
            [else '()])
          (cond
            [(and (eq? (asm-dialect-spec-dialect spec) 'kick)
                  (positive? (scanner-state-script-depth state)))
             '(kick-script-region)]
            [else '()])
          (cond
            [(and (eq? (asm-dialect-spec-dialect spec) '|64tass|)
                  (positive? (scanner-state-namespace-depth state)))
             (list (profile-tag spec "namespace-region"))]
            [else '()])
          (cond
            [(and (eq? (asm-dialect-spec-dialect spec) '|64tass|)
                  (scanner-state-emitted-encoding? state))
             (list (profile-tag spec "emitted-encoding"))]
            [else '()])))

;; make-profiled-asm-derived-lexer : keyword-arguments -> procedure?
;;   Construct a byte-preserving scanner for one explicit profile selection.
(define (make-profiled-asm-derived-lexer #:dialect      dialect
                                         #:architecture architecture
                                         #:encoding     encoding
                                         #:fragment-cap fragment-cap
                                         #:make-derived-token make-derived-token
                                         #:make-effective-state make-effective-state
                                         #:make-diagnostic make-diagnostic)
  (define spec
    (asm-profile-spec dialect architecture))
  (unless spec
    (raise-arguments-error 'make-asm-derived-lexer
                           "the requested dialect needs a supported architecture"
                           "dialect" dialect
                           "architecture" architecture))
  (define reader
    (make-source-reader #:encoding encoding))
  (define pending-unit
    #f)
  (define next-continuation-id
    1)
  (define state
    (scanner-state 'code
                   #f
                   (asm-dialect-spec-c-comments? spec)
                   (cond
                     [(eq? dialect 'nasm)
                      'intel]
                     [(and (eq? dialect 'gas)
                           (eq? architecture 'x86))
                      'att]
                     [else
                      #f])
                   #f
                   0
                   0
                   #f
                   #t
                   #f
                   0
                   0
                   #f))

  ;; read-unit! : input-port? -> (or/c source-unit? eof-object?)
  ;;   Consume one source unit, first using the scanner's one-unit boundary cache.
  (define (read-unit! in)
    (cond
      [pending-unit
       (define result
         pending-unit)
       (set! pending-unit #f)
       result]
      [else
       (source-read-unit! reader in)]))

  ;; unread-unit! : source-unit? -> void?
  ;;   Retain the one lexical boundary unit read ahead by a bounded scanner.
  (define (unread-unit! unit)
    (when pending-unit
      (error 'make-asm-derived-lexer "scanner boundary cache already occupied"))
    (set! pending-unit unit))

  ;; units-bytes : (listof source-unit?) -> immutable-bytes?
  ;;   Concatenate the bounded raw fragments owned by one derived token.
  (define (units-bytes units)
    (bytes->immutable-bytes
     (apply bytes-append
            (map source-unit-raw-bytes units))))

  ;; units-text : (listof source-unit?) -> string?
  ;;   Concatenate decoded text associated with a bounded token fragment.
  (define (units-text units)
    (apply string-append
           (map source-unit-text units)))

  ;; units-have-newline? : (listof source-unit?) -> boolean?
  ;;   Determine whether token text contains a physical line terminator.
  (define (units-have-newline? units)
    (for/or ([unit (in-list units)])
      (define ch
        (source-unit-character unit))
      (or (char=? ch #\return)
          (char=? ch #\newline))))

  ;; make-token : (listof source-unit?) symbol? (listof symbol?) ... -> asm-derived-token?
  ;;   Build one lossless token and attach source-position and profile context.
  (define (make-token units category tags
                      #:continuation-id [continuation-id #f]
                      #:diagnostics     [diagnostics '()])
    (define first-unit
      (car units))
    (define last-unit
      (last units))
    (make-derived-token category
                        (units-text units)
                        (append tags (profile-state-tags spec state))
                        (source-unit-start first-unit)
                        (source-unit-end last-unit)
                        (source-span (source-span-start
                                      (source-unit-byte-span first-unit))
                                     (source-span-end
                                      (source-unit-byte-span last-unit)))
                        (units-bytes units)
                        continuation-id
                        (make-effective-state dialect
                                              (source-unit-encoding first-unit)
                                              (scanner-state-line-start? state))
                        diagnostics))

  ;; make-malformed-token : source-unit? -> asm-derived-token?
  ;;   Preserve a malformed encoding unit and report it as a recoverable issue.
  (define (make-malformed-token unit)
    (make-token (list unit)
                'unknown
                '(asm-invalid-encoding malformed-token)
                #:diagnostics
                (list (make-diagnostic 'invalid-encoding
                                       'warning
                                       "The source bytes do not form a valid encoded character."
                                       (source-unit-byte-span unit)))))

  ;; word-start? : source-unit? -> boolean?
  ;;   Determine whether a code unit can begin an identifier, literal, or directive.
  (define (word-start? unit)
    (define ch
      (source-unit-character unit))
    (or (base-word-character? ch)
        (member ch (asm-dialect-spec-word-prefixes spec))))

  ;; word-follow? : char? char? -> boolean?
  ;;   Determine whether a unit continues the bounded word introduced by first.
  (define (word-follow? first next)
    (or (base-word-character? next)
        (and (char=? first #\%)
             (char=? next #\%))))

  ;; word-boundary-colon? : -> boolean?
  ;;   Inspect the cached word boundary without consuming a label delimiter.
  (define (word-boundary-colon?)
    (and pending-unit
         (not (source-unit-malformed? pending-unit))
         (char=? (source-unit-character pending-unit) #\:)))

  ;; numeric-word? : string? -> boolean?
  ;;   Recognize common decimal, hexadecimal, and binary literal spellings.
  (define (numeric-word? text)
    (or (regexp-match? #px"^[0-9][0-9a-fA-F_]*[hH]?$" text)
        (regexp-match? #px"^\\$[0-9a-fA-F_]+$" text)
        (regexp-match? #px"^%[01_]+$" text)
        (regexp-match? #px"^0[xX][0-9a-fA-F_]+$" text)))

  ;; conditional-open? : symbol? -> boolean?
  ;;   Recognize a directive that introduces a conservative conditional region.
  (define (conditional-open? word)
    (or (member word '(.if .ifdef .ifndef .ifblank .ifnblank !if !ifdef !ifndef
                         %if %ifdef %ifndef))
        (and (eq? dialect 'vasm)
             (member word '(if if1 if2 ifb ifc ifd ifeq ifge ifgt ifle iflt
                              ifmacrod ifmacrond ifnb ifnc ifnd ifne ifp1)))))

  ;; conditional-close? : symbol? -> boolean?
  ;;   Recognize a directive that ends a conditional region.
  (define (conditional-close? word)
    (or (member word '(.endif !endif %endif))
        (and (eq? dialect 'vasm)
             (eq? word 'endif))))

  ;; update-directive-state! : string? -> void?
  ;;   Apply only local, lexical setting changes outside macros and conditionals.
  (define (update-directive-state! text)
    (define word
      (string->symbol (string-downcase text)))
    (cond
      [(conditional-open? word)
       (when (< (scanner-state-conditional-depth state) maximum-context-depth)
         (set-scanner-state-conditional-depth!
          state
          (add1 (scanner-state-conditional-depth state))))]
      [(conditional-close? word)
       (when (positive? (scanner-state-conditional-depth state))
         (set-scanner-state-conditional-depth!
          state
          (sub1 (scanner-state-conditional-depth state))))]
      [(asm-word-macro-open? spec text)
       (when (< (scanner-state-macro-depth state) maximum-context-depth)
         (set-scanner-state-macro-depth!
          state
          (add1 (scanner-state-macro-depth state))))]
      [(asm-word-macro-close? spec text)
       (when (positive? (scanner-state-macro-depth state))
         (set-scanner-state-macro-depth!
          state
          (sub1 (scanner-state-macro-depth state))))]
      [(or (positive? (scanner-state-macro-depth state))
           (positive? (scanner-state-conditional-depth state)))
       (void)]
      [(and (eq? dialect 'ca65)
            (eq? word '.feature))
       (set-scanner-state-pending-directive! state 'ca65-feature)]
      [(and (eq? dialect 'gas)
            (eq? architecture 'x86)
            (eq? word '.intel_syntax))
       (set-scanner-state-syntax! state 'intel)]
      [(and (eq? dialect 'gas)
            (eq? architecture 'x86)
            (eq? word '.att_syntax))
       (set-scanner-state-syntax! state 'att)]
      [(and (eq? dialect 'gas)
            (eq? architecture 'x86)
            (member word '(.code16 .code16gcc)))
       (set-scanner-state-cpu-mode! state 'code16)]
      [(and (eq? dialect 'gas)
            (eq? architecture 'x86)
            (eq? word '.code32))
       (set-scanner-state-cpu-mode! state 'code32)]
      [(and (eq? dialect 'gas)
            (eq? architecture 'x86)
            (eq? word '.code64))
       (set-scanner-state-cpu-mode! state 'code64)]
      [(and (eq? dialect 'gas)
            (eq? architecture 'arm)
            (eq? word '.syntax))
       (set-scanner-state-pending-directive! state 'gas-arm-syntax)]
      [(and (eq? dialect 'gas)
            (eq? architecture 'arm)
            (eq? word '.arm))
       (set-scanner-state-cpu-mode! state 'arm)]
      [(and (eq? dialect 'gas)
            (eq? architecture 'arm)
            (eq? word '.thumb))
       (set-scanner-state-cpu-mode! state 'thumb)]
      [(and (eq? dialect 'nasm)
            (eq? word 'bits))
       (set-scanner-state-pending-directive! state 'nasm-bits)]
      [(and (eq? dialect '|64tass|)
            (eq? word '.namespace))
       (when (< (scanner-state-namespace-depth state) maximum-context-depth)
         (set-scanner-state-namespace-depth!
          state
          (add1 (scanner-state-namespace-depth state))))]
      [(and (eq? dialect '|64tass|)
            (eq? word '.endnamespace))
       (when (positive? (scanner-state-namespace-depth state))
         (set-scanner-state-namespace-depth!
          state
          (sub1 (scanner-state-namespace-depth state))))]
      [(and (eq? dialect '|64tass|)
            (eq? word '.enc))
       (set-scanner-state-pending-directive! state 'tass-encoding)]
      [else
       (void)]))

  ;; update-pending-directive! : string? -> void?
  ;;   Consume one setting token expected by a preceding lexical directive.
  (define (update-pending-directive! text)
    (define pending
      (scanner-state-pending-directive state))
    (when pending
      (define word
        (string->symbol (string-downcase text)))
      (case pending
        [(ca65-feature)
         (when (eq? word 'c_comments)
           (set-scanner-state-c-comments?! state #t))]
        [(gas-arm-syntax)
         (when (memq word '(unified divided))
           (set-scanner-state-syntax! state word))]
        [(nasm-bits)
         (define maybe-mode
           (string->number text))
         (case maybe-mode
           [(16)
            (set-scanner-state-cpu-mode! state 'bits16)]
           [(32)
            (set-scanner-state-cpu-mode! state 'bits32)]
           [(64)
            (set-scanner-state-cpu-mode! state 'bits64)]
           [else
            (void)])]
        [(tass-encoding)
         (set-scanner-state-emitted-encoding?! state #t)]
        [else
         (void)])
      (set-scanner-state-pending-directive! state #f)))

  ;; update-delimiter-state! : char? -> void?
  ;;   Track bounded Kick script braces independently from macro and conditional state.
  (define (update-delimiter-state! ch)
    (when (eq? dialect 'kick)
      (case ch
        [(#\{)
         (when (< (scanner-state-script-depth state) maximum-context-depth)
           (set-scanner-state-script-depth!
            state
            (add1 (scanner-state-script-depth state))))]
        [(#\})
         (when (positive? (scanner-state-script-depth state))
           (set-scanner-state-script-depth!
            state
            (sub1 (scanner-state-script-depth state))))]
        [else
         (void)])))

  ;; word-tags : string? -> (values symbol? (listof symbol?))
  ;;   Assign a syntax role to a complete bounded word in the selected profile.
  (define (word-tags text)
    (define word
      (string->symbol (string-downcase text)))
    (cond
      [(asm-word-macro-open? spec text)
       (values 'keyword
               (list 'keyword 'asm-directive 'asm-macro-open
                     (profile-tag spec "macro-open")))]
      [(asm-word-macro-close? spec text)
       (values 'keyword
               (list 'keyword 'asm-directive 'asm-macro-close
                     (profile-tag spec "macro-close")))]
      [(asm-word-directive? spec text)
       (values 'keyword
               (append '(keyword asm-directive)
                       (cond
                         [(or (conditional-open? word)
                              (conditional-close? word)
                              (member word '(.else !else %else))
                              (and (eq? dialect 'vasm)
                                   (eq? word 'else)))
                          '(asm-conditional-directive)]
                         [else '()])
                       (list (profile-tag spec "directive"))))]
      [(asm-word-register? spec text)
       (values 'keyword
               (list 'keyword 'asm-register
                     (profile-tag spec "register")))]
      [(asm-word-instruction? spec text)
       (values 'keyword
               (list 'keyword 'asm-instruction
                     (profile-tag spec "instruction")))]
      [(and (eq? dialect 'acme)
            (string-prefix? text "+"))
       (values 'identifier '(identifier acme-macro-call))]
      [(and (eq? dialect 'kick)
            (regexp-match? #px"^#(?:\\$[0-9a-fA-F_]+|[0-9][0-9a-fA-F_]*)$"
                           text))
       (values 'literal '(literal asm-immediate-literal kick-immediate-literal))]
      [(and (eq? dialect 'nasm)
            (regexp-match? #px"^%[0-9]+$" text))
       (values 'identifier '(identifier nasm-macro-parameter))]
      [(and (eq? dialect 'nasm)
            (string=? text "%"))
       (values 'operator '(operator nasm-token-concatenation))]
      [(and (eq? dialect 'nasm)
            (eq? (scanner-state-pending-directive state) 'nasm-bits)
            (member text '("16" "32" "64")))
       (values 'literal '(literal asm-numeric-literal nasm-bits-mode))]
      [(and (eq? dialect 'gas)
            (regexp-match? #px"^[0-9]+$" text)
            (word-boundary-colon?))
       (values 'identifier
               '(identifier asm-label-definition gas-numeric-local-label))]
      [(and (eq? dialect 'gas)
            (eq? architecture 'aarch64)
            (regexp-match? #px"^(?:lo12|got|got_lo12|page|pageoff|prel_pg_hi21)$"
                           (string-downcase text))
            (word-boundary-colon?))
       (values 'identifier '(identifier gas-relocation-modifier))]
      [(numeric-word? text)
       (values 'literal '(literal asm-numeric-literal))]
      [(and (eq? dialect 'gas)
            (regexp-match? #px"^[0-9]+[fb]$" text))
       (values 'identifier '(identifier asm-numeric-local-reference))]
      [(word-boundary-colon?)
       (values 'identifier
               (append '(identifier asm-label-definition)
                       (cond
                         [(and (eq? dialect 'ca65)
                               (positive? (string-length text))
                               (memv (string-ref text 0) '(#\@ #\?)))
                          '(ca65-cheap-local)]
                         [(and (eq? dialect 'kick)
                               (string-prefix? text "!"))
                          '(kick-anonymous-label)]
                         [(and (eq? dialect 'nasm)
                               (string-prefix? text "%%"))
                          '(nasm-macro-local)]
                         [else '()])))]
      [else
       (values 'identifier '(identifier asm-symbol))]))

  ;; scan-word : input-port? source-unit? -> asm-derived-token?
  ;;   Read a bounded complete word, retaining its delimiter as scanner lookahead.
  (define (scan-word in first-unit)
    (define first-character
      (source-unit-character first-unit))
    (define units-reversed
      (list first-unit))
    (define byte-count
      (bytes-length (source-unit-raw-bytes first-unit)))
    (define continuation-id
      (scanner-state-word-continuation-id state))
    (define continues?
      #f)
    (let loop ()
      ;; Do not make first visible output wait for a word delimiter on a live
      ;; pipe.  A future batch begins a new neutral symbol fragment instead.
      (cond
        [(not (char-ready? in))
         (set-scanner-state-word-continuation-id! state #f)]
        [else
         (define next-unit
           (read-unit! in))
         (cond
           [(eof-object? next-unit)
            (set-scanner-state-word-continuation-id! state #f)]
           [(and (not (source-unit-malformed? next-unit))
                 (word-follow? first-character
                               (source-unit-character next-unit)))
            (define next-length
              (bytes-length (source-unit-raw-bytes next-unit)))
            (cond
              [(<= (+ byte-count next-length) fragment-cap)
               (set! units-reversed (cons next-unit units-reversed))
               (set! byte-count (+ byte-count next-length))
               (loop)]
              [else
               (unread-unit! next-unit)
               (set! continues? #t)
               (unless continuation-id
                 (set! continuation-id next-continuation-id)
                 (set! next-continuation-id (add1 next-continuation-id)))
               (set-scanner-state-word-continuation-id! state continuation-id)])]
           [else
            (unread-unit! next-unit)
            (set-scanner-state-word-continuation-id! state #f)])]))
    (define units
      (reverse units-reversed))
    (define text
      (units-text units))
    (define-values (category tags)
      (word-tags text))
    ;; Settings apply after the directive or setting has received its own tags.
    (cond
      [(scanner-state-pending-directive state)
       (update-pending-directive! text)]
      [else
       (update-directive-state! text)])
    (make-token units category tags
                #:continuation-id (and (or continues? continuation-id)
                                       continuation-id)))

  ;; line-comment-start? : char? -> boolean?
  ;;   Determine whether a character starts a profile-specific line comment.
  (define (line-comment-start? ch)
    (or (member ch (asm-dialect-spec-line-comments spec))
        (and (memq dialect '(native-motorola asmone devpac vasm))
             (scanner-state-line-start? state)
             (char=? ch #\*)) ))

  ;; basic-delimiter-token : source-unit? -> asm-derived-token?
  ;;   Classify ordinary punctuation without consuming profile-specific lookahead.
  (define (basic-delimiter-token unit)
    (define ch
      (source-unit-character unit))
    (define tags
      (cond
        [(and (char=? ch #\:)
              (eq? dialect 'ca65)
              (scanner-state-line-start? state))
         '(identifier asm-anonymous-label-definition ca65-anonymous-label)]
        [(and (char=? ch #\;)
              (asm-dialect-spec-semicolon-delimiter? spec))
         '(delimiter asm-statement-separator)]
        [(memv ch '(#\: #\, #\( #\) #\[ #\] #\{ #\}))
         '(delimiter asm-punctuation)]
        [(memv ch '(#\+ #\- #\* #\/ #\% #\= #\< #\> #\& #\| #\^ #\~
                    #\! #\; #\# #\\))
         '(operator asm-operator)]
        [else
         '(unknown asm-unclassified)]))
    (define category
      (cond
        [(member 'identifier tags) 'identifier]
        [(member 'delimiter tags)  'delimiter]
        [(member 'operator tags)   'operator]
        [else                      'unknown]))
    (make-token (list unit) category tags))

  ;; delimiter-token : input-port? source-unit? -> asm-derived-token?
  ;;   Recognize punctuation forms that need one unit of profile-specific lookahead.
  (define (delimiter-token in unit)
    (define ch
      (source-unit-character unit))
    (cond
      [(and (char=? ch #\:)
            (eq? dialect 'ca65)
            (not (scanner-state-line-start? state))
            (char-ready? in))
       (define next-unit
         (read-unit! in))
       (cond
         [(and (source-unit? next-unit)
               (not (source-unit-malformed? next-unit))
               (memv (source-unit-character next-unit) '(#\+ #\- #\< #\>)))
          (make-token (list unit next-unit)
                      'identifier
                      '(identifier asm-anonymous-label-reference
                                   ca65-anonymous-label
                                   ca65-anonymous-label-reference))]
         [(eof-object? next-unit)
          (basic-delimiter-token unit)]
         [else
          (unread-unit! next-unit)
          (basic-delimiter-token unit)])]
      [else
       (basic-delimiter-token unit)]))

  ;; scan-next : input-port? -> (or/c asm-derived-token? 'eof)
  ;;   Produce one bounded token while preserving scanner state for later calls.
  (define (scan-next in)
    (define unit
      (read-unit! in))
    (cond
      [(eof-object? unit)
       'eof]
      [(source-unit-malformed? unit)
       (make-malformed-token unit)]
      [(source-unit-bom? unit)
       (make-token (list unit) 'unknown '(asm-byte-order-mark))]
      [(eq? (scanner-state-mode state) 'line-comment)
       (define token
         (make-token (list unit)
                     'comment
                     (list 'comment 'asm-line-comment
                           (profile-tag spec "comment"))))
       (when (units-have-newline? (list unit))
         (set-scanner-state-mode! state 'code)
         (set-scanner-state-line-start?! state #t)
         (set-scanner-state-pending-directive! state #f))
       token]
      [(eq? (scanner-state-mode state) 'block-comment)
       (define ch
         (source-unit-character unit))
       (cond
         [(char=? ch #\*)
          (define maybe-close
            (read-unit! in))
          (cond
            [(and (source-unit? maybe-close)
                  (not (source-unit-malformed? maybe-close))
                  (char=? (source-unit-character maybe-close) #\/))
             (set-scanner-state-mode! state 'code)
             (make-token (list unit maybe-close)
                         'comment
                         (list 'comment 'asm-block-comment
                               (profile-tag spec "comment")))]
            [(eof-object? maybe-close)
             (set-scanner-state-mode! state 'code)
             (make-token (list unit)
                         'comment
                         (list 'comment 'asm-block-comment)
                         #:diagnostics
                         (list (make-diagnostic 'unterminated-comment
                                                'warning
                                                "The block comment ends at end of input."
                                                (source-unit-byte-span unit))))]
            [else
             (unread-unit! maybe-close)
             (make-token (list unit)
                         'comment
                         (list 'comment 'asm-block-comment
                               (profile-tag spec "comment")))])]
         [else
          (make-token (list unit)
                      'comment
                      (list 'comment 'asm-block-comment
                            (profile-tag spec "comment")))])]
      [(eq? (scanner-state-mode state) 'string)
       (define ch
         (source-unit-character unit))
       (define terminator?
         (char=? ch (scanner-state-string-delimiter state)))
       (define newline?
         (or (char=? ch #\return)
             (char=? ch #\newline)))
       (when (or terminator? newline?)
         (set-scanner-state-mode! state 'code)
         (when newline?
           (set-scanner-state-line-start?! state #t)
           (set-scanner-state-pending-directive! state #f)))
       (make-token (list unit)
                   'literal
                   '(literal asm-string-literal)
                   #:diagnostics
                   (if (and newline? (not terminator?))
                       (list (make-diagnostic 'unterminated-string
                                              'warning
                                              "The string literal ends at a newline."
                                              (source-unit-byte-span unit)))
                       '()))]
      [else
       (define ch
         (source-unit-character unit))
       (cond
         [(char-whitespace? ch)
          (define token
            (make-token (list unit) 'whitespace '(whitespace)))
          (when (units-have-newline? (list unit))
            (set-scanner-state-line-start?! state #t)
            (set-scanner-state-pending-directive! state #f))
          token]
         [(line-comment-start? ch)
          (set-scanner-state-mode! state 'line-comment)
          (make-token (list unit)
                      'comment
                      (list 'comment 'asm-line-comment
                            (profile-tag spec "comment")))]
         [(and (scanner-state-c-comments? state)
               (char=? ch #\/))
          (cond
            [(not (char-ready? in))
             (delimiter-token in unit)]
            [else
             (define next-unit
               (read-unit! in))
             (cond
               [(and (source-unit? next-unit)
                     (not (source-unit-malformed? next-unit))
                     (char=? (source-unit-character next-unit) #\/))
                (set-scanner-state-mode! state 'line-comment)
                (make-token (list unit next-unit)
                            'comment
                            (list 'comment 'asm-line-comment
                                  (profile-tag spec "comment")))]
               [(and (source-unit? next-unit)
                     (not (source-unit-malformed? next-unit))
                     (char=? (source-unit-character next-unit) #\*))
                (set-scanner-state-mode! state 'block-comment)
                (make-token (list unit next-unit)
                            'comment
                            (list 'comment 'asm-block-comment
                                  (profile-tag spec "comment")))]
               [(eof-object? next-unit)
                (delimiter-token in unit)]
               [else
                (unread-unit! next-unit)
                (delimiter-token in unit)])])]
         [(memv ch '(#\" #\'))
          (set-scanner-state-mode! state 'string)
          (set-scanner-state-string-delimiter! state ch)
          (when (eq? (scanner-state-pending-directive state) 'tass-encoding)
            (set-scanner-state-emitted-encoding?! state #t)
            (set-scanner-state-pending-directive! state #f))
          (make-token (list unit) 'literal '(literal asm-string-delimiter))]
         [(word-start? unit)
          (define token
            (scan-word in unit))
          (set-scanner-state-line-start?! state #f)
          token]
         [else
          (define token
            (delimiter-token in unit))
          (update-delimiter-state! ch)
          (set-scanner-state-line-start?! state #f)
          token])]))

  (lambda (in)
    (scan-next in)))
