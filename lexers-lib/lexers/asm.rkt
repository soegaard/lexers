#lang racket/base

;;;
;;; Assembly Lexer
;;;

;; Byte-preserving, streaming entry points for the conservative generic
;; assembly-source lexer.  Dialect-specific lexical rules intentionally live
;; in later profiles rather than being guessed from common punctuation.

;; make-asm-lexer                  : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a projected lexer for generic assembly source on a binary port.
;; make-asm-derived-lexer          : keyword-arguments -> (input-port? -> (or/c asm-derived-token? 'eof))
;;   Construct a byte-preserving derived lexer for generic assembly source.
;; asm-derived-token?              : any/c -> boolean?
;;   Recognize a derived assembly token.
;; asm-derived-token-category      : asm-derived-token? -> symbol?
;;   Extract the common projected category for a derived token.
;; asm-derived-token-text          : asm-derived-token? -> string?
;;   Extract decoded text for one token, using U+FFFD for malformed bytes.
;; asm-derived-token-tags          : asm-derived-token? -> (listof symbol?)
;;   Extract syntax-role tags for one derived token.
;; asm-derived-token-has-tag?      : asm-derived-token? symbol? -> boolean?
;;   Determine whether a derived token carries a syntax-role tag.
;; asm-derived-token-start         : asm-derived-token? -> position?
;;   Extract a token's parser-tools-compatible starting position.
;; asm-derived-token-end           : asm-derived-token? -> position?
;;   Extract a token's parser-tools-compatible ending position.
;; asm-derived-token-byte-span     : asm-derived-token? -> source-span?
;;   Extract the authoritative zero-based, half-open byte span.
;; asm-byte-span?                  : any/c -> boolean?
;;   Recognize a byte span returned by the assembly derived-token API.
;; asm-byte-span-start             : asm-byte-span? -> exact-nonnegative-integer?
;;   Extract the inclusive starting offset of an assembly byte span.
;; asm-byte-span-end               : asm-byte-span? -> exact-nonnegative-integer?
;;   Extract the exclusive ending offset of an assembly byte span.
;; asm-derived-token-raw-bytes     : asm-derived-token? -> immutable-bytes?
;;   Extract the exact source bytes owned by a token.
;; asm-derived-token-continuation-id : asm-derived-token? -> (or/c exact-positive-integer? #f)
;;   Extract the identity shared by fragments of one capped logical token.
;; asm-derived-token-effective-state : asm-derived-token? -> asm-effective-state?
;;   Extract the lexical state effective for one token.
;; asm-derived-token-diagnostics   : asm-derived-token? -> (listof asm-diagnostic?)
;;   Extract bounded diagnostics attached to one token.
;; asm-string->tokens              : string? keyword-arguments -> (listof token-like?)
;;   Tokenize UTF-8 string input through the projected API.
;; asm-string->derived-tokens      : string? keyword-arguments -> (listof asm-derived-token?)
;;   Tokenize UTF-8 string input through the byte-preserving derived API.
;; asm-profiles                    : immutable-hash?
;;   Describe implemented and recognized-but-unavailable assembly profiles.
;; asm-profile?                    : any/c -> boolean?
;;   Recognize an assembly profile registry entry.
;; asm-effective-state?            : any/c -> boolean?
;;   Recognize a generic assembly lexical-state snapshot.
;; asm-diagnostic?                 : any/c -> boolean?
;;   Recognize one recoverable assembly lexer diagnostic.

(provide make-asm-lexer
         make-asm-derived-lexer
         asm-derived-token?
         asm-derived-token-category
         asm-derived-token-text
         asm-derived-token-tags
         asm-derived-token-has-tag?
         asm-derived-token-start
         asm-derived-token-end
         asm-derived-token-byte-span
         asm-byte-span?
         asm-byte-span-start
         asm-byte-span-end
         asm-derived-token-raw-bytes
         asm-derived-token-continuation-id
         asm-derived-token-effective-state
         asm-derived-token-diagnostics
         asm-string->tokens
         asm-string->derived-tokens
         (struct-out asm-profile)
         asm-profiles
         (struct-out asm-effective-state)
         (struct-out asm-diagnostic))

(require parser-tools/lex
         racket/list
         "private/parser-tools-compat.rkt"
         "private/source.rkt"
         "token.rkt")

;; A profile record separates implementation status from dialect identity.
(struct asm-profile (name status architecture dialect description) #:transparent)

;; The lexical context recorded for each generic token.  Future dialects can
;; extend this value without conflating syntax with renderer color policy.
(struct asm-effective-state (dialect encoding line-start?) #:transparent)

;; A bounded recoverable issue associated with a source fragment.
(struct asm-diagnostic (code severity message byte-span) #:transparent)

;; A lossless generic token.  `continuation-id` is #f while every fragment is
;; independently bounded; future profiles may use it for larger logical forms.
(struct asm-derived-token (category text tags start end byte-span raw-bytes
                                   continuation-id effective-state diagnostics)
  #:transparent)

;; asm-byte-span? : any/c -> boolean?
;;   Recognize a byte span returned by the assembly derived-token API.
(define asm-byte-span?
  source-span?)

;; asm-byte-span-start : asm-byte-span? -> exact-nonnegative-integer?
;;   Extract the inclusive starting byte offset of an assembly source span.
(define asm-byte-span-start
  source-span-start)

;; asm-byte-span-end : asm-byte-span? -> exact-nonnegative-integer?
;;   Extract the exclusive ending byte offset of an assembly source span.
(define asm-byte-span-end
  source-span-end)

;; The assembly-profile registry.  Only generic is a selectable lexer today;
;; the remaining entries retain useful identities without pretending to parse
;; an unavailable dialect as a neighbouring grammar.
(define asm-profiles
  (hasheq 'generic
          (asm-profile 'generic
                       'implemented
                       #f
                       'generic
                       "Conservative source-preserving assembly syntax.")
          'ca65
          (asm-profile 'ca65
                       'recognized-only
                       'mos-6502
                       'ca65
                       "ca65 requires a dialect-specific lexer profile.")
          'acme
          (asm-profile 'acme
                       'recognized-only
                       'mos-6502
                       'acme
                       "ACME requires a dialect-specific lexer profile.")
          'native-motorola
          (asm-profile 'native-motorola
                       'recognized-only
                       'm68k
                       'native-motorola
                       "Native Motorola syntax requires a lexer profile.")
          'kick
          (asm-profile 'kick
                       'recognized-only
                       'mos-6502
                       'kick
                       "Kick Assembler requires a lexer profile.")
          '64tass
          (asm-profile '64tass
                       'recognized-only
                       'mos-6502
                       '64tass
                       "64tass requires a lexer profile.")
          'gas
          (asm-profile 'gas
                       'recognized-only
                       #f
                       'gas
                       "GNU assembler requires a target-specific lexer profile.")
          'nasm
          (asm-profile 'nasm
                       'recognized-only
                       'x86
                       'nasm
                       "NASM requires a dialect-specific lexer profile.")))

;; Default settings for the ordinary consumer profiles.
(define asm-lexer-profile-defaults
  (hasheq 'coloring (hasheq 'trivia 'keep
                            'source-positions #t
                            'errors 'recover)
          'compiler (hasheq 'trivia 'skip
                            'source-positions #t
                            'errors 'raise)))

;; The resolved settings for one projected assembly reader.
(struct asm-config (profile trivia source-positions errors dialect encoding fragment-cap)
  #:transparent)

;; implemented-dialect? : symbol? -> boolean?
;;   Determine whether a dialect has a real lexer implementation.
(define (implemented-dialect? dialect)
  (define candidate
    (hash-ref asm-profiles dialect #f))
  (and candidate
       (eq? (asm-profile-status candidate) 'implemented)))

;; validate-dialect : symbol? -> symbol?
;;   Reject unimplemented dialects instead of silently substituting a grammar.
(define (validate-dialect dialect who)
  (cond
    [(not (symbol? dialect))
     (raise-argument-error who "symbol?" dialect)]
    [(implemented-dialect? dialect)
     dialect]
    [(hash-has-key? asm-profiles dialect)
     (raise-arguments-error who
                            "the requested assembly dialect is recognized but unavailable"
                            "dialect" dialect)]
    [else
     (raise-arguments-error who
                            "unknown assembly dialect"
                            "dialect" dialect)]))

;; validate-fragment-cap : exact-positive-integer? -> exact-positive-integer?
;;   Require a cap that can hold the largest supported decoded scalar value.
(define (validate-fragment-cap fragment-cap who)
  (cond
    [(not (exact-positive-integer? fragment-cap))
     (raise-argument-error who "exact-positive-integer?" fragment-cap)]
    [(< fragment-cap 4)
     (raise-arguments-error who
                            "fragment caps must hold one four-byte UTF-8 scalar"
                            "fragment cap" fragment-cap)]
    [(> fragment-cap 16384)
     (raise-arguments-error who
                            "fragment caps must be at most 16384 bytes"
                            "fragment cap" fragment-cap)]
    [else
     fragment-cap]))

;; make-asm-config : keyword-arguments -> asm-config?
;;   Resolve the generic lexer consumer profile and byte-source options.
(define (make-asm-config #:profile          [profile 'coloring]
                         #:trivia           [trivia 'profile-default]
                         #:source-positions [source-positions 'profile-default]
                         #:dialect          [dialect 'generic]
                         #:encoding         [encoding 'auto]
                         #:fragment-cap     [fragment-cap 16384])
  (unless (hash-has-key? asm-lexer-profile-defaults profile)
    (raise-arguments-error 'make-asm-lexer
                           "unknown lexer profile"
                           "profile" profile))
  (define defaults
    (hash-ref asm-lexer-profile-defaults profile))
  (define resolved-trivia
    (cond
      [(eq? trivia 'profile-default)
       (hash-ref defaults 'trivia)]
      [(memq trivia '(keep skip))
       trivia]
      [else
       (raise-argument-error 'make-asm-lexer
                             "(or/c 'profile-default 'keep 'skip)"
                             trivia)]))
  (define resolved-positions
    (cond
      [(eq? source-positions 'profile-default)
       (hash-ref defaults 'source-positions)]
      [(boolean? source-positions)
       source-positions]
      [else
       (raise-argument-error 'make-asm-lexer
                             "(or/c 'profile-default boolean?)"
                             source-positions)]))
  (asm-config profile
              resolved-trivia
              resolved-positions
              (hash-ref defaults 'errors)
              (validate-dialect dialect 'make-asm-lexer)
              (validate-asm-encoding encoding 'make-asm-lexer)
              (validate-fragment-cap fragment-cap 'make-asm-lexer)))

;; validate-asm-encoding : symbol? symbol? -> symbol?
;;   Validate the encoding names exposed by the public assembly API.
(define (validate-asm-encoding encoding who)
  (case encoding
    [(auto utf-8 utf-16le utf-16be) encoding]
    [else
     (raise-argument-error who
                           "(or/c 'auto 'utf-8 'utf-16le 'utf-16be)"
                           encoding)]))

;; symbol-character? : char? -> boolean?
;;   Recognize a conservative one-character assembly symbol fragment.
(define (symbol-character? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (memv ch '(#\_ #\. #\$ #\@ #\?))))

;; delimiter-character? : char? -> boolean?
;;   Recognize punctuation that is structurally delimited across common syntaxes.
(define (delimiter-character? ch)
  (memv ch '(#\: #\, #\( #\) #\[ #\] #\{ #\} #\" #\')))

;; operator-character? : char? -> boolean?
;;   Recognize punctuation without assuming that it starts a dialect comment.
(define (operator-character? ch)
  (memv ch '(#\+ #\- #\* #\/ #\% #\= #\< #\> #\& #\| #\^ #\~
             #\! #\; #\# #\\)))

;; classify-unit : source-unit? -> (values symbol? (listof symbol?))
;;   Project one decoded scalar conservatively without claiming a dialect.
(define (classify-unit unit)
  (define text
    (source-unit-text unit))
  (define ch
    (string-ref text 0))
  (cond
    [(source-unit-bom? unit)
     (values 'unknown '(asm-byte-order-mark))]
    [(source-unit-malformed? unit)
     (values 'unknown '(asm-invalid-encoding malformed-token))]
    [(char-whitespace? ch)
     (values 'whitespace '(whitespace))]
    [(symbol-character? ch)
     (values 'identifier '(identifier asm-symbol))]
    [(delimiter-character? ch)
     (values 'delimiter '(delimiter asm-punctuation))]
    [(operator-character? ch)
     (values 'operator '(operator asm-operator))]
    [else
     (values 'unknown '(asm-unclassified))]))

;; make-unit-diagnostic : source-unit? -> (listof asm-diagnostic?)
;;   Attach a recoverable decoding diagnostic only where source bytes require it.
(define (make-unit-diagnostic unit)
  (cond
    [(source-unit-malformed? unit)
     (list (asm-diagnostic 'invalid-encoding
                           'warning
                           "The source bytes do not form a valid encoded character."
                           (source-unit-byte-span unit)))]
    [else
     '()]))

;; make-asm-derived-lexer : keyword-arguments -> (input-port? -> (or/c asm-derived-token? 'eof))
;;   Construct a streaming, byte-preserving reader for conservative assembly.
(define (make-asm-derived-lexer #:dialect      [dialect 'generic]
                                #:encoding     [encoding 'auto]
                                #:fragment-cap [fragment-cap 16384])
  (define resolved-dialect
    (validate-dialect dialect 'make-asm-derived-lexer))
  (define resolved-encoding
    (validate-asm-encoding encoding 'make-asm-derived-lexer))
  (validate-fragment-cap fragment-cap 'make-asm-derived-lexer)
  (define reader
    (make-source-reader #:encoding resolved-encoding))
  (lambda (in)
    (define unit
      (source-read-unit! reader in))
    (cond
      [(eof-object? unit)
       'eof]
      [else
       (define-values (category tags)
         (classify-unit unit))
       (define effective-state
         (asm-effective-state resolved-dialect
                              (source-unit-encoding unit)
                              (zero? (position-col (source-unit-start unit)))))
       (asm-derived-token category
                          (source-unit-text unit)
                          tags
                          (source-unit-start unit)
                          (source-unit-end unit)
                          (source-unit-byte-span unit)
                          (source-unit-raw-bytes unit)
                          #f
                          effective-state
                          (make-unit-diagnostic unit))])))

;; project-derived-token : (or/c asm-derived-token? 'eof) asm-config? -> (or/c token-like? #f)
;;   Convert one derived token into the parser-tools-compatible public stream.
(define (project-derived-token token config)
  (cond
    [(eq? token 'eof)
     'eof]
    [(and (eq? (asm-config-trivia config) 'skip)
          (eq? (asm-derived-token-category token) 'whitespace))
     #f]
    [(and (eq? (asm-config-errors config) 'raise)
          (pair? (asm-derived-token-diagnostics token)))
     (raise-arguments-error 'make-asm-lexer
                            "malformed encoded source"
                            "byte span" (asm-derived-token-byte-span token))]
    [else
     (define projected
       (make-stream-token (asm-derived-token-category token)
                          (asm-derived-token-text token)))
     (wrap-token-with-pos projected
                          (asm-derived-token-start token)
                          (asm-derived-token-end token)
                          (asm-config-source-positions config))]))

;; make-asm-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a parser-tools-compatible lexer over the derived byte stream.
(define (make-asm-lexer #:profile          [profile 'coloring]
                        #:trivia           [trivia 'profile-default]
                        #:source-positions [source-positions 'profile-default]
                        #:dialect          [dialect 'generic]
                        #:encoding         [encoding 'auto]
                        #:fragment-cap     [fragment-cap 16384])
  (define config
    (make-asm-config #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions
                     #:dialect          dialect
                     #:encoding         encoding
                     #:fragment-cap     fragment-cap))
  (define next-derived-token
    (make-asm-derived-lexer #:dialect      (asm-config-dialect config)
                            #:encoding     (asm-config-encoding config)
                            #:fragment-cap (asm-config-fragment-cap config)))
  (lambda (in)
    (let loop ()
      (define projected
        (project-derived-token (next-derived-token in) config))
      (cond
        [projected projected]
        [else      (loop)]))))

;; asm-derived-token-has-tag? : asm-derived-token? symbol? -> boolean?
;;   Determine whether a derived assembly token has a given syntax-role tag.
(define (asm-derived-token-has-tag? token tag)
  (and (member tag (asm-derived-token-tags token)) #t))

;; asm-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize a Racket string with UTF-8-relative byte provenance.
(define (asm-string->tokens source
                            #:profile          [profile 'coloring]
                            #:trivia           [trivia 'profile-default]
                            #:source-positions [source-positions 'profile-default]
                            #:dialect          [dialect 'generic]
                            #:fragment-cap     [fragment-cap 16384])
  (unless (string? source)
    (raise-argument-error 'asm-string->tokens "string?" source))
  (define lexer
    (make-asm-lexer #:profile          profile
                    #:trivia           trivia
                    #:source-positions source-positions
                    #:dialect          dialect
                    #:encoding         'utf-8
                    #:fragment-cap     fragment-cap))
  (define in
    (open-input-bytes (string->bytes/utf-8 source)))
  (let loop ([tokens '()])
    (define token
      (lexer in))
    (cond
      [(lexer-token-eof? token)
       (reverse (cons token tokens))]
      [else
       (loop (cons token tokens))])))

;; asm-string->derived-tokens : string? keyword-arguments -> (listof asm-derived-token?)
;;   Tokenize a Racket string with UTF-8-relative byte provenance.
(define (asm-string->derived-tokens source
                                    #:dialect      [dialect 'generic]
                                    #:fragment-cap [fragment-cap 16384])
  (unless (string? source)
    (raise-argument-error 'asm-string->derived-tokens "string?" source))
  (define lexer
    (make-asm-derived-lexer #:dialect      dialect
                            #:encoding     'utf-8
                            #:fragment-cap fragment-cap))
  (define in
    (open-input-bytes (string->bytes/utf-8 source)))
  (let loop ([tokens '()])
    (define token
      (lexer in))
    (cond
      [(eq? token 'eof)
       (reverse tokens)]
      [else
       (loop (cons token tokens))])))

(module+ test
  (require rackunit
           racket/list)

  ;; contiguous-byte-stream? : (listof asm-derived-token?) -> boolean?
  ;;   Determine whether derived token spans form one exact byte partition.
  (define (contiguous-byte-stream? tokens)
    (and (or (null? tokens)
             (zero? (source-span-start (asm-derived-token-byte-span (car tokens)))))
         (for/and ([left  (in-list tokens)]
                   [right (in-list (cdr tokens))])
           (= (source-span-end (asm-derived-token-byte-span left))
              (source-span-start (asm-derived-token-byte-span right))))))

  ;; collect-derived : bytes? keyword-arguments -> (listof asm-derived-token?)
  ;;   Read derived tokens from byte-authoritative input.
  (define (collect-derived source #:encoding [encoding 'auto])
    (define lexer
      (make-asm-derived-lexer #:encoding encoding))
    (define in
      (open-input-bytes source))
    (let loop ([tokens '()])
      (define token
        (lexer in))
      (cond
        [(eq? token 'eof)
         (reverse tokens)]
        [else
         (loop (cons token tokens))])))

  (define generic-source
    "entry: mov r0, #42 ; not a generic comment\r\n")
  (define generic-derived
    (asm-string->derived-tokens generic-source))
  (define generic-projected
    (asm-string->tokens generic-source #:source-positions #f))
  (define utf-16-derived
    (collect-derived #"\377\376l\0d\0a\0:\0\r\0\n\0"))
  (define malformed-derived
    (collect-derived #"a\303(b"))
  (define long-derived
    (asm-string->derived-tokens (make-string 20000 #\x)))
  (define semicolon-token
    (findf (lambda (token)
             (string=? (asm-derived-token-text token) ";"))
           generic-derived))
  (define first-token-channel
    (make-channel))
  (define-values (pipe-in pipe-out)
    (make-pipe))
  (define pipe-lexer
    (make-asm-derived-lexer))
  (void (write-bytes #"mov" pipe-out))
  (flush-output pipe-out)
  (void
   (thread
    (lambda ()
      (channel-put first-token-channel (pipe-lexer pipe-in)))))
  (define first-pipe-token
    (sync/timeout 1 first-token-channel))
  (close-output-port pipe-out)

  (check-true (contiguous-byte-stream? generic-derived))
  (check-equal? (apply bytes-append (map asm-derived-token-raw-bytes generic-derived))
                (string->bytes/utf-8 generic-source))
  (check-equal? (map lexer-token-name generic-projected)
                (append (map asm-derived-token-category generic-derived) '(eof)))
  (check-not-false semicolon-token)
  (check-equal? (asm-derived-token-category semicolon-token) 'operator)
  (check-false (asm-derived-token-has-tag? semicolon-token 'comment))
  (check-equal? (apply bytes-append (map asm-derived-token-raw-bytes utf-16-derived))
                #"\377\376l\0d\0a\0:\0\r\0\n\0")
  (check-true (asm-derived-token-has-tag? (car utf-16-derived) 'asm-byte-order-mark))
  (check-equal? (map asm-derived-token-text malformed-derived)
                '("a" "\uFFFD" "(" "b"))
  (check-true (pair? (asm-derived-token-diagnostics (cadr malformed-derived))))
  (check-equal? (apply bytes-append (map asm-derived-token-raw-bytes malformed-derived))
                #"a\303(b")
  (check-true (for/and ([token (in-list long-derived)])
                (<= (bytes-length (asm-derived-token-raw-bytes token)) 4)))
  (check-equal? (length long-derived) 20000)
  (check-not-false first-pipe-token)
  (check-equal? (asm-derived-token-text first-pipe-token) "m")
  (check-exn exn:fail?
             (lambda ()
               (define lexer
                 (make-asm-lexer #:profile 'compiler))
               (lexer (open-input-bytes #"\303"))))
  (check-exn exn:fail?
             (lambda ()
               (make-asm-derived-lexer #:dialect 'ca65))))
