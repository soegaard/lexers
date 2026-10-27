#lang racket/base

;;;
;;; Assembly Lexer
;;;

;; Byte-preserving, streaming entry points for generic and explicit assembly
;; profiles.  Each profile keeps its own comment, directive, label, and macro
;; rules instead of applying one punctuation grammar to every assembler.

;; make-asm-lexer                  : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a projected lexer for one explicit assembly profile on a binary port.
;; make-asm-derived-lexer          : keyword-arguments -> (input-port? -> (or/c asm-derived-token? 'eof))
;;   Construct a byte-preserving derived lexer for one explicit assembly profile.
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
;;   Describe implemented assembly profiles and their required selections.
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
         "private/asm/profiles.rkt"
         "private/asm/scanner.rkt"
         "token.rkt")

;; A profile record separates implementation status from dialect identity.
(struct asm-profile (name status architecture dialect description) #:transparent)

;; The lexical context effective for one token. Detailed roles are carried by
;; derived-token tags rather than renderer-specific styling fields.
(struct asm-effective-state (dialect encoding line-start?) #:transparent)

;; A bounded recoverable issue associated with a source fragment.
(struct asm-diagnostic (code severity message byte-span) #:transparent)

;; A lossless, bounded source fragment. Continuation identifiers connect the
;; pieces of one source word when the configured byte cap splits it.
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

;; The assembly-profile registry.  Each entry has its own lexical rules; a
;; profile is never silently replaced by a nearby assembly language.
(define asm-profiles
  (hasheq 'generic
          (asm-profile 'generic
                       'implemented
                       #f
                       'generic
                       "Conservative source-preserving assembly syntax.")
          'ca65
          (asm-profile 'ca65
                       'implemented
                       'mos-6502
                       'ca65
                       "ca65 6502-family source syntax.")
          'acme
          (asm-profile 'acme
                       'implemented
                       'mos-6502
                       'acme
                       "ACME 6502-family source syntax.")
          'native-motorola
          (asm-profile 'native-motorola
                       'implemented
                       'm68k
                       'native-motorola
                       "Conservative native Motorola 68000-family syntax.")
          'asmone
          (asm-profile 'asmone
                       'implemented
                       'm68k
                       'asmone
                       "Asm-One-compatible Motorola syntax defaults.")
          'devpac
          (asm-profile 'devpac
                       'implemented
                       'm68k
                       'devpac
                       "Devpac-compatible Motorola syntax defaults.")
          'kick
          (asm-profile 'kick
                       'implemented
                       'mos-6502
                       'kick
                       "Kick Assembler source and script syntax.")
          '|64tass|
          (asm-profile '|64tass|
                       'implemented
                       'mos-6502
                       '|64tass|
                       "64tass source syntax.")
          'gas
          (asm-profile 'gas
                       'implemented
                       #f
                       'gas
                       "GNU assembler syntax; a target architecture is required.")
          'nasm
          (asm-profile 'nasm
                       'implemented
                       'x86
                       'nasm
                       "NASM-compatible source and preprocessor syntax.")))

;; Default settings for the ordinary consumer profiles.
(define asm-lexer-profile-defaults
  (hasheq 'coloring (hasheq 'trivia 'keep
                            'source-positions #t
                            'errors 'recover)
          'compiler (hasheq 'trivia 'skip
                            'source-positions #t
                            'errors 'raise)))

;; The resolved settings for one projected assembly reader.
(struct asm-config (profile trivia source-positions errors dialect architecture encoding fragment-cap)
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

;; validate-architecture : symbol? (or/c symbol? #f) symbol? -> (or/c symbol? #f)
;;   Require target selection when a dialect has target-dependent lexical rules.
(define (validate-architecture dialect architecture who)
  (define required
    (asm-profile-architectures dialect))
  (cond
    [(null? required)
     (define profile
       (hash-ref asm-profiles dialect))
     (cond
       [(and architecture
             (not (eq? architecture (asm-profile-architecture profile))))
        (raise-arguments-error who
                               "the selected dialect does not support that architecture"
                               "dialect" dialect
                               "architecture" architecture)]
       [else
        (asm-profile-architecture profile)])]
    [(not architecture)
     (raise-arguments-error who
                            "the selected dialect requires an explicit architecture"
                            "dialect" dialect
                            "supported architectures" required)]
    [(member architecture required)
     architecture]
    [else
     (raise-arguments-error who
                            "the selected dialect does not support that architecture"
                            "dialect" dialect
                            "architecture" architecture
                            "supported architectures" required)]))

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
                         #:architecture     [architecture #f]
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
  (define resolved-dialect
    (validate-dialect dialect 'make-asm-lexer))
  (asm-config profile
              resolved-trivia
              resolved-positions
              (hash-ref defaults 'errors)
              resolved-dialect
              (validate-architecture resolved-dialect architecture 'make-asm-lexer)
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

;; make-asm-derived-lexer : keyword-arguments -> (input-port? -> (or/c asm-derived-token? 'eof))
;;   Construct a streaming, byte-preserving reader for one explicit profile.
(define (make-asm-derived-lexer #:dialect      [dialect 'generic]
                                #:architecture [architecture #f]
                                #:encoding     [encoding 'auto]
                                #:fragment-cap [fragment-cap 16384])
  (define resolved-dialect
    (validate-dialect dialect 'make-asm-derived-lexer))
  (define resolved-architecture
    (validate-architecture resolved-dialect architecture 'make-asm-derived-lexer))
  (define resolved-encoding
    (validate-asm-encoding encoding 'make-asm-derived-lexer))
  (make-profiled-asm-derived-lexer
   #:dialect      resolved-dialect
   #:architecture resolved-architecture
   #:encoding     resolved-encoding
   #:fragment-cap (validate-fragment-cap fragment-cap 'make-asm-derived-lexer)
   #:make-derived-token asm-derived-token
   #:make-effective-state asm-effective-state
   #:make-diagnostic asm-diagnostic))

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
                        #:architecture     [architecture #f]
                        #:encoding         [encoding 'auto]
                        #:fragment-cap     [fragment-cap 16384])
  (define config
    (make-asm-config #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions
                     #:dialect          dialect
                     #:architecture     architecture
                     #:encoding         encoding
                     #:fragment-cap     fragment-cap))
  (define next-derived-token
    (make-asm-derived-lexer #:dialect      (asm-config-dialect config)
                            #:architecture (asm-config-architecture config)
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
                            #:architecture     [architecture #f]
                            #:fragment-cap     [fragment-cap 16384])
  (unless (string? source)
    (raise-argument-error 'asm-string->tokens "string?" source))
  (define lexer
    (make-asm-lexer #:profile          profile
                    #:trivia           trivia
                    #:source-positions source-positions
                    #:dialect          dialect
                    #:architecture     architecture
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
                                    #:architecture [architecture #f]
                                    #:fragment-cap [fragment-cap 16384])
  (unless (string? source)
    (raise-argument-error 'asm-string->derived-tokens "string?" source))
  (define lexer
    (make-asm-derived-lexer #:dialect      dialect
                            #:architecture architecture
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
  (define (collect-derived source
                           #:encoding [encoding 'auto]
                           #:dialect [dialect 'generic]
                           #:architecture [architecture #f])
    (define lexer
      (make-asm-derived-lexer #:dialect      dialect
                              #:architecture architecture
                              #:encoding     encoding))
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
  (define ca65-source
    ".feature c_comments\n// retained C comment\n@loop: lda #$20 ; tail\njmp :-\n")
  (define acme-source
    "!cpu 6510\n!zone main\n+draw\n!byte $01 ; tail\n")
  (define motorola-source
    "start: move.l d0,(a0)\n        dc.b 1,2 ; tail\n")
  (define kick-source
    "#import \"lib.asm\"\n.if (true) { .for (i=0; i<3; i++) { !loop: lda #$00 // tail\n} }\n.endif\n")
  (define tass-source
    ".cpu \"6502\"\n.namespace demo\n.logical $0801\n.enc \"screen\"\n.endlogical\n.endnamespace\n")
  (define gas-m68k-source
    "1: move.l %d0,%a0 | tail\n1b\n")
  (define gas-x86-source
    ".att_syntax\nmov %rax, %rbx # AT&T tail\n.intel_syntax noprefix\n.code64\nmov rax, 1 # tail\n")
  (define gas-arm-source
    ".syntax unified\n.thumb\nmov r0, #1 @ tail\n")
  (define gas-aarch64-source
    ".text\nadrp x0, :lo12:symbol\n// tail\n")
  (define nasm-source
    "%macro m 1\n%%local: mov rax, %1\n%endmacro\ndb %{1}\nBITS 64\n")
  (define ca65-derived
    (asm-string->derived-tokens ca65-source #:dialect 'ca65))
  (define acme-derived
    (asm-string->derived-tokens acme-source #:dialect 'acme))
  (define motorola-derived
    (asm-string->derived-tokens motorola-source #:dialect 'native-motorola))
  (define asmone-derived
    (asm-string->derived-tokens motorola-source #:dialect 'asmone))
  (define devpac-derived
    (asm-string->derived-tokens motorola-source #:dialect 'devpac))
  (define kick-derived
    (asm-string->derived-tokens kick-source #:dialect 'kick))
  (define tass-derived
    (asm-string->derived-tokens tass-source #:dialect '|64tass|))
  (define gas-m68k-derived
    (asm-string->derived-tokens gas-m68k-source #:dialect 'gas #:architecture 'm68k))
  (define gas-x86-derived
    (asm-string->derived-tokens gas-x86-source #:dialect 'gas #:architecture 'x86))
  (define gas-arm-derived
    (asm-string->derived-tokens gas-arm-source #:dialect 'gas #:architecture 'arm))
  (define gas-aarch64-derived
    (asm-string->derived-tokens gas-aarch64-source #:dialect 'gas #:architecture 'aarch64))
  (define nasm-derived
    (asm-string->derived-tokens nasm-source #:dialect 'nasm))
  (define profile-source-cases
    (list (list (string->bytes/utf-8 ca65-source)        ca65-derived)
          (list (string->bytes/utf-8 acme-source)        acme-derived)
          (list (string->bytes/utf-8 motorola-source)    motorola-derived)
          (list (string->bytes/utf-8 motorola-source)    asmone-derived)
          (list (string->bytes/utf-8 motorola-source)    devpac-derived)
          (list (string->bytes/utf-8 kick-source)        kick-derived)
          (list (string->bytes/utf-8 tass-source)        tass-derived)
          (list (string->bytes/utf-8 gas-m68k-source)    gas-m68k-derived)
          (list (string->bytes/utf-8 gas-x86-source)     gas-x86-derived)
          (list (string->bytes/utf-8 gas-arm-source)     gas-arm-derived)
          (list (string->bytes/utf-8 gas-aarch64-source) gas-aarch64-derived)
          (list (string->bytes/utf-8 nasm-source)        nasm-derived)))
  (define malformed-profile-source
    #"a\303(")
  (define profile-malformed-cases
    (list (collect-derived malformed-profile-source #:dialect 'ca65)
          (collect-derived malformed-profile-source #:dialect 'acme)
          (collect-derived malformed-profile-source #:dialect 'native-motorola)
          (collect-derived malformed-profile-source #:dialect 'asmone)
          (collect-derived malformed-profile-source #:dialect 'devpac)
          (collect-derived malformed-profile-source #:dialect 'kick)
          (collect-derived malformed-profile-source #:dialect '|64tass|)
          (collect-derived malformed-profile-source #:dialect 'gas #:architecture 'm68k)
          (collect-derived malformed-profile-source #:dialect 'gas #:architecture 'x86)
          (collect-derived malformed-profile-source #:dialect 'gas #:architecture 'arm)
          (collect-derived malformed-profile-source #:dialect 'gas #:architecture 'aarch64)
          (collect-derived malformed-profile-source #:dialect 'nasm)))
  ;; has-tag? : (listof asm-derived-token?) symbol? -> boolean?
  ;;   Determine whether a profile emitted one expected semantic role.
  (define (has-tag? tokens tag)
    (ormap (lambda (token)
             (asm-derived-token-has-tag? token tag))
           tokens))
  (define semicolon-token
    (findf (lambda (token)
             (string=? (asm-derived-token-text token) ";"))
           generic-derived))
  (define kick-semicolon-token
    (findf (lambda (token)
             (string=? (asm-derived-token-text token) ";"))
           kick-derived))
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
                (<= (bytes-length (asm-derived-token-raw-bytes token)) 16384)))
  (check-equal? (length long-derived) 2)
  (check-equal? (map asm-derived-token-continuation-id long-derived)
                '(1 1))
  (check-not-false first-pipe-token)
  (check-equal? (asm-derived-token-text first-pipe-token) "mov")
  (check-exn exn:fail?
             (lambda ()
               (define lexer
                 (make-asm-lexer #:profile 'compiler))
               (lexer (open-input-bytes #"\303"))))
  (check-not-exn
   (lambda ()
     (make-asm-derived-lexer #:dialect 'ca65)))
  (check-exn exn:fail?
             (lambda ()
               (make-asm-derived-lexer #:dialect 'gas)))
  (for ([tokens (in-list (list ca65-derived
                               acme-derived
                               motorola-derived
                               asmone-derived
                               devpac-derived
                               kick-derived
                               tass-derived
                               gas-m68k-derived
                               gas-x86-derived
                               gas-arm-derived
                               gas-aarch64-derived
                               nasm-derived))])
    (check-true (contiguous-byte-stream? tokens)))
  (for ([source+tokens (in-list profile-source-cases)])
    (define source
      (car source+tokens))
    (define tokens
      (cadr source+tokens))
    (check-equal? (apply bytes-append (map asm-derived-token-raw-bytes tokens))
                  source))
  (for ([tokens (in-list profile-malformed-cases)])
    (check-true (contiguous-byte-stream? tokens))
    (check-equal? (apply bytes-append (map asm-derived-token-raw-bytes tokens))
                  malformed-profile-source)
    (check-true
     (ormap (lambda (token)
              (pair? (asm-derived-token-diagnostics token)))
            tokens)))
  (check-true (has-tag? ca65-derived 'ca65-directive))
  (check-true (has-tag? ca65-derived 'ca65-cheap-local))
  (check-true (has-tag? ca65-derived 'ca65-anonymous-label-reference))
  (check-true (has-tag? ca65-derived 'asm-line-comment))
  (check-true (has-tag? acme-derived 'acme-directive))
  (check-true (has-tag? acme-derived 'acme-macro-call))
  (check-true (has-tag? motorola-derived 'native-motorola-instruction))
  (check-true (has-tag? asmone-derived 'asmone-comment))
  (check-true (has-tag? devpac-derived 'devpac-comment))
  (check-true (has-tag? kick-derived 'kick-directive))
  (check-true (has-tag? kick-derived 'kick-immediate-literal))
  (check-true (has-tag? kick-derived 'asm-statement-separator))
  (check-true (has-tag? kick-derived 'kick-script-region))
  (check-true (has-tag? kick-derived 'asm-conditional-region))
  (check-not-false kick-semicolon-token)
  (check-equal? (asm-derived-token-category kick-semicolon-token) 'delimiter)
  (check-true (has-tag? kick-derived 'asm-line-comment))
  (check-true (has-tag? tass-derived '|64tass-directive|))
  (check-true (has-tag? tass-derived '|64tass-namespace-region|))
  (check-true (has-tag? tass-derived '|64tass-emitted-encoding|))
  (check-true (has-tag? gas-m68k-derived 'gas-register))
  (check-true (has-tag? gas-m68k-derived 'gas-numeric-local-label))
  (check-true (has-tag? gas-m68k-derived 'asm-line-comment))
  (check-true (has-tag? gas-x86-derived 'gas-syntax-att))
  (check-true (has-tag? gas-x86-derived 'gas-syntax-intel))
  (check-true (has-tag? gas-x86-derived 'gas-mode-code64))
  (check-true (has-tag? gas-arm-derived 'gas-syntax-unified))
  (check-true (has-tag? gas-arm-derived 'gas-mode-thumb))
  (check-true (has-tag? gas-aarch64-derived 'gas-register))
  (check-true (has-tag? gas-aarch64-derived 'gas-relocation-modifier))
  (check-true (has-tag? gas-aarch64-derived 'asm-line-comment))
  (check-true (has-tag? nasm-derived 'nasm-macro-open))
  (check-true (has-tag? nasm-derived 'asm-macro-body))
  (check-true (has-tag? nasm-derived 'nasm-macro-local))
  (check-true (has-tag? nasm-derived 'nasm-macro-parameter))
  (check-true (has-tag? nasm-derived 'nasm-token-concatenation))
  (check-true (has-tag? nasm-derived 'nasm-bits-mode)))
