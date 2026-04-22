#lang racket/base

;;;
;;; C Derived Tokens
;;;
;;
;; Stateful C tokenization and reusable C-specific classifications.

;; c-derived-token?         : any/c -> boolean?
;;   Recognize a derived C token.
;; c-derived-token-text     : c-derived-token? -> string?
;;   Extract the source text for one derived C token.
;; c-derived-token-start    : c-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; c-derived-token-end      : c-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; c-derived-token-tags     : c-derived-token? -> (listof symbol?)
;;   Extract reusable C classification tags.
;; c-derived-token-has-tag? : c-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-c-derived-reader    : -> (input-port? -> (or/c c-derived-token? 'eof))
;;   Construct a stateful C derived-token reader.

(provide c-derived-token?
         c-derived-token-text
         c-derived-token-start
         c-derived-token-end
         c-derived-token-tags
         c-derived-token-has-tag?
         make-c-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         racket/string
         "parser-tools-compat.rkt")

;; A C token plus reusable tags.
(struct c-derived-token (kind text start end tags) #:transparent)

;; c-derived-token-has-tag? : c-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (c-derived-token-has-tag? token tag)
  (member tag (c-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; C classification tables

;; Reserved C11 keywords.
(define c-keywords
  (list->set
   '("auto" "break" "case" "char" "const" "continue" "default" "do"
     "double" "else" "enum" "extern" "float" "for" "goto" "if"
     "inline" "int" "long" "register" "restrict" "return" "short"
     "signed" "sizeof" "static" "struct" "switch" "typedef" "union"
     "unsigned" "void" "volatile" "while"
     "_Alignas" "_Alignof" "_Atomic" "_Bool" "_Complex" "_Generic"
     "_Imaginary" "_Noreturn" "_Static_assert" "_Thread_local")))

;; C delimiters.
(define c-delimiters
  (list->set
   '("#" "%:" "(" ")" "[" "]" "{" "}" "," ";" ":" "?")))

;; C operators and punctuators in longest-match order.
(define c-symbol-tokens
  '("%:%:" ">>=" "<<=" "..."
    "??=" "??/" "??'" "??(" "??)" "??!" "??<" "??>" "??-"
    "##" "%:" "<%" "%>" "<:" ":>" "->" "++" "--" "<<"
    ">>" "<=" ">=" "==" "!="
    "&&" "||" "*=" "/=" "%="
    "+=" "-=" "&=" "^=" "|="
    "." "&" "*" "+" "-" "~"
    "!" "/" "%" "<" ">" "^"
    "|" "=" "#" "(" ")" "["
    "]" "{" "}" "," ";" ":"
    "?"))

;; -----------------------------------------------------------------------------
;; Port helpers

;; write-one! : input-port? output-port? -> void?
;;   Consume one character and append it to the output accumulator.
(define (write-one! in out)
  (write-char (read-char in) out))

;; peek-next : input-port? [exact-nonnegative-integer?] -> (or/c char? eof-object?)
;;   Peek ahead in the input stream.
(define (peek-next in [skip 0])
  (peek-char in skip))

;; read-while! : input-port? output-port? (char? -> boolean?) -> void?
;;   Consume characters while pred? holds.
(define (read-while! in out pred?)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(and (char? next) (pred? next))
       (write-one! in out)
       (loop)]
      [else
       (void)])))

;; current-stream-position : input-port? -> position?
;;   Read the current parser-tools-compatible source position from a port.
(define (current-stream-position in)
  (let-values ([(line col offset) (port-next-location in)])
    (define safe-line
      (cond
        [(exact-positive-integer? line)   line]
        [else                             1]))
    (define safe-col
      (cond
        [(exact-nonnegative-integer? col) col]
        [else                             0]))
    (define safe-offset
      (cond
        [(exact-positive-integer? offset) offset]
        [else                             1]))
    (make-stream-position safe-offset safe-line safe-col)))

;; make-token-from-text : position? position? string? (listof symbol?) -> c-derived-token?
;;   Construct one derived token from explicit positions, text, and tags.
(define (make-token-from-text start-pos end-pos text tags)
  (define kind
    (cond
      [(member 'comment tags)         'comment]
      [(member 'whitespace tags)      'whitespace]
      [(member 'malformed-token tags) 'malformed]
      [(member 'keyword tags)         'keyword]
      [(member 'literal tags)         'literal]
      [(member 'operator tags)        'operator]
      [(member 'delimiter tags)       'delimiter]
      [else                           'identifier]))
  (c-derived-token kind
                   text
                   start-pos
                   end-pos
                   (remove-duplicates tags)))

;; -----------------------------------------------------------------------------
;; Character classes

;; inline-whitespace-char? : char? -> boolean?
;;   Recognize inline C whitespace other than newlines.
(define (inline-whitespace-char? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\page)))

;; newline-start? : (or/c char? eof-object?) -> boolean?
;;   Determine whether a character begins a physical newline.
(define (newline-start? ch)
  (or (char=? ch #\newline)
      (char=? ch #\return)))

;; identifier-start-char? : char? -> boolean?
;;   Recognize an identifier-start character.
(define (identifier-start-char? ch)
  (or (char-alphabetic? ch)
      (char=? ch #\_)))

;; identifier-char? : char? -> boolean?
;;   Recognize an identifier continuation character.
(define (identifier-char? ch)
  (or (identifier-start-char? ch)
      (char-numeric? ch)))

;; identifier-start-at? : input-port? -> boolean?
;;   Determine whether the next source begins one identifier-start unit.
(define (identifier-start-at? in)
  (define next
    (peek-next in))
  (or (and (char? next)
           (identifier-start-char? next))
      (universal-character-name-length-at in)))

;; identifier-continuation-at? : input-port? -> boolean?
;;   Determine whether the next source begins one identifier-continuation unit.
(define (identifier-continuation-at? in)
  (define next
    (peek-next in))
  (or (and (char? next)
           (identifier-char? next))
      (universal-character-name-length-at in)))

;; pp-number-char? : char? -> boolean?
;;   Recognize a broad preprocessing-number continuation character.
(define (pp-number-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (char=? ch #\_)
      (char=? ch #\.)
      (char=? ch #\+)
      (char=? ch #\-)))

;; delimiter-token? : string? -> boolean?
;;   Determine whether a token spelling is delimiter-like.
(define (delimiter-token? text)
  (or (set-member? c-delimiters text)
      (member text '("<:" ":>" "<%" "%>" "??=" "??(" "??)" "??<" "??>"))))

;; hex-digit? : char? -> boolean?
;;   Recognize one hexadecimal digit.
(define (hex-digit? ch)
  (or (char-numeric? ch)
      (member (char-downcase ch)
              '(#\a #\b #\c #\d #\e #\f))))

;; octal-digit? : char? -> boolean?
;;   Recognize one octal digit.
(define (octal-digit? ch)
  (and (char-numeric? ch)
       (char<=? ch #\7)))

;; universal-character-introducer? : (or/c char? eof-object?) -> boolean?
;;   Determine whether a character introduces a universal character name.
(define (universal-character-introducer? ch)
  (and (char? ch)
       (member ch '(#\u #\U))))

;; universal-character-name-length-at : input-port? [exact-nonnegative-integer?] -> (or/c exact-nonnegative-integer? #f)
;;   Return the raw length of a universal character name beginning at the current position.
(define (universal-character-name-length-at in [skip 0])
  (define slash
    (peek-next in skip))
  (define marker
    (peek-next in (add1 skip)))
  (cond
    [(or (not (char? slash))
         (not (char=? slash #\\))
         (not (universal-character-introducer? marker)))
     #f]
    [else
     (define hex-count
       (cond
         [(char=? marker #\u) 4]
         [else                8]))
     (define ok?
       (for/and ([i (in-range hex-count)])
         (define ch
           (peek-next in (+ skip 2 i)))
         (and (char? ch)
              (hex-digit? ch))))
     (and ok?
          (+ 2 hex-count))]))

;; consume-universal-character-name! : input-port? output-port? -> void?
;;   Consume one universal character name known to be present.
(define (consume-universal-character-name! in out)
  (define len
    (universal-character-name-length-at in))
  (for ([i (in-range len)])
    (write-one! in out)))

;; -----------------------------------------------------------------------------
;; Small scanners

;; read-newline! : input-port? output-port? -> void?
;;   Consume one physical newline sequence, preserving CRLF.
(define (read-newline! in out)
  (define next
    (peek-next in))
  (cond
    [(char=? next #\return)
     (write-one! in out)
     (when (and (char? (peek-next in))
                (char=? (peek-next in) #\newline))
       (write-one! in out))]
    [else
     (write-one! in out)]))

;; trigraph-line-splice-at? : input-port? -> boolean?
;;   Determine whether the next input begins a trigraph backslash line splice.
(define (trigraph-line-splice-at? in)
  (and (char? (peek-next in 0))
       (char=? (peek-next in 0) #\?)
       (char? (peek-next in 1))
       (char=? (peek-next in 1) #\?)
       (char? (peek-next in 2))
       (char=? (peek-next in 2) #\/)
       (newline-start? (peek-next in 3))))

;; read-trigraph-line-splice! : input-port? output-port? -> void?
;;   Consume one trigraph backslash line splice.
(define (read-trigraph-line-splice! in out)
  (for ([i (in-range 3)])
    (write-one! in out))
  (read-newline! in out))

;; read-line-comment! : input-port? output-port? -> void?
;;   Consume one // comment without its terminating newline.
(define (read-line-comment! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       (void)]
      [(newline-start? next)
       (void)]
      [else
       (write-one! in out)
       (loop)])))

;; read-block-comment! : input-port? output-port? -> boolean?
;;   Consume one /* */ comment and report whether it terminated.
(define (read-block-comment! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(char=? next #\*)
       (write-one! in out)
       (define maybe-close
         (peek-next in))
       (cond
         [(and (char? maybe-close)
               (char=? maybe-close #\/))
          (write-one! in out)
          #t]
         [else
          (loop)])]
      [else
       (write-one! in out)
       (loop)])))

;; read-identifier! : input-port? output-port? -> void?
;;   Consume one C identifier candidate.
(define (read-identifier! in out)
  (let loop ()
    (cond
      [(universal-character-name-length-at in)
       (consume-universal-character-name! in out)
       (loop)]
      [else
       (define next
         (peek-next in))
       (cond
         [(and (char? next)
               (identifier-char? next))
          (write-one! in out)
          (loop)]
         [else
          (void)])])))

;; string-prefix-length : input-port? -> exact-nonnegative-integer?
;;   Determine the length of the current string-literal prefix.
(define (string-prefix-length in)
  (define first
    (peek-next in))
  (define second
    (peek-next in 1))
  (define third
    (peek-next in 2))
  (cond
    [(and (char? first)
          (char=? first #\u)
          (char? second)
          (char=? second #\8)
          (char? third)
          (char=? third #\"))
     2]
    [(and (char? first)
          (member first '(#\u #\U #\L))
          (char? second)
          (char=? second #\"))
     1]
    [(and (char? first)
          (char=? first #\"))
     0]
    [else
     0]))

;; char-prefix-length : input-port? -> exact-nonnegative-integer?
;;   Determine the length of the current character-literal prefix.
(define (char-prefix-length in)
  (define first
    (peek-next in))
  (define second
    (peek-next in 1))
  (cond
    [(and (char? first)
          (member first '(#\u #\U #\L))
          (char? second)
          (char=? second #\'))
     1]
    [(and (char? first)
          (char=? first #\'))
     0]
    [else
     0]))

;; read-delimited-literal! : input-port? output-port? exact-nonnegative-integer? char? -> boolean?
;;                        -> (values boolean? boolean?)
;;   Consume one string-like literal with an optional prefix and report
;;   termination and lexical escape validity.
(define (read-delimited-literal! in out prefix-length delimiter)
  (for ([i (in-range prefix-length)])
    (write-one! in out))
  (write-one! in out)
  (let loop ([valid? #t])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       (values #f valid?)]
      [else
       (define ch
         next)
       (cond
         [(char=? ch delimiter)
          (write-one! in out)
          (values #t valid?)]
         [(newline-start? ch)
          (values #f valid?)]
         [(char=? ch #\\)
          (write-one! in out)
          (define escaped
            (peek-next in))
          (cond
            [(eof-object? escaped)
             (values #f #f)]
            [(newline-start? escaped)
             (read-newline! in out)
             (loop valid?)]
            [(member escaped '(#\' #\" #\? #\\ #\a #\b #\f #\n #\r #\t #\v))
             (write-one! in out)
             (loop valid?)]
            [(universal-character-introducer? escaped)
             (cond
               [(universal-character-name-length-at in 1)
                (write-one! in out)
                (consume-universal-character-name! in out)
                (loop valid?)]
               [else
                (write-one! in out)
                (loop #f)])]
            [(char=? escaped #\x)
             (write-one! in out)
             (define first-hex
               (peek-next in))
             (cond
               [(and (char? first-hex)
                     (hex-digit? first-hex))
                (read-while! in out hex-digit?)
                (loop valid?)]
               [else
                (loop #f)])]
            [(octal-digit? escaped)
             (write-one! in out)
             (for ([i (in-range 2)])
               (when (and (char? (peek-next in))
                          (octal-digit? (peek-next in)))
                 (write-one! in out)))
             (loop valid?)]
            [else
             (write-one! in out)
             (loop #f)])]
         [else
          (write-one! in out)
          (loop valid?)])])))

;; read-angle-header! : input-port? output-port? -> boolean?
;;   Consume one <...> include header name.
(define (read-angle-header! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(newline-start? next)
       #f]
      [else
       (define ch
         next)
       (write-one! in out)
       (cond
         [(char=? ch #\>)
          #t]
         [else
          (loop)])])))

;; read-pp-number! : input-port? output-port? -> void?
;;   Consume one preprocessing-number candidate.
(define (read-pp-number! in out)
  (write-one! in out)
  (read-while! in out pp-number-char?))

;; symbol-token-at : input-port? -> (or/c string? #f)
;;   Find the longest symbolic C token at the current port position.
(define (symbol-token-at in)
  (for/or ([text (in-list c-symbol-tokens)])
    (define len
      (string-length text))
    (define maybe
      (for/and ([i (in-range len)])
        (define ch
          (peek-next in i))
        (and (char? ch)
             (char=? ch (string-ref text i)))))
    (and maybe text)))

;; -----------------------------------------------------------------------------
;; Reader

;; make-c-derived-reader : -> (input-port? -> (or/c c-derived-token? 'eof))
;;   Construct a stateful C derived-token reader.
(define (make-c-derived-reader)
  (define line-prefix-only?
    #t)
  (define in-preprocessor?
    #f)
  (define expect-directive-name?
    #f)
  (define expect-include-target?
    #f)
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-c-derived-reader "input-port?" in))
    (port-count-lines! in)
    (define start-pos
      (current-stream-position in))
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       'eof]
      [(inline-whitespace-char? next)
       (define out
         (open-output-string))
       (read-while! in out inline-whitespace-char?)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(whitespace c-whitespace))]
      [(newline-start? next)
       (define out
         (open-output-string))
       (read-newline! in out)
       (set! line-prefix-only? #t)
       (set! in-preprocessor? #f)
       (set! expect-directive-name? #f)
       (set! expect-include-target? #f)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(whitespace c-whitespace))]
      [(or (and (char=? next #\\)
                (newline-start? (peek-next in 1)))
           (trigraph-line-splice-at? in))
       (define out
         (open-output-string))
       (cond
         [(trigraph-line-splice-at? in)
          (read-trigraph-line-splice! in out)]
         [else
          (write-one! in out)
          (read-newline! in out)])
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(whitespace c-whitespace c-line-splice))]
      [(and (char=? next #\/)
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\/))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (read-line-comment! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(comment c-comment))]
      [(and (char=? next #\/)
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\*))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (define terminated?
         (read-block-comment! in out))
       (define text
         (get-output-string out))
       (when (regexp-match? #px"\r\n|\r|\n" text)
         (set! in-preprocessor? #f)
         (set! expect-directive-name? #f)
         (set! expect-include-target? #f))
       (make-token-from-text start-pos
                             (current-stream-position in)
                             text
                             (append '(comment c-comment)
                                     (if terminated?
                                         '()
                                         '(c-error malformed-token))))]
      [(and expect-include-target?
            (char=? next #\<))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (set! expect-include-target? #f)
       (define terminated?
         (read-angle-header! in out))
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             (append '(literal c-header-name)
                                     (if terminated?
                                         '()
                                         '(c-error malformed-token))))]
      [(or (positive? (string-prefix-length in))
           (char=? next #\"))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (define header-name?
         expect-include-target?)
       (define prefix-length
         (string-prefix-length in))
       (define-values (terminated? valid?)
         (read-delimited-literal! in out prefix-length #\"))
       (set! expect-include-target? #f)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             (append (if header-name?
                                         '(literal c-header-name)
                                         '(literal c-string-literal))
                                     (if (and terminated? valid?)
                                         '()
                                         '(c-error malformed-token))))]
      [(or (positive? (char-prefix-length in))
           (char=? next #\'))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (set! expect-include-target? #f)
       (define-values (terminated? valid?)
         (read-delimited-literal! in out (char-prefix-length in) #\'))
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             (append '(literal c-char-literal)
                                     (if (and terminated? valid?)
                                         '()
                                         '(c-error malformed-token))))]
      [(identifier-start-at? in)
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (read-identifier! in out)
       (define text
         (get-output-string out))
       (define directive-name?
         expect-directive-name?)
       (define include-name?
         (and directive-name?
              (string=? text "include")))
       (set! expect-directive-name? #f)
       (when expect-include-target?
         (set! expect-include-target? #f))
       (when include-name?
         (set! expect-include-target? #t))
       (make-token-from-text
        start-pos
        (current-stream-position in)
        text
        (append
         (cond
           [directive-name?
            '(keyword c-keyword c-preprocessor-directive)]
           [(set-member? c-keywords text)
            '(keyword c-keyword)]
           [else
            '(identifier c-identifier)])
         '()))]
      [(or (char-numeric? next)
           (and (char=? next #\.)
                (char? (peek-next in 1))
                (char-numeric? (peek-next in 1))))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (set! expect-include-target? #f)
       (read-pp-number! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(literal c-numeric-literal))]
      [else
       (define symbol-text
         (symbol-token-at in))
       (cond
         [symbol-text
          (define out
            (open-output-string))
          (for ([i (in-range (string-length symbol-text))])
            (write-one! in out))
          (define token-text
            (get-output-string out))
          (define pp-marker?
            (and line-prefix-only?
                 (not in-preprocessor?)
                 (member token-text '("#" "%:"))))
          (set! line-prefix-only? #f)
          (cond
            [pp-marker?
             (set! in-preprocessor? #t)
             (set! expect-directive-name? #t)
             (set! expect-include-target? #f)]
            [else
             (when expect-include-target?
               (set! expect-include-target? #f))])
          (make-token-from-text
           start-pos
           (current-stream-position in)
           token-text
           (cond
             [(delimiter-token? token-text)
              '(delimiter c-delimiter)]
             [else
              '(operator c-operator)]))]
         [else
          (define out
            (open-output-string))
          (write-one! in out)
          (set! line-prefix-only? #f)
          (set! expect-include-target? #f)
          (make-token-from-text start-pos
                                (current-stream-position in)
                                (get-output-string out)
                                '(c-error malformed-token))])])))
