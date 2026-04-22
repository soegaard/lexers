#lang racket/base

;;;
;;; Go Derived Tokens
;;;
;;
;; Stateful Go tokenization and reusable Go-specific classifications.

;; go-derived-token?         : any/c -> boolean?
;;   Recognize a derived Go token.
;; go-derived-token-text     : go-derived-token? -> string?
;;   Extract the source text for one derived token.
;; go-derived-token-start    : go-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; go-derived-token-end      : go-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; go-derived-token-tags     : go-derived-token? -> (listof symbol?)
;;   Extract reusable Go classification tags.
;; go-derived-token-has-tag? : go-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-go-derived-reader    : -> (input-port? -> (or/c go-derived-token? 'eof))
;;   Construct a stateful Go derived-token reader.

(provide go-derived-token?
         go-derived-token-text
         go-derived-token-start
         go-derived-token-end
         go-derived-token-tags
         go-derived-token-has-tag?
         make-go-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         "parser-tools-compat.rkt")

;; A Go token plus reusable tags.
(struct go-derived-token (kind text start end tags) #:transparent)

;; go-derived-token-has-tag? : go-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (go-derived-token-has-tag? token tag)
  (member tag (go-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Classification tables

;; Go reserved keywords.
(define go-keywords
  (list->set
   '("break" "default" "func" "interface" "select"
     "case" "defer" "go" "map" "struct"
     "chan" "else" "goto" "package" "switch"
     "const" "fallthrough" "if" "range" "type"
     "continue" "for" "import" "return" "var")))

;; Go punctuation and operator sequences in longest-match order.
(define go-punctuators
  '("<<=" ">>=" "&^=" "..." ":=" "&&" "||" "<-" "++" "--" "==" "!="
    "<=" ">=" "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<"
    ">>" "&^" "(" ")" "[" "]" "{" "}" "," ";" ":" "."
    "+" "-" "*" "/" "%" "&" "|" "^" "<" ">" "=" "!" "~"))

;; Go delimiters and punctuation that are not operator-like.
(define go-delimiters
  (list->set
   '("(" ")" "[" "]" "{" "}" "," ";" ":" "." "...")))

;; -----------------------------------------------------------------------------
;; Port helpers

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

;; peek-next : input-port? [exact-nonnegative-integer?] -> (or/c char? eof-object?)
;;   Peek ahead in the input stream.
(define (peek-next in [skip 0])
  (peek-char in skip))

;; write-one! : input-port? output-port? -> void?
;;   Consume one character and append it to the output accumulator.
(define (write-one! in out)
  (write-char (read-char in) out))

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

;; make-token-from-text : position? position? string? (listof symbol?) -> go-derived-token?
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
  (go-derived-token kind
                    text
                    start-pos
                    end-pos
                    (remove-duplicates tags)))

;; -----------------------------------------------------------------------------
;; Character classes

;; newline-start? : (or/c char? eof-object?) -> boolean?
;;   Determine whether a character begins a physical newline.
(define (newline-start? ch)
  (and (char? ch)
       (or (char=? ch #\newline)
           (char=? ch #\return))))

;; go-inline-whitespace? : char? -> boolean?
;;   Recognize Go whitespace other than newlines.
(define (go-inline-whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)))

;; ident-start? : char? -> boolean?
;;   Recognize an identifier-start character for the first Go slice.
(define (ident-start? ch)
  (or (char-alphabetic? ch)
      (char=? ch #\_)))

;; ident-char? : char? -> boolean?
;;   Recognize an identifier continuation character.
(define (ident-char? ch)
  (or (ident-start? ch)
      (char-numeric? ch)))

;; binary-digit? : char? -> boolean?
;;   Recognize one binary digit.
(define (binary-digit? ch)
  (or (char=? ch #\0)
      (char=? ch #\1)))

;; octal-digit? : char? -> boolean?
;;   Recognize one octal digit.
(define (octal-digit? ch)
  (and (char-numeric? ch)
       (char<=? ch #\7)))

;; decimal-digit? : char? -> boolean?
;;   Recognize one decimal digit.
(define (decimal-digit? ch)
  (char-numeric? ch))

;; hex-digit? : char? -> boolean?
;;   Recognize one hexadecimal digit.
(define (hex-digit? ch)
  (or (decimal-digit? ch)
      (member (char-downcase ch)
              '(#\a #\b #\c #\d #\e #\f))))

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

;; read-whitespace! : input-port? output-port? -> void?
;;   Consume one run of Go whitespace.
(define (read-whitespace! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       (void)]
      [(newline-start? next)
       (read-newline! in out)
       (loop)]
      [(go-inline-whitespace? next)
       (write-one! in out)
       (loop)]
      [else
       (void)])))

;; read-line-comment! : input-port? output-port? -> void?
;;   Consume one Go line comment without its terminating newline.
(define (read-line-comment! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(or (eof-object? next)
           (newline-start? next))
       (void)]
      [else
       (write-one! in out)
       (loop)])))

;; read-general-comment! : input-port? output-port? -> boolean?
;;   Consume one Go general comment and report whether it terminated.
(define (read-general-comment! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(and (char=? next #\*)
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\/))
       (write-one! in out)
       (write-one! in out)
       #t]
      [else
       (write-one! in out)
       (loop)])))

;; read-identifier! : input-port? output-port? -> void?
;;   Consume one identifier-like token.
(define (read-identifier! in out)
  (read-while! in out ident-char?))

;; read-digit-sequence! : input-port? output-port? (char? -> boolean?) -> void?
;;   Consume one digit sequence that may contain separating underscores.
(define (read-digit-sequence! in out digit?)
  (let loop ([saw-digit? #f])
    (define next
      (peek-next in))
    (cond
      [(and (char? next) (digit? next))
       (write-one! in out)
       (loop #t)]
      [(and saw-digit?
            (char? next)
            (char=? next #\_)
            (char? (peek-next in 1))
            (digit? (peek-next in 1)))
       (write-one! in out)
       (write-one! in out)
       (loop #t)]
      [else
       (void)])))

;; read-exponent! : input-port? output-port? char? char? -> void?
;;   Consume one exponent part after the exponent marker has been seen.
(define (read-exponent! in out lower upper)
  (when (and (char? (peek-next in))
             (or (char=? (peek-next in) lower)
                 (char=? (peek-next in) upper)))
    (write-one! in out)
    (when (and (char? (peek-next in))
               (or (char=? (peek-next in) #\+)
                   (char=? (peek-next in) #\-)))
      (write-one! in out))
    (read-digit-sequence! in out decimal-digit?)))

;; read-imaginary-suffix! : input-port? output-port? -> boolean?
;;   Consume a trailing imaginary suffix and report whether it was present.
(define (read-imaginary-suffix! in out)
  (cond
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\i))
     (write-one! in out)
     #t]
    [else
     #f]))

;; read-decimal-number! : input-port? output-port? -> boolean?
;;   Consume a decimal or floating-point number, returning whether it is imaginary.
(define (read-decimal-number! in out)
  (read-digit-sequence! in out decimal-digit?)
  (cond
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\.)
          (not (and (char? (peek-next in 1))
                    (char=? (peek-next in 1) #\.))))
     (write-one! in out)
     (read-digit-sequence! in out decimal-digit?)
     (read-exponent! in out #\e #\E)
     (read-imaginary-suffix! in out)]
    [else
     (read-exponent! in out #\e #\E)
     (read-imaginary-suffix! in out)]))

;; read-number-from-dot! : input-port? output-port? -> boolean?
;;   Consume a floating literal that starts with a decimal point.
(define (read-number-from-dot! in out)
  (write-one! in out)
  (read-digit-sequence! in out decimal-digit?)
  (read-exponent! in out #\e #\E)
  (read-imaginary-suffix! in out))

;; read-prefixed-number! : input-port? output-port? (char? -> boolean?) boolean? -> boolean?
;;   Consume one prefixed integer or floating literal and report whether it is imaginary.
(define (read-prefixed-number! in out digit? allow-radix-point?)
  (read-digit-sequence! in out digit?)
  (when (and allow-radix-point?
             (char? (peek-next in))
             (char=? (peek-next in) #\.))
    (write-one! in out)
    (read-digit-sequence! in out digit?))
  (cond
    [allow-radix-point?
     (read-exponent! in out #\p #\P)
     (read-imaginary-suffix! in out)]
    [else
     (read-imaginary-suffix! in out)]))

;; read-number! : input-port? output-port? -> boolean?
;;   Consume one Go numeric literal and report whether it is imaginary.
(define (read-number! in out)
  (cond
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\0)
          (char? (peek-next in 1))
          (member (peek-next in 1) '(#\x #\X)))
     (write-one! in out)
     (write-one! in out)
     (read-prefixed-number! in out hex-digit? #t)]
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\0)
          (char? (peek-next in 1))
          (member (peek-next in 1) '(#\b #\B)))
     (write-one! in out)
     (write-one! in out)
     (read-prefixed-number! in out binary-digit? #f)]
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\0)
          (char? (peek-next in 1))
          (member (peek-next in 1) '(#\o #\O)))
     (write-one! in out)
     (write-one! in out)
     (read-prefixed-number! in out octal-digit? #f)]
    [else
     (read-decimal-number! in out)]))

;; read-interpreted-string! : input-port? output-port? -> boolean?
;;   Consume one interpreted string literal and report whether it terminated.
(define (read-interpreted-string! in out)
  (write-one! in out)
  (let loop ([escaped? #f])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(newline-start? next)
       #f]
      [escaped?
       (write-one! in out)
       (loop #f)]
      [(char=? next #\\)
       (write-one! in out)
       (loop #t)]
      [(char=? next #\")
       (write-one! in out)
       #t]
      [else
       (write-one! in out)
       (loop #f)])))

;; read-raw-string! : input-port? output-port? -> boolean?
;;   Consume one raw string literal and report whether it terminated.
(define (read-raw-string! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(char=? next #\`)
       (write-one! in out)
       #t]
      [else
       (write-one! in out)
       (loop)])))

;; read-rune-literal! : input-port? output-port? -> boolean?
;;   Consume one rune literal and report whether it terminated.
(define (read-rune-literal! in out)
  (write-one! in out)
  (let loop ([escaped? #f])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(newline-start? next)
       #f]
      [escaped?
       (write-one! in out)
       (loop #f)]
      [(char=? next #\\)
       (write-one! in out)
       (loop #t)]
      [(char=? next #\')
       (write-one! in out)
       #t]
      [else
       (write-one! in out)
       (loop #f)])))

;; punctuator-at : input-port? -> (or/c string? #f)
;;   Find the longest Go punctuator at the current port position.
(define (punctuator-at in)
  (for/or ([candidate (in-list go-punctuators)])
    (define len
      (string-length candidate))
    (define ok?
      (for/and ([i (in-range len)])
        (define next
          (peek-next in i))
        (and (char? next)
             (char=? next (string-ref candidate i)))))
    (and ok? candidate)))

;; consume-punctuator! : input-port? output-port? string? -> void?
;;   Consume one known punctuator string.
(define (consume-punctuator! in out punctuator)
  (for ([ch (in-string punctuator)])
    (write-char ch out)
    (read-char in)))

;; -----------------------------------------------------------------------------
;; Reader

;; make-go-derived-reader : -> (input-port? -> (or/c go-derived-token? 'eof))
;;   Construct a stateful Go derived-token reader.
(define (make-go-derived-reader)
  (lambda (in)
    (define start-pos
      (current-stream-position in))
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       'eof]
      [else
       (define out
         (open-output-string))
       (define tags
         (cond
           [(or (go-inline-whitespace? next)
                (newline-start? next))
            (read-whitespace! in out)
            '(whitespace go-whitespace)]
           [(and (char=? next #\/)
                 (char? (peek-next in 1))
                 (char=? (peek-next in 1) #\/))
            (read-line-comment! in out)
            '(comment go-comment go-line-comment)]
           [(and (char=? next #\/)
                 (char? (peek-next in 1))
                 (char=? (peek-next in 1) #\*))
            (define terminated?
              (read-general-comment! in out))
            (cond
              [terminated?
               '(comment go-comment go-general-comment)]
              [else
               '(comment go-comment go-general-comment malformed-token)])]
           [(ident-start? next)
            (read-identifier! in out)
            (define text
              (get-output-string out))
            (cond
              [(set-member? go-keywords text)
               '(keyword go-keyword)]
              [else
               '(identifier go-identifier)])]
           [(char=? next #\")
            (define terminated?
              (read-interpreted-string! in out))
            (cond
              [terminated?
               '(literal go-string-literal)]
              [else
               '(literal go-string-literal malformed-token)])]
           [(char=? next #\`)
            (define terminated?
              (read-raw-string! in out))
            (cond
              [terminated?
               '(literal go-raw-string-literal)]
              [else
               '(literal go-raw-string-literal malformed-token)])]
           [(char=? next #\')
            (define terminated?
              (read-rune-literal! in out))
            (cond
              [terminated?
               '(literal go-rune-literal)]
              [else
               '(literal go-rune-literal malformed-token)])]
           [(decimal-digit? next)
            (define imaginary?
              (read-number! in out))
            (cond
              [imaginary?
               '(literal go-numeric-literal go-imaginary-literal)]
              [else
               '(literal go-numeric-literal)])]
           [(and (char=? next #\.)
                 (char? (peek-next in 1))
                 (decimal-digit? (peek-next in 1)))
            (define imaginary?
              (read-number-from-dot! in out))
            (cond
              [imaginary?
               '(literal go-numeric-literal go-imaginary-literal)]
              [else
               '(literal go-numeric-literal)])]
           [else
            (define punctuator
              (punctuator-at in))
            (cond
              [punctuator
               (consume-punctuator! in out punctuator)
               (cond
                 [(set-member? go-delimiters punctuator)
                  '(delimiter go-delimiter)]
                 [else
                  '(operator go-operator)])]
              [else
               (write-one! in out)
               '(malformed-token)])]))
       (define text
         (get-output-string out))
       (define end-pos
         (current-stream-position in))
       (make-token-from-text start-pos end-pos text tags)])))
