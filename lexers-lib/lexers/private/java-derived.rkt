#lang racket/base

;;;
;;; Java Derived Tokens
;;;
;;
;; Stateful Java tokenization and reusable Java-specific classifications.

;; java-derived-token?         : any/c -> boolean?
;;   Recognize a derived Java token.
;; java-derived-token-text     : java-derived-token? -> string?
;;   Extract the source text for one derived token.
;; java-derived-token-start    : java-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; java-derived-token-end      : java-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; java-derived-token-tags     : java-derived-token? -> (listof symbol?)
;;   Extract reusable Java classification tags.
;; java-derived-token-has-tag? : java-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-java-derived-reader    : -> (input-port? -> (or/c java-derived-token? 'eof))
;;   Construct a stateful Java derived-token reader.

(provide java-derived-token?
         java-derived-token-text
         java-derived-token-start
         java-derived-token-end
         java-derived-token-tags
         java-derived-token-has-tag?
         make-java-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         "parser-tools-compat.rkt")

;; A Java token plus reusable tags.
(struct java-derived-token (kind text start end tags) #:transparent)

;; One logical Java input character together with its raw source width.
(struct java-logical-char (char raw-width) #:transparent)

;; java-derived-token-has-tag? : java-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (java-derived-token-has-tag? token tag)
  (member tag (java-derived-token-tags token)))

;; trivia-token? : java-derived-token? -> boolean?
;;   Determine whether a derived token is trivia for reader state updates.
(define (trivia-token? token)
  (or (java-derived-token-has-tag? token 'whitespace)
      (java-derived-token-has-tag? token 'comment)))

;; -----------------------------------------------------------------------------
;; Classification tables

;; Reserved Java keywords for the first lexer slice.
(define java-keywords
  (list->set
   '("abstract" "assert" "boolean" "break" "byte" "case" "catch" "char"
     "class" "const" "continue" "default" "do" "double" "else" "enum"
     "extends" "final" "finally" "float" "for" "goto" "if" "implements"
     "import" "instanceof" "int" "interface" "long" "native" "new"
     "package" "private" "protected" "public" "return" "short" "static"
     "strictfp" "super" "switch" "synchronized" "this" "throw" "throws"
     "transient" "try" "void" "volatile" "while" "_" "exports" "module"
     "non-sealed" "open" "opens" "permits" "provides" "record"
     "requires" "sealed" "to" "transitive" "uses" "var" "with" "yield"
     "when")))

;; Java punctuation and operators in longest-match order.
(define java-punctuators
  '(">>>=" "<<=" ">>=" "..." "::" "->" "==" ">=" "<=" "!=" "&&" "||"
    "++" "--" "+=" "-=" "*=" "/=" "&=" "|=" "^=" "%="
    "<<" ">>" ">>>" "(" ")" "[" "]" "{" "}" ";" "," "." "@"
    "=" ">" "<" "!" "~" "?" ":" "+" "-" "*" "/" "&" "|" "^" "%"))

;; Java delimiters and separators.
(define java-delimiters
  (list->set
   '("(" ")" "[" "]" "{" "}" ";" "," "." "..." "@" "::")))

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

;; raw-peek-next : input-port? [exact-nonnegative-integer?] -> (or/c char? eof-object?)
;;   Peek ahead in the raw input stream.
(define (raw-peek-next in [skip 0])
  (peek-char in skip))

;; raw-unicode-escape-at : input-port? exact-nonnegative-integer? exact-nonnegative-integer?
;;                        -> (or/c java-logical-char? #f)
;;   Decode one raw Unicode escape when present and eligible.
(define (raw-unicode-escape-at in skip backslash-run)
  (define first
    (raw-peek-next in skip))
  (cond
    [(or (eof-object? first)
         (not (char=? first #\\))
         (odd? backslash-run))
     #f]
    [else
     (define u-start
       (raw-peek-next in (add1 skip)))
     (cond
       [(or (eof-object? u-start)
            (not (char=? (char-downcase u-start) #\u)))
        #f]
       [else
        (define u-count
          (let loop ([i (add1 skip)]
                     [count 0])
            (define next
              (raw-peek-next in i))
            (cond
              [(and (char? next)
                    (char=? (char-downcase next) #\u))
               (loop (add1 i) (add1 count))]
              [else
               count])))
        (define hex-start
          (+ skip 1 u-count))
        (define hex-chars
          (for/list ([i (in-range 4)])
            (raw-peek-next in (+ hex-start i))))
        (cond
          [(for/and ([ch (in-list hex-chars)])
             (and (char? ch) (hex-digit? ch)))
           (define scalar
             (string->number (list->string hex-chars) 16))
           (define logical-char
             (cond
               [(or (<= #xD800 scalar #xDFFF)
                    (> scalar #x10FFFF))
                #\uFFFD]
               [else
                (integer->char scalar)]))
           (java-logical-char logical-char
                              (+ 1 u-count 4))]
          [else
           #f])])]))

;; peek-java-char : input-port? exact-nonnegative-integer? [exact-nonnegative-integer?]
;;                  -> (or/c char? eof-object?)
;;   Peek ahead in the logical Java character stream.
(define (peek-java-char in [logical-skip 0] [initial-backslash-run 0])
  (define-values (result _skip _run)
    (peek-java-char/scan in logical-skip initial-backslash-run))
  result)

;; peek-java-char/scan : input-port? exact-nonnegative-integer? exact-nonnegative-integer?
;;                       -> (values (or/c char? eof-object?) exact-nonnegative-integer? exact-nonnegative-integer?)
;;   Peek ahead in the logical Java character stream while also returning raw skip and backslash-run state.
(define (peek-java-char/scan in logical-skip initial-backslash-run)
  (let loop ([logical-index 0]
             [raw-skip      0]
             [backslash-run initial-backslash-run])
    (define decoded
      (raw-unicode-escape-at in raw-skip backslash-run))
    (define next-char
      (cond
        [decoded
         (java-logical-char-char decoded)]
        [else
         (raw-peek-next in raw-skip)]))
    (define raw-width
      (cond
        [decoded
         (java-logical-char-raw-width decoded)]
        [(eof-object? next-char)
         0]
        [else
         1]))
    (define next-backslash-run
      (cond
        [(eof-object? next-char)
         backslash-run]
        [(char=? next-char #\\)
         (add1 backslash-run)]
        [else
         0]))
    (cond
      [(= logical-index logical-skip)
       (values next-char raw-skip backslash-run)]
      [(eof-object? next-char)
       (values next-char raw-skip backslash-run)]
      [else
       (loop (add1 logical-index)
             (+ raw-skip raw-width)
             next-backslash-run)])))

;; consume-java-char! : input-port? output-port? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Consume one logical Java character, append its raw source, and return the next backslash-run state.
(define (consume-java-char! in out backslash-run)
  (define decoded
    (raw-unicode-escape-at in 0 backslash-run))
  (define logical-char
    (cond
      [decoded
       (java-logical-char-char decoded)]
      [else
       (define raw-char
         (read-char in))
       (write-char raw-char out)
       raw-char]))
  (when decoded
    (for ([i (in-range (java-logical-char-raw-width decoded))])
      (write-char (read-char in) out)))
  (cond
    [(char=? logical-char #\\)
     (add1 backslash-run)]
    [else
     0]))

;; read-java-while! : input-port? output-port? exact-nonnegative-integer? (char? -> boolean?)
;;                    -> exact-nonnegative-integer?
;;   Consume logical Java characters while pred? holds and return the next backslash-run state.
(define (read-java-while! in out backslash-run pred?)
  (let loop ([backslash-run backslash-run])
    (define next
      (peek-java-char in 0 backslash-run))
    (cond
      [(and (char? next) (pred? next))
       (loop (consume-java-char! in out backslash-run))]
      [else
       backslash-run])))

;; make-token-from-text : position? position? string? (listof symbol?) -> java-derived-token?
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
  (java-derived-token kind
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

;; java-inline-whitespace? : char? -> boolean?
;;   Recognize Java whitespace other than newlines.
(define (java-inline-whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\page)))

;; ident-start? : char? -> boolean?
;;   Recognize one Java identifier-start character.
(define (ident-start? ch)
  (define category
    (char-general-category ch))
  (or (char-alphabetic? ch)
      (char=? ch #\_)
      (char=? ch #\$)
      (member category '(lu ll lt lm lo nl sc pc))))

;; ident-char? : char? -> boolean?
;;   Recognize one Java identifier continuation character.
(define (ident-char? ch)
  (or (ident-start? ch)
      (char-numeric? ch)
      (member (char-general-category ch)
              '(mn mc nd cf))))

;; decimal-digit? : char? -> boolean?
;;   Recognize one decimal digit.
(define (decimal-digit? ch)
  (char-numeric? ch))

;; octal-digit? : char? -> boolean?
;;   Recognize one octal digit.
(define (octal-digit? ch)
  (and (char-numeric? ch)
       (char<=? ch #\7)))

;; binary-digit? : char? -> boolean?
;;   Recognize one binary digit.
(define (binary-digit? ch)
  (or (char=? ch #\0)
      (char=? ch #\1)))

;; hex-digit? : char? -> boolean?
;;   Recognize one hexadecimal digit.
(define (hex-digit? ch)
  (or (decimal-digit? ch)
      (member (char-downcase ch)
              '(#\a #\b #\c #\d #\e #\f))))

;; sign-char? : char? -> boolean?
;;   Recognize one sign character.
(define (sign-char? ch)
  (or (char=? ch #\+)
      (char=? ch #\-)))

;; text-block-leading-whitespace? : char? -> boolean?
;;   Recognize one text-block opening-delimiter whitespace character.
(define (text-block-leading-whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\page)))

;; -----------------------------------------------------------------------------
;; Small scanners

;; read-newline! : input-port? output-port? -> void?
;;   Consume one physical newline sequence, preserving CRLF.
(define (read-newline! in out backslash-run)
  (define next
    (peek-java-char in 0 backslash-run))
  (cond
    [(char=? next #\return)
     (define next-run
       (consume-java-char! in out backslash-run))
     (cond
       [(and (char? (peek-java-char in 0 next-run))
             (char=? (peek-java-char in 0 next-run) #\newline))
        (consume-java-char! in out next-run)]
       [else
        next-run])]
    [else
     (consume-java-char! in out backslash-run)]))

;; read-whitespace! : input-port? output-port? -> void?
;;   Consume one run of Java whitespace.
(define (read-whitespace! in out backslash-run)
  (let loop ([backslash-run backslash-run])
    (define next
      (peek-java-char in 0 backslash-run))
    (cond
      [(eof-object? next)
       backslash-run]
      [(newline-start? next)
       (loop (read-newline! in out backslash-run))]
      [(java-inline-whitespace? next)
       (loop (consume-java-char! in out backslash-run))]
      [else
       backslash-run])))

;; read-line-comment! : input-port? output-port? -> void?
;;   Consume one Java line comment without its terminating newline.
(define (read-line-comment! in out backslash-run)
  (define after-first
    (consume-java-char! in out backslash-run))
  (define after-second
    (consume-java-char! in out after-first))
  (let loop ([backslash-run after-second])
    (define next
      (peek-java-char in 0 backslash-run))
    (cond
      [(or (eof-object? next)
           (newline-start? next))
       backslash-run]
      [else
       (loop (consume-java-char! in out backslash-run))])))

;; read-block-comment! : input-port? output-port? exact-nonnegative-integer?
;;                      -> (values (or/c 'terminated 'doc 'unterminated 'unterminated-doc)
;;                                 exact-nonnegative-integer?)
;;   Consume one Java block comment and report whether it terminated and whether it is a doc comment.
(define (read-block-comment! in out backslash-run)
  (define doc?
    (and (char? (peek-java-char in 2 backslash-run))
         (char=? (peek-java-char in 2 backslash-run) #\*)
         (not (and (char? (peek-java-char in 3 backslash-run))
                   (char=? (peek-java-char in 3 backslash-run) #\/)))))
  (define after-first
    (consume-java-char! in out backslash-run))
  (define after-second
    (consume-java-char! in out after-first))
  (let loop ([backslash-run after-second])
    (define next
      (peek-java-char in 0 backslash-run))
    (cond
      [(eof-object? next)
       (values (cond
                 [doc? 'unterminated-doc]
                 [else 'unterminated])
               backslash-run)]
      [(and (char=? next #\*)
            (char? (peek-java-char in 1 backslash-run))
            (char=? (peek-java-char in 1 backslash-run) #\/))
       (define after-star
         (consume-java-char! in out backslash-run))
       (define after-slash
         (consume-java-char! in out after-star))
       (values (cond
                 [doc? 'doc]
                 [else 'terminated])
               after-slash)]
      [else
       (loop (consume-java-char! in out backslash-run))])))

;; read-identifier! : input-port? output-port? -> void?
;;   Consume one identifier-like token.
(define (read-identifier! in out backslash-run)
  (read-java-while! in out backslash-run ident-char?))

;; read-digit-sequence! : input-port? output-port? (char? -> boolean?) -> void?
;;   Consume one digit sequence that may contain separating underscores.
(define (read-digit-sequence! in out backslash-run digit?)
  (let loop ([backslash-run backslash-run]
             [saw-digit?     #f])
    (define next
      (peek-java-char in 0 backslash-run))
    (cond
      [(and (char? next) (digit? next))
       (loop (consume-java-char! in out backslash-run) #t)]
      [(and saw-digit?
            (char? next)
            (char=? next #\_)
            (char? (peek-java-char in 1 backslash-run))
            (digit? (peek-java-char in 1 backslash-run)))
       (define after-underscore
         (consume-java-char! in out backslash-run))
       (loop (consume-java-char! in out after-underscore) #t)]
      [else
       backslash-run])))

;; read-int-suffix! : input-port? output-port? -> void?
;;   Consume an integer suffix when present.
(define (read-int-suffix! in out backslash-run)
  (cond
    [(and (char? (peek-java-char in 0 backslash-run))
          (member (char-downcase (peek-java-char in 0 backslash-run))
                  '(#\l)))
     (consume-java-char! in out backslash-run)]
    [else
     backslash-run]))

;; read-float-suffix! : input-port? output-port? -> void?
;;   Consume a floating suffix when present.
(define (read-float-suffix! in out backslash-run)
  (cond
    [(and (char? (peek-java-char in 0 backslash-run))
          (member (char-downcase (peek-java-char in 0 backslash-run))
                  '(#\f #\d)))
     (consume-java-char! in out backslash-run)]
    [else
     backslash-run]))

;; read-decimal-exponent! : input-port? output-port? -> void?
;;   Consume one decimal exponent when present.
(define (read-decimal-exponent! in out backslash-run)
  (cond
    [(and (char? (peek-java-char in 0 backslash-run))
          (member (peek-java-char in 0 backslash-run) '(#\e #\E)))
     (define after-e
       (consume-java-char! in out backslash-run))
     (define after-sign
       (cond
         [(and (char? (peek-java-char in 0 after-e))
               (sign-char? (peek-java-char in 0 after-e)))
          (consume-java-char! in out after-e)]
         [else
          after-e]))
     (read-digit-sequence! in out after-sign decimal-digit?)]
    [else
     backslash-run]))

;; read-binary-exponent! : input-port? output-port? -> void?
;;   Consume one hexadecimal floating exponent when present.
(define (read-binary-exponent! in out backslash-run)
  (cond
    [(and (char? (peek-java-char in 0 backslash-run))
          (member (peek-java-char in 0 backslash-run) '(#\p #\P)))
     (define after-p
       (consume-java-char! in out backslash-run))
     (define after-sign
       (cond
         [(and (char? (peek-java-char in 0 after-p))
               (sign-char? (peek-java-char in 0 after-p)))
          (consume-java-char! in out after-p)]
         [else
          after-p]))
     (read-digit-sequence! in out after-sign decimal-digit?)]
    [else
     backslash-run]))

;; read-decimal-number! : input-port? output-port? -> void?
;;   Consume one decimal integer or floating literal.
(define (read-decimal-number! in out backslash-run)
  (define after-digits
    (read-digit-sequence! in out backslash-run decimal-digit?))
  (cond
    [(and (char? (peek-java-char in 0 after-digits))
          (char=? (peek-java-char in 0 after-digits) #\.)
          (not (and (char? (peek-java-char in 1 after-digits))
                    (char=? (peek-java-char in 1 after-digits) #\.))))
     (define after-dot
       (consume-java-char! in out after-digits))
     (define after-fraction
       (read-digit-sequence! in out after-dot decimal-digit?))
     (define after-exponent
       (read-decimal-exponent! in out after-fraction))
     (read-float-suffix! in out after-exponent)]
    [else
     (define after-exponent
       (read-decimal-exponent! in out after-digits))
     (cond
       [(and (char? (peek-java-char in 0 after-exponent))
             (member (char-downcase (peek-java-char in 0 after-exponent))
                     '(#\f #\d)))
        (read-float-suffix! in out after-exponent)]
       [else
        (read-int-suffix! in out after-exponent)])]))

;; read-number-from-dot! : input-port? output-port? -> void?
;;   Consume one floating literal that starts with a decimal point.
(define (read-number-from-dot! in out backslash-run)
  (define after-dot
    (consume-java-char! in out backslash-run))
  (define after-fraction
    (read-digit-sequence! in out after-dot decimal-digit?))
  (define after-exponent
    (read-decimal-exponent! in out after-fraction))
  (read-float-suffix! in out after-exponent))

;; read-prefixed-integer! : input-port? output-port? (char? -> boolean?) -> void?
;;   Consume one prefixed integer literal and an optional integer suffix.
(define (read-prefixed-integer! in out backslash-run digit?)
  (define after-digits
    (read-digit-sequence! in out backslash-run digit?))
  (read-int-suffix! in out after-digits))

;; read-hex-number! : input-port? output-port? -> void?
;;   Consume one hexadecimal integer or floating literal.
(define (read-hex-number! in out backslash-run)
  (define after-digits
    (read-digit-sequence! in out backslash-run hex-digit?))
  (cond
    [(and (char? (peek-java-char in 0 after-digits))
          (char=? (peek-java-char in 0 after-digits) #\.))
     (define after-dot
       (consume-java-char! in out after-digits))
     (define after-fraction
       (read-digit-sequence! in out after-dot hex-digit?))
     (define after-exponent
       (read-binary-exponent! in out after-fraction))
     (read-float-suffix! in out after-exponent)]
    [(and (char? (peek-java-char in 0 after-digits))
          (member (peek-java-char in 0 after-digits) '(#\p #\P)))
     (define after-exponent
       (read-binary-exponent! in out after-digits))
     (read-float-suffix! in out after-exponent)]
    [else
     (read-int-suffix! in out after-digits)]))

;; read-number! : input-port? output-port? -> void?
;;   Consume one Java numeric literal.
(define (read-number! in out backslash-run)
  (cond
    [(and (char? (peek-java-char in 0 backslash-run))
          (char=? (peek-java-char in 0 backslash-run) #\0)
          (char? (peek-java-char in 1 backslash-run))
          (member (peek-java-char in 1 backslash-run) '(#\x #\X)))
     (define after-zero
       (consume-java-char! in out backslash-run))
     (define after-prefix
       (consume-java-char! in out after-zero))
     (read-hex-number! in out after-prefix)]
    [(and (char? (peek-java-char in 0 backslash-run))
          (char=? (peek-java-char in 0 backslash-run) #\0)
          (char? (peek-java-char in 1 backslash-run))
          (member (peek-java-char in 1 backslash-run) '(#\b #\B)))
     (define after-zero
       (consume-java-char! in out backslash-run))
     (define after-prefix
       (consume-java-char! in out after-zero))
     (read-prefixed-integer! in out after-prefix binary-digit?)]
    [(and (char? (peek-java-char in 0 backslash-run))
          (char=? (peek-java-char in 0 backslash-run) #\0)
          (char? (peek-java-char in 1 backslash-run))
          (octal-digit? (peek-java-char in 1 backslash-run)))
     (define after-zero
       (consume-java-char! in out backslash-run))
     (read-prefixed-integer! in out after-zero octal-digit?)]
    [else
     (read-decimal-number! in out backslash-run)]))

;; starts-text-block? : input-port? exact-nonnegative-integer? -> boolean?
;;   Determine whether the next input begins a Java text block opening delimiter.
(define (starts-text-block? in backslash-run)
  (and (char=? (peek-java-char in 0 backslash-run) #\")
       (char? (peek-java-char in 1 backslash-run))
       (char=? (peek-java-char in 1 backslash-run) #\")
       (char? (peek-java-char in 2 backslash-run))
       (char=? (peek-java-char in 2 backslash-run) #\")
       (let loop ([i 3])
         (define next
           (peek-java-char in i backslash-run))
         (cond
           [(and (char? next)
                 (text-block-leading-whitespace? next))
            (loop (add1 i))]
           [else
            (and (char? next)
                 (newline-start? next))]))))

;; read-text-block-opening! : input-port? output-port? exact-nonnegative-integer?
;;                            -> exact-nonnegative-integer?
;;   Consume one Java text-block opening delimiter.
(define (read-text-block-opening! in out backslash-run)
  (define after-1
    (consume-java-char! in out backslash-run))
  (define after-2
    (consume-java-char! in out after-1))
  (define after-3
    (consume-java-char! in out after-2))
  (let loop ([backslash-run after-3])
    (define next
      (peek-java-char in 0 backslash-run))
    (cond
      [(and (char? next)
            (text-block-leading-whitespace? next))
       (loop (consume-java-char! in out backslash-run))]
      [else
       (read-newline! in out backslash-run)])))

;; read-java-octal-escape! : input-port? output-port? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Consume one Java octal escape after its leading backslash.
(define (read-java-octal-escape! in out backslash-run)
  (define first-digit
    (peek-java-char in 0 backslash-run))
  (define after-first
    (consume-java-char! in out backslash-run))
  (define after-second
    (cond
      [(and (char? (peek-java-char in 0 after-first))
            (octal-digit? (peek-java-char in 0 after-first)))
       (consume-java-char! in out after-first)]
      [else
       after-first]))
  (cond
    [(and (char? first-digit)
          (member first-digit '(#\0 #\1 #\2 #\3))
          (char? (peek-java-char in 0 after-second))
          (octal-digit? (peek-java-char in 0 after-second)))
     (consume-java-char! in out after-second)]
    [else
     after-second]))

;; read-java-escape! : input-port? output-port? exact-nonnegative-integer? boolean?
;;                     -> (values boolean? exact-nonnegative-integer?)
;;   Consume one Java escape sequence and report whether it is lexically valid.
(define (read-java-escape! in out backslash-run allow-line-continuation?)
  (define after-backslash
    (consume-java-char! in out backslash-run))
  (define next
    (peek-java-char in 0 after-backslash))
  (cond
    [(eof-object? next)
     (values #f after-backslash)]
    [(member next '(#\b #\s #\t #\n #\f #\r #\" #\' #\\))
     (values #t
             (consume-java-char! in out after-backslash))]
    [(octal-digit? next)
     (values #t
             (read-java-octal-escape! in out after-backslash))]
    [(newline-start? next)
     (cond
       [allow-line-continuation?
        (values #t
                (read-newline! in out after-backslash))]
       [else
        (values #f after-backslash)])]
    [else
     (values #f
             (consume-java-char! in out after-backslash))]))

;; read-string-literal! : input-port? output-port? exact-nonnegative-integer?
;;                       -> (values boolean? boolean? exact-nonnegative-integer?)
;;   Consume one Java string literal and report whether it terminated.
(define (read-string-literal! in out backslash-run)
  (define after-open
    (consume-java-char! in out backslash-run))
  (let loop ([backslash-run after-open]
             [valid?        #t])
    (define next
      (peek-java-char in 0 backslash-run))
    (cond
      [(eof-object? next)
       (values #f valid? backslash-run)]
      [(newline-start? next)
       (values #f valid? backslash-run)]
      [(char=? next #\\)
       (define-values (escape-valid? next-run)
         (read-java-escape! in out backslash-run #f))
       (loop next-run
             (and valid? escape-valid?))]
      [(char=? next #\")
       (values #t
               valid?
               (consume-java-char! in out backslash-run))]
      [else
       (loop (consume-java-char! in out backslash-run)
             valid?)])))

;; read-char-literal! : input-port? output-port? exact-nonnegative-integer?
;;                     -> (values boolean? boolean? exact-nonnegative-integer?)
;;   Consume one Java char literal and report whether it terminated.
(define (read-char-literal! in out backslash-run)
  (define after-open
    (consume-java-char! in out backslash-run))
  (let loop ([backslash-run after-open]
             [valid?        #t]
             [unit-count    0])
    (define next
      (peek-java-char in 0 backslash-run))
    (cond
      [(eof-object? next)
       (values #f #f backslash-run)]
      [(newline-start? next)
       (values #f #f backslash-run)]
      [(char=? next #\\)
       (define-values (escape-valid? next-run)
         (read-java-escape! in out backslash-run #f))
       (loop next-run
             (and valid? escape-valid?)
             (add1 unit-count))]
      [(char=? next #\')
       (values #t
               (and valid? (= unit-count 1))
               (consume-java-char! in out backslash-run))]
      [else
      (define next-run
         (consume-java-char! in out backslash-run))
       (loop next-run
             (and valid?
                  (<= (char->integer next) #xFFFF))
             (add1 unit-count))])))

;; read-text-block! : input-port? output-port? exact-nonnegative-integer?
;;                   -> (values boolean? boolean? exact-nonnegative-integer?)
;;   Consume one Java text block and report whether it terminated.
(define (read-text-block! in out backslash-run)
  (define after-open
    (read-text-block-opening! in out backslash-run))
  (let loop ([backslash-run after-open]
             [valid?       #t]
             [backslashes   0])
    (define next
      (peek-java-char in 0 backslash-run))
    (cond
      [(eof-object? next)
       (values #f valid? backslash-run)]
      [(and (char=? next #\")
            (char? (peek-java-char in 1 backslash-run))
            (char=? (peek-java-char in 1 backslash-run) #\")
            (char? (peek-java-char in 2 backslash-run))
            (char=? (peek-java-char in 2 backslash-run) #\")
            (even? backslashes))
       (define after-close-1
         (consume-java-char! in out backslash-run))
       (define after-close-2
         (consume-java-char! in out after-close-1))
       (values #t
               valid?
               (consume-java-char! in out after-close-2))]
      [(char=? next #\\)
       (define-values (escape-valid? next-run)
         (read-java-escape! in out backslash-run #t))
       (loop next-run
             (and valid? escape-valid?)
             0)]
      [else
       (define next-run
         (consume-java-char! in out backslash-run))
       (loop next-run valid? 0)])))

;; punctuator-at : input-port? -> (or/c string? #f)
;;   Find the longest Java punctuator at the current port position.
(define (punctuator-at in backslash-run)
  (for/or ([candidate (in-list java-punctuators)])
    (define len
      (string-length candidate))
    (define ok?
      (for/and ([i (in-range len)])
        (define next
          (peek-java-char in i backslash-run))
        (and (char? next)
             (char=? next (string-ref candidate i)))))
    (and ok? candidate)))

;; consume-punctuator! : input-port? output-port? string? -> void?
;;   Consume one known punctuator string.
(define (consume-punctuator! in out backslash-run punctuator)
  (for/fold ([backslash-run backslash-run])
            ([ch (in-string punctuator)])
    (consume-java-char! in out backslash-run)))

;; non-sealed-keyword-at? : input-port? exact-nonnegative-integer? -> boolean?
;;   Determine whether the next input begins the contextual keyword @tt{non-sealed}.
(define (non-sealed-keyword-at? in backslash-run)
  (define candidate
    "non-sealed")
  (define matches?
    (for/and ([i (in-range (string-length candidate))])
      (define next
        (peek-java-char in i backslash-run))
      (and (char? next)
           (char=? next (string-ref candidate i)))))
  (and matches?
       (let ([after
              (peek-java-char in (string-length candidate) backslash-run)])
         (or (eof-object? after)
             (not (ident-char? after))))))

;; consume-contextual-keyword! : input-port? output-port? exact-nonnegative-integer? string?
;;                               -> exact-nonnegative-integer?
;;   Consume one contextual keyword with fixed source spelling.
(define (consume-contextual-keyword! in out backslash-run keyword)
  (for/fold ([backslash-run backslash-run])
            ([ch (in-string keyword)])
    (consume-java-char! in out backslash-run)))

;; -----------------------------------------------------------------------------
;; Reader

;; make-java-derived-reader : -> (input-port? -> (or/c java-derived-token? 'eof))
;;   Construct a stateful Java derived-token reader.
(define (make-java-derived-reader)
  (define annotation-context? #f)
  (define backslash-run 0)
  (lambda (in)
    (define start-pos
      (current-stream-position in))
    (define next
      (peek-java-char in 0 backslash-run))
    (cond
      [(eof-object? next)
       'eof]
      [else
       (define out
         (open-output-string))
       (define tags
         (cond
           [(or (java-inline-whitespace? next)
                (newline-start? next))
            (set! backslash-run (read-whitespace! in out backslash-run))
            '(whitespace java-whitespace)]
           [(and (char=? next #\/)
                 (char? (peek-java-char in 1 backslash-run))
                 (char=? (peek-java-char in 1 backslash-run) #\/))
            (set! backslash-run (read-line-comment! in out backslash-run))
            '(comment java-comment java-line-comment)]
           [(and (char=? next #\/)
                 (char? (peek-java-char in 1 backslash-run))
                 (char=? (peek-java-char in 1 backslash-run) #\*))
            (define-values (comment-state next-backslash-run)
              (read-block-comment! in out backslash-run))
            (set! backslash-run next-backslash-run)
            (cond
              [(eq? comment-state 'terminated)
               '(comment java-comment java-block-comment)]
              [(eq? comment-state 'doc)
               '(comment java-comment java-block-comment java-doc-comment)]
              [else
               '(comment java-comment java-block-comment malformed-token)])]
           [(starts-text-block? in backslash-run)
            (define-values (terminated? valid? next-backslash-run)
              (read-text-block! in out backslash-run))
            (set! backslash-run next-backslash-run)
            (cond
              [(and terminated? valid?)
               '(literal java-text-block)]
              [else
               '(literal java-text-block malformed-token)])]
           [(char=? next #\")
            (define-values (terminated? valid? next-backslash-run)
              (read-string-literal! in out backslash-run))
            (set! backslash-run next-backslash-run)
            (cond
              [(and terminated? valid?)
               '(literal java-string-literal)]
              [else
               '(literal java-string-literal malformed-token)])]
           [(char=? next #\')
            (define-values (terminated? valid? next-backslash-run)
              (read-char-literal! in out backslash-run))
            (set! backslash-run next-backslash-run)
            (cond
              [(and terminated? valid?)
               '(literal java-char-literal)]
              [else
               '(literal java-char-literal malformed-token)])]
           [(non-sealed-keyword-at? in backslash-run)
            (set! backslash-run
                  (consume-contextual-keyword! in out backslash-run "non-sealed"))
            '(keyword java-keyword)]
           [(ident-start? next)
            (set! backslash-run (read-identifier! in out backslash-run))
            (define text
              (get-output-string out))
            (cond
              [(set-member? java-keywords text)
               '(keyword java-keyword)]
              [(string=? text "true")
               '(literal java-literal java-boolean-literal java-true-literal)]
              [(string=? text "false")
               '(literal java-literal java-boolean-literal java-false-literal)]
              [(string=? text "null")
               '(literal java-literal java-null-literal)]
              [annotation-context?
               '(identifier java-identifier java-annotation-name)]
              [else
               '(identifier java-identifier)])]
           [(decimal-digit? next)
            (set! backslash-run (read-number! in out backslash-run))
            '(literal java-numeric-literal)]
           [(and (char=? next #\.)
                 (char? (peek-java-char in 1 backslash-run))
                 (decimal-digit? (peek-java-char in 1 backslash-run)))
            (set! backslash-run (read-number-from-dot! in out backslash-run))
            '(literal java-numeric-literal)]
           [else
            (define punctuator
              (punctuator-at in backslash-run))
            (cond
              [punctuator
               (set! backslash-run
                     (consume-punctuator! in out backslash-run punctuator))
               (cond
                 [(set-member? java-delimiters punctuator)
                  (cond
                    [(string=? punctuator "@")
                     '(delimiter java-delimiter java-annotation-marker)]
                    [else
                     '(delimiter java-delimiter)])]
                 [else
                 '(operator java-operator)])]
              [else
               (set! backslash-run (consume-java-char! in out backslash-run))
               '(malformed-token)])]))
       (define text
         (get-output-string out))
       (define end-pos
         (current-stream-position in))
       (define token
         (make-token-from-text start-pos end-pos text tags))
       (cond
         [(java-derived-token-has-tag? token 'java-annotation-marker)
          (set! annotation-context? #t)]
         [(trivia-token? token)
          (void)]
         [else
          (set! annotation-context? #f)])
       token])))
