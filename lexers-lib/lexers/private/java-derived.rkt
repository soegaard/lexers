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
;;   Recognize an identifier-start character for the first Java slice.
(define (ident-start? ch)
  (or (char-alphabetic? ch)
      (char=? ch #\_)
      (char=? ch #\$)))

;; ident-char? : char? -> boolean?
;;   Recognize an identifier continuation character.
(define (ident-char? ch)
  (or (ident-start? ch)
      (char-numeric? ch)))

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
;;   Consume one run of Java whitespace.
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
      [(java-inline-whitespace? next)
       (write-one! in out)
       (loop)]
      [else
       (void)])))

;; read-line-comment! : input-port? output-port? -> void?
;;   Consume one Java line comment without its terminating newline.
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

;; read-block-comment! : input-port? output-port? -> (or/c 'terminated 'doc 'unterminated 'unterminated-doc)
;;   Consume one Java block comment and report whether it terminated and whether it is a doc comment.
(define (read-block-comment! in out)
  (define doc?
    (and (char? (peek-next in 2))
         (char=? (peek-next in 2) #\*)
         (not (and (char? (peek-next in 3))
                   (char=? (peek-next in 3) #\/)))))
  (write-one! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       (cond
         [doc? 'unterminated-doc]
         [else 'unterminated])]
      [(and (char=? next #\*)
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\/))
       (write-one! in out)
       (write-one! in out)
       (cond
         [doc? 'doc]
         [else 'terminated])]
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

;; read-int-suffix! : input-port? output-port? -> void?
;;   Consume an integer suffix when present.
(define (read-int-suffix! in out)
  (when (and (char? (peek-next in))
             (member (char-downcase (peek-next in))
                     '(#\l)))
    (write-one! in out)))

;; read-float-suffix! : input-port? output-port? -> void?
;;   Consume a floating suffix when present.
(define (read-float-suffix! in out)
  (when (and (char? (peek-next in))
             (member (char-downcase (peek-next in))
                     '(#\f #\d)))
    (write-one! in out)))

;; read-decimal-exponent! : input-port? output-port? -> void?
;;   Consume one decimal exponent when present.
(define (read-decimal-exponent! in out)
  (when (and (char? (peek-next in))
             (member (peek-next in) '(#\e #\E)))
    (write-one! in out)
    (when (and (char? (peek-next in))
               (sign-char? (peek-next in)))
      (write-one! in out))
    (read-digit-sequence! in out decimal-digit?)))

;; read-binary-exponent! : input-port? output-port? -> void?
;;   Consume one hexadecimal floating exponent when present.
(define (read-binary-exponent! in out)
  (when (and (char? (peek-next in))
             (member (peek-next in) '(#\p #\P)))
    (write-one! in out)
    (when (and (char? (peek-next in))
               (sign-char? (peek-next in)))
      (write-one! in out))
    (read-digit-sequence! in out decimal-digit?)))

;; read-decimal-number! : input-port? output-port? -> void?
;;   Consume one decimal integer or floating literal.
(define (read-decimal-number! in out)
  (read-digit-sequence! in out decimal-digit?)
  (cond
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\.)
          (not (and (char? (peek-next in 1))
                    (char=? (peek-next in 1) #\.))))
     (write-one! in out)
     (read-digit-sequence! in out decimal-digit?)
     (read-decimal-exponent! in out)
     (read-float-suffix! in out)]
    [else
     (read-decimal-exponent! in out)
     (cond
       [(and (char? (peek-next in))
             (member (char-downcase (peek-next in)) '(#\f #\d)))
        (read-float-suffix! in out)]
       [else
        (read-int-suffix! in out)])]))

;; read-number-from-dot! : input-port? output-port? -> void?
;;   Consume one floating literal that starts with a decimal point.
(define (read-number-from-dot! in out)
  (write-one! in out)
  (read-digit-sequence! in out decimal-digit?)
  (read-decimal-exponent! in out)
  (read-float-suffix! in out))

;; read-prefixed-integer! : input-port? output-port? (char? -> boolean?) -> void?
;;   Consume one prefixed integer literal and an optional integer suffix.
(define (read-prefixed-integer! in out digit?)
  (read-digit-sequence! in out digit?)
  (read-int-suffix! in out))

;; read-hex-number! : input-port? output-port? -> void?
;;   Consume one hexadecimal integer or floating literal.
(define (read-hex-number! in out)
  (read-digit-sequence! in out hex-digit?)
  (cond
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\.))
     (write-one! in out)
     (read-digit-sequence! in out hex-digit?)
     (read-binary-exponent! in out)
     (read-float-suffix! in out)]
    [(and (char? (peek-next in))
          (member (peek-next in) '(#\p #\P)))
     (read-binary-exponent! in out)
     (read-float-suffix! in out)]
    [else
     (read-int-suffix! in out)]))

;; read-number! : input-port? output-port? -> void?
;;   Consume one Java numeric literal.
(define (read-number! in out)
  (cond
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\0)
          (char? (peek-next in 1))
          (member (peek-next in 1) '(#\x #\X)))
     (write-one! in out)
     (write-one! in out)
     (read-hex-number! in out)]
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\0)
          (char? (peek-next in 1))
          (member (peek-next in 1) '(#\b #\B)))
     (write-one! in out)
     (write-one! in out)
     (read-prefixed-integer! in out binary-digit?)]
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\0)
          (char? (peek-next in 1))
          (octal-digit? (peek-next in 1)))
     (write-one! in out)
     (read-prefixed-integer! in out octal-digit?)]
    [else
     (read-decimal-number! in out)]))

;; read-string-literal! : input-port? output-port? -> boolean?
;;   Consume one Java string literal and report whether it terminated.
(define (read-string-literal! in out)
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

;; read-char-literal! : input-port? output-port? -> boolean?
;;   Consume one Java char literal and report whether it terminated.
(define (read-char-literal! in out)
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

;; read-text-block! : input-port? output-port? -> boolean?
;;   Consume one Java text block and report whether it terminated.
(define (read-text-block! in out)
  (write-one! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ([backslashes 0])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(and (char=? next #\")
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\")
            (char? (peek-next in 2))
            (char=? (peek-next in 2) #\")
            (even? backslashes))
       (write-one! in out)
       (write-one! in out)
       (write-one! in out)
       #t]
      [else
       (write-one! in out)
       (cond
         [(char=? next #\\)
          (loop (add1 backslashes))]
         [else
          (loop 0)])])))

;; punctuator-at : input-port? -> (or/c string? #f)
;;   Find the longest Java punctuator at the current port position.
(define (punctuator-at in)
  (for/or ([candidate (in-list java-punctuators)])
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

;; make-java-derived-reader : -> (input-port? -> (or/c java-derived-token? 'eof))
;;   Construct a stateful Java derived-token reader.
(define (make-java-derived-reader)
  (define annotation-context? #f)
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
           [(or (java-inline-whitespace? next)
                (newline-start? next))
            (read-whitespace! in out)
            '(whitespace java-whitespace)]
           [(and (char=? next #\/)
                 (char? (peek-next in 1))
                 (char=? (peek-next in 1) #\/))
            (read-line-comment! in out)
            '(comment java-comment java-line-comment)]
           [(and (char=? next #\/)
                 (char? (peek-next in 1))
                 (char=? (peek-next in 1) #\*))
            (define comment-state
              (read-block-comment! in out))
            (cond
              [(eq? comment-state 'terminated)
               '(comment java-comment java-block-comment)]
              [(eq? comment-state 'doc)
               '(comment java-comment java-block-comment java-doc-comment)]
              [else
               '(comment java-comment java-block-comment malformed-token)])]
           [(and (char=? next #\")
                 (char? (peek-next in 1))
                 (char=? (peek-next in 1) #\")
                 (char? (peek-next in 2))
                 (char=? (peek-next in 2) #\"))
            (define terminated?
              (read-text-block! in out))
            (cond
              [terminated?
               '(literal java-text-block)]
              [else
               '(literal java-text-block malformed-token)])]
           [(char=? next #\")
            (define terminated?
              (read-string-literal! in out))
            (cond
              [terminated?
               '(literal java-string-literal)]
              [else
               '(literal java-string-literal malformed-token)])]
           [(char=? next #\')
            (define terminated?
              (read-char-literal! in out))
            (cond
              [terminated?
               '(literal java-char-literal)]
              [else
               '(literal java-char-literal malformed-token)])]
           [(ident-start? next)
            (read-identifier! in out)
            (define text
              (get-output-string out))
            (cond
              [(set-member? java-keywords text)
               '(keyword java-keyword)]
              [(member text '("true" "false" "null"))
               '(literal java-literal)]
              [annotation-context?
               '(identifier java-identifier java-annotation-name)]
              [else
               '(identifier java-identifier)])]
           [(decimal-digit? next)
            (read-number! in out)
            '(literal java-numeric-literal)]
           [(and (char=? next #\.)
                 (char? (peek-next in 1))
                 (decimal-digit? (peek-next in 1)))
            (read-number-from-dot! in out)
            '(literal java-numeric-literal)]
           [else
            (define punctuator
              (punctuator-at in))
            (cond
              [punctuator
               (consume-punctuator! in out punctuator)
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
               (write-one! in out)
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
