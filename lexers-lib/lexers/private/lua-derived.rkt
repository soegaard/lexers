#lang racket/base

;;;
;;; Lua Derived Tokens
;;;

;; Streaming Lua tokenization and reusable Lua-specific classifications.

(provide lua-derived-token?
         lua-derived-token-text
         lua-derived-token-start
         lua-derived-token-end
         lua-derived-token-tags
         lua-derived-token-has-tag?
         make-lua-derived-reader)

(require parser-tools/lex
         racket/list
         "parser-tools-compat.rkt")

;; A Lua token plus reusable tags.
(struct lua-derived-token (kind text start end tags) #:transparent)

;; lua-derived-token-has-tag? : lua-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (lua-derived-token-has-tag? token tag)
  (member tag (lua-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Classification tables

;; Lua reserved words.
(define lua-keywords
  '("and" "break" "do" "else" "elseif" "end" "false" "for" "function"
    "goto" "if" "in" "local" "nil" "not" "or" "repeat" "return" "then"
    "true" "until" "while"))

;; Lua operators in longest-match order.
(define lua-operators
  '("..." "//" "<<" ">>" ".." "==" "~=" "<=" ">=" "::"
    "+" "-" "*" "/" "%" "^" "#" "&" "~" "|" "<" ">" "="))

;; Lua punctuation tokens.
(define lua-delimiters '("(" ")" "{" "}" "[" "]" ";" ":" "," "."))

;; -----------------------------------------------------------------------------
;; Port helpers

;; peek-next : input-port? [exact-nonnegative-integer?] -> (or/c char? eof-object?)
;;   Peek ahead in an input port without consuming characters.
(define (peek-next in [skip 0]) (peek-char in skip))

;; write-one! : input-port? output-port? -> void?
;;   Consume one character and append it to the output accumulator.
(define (write-one! in out) (write-char (read-char in) out))

;; peek-string=? : input-port? string? -> boolean?
;;   Determine whether upcoming input matches text.
(define (peek-string=? in text)
  (for/and ([ch (in-string text)] [i (in-naturals)])
    (define next (peek-next in i))
    (and (char? next) (char=? next ch))))

;; current-stream-position : input-port? -> position?
;;   Read a parser-tools-compatible source position from a port.
(define (current-stream-position in)
  (let-values ([(line col offset) (port-next-location in)])
    (make-stream-position (if (exact-positive-integer? offset) offset 1)
                          (if (exact-positive-integer? line) line 1)
                          (if (exact-nonnegative-integer? col) col 0))))

;; make-token : input-port? position? string? (listof symbol?) -> lua-derived-token?
;;   Construct a token ending at the input port's current position.
(define (make-token in start text tags)
  (define kind
    (cond [(member 'comment tags) 'comment]
          [(member 'whitespace tags) 'whitespace]
          [(member 'malformed-token tags) 'malformed]
          [(member 'keyword tags) 'keyword]
          [(member 'literal tags) 'literal]
          [(member 'operator tags) 'operator]
          [(member 'delimiter tags) 'delimiter]
          [else 'identifier]))
  (lua-derived-token kind text start (current-stream-position in)
                     (remove-duplicates tags)))

;; take-while! : input-port? output-port? (char? -> boolean?) -> void?
;;   Consume consecutive characters accepted by predicate.
(define (take-while! in out predicate)
  (let loop ()
    (define next (peek-next in))
    (when (and (char? next) (predicate next))
      (write-one! in out)
      (loop))))

;; -----------------------------------------------------------------------------
;; Lexical helpers

;; identifier-start? : char? -> boolean?
;;   Recognize a Lua identifier's initial character.
(define (identifier-start? ch) (or (char-alphabetic? ch) (char=? ch #\_)))

;; identifier-char? : char? -> boolean?
;;   Recognize a Lua identifier continuation character.
(define (identifier-char? ch) (or (identifier-start? ch) (char-numeric? ch)))

;; number-char? : char? -> boolean?
;;   Recognize a character in a Lua numeral candidate.
(define (number-char? ch)
  (or (char-numeric? ch) (char-alphabetic? ch) (member ch '(#\_ #\. #\+ #\-))))

;; lua-number? : string? -> boolean?
;;   Recognize Lua 5.4 decimal and hexadecimal numeral forms.
(define (lua-number? text)
  (or (regexp-match? #px"^(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$" text)
      (regexp-match? #px"^0[xX](?:[0-9A-Fa-f]+(?:\\.[0-9A-Fa-f]*)?|\\.[0-9A-Fa-f]+)(?:[pP][+-]?[0-9]+)?$" text)))

;; read-number! : input-port? output-port? -> void?
;;   Consume a Lua numeral candidate without absorbing a following operator.
(define (read-number! in out)
  (define first (peek-next in))
  (write-one! in out)
  (define hexadecimal?
    (and (char=? first #\0)
         (char? (peek-next in))
         (member (peek-next in) '(#\x #\X))))
  (when hexadecimal? (write-one! in out))
  (let loop ()
    (define next (peek-next in))
    (cond
      [(eof-object? next) (void)]
      [(or (and hexadecimal? (member next '(#\p #\P)))
           (and (not hexadecimal?) (member next '(#\e #\E))))
       (write-one! in out)
       (when (and (char? (peek-next in))
                  (member (peek-next in) '(#\+ #\-)))
         (write-one! in out))
       (loop)]
      [(or (char=? next #\.)
           (and hexadecimal? (or (char-numeric? next)
                                 (member (char-downcase next) '(#\a #\b #\c #\d #\e #\f))))
           (and (not hexadecimal?) (char-numeric? next)))
       (write-one! in out)
       (loop)]
      [else (void)])))

;; long-bracket-level : input-port? -> (or/c exact-nonnegative-integer? #f)
;;   Return the equals-sign count in an opening long-bracket delimiter.
(define (long-bracket-level in)
  (cond
    [(not (and (char? (peek-next in)) (char=? (peek-next in) #\[))) #f]
    [else
     (let loop ([index 1] [equals 0])
       (define next (peek-next in index))
       (cond [(and (char? next) (char=? next #\=)) (loop (add1 index) (add1 equals))]
             [(and (char? next) (char=? next #\[)) equals]
             [else #f]))]))

;; read-long-bracket! : input-port? output-port? exact-nonnegative-integer? -> boolean?
;;   Consume a long-bracket string or comment and report whether it closes.
(define (read-long-bracket! in out level)
  (define opener (string-append "[" (make-string level #\=) "["))
  (define closer (string-append "]" (make-string level #\=) "]"))
  (for ([ch (in-string opener)]) (write-one! in out))
  (let loop ()
    (cond [(eof-object? (peek-next in)) #f]
          [(peek-string=? in closer)
           (for ([ch (in-string closer)]) (write-one! in out))
           #t]
          [else (write-one! in out) (loop)])))

;; read-quoted-string! : input-port? output-port? char? -> boolean?
;;   Consume a quoted Lua string and report whether it closes cleanly.
(define (read-quoted-string! in out quote)
  (write-one! in out)
  (let loop ()
    (define next (peek-next in))
    (cond [(eof-object? next) #f]
          [(or (char=? next #\newline) (char=? next #\return)) #f]
          [else
           (write-one! in out)
           (cond [(char=? next quote) #t]
                 [(char=? next #\\)
                  (define escaped (peek-next in))
                  (cond [(eof-object? escaped) #f]
                        [else (write-one! in out) (loop)])]
                 [else (loop)])])))

;; read-line-comment! : input-port? output-port? -> void?
;;   Consume a short Lua comment through, but not including, its newline.
(define (read-line-comment! in out)
  (take-while! in out (lambda (ch) (not (or (char=? ch #\newline) (char=? ch #\return))))))

;; -----------------------------------------------------------------------------
;; Streaming reader

;; make-lua-derived-reader : -> (input-port? -> (or/c lua-derived-token? 'eof))
;;   Construct a stateful reader for Lua derived tokens.
(define (make-lua-derived-reader)
  (lambda (in)
    (define start (current-stream-position in))
    (define next (peek-next in))
    (cond
      [(eof-object? next) 'eof]
      [(char-whitespace? next)
       (define out (open-output-string))
       (take-while! in out char-whitespace?)
       (make-token in start (get-output-string out) '(whitespace lua-whitespace))]
      [(and (char=? next #\-) (peek-string=? in "--"))
       (define out (open-output-string))
       (write-one! in out) (write-one! in out)
       (define level (long-bracket-level in))
       (define closed? (and level (read-long-bracket! in out level)))
       (unless level (read-line-comment! in out))
       (make-token in start (get-output-string out)
                   (append '(comment lua-comment)
                           (if level '(lua-long-comment) '(lua-line-comment))
                           (if (or (not level) closed?) '() '(malformed-token lua-error))))]
      [(or (char=? next #\") (char=? next #\'))
       (define out (open-output-string))
       (define closed? (read-quoted-string! in out next))
       (make-token in start (get-output-string out)
                   (append '(literal lua-string-literal)
                           (if closed? '() '(malformed-token lua-error))))]
      [(long-bracket-level in)
       (define out (open-output-string))
       (define closed? (read-long-bracket! in out (long-bracket-level in)))
       (make-token in start (get-output-string out)
                   (append '(literal lua-long-string)
                           (if closed? '() '(malformed-token lua-error))))]
      [(identifier-start? next)
       (define out (open-output-string))
       (take-while! in out identifier-char?)
       (define text (get-output-string out))
       (make-token in start text
                   (cond [(member text lua-keywords)
                          (append '(keyword lua-keyword)
                                  (if (member text '(true false nil)) '(literal lua-constant) '()))]
                         [else '(identifier lua-identifier)]))]
      [(or (char-numeric? next)
           (and (char=? next #\.) (char? (peek-next in 1)) (char-numeric? (peek-next in 1))))
       (define out (open-output-string))
       (read-number! in out)
       (define text (get-output-string out))
       (make-token in start text
                   (if (lua-number? text) '(literal lua-number)
                       '(malformed-token lua-error)))]
      [else
       (define matched-operator (findf (lambda (text) (peek-string=? in text)) lua-operators))
       (define matched-delimiter (findf (lambda (text) (peek-string=? in text)) lua-delimiters))
       (define text (or matched-operator matched-delimiter (string next)))
       (for ([ch (in-string text)]) (read-char in))
       (make-token in start text
                   (cond [matched-operator '(operator lua-operator)]
                         [matched-delimiter '(delimiter lua-delimiter)]
                         [else '(malformed-token lua-error)]))])))
