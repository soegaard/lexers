#lang racket/base

;;;
;;; TOML Derived Tokens
;;;

;; Streaming TOML tokenization and reusable TOML-specific classifications.

;; toml-derived-token?         : any/c -> boolean?
;;   Recognize a derived TOML token.
;; toml-derived-token-text     : toml-derived-token? -> string?
;;   Extract the source text for one derived token.
;; toml-derived-token-start    : toml-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; toml-derived-token-end      : toml-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; toml-derived-token-tags     : toml-derived-token? -> (listof symbol?)
;;   Extract reusable TOML classification tags.
;; toml-derived-token-has-tag? : toml-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-toml-derived-reader    : -> (input-port? -> (or/c toml-derived-token? 'eof))
;;   Construct a stateful TOML derived-token reader.

(provide toml-derived-token?
         toml-derived-token-text
         toml-derived-token-start
         toml-derived-token-end
         toml-derived-token-tags
         toml-derived-token-has-tag?
         make-toml-derived-reader)

(require parser-tools/lex
         racket/list
         racket/string
         "parser-tools-compat.rkt")

;; A TOML token plus reusable tags.
(struct toml-derived-token (kind text start end tags) #:transparent)

;; toml-derived-token-has-tag? : toml-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (toml-derived-token-has-tag? token tag)
  (member tag (toml-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Port and position helpers

;; peek-next : input-port? [exact-nonnegative-integer?] -> (or/c char? eof-object?)
;;   Peek ahead in an input port without consuming characters.
(define (peek-next in [skip 0])
  (peek-char in skip))

;; peek-char=? : input-port? exact-nonnegative-integer? char? -> boolean?
;;   Determine whether a lookahead position contains a particular character.
(define (peek-char=? in skip expected)
  (define actual (peek-next in skip))
  (and (char? actual)
       (char=? actual expected)))

;; write-one! : input-port? output-port? -> void?
;;   Consume one character and append it to the output accumulator.
(define (write-one! in out)
  (write-char (read-char in) out))

;; current-stream-position : input-port? -> position?
;;   Read a parser-tools-compatible source position from a port.
(define (current-stream-position in)
  (let-values ([(line col offset) (port-next-location in)])
    (make-stream-position (if (exact-positive-integer? offset) offset 1)
                          (if (exact-positive-integer? line) line 1)
                          (if (exact-nonnegative-integer? col) col 0))))

;; make-token : input-port? position? string? (listof symbol?) -> toml-derived-token?
;;   Construct a derived token whose end position is the port's current position.
(define (make-token in start text tags)
  (define kind
    (cond
      [(member 'comment tags)         'comment]
      [(member 'whitespace tags)      'whitespace]
      [(member 'malformed-token tags) 'malformed]
      [(member 'literal tags)         'literal]
      [(member 'operator tags)        'operator]
      [(member 'delimiter tags)       'delimiter]
      [else                           'identifier]))
  (toml-derived-token kind
                      text
                      start
                      (current-stream-position in)
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
;; TOML lexical classification

;; bare-key-char? : char? -> boolean?
;;   Recognize the ASCII characters permitted in TOML bare keys.
(define (bare-key-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (member ch '(#\_ #\-))))

;; value-char? : char? -> boolean?
;;   Recognize a non-delimiting character in an unquoted TOML value candidate.
(define (value-char? ch)
  (not (or (char-whitespace? ch)
           (member ch '(#\# #\, #\[ #\] #\{ #\} #\= #\" #\')))))

;; toml-number? : string? -> boolean?
;;   Recognize the TOML integer, float, and special-float lexical forms.
(define (toml-number? text)
  (or (regexp-match? #px"^[+-]?(?:0|[1-9](?:_?[0-9])*)$" text)
      (regexp-match? #px"^0[xX][0-9A-Fa-f](?:_?[0-9A-Fa-f])*$" text)
      (regexp-match? #px"^0[oO][0-7](?:_?[0-7])*$" text)
      (regexp-match? #px"^0[bB][01](?:_?[01])*$" text)
      (regexp-match? #px"^[+-]?(?:(?:0|[1-9](?:_?[0-9])*)\\.(?:[0-9](?:_?[0-9])*)|(?:0|[1-9](?:_?[0-9])*)[eE][+-]?[0-9](?:_?[0-9])*|(?:0|[1-9](?:_?[0-9])*)\\.(?:[0-9](?:_?[0-9])*)[eE][+-]?[0-9](?:_?[0-9])*)$" text)
      (member text '("inf" "+inf" "-inf" "nan" "+nan" "-nan"))))

;; toml-date-time? : string? -> boolean?
;;   Recognize RFC 3339-like TOML date, time, and date-time lexical forms.
(define (toml-date-time? text)
  (or (regexp-match? #px"^[0-9]{4}-[0-9]{2}-[0-9]{2}$" text)
      (regexp-match? #px"^[0-9]{2}:[0-9]{2}:[0-9]{2}(?:\\.[0-9]+)?$" text)
      (regexp-match? #px"^[0-9]{4}-[0-9]{2}-[0-9]{2}[Tt ][0-9]{2}:[0-9]{2}:[0-9]{2}(?:\\.[0-9]+)?(?:[Zz]|[+-][0-9]{2}:[0-9]{2})?$" text)))

;; valid-basic-escape? : input-port? output-port? -> boolean?
;;   Check the escape following a consumed TOML basic-string backslash.
(define (valid-basic-escape? in out)
  (define escaped (peek-next in))
  (cond
    [(eof-object? escaped) #f]
    [(member escaped '(#\b #\t #\n #\f #\r #\" #\\))
     (write-one! in out)
     #t]
    [(member escaped '(#\u #\U))
     (define digits (if (char=? escaped #\u) 4 8))
     (write-one! in out)
     (define valid?
       (for/and ([i (in-range digits)])
         (define ch (peek-next in))
         (and (char? ch)
              (or (char-numeric? ch)
                  (member (char-downcase ch) '(#\a #\b #\c #\d #\e #\f))))))
     (for ([i (in-range digits)] #:when (char? (peek-next in)))
       (write-one! in out))
     valid?]
    [else
     (write-one! in out)
     #f]))

;; read-string! : input-port? output-port? char? boolean? -> boolean?
;;   Consume a TOML basic or literal string and report lexical validity.
(define (read-string! in out quote multiline?)
  (define delimiter-length (if multiline? 3 1))
  (for ([i (in-range delimiter-length)]) (write-one! in out))
  (let loop ()
    (define next (peek-next in))
    (cond
      [(eof-object? next) #f]
      [(and multiline?
            (char=? next quote)
            (peek-char=? in 1 quote)
            (peek-char=? in 2 quote))
       (for ([i (in-range 3)]) (write-one! in out))
       #t]
      [(and (not multiline?) (char=? next quote))
       (write-one! in out)
       #t]
      [(and (not multiline?) (or (char=? next #\newline) (char=? next #\return)))
       #f]
      [else
       (write-one! in out)
       (cond
         [(and (char=? quote #\") (char=? next #\\))
          (and (valid-basic-escape? in out) (loop))]
         [else (loop)])])))

;; -----------------------------------------------------------------------------
;; Streaming reader

;; make-toml-derived-reader : -> (input-port? -> (or/c toml-derived-token? 'eof))
;;   Construct a stateful TOML reader that preserves every source character.
(define (make-toml-derived-reader)
  (define expecting-key? #t)
  (define header-depth 0)
  (lambda (in)
    (define start (current-stream-position in))
    (define next (peek-next in))
    (cond
      [(eof-object? next) 'eof]
      [(char-whitespace? next)
       (define out (open-output-string))
       (take-while! in out char-whitespace?)
       (define text (get-output-string out))
       (when (or (string-contains? text "\n")
                 (string-contains? text "\r"))
         (set! expecting-key? #t))
       (make-token in start text '(whitespace toml-whitespace))]
      [(char=? next #\#)
       (define out (open-output-string))
       (take-while! in out (lambda (ch) (not (or (char=? ch #\newline) (char=? ch #\return)))))
       (make-token in start (get-output-string out) '(comment toml-comment))]
      [(or (char=? next #\") (char=? next #\'))
       (define quote next)
       (define multiline?
         (and (peek-char=? in 1 quote)
              (peek-char=? in 2 quote)))
       (define out (open-output-string))
       (define valid? (read-string! in out quote multiline?))
       (make-token in start (get-output-string out)
                   (append (list (string->symbol "literal")
                                 (string->symbol "toml-string"))
                           (if multiline?
                               (list (string->symbol "toml-multiline-string"))
                               null)
                           (if valid?
                               null
                               (list (string->symbol "malformed-token")
                                     (string->symbol "toml-error")))))]
      [(char=? next #\=)
       (read-char in)
       (set! expecting-key? #f)
       (make-token in start "=" '(operator toml-key-value-separator))]
      [(char=? next #\,)
       (read-char in)
       (set! expecting-key? #t)
       (make-token in start "," '(delimiter toml-comma))]
      [(char=? next #\.)
       (read-char in)
       (make-token in start "." '(delimiter toml-dot))]
      [(member next '(#\[ #\] #\{ #\}))
       (read-char in)
       (cond
         [(char=? next #\[) (set! header-depth (add1 header-depth))]
         [(char=? next #\]) (set! header-depth (max 0 (sub1 header-depth)))]
         [else (void)])
       (make-token in start (string next)
                   (append '(delimiter toml-structural-delimiter)
                           (cond
                             [(and (char=? next #\[) (peek-char=? in 0 #\[))
                              '(toml-array-table-delimiter)]
                             [(positive? header-depth) '(toml-table-delimiter)]
                             [else '()])))]
      [else
       (define out (open-output-string))
       (take-while! in out value-char?)
       (define text (get-output-string out))
       (cond
         [(zero? (string-length text))
          (write-one! in out)
          (make-token in start (get-output-string out) '(malformed-token toml-error))]
         [(and expecting-key? (or (positive? header-depth) (regexp-match? #px"^[A-Za-z0-9_-]+$" text)))
          (make-token in start text
                      (append '(identifier toml-key)
                              (if (positive? header-depth) '(toml-table-key) '())))]
         [(member text '("true" "false"))
          (make-token in start text '(literal toml-boolean))]
         [(toml-number? text)
          (make-token in start text '(literal toml-number))]
         [(toml-date-time? text)
          (make-token in start text '(literal toml-date-time))]
         [else
          (make-token in start text '(identifier toml-bare-value))])])))
