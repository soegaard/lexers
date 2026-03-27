#lang racket/base

;;;
;;; CSS Raw Tokens
;;;
;;
;; Minimal raw CSS tokenization, kept separate from reusable-stream projection.

;; css-raw-token?        : any/c -> boolean?
;;   Recognize a raw CSS token.
;; css-raw-token-kind    : css-raw-token? -> symbol?
;;   Extract the raw token kind.
;; css-raw-token-text    : css-raw-token? -> string?
;;   Extract the token text.
;; css-raw-token-start   : css-raw-token? -> position?
;;   Extract the starting source position.
;; css-raw-token-end     : css-raw-token? -> position?
;;   Extract the ending source position.
;; read-css-raw-token    : input-port? -> (or/c css-raw-token? 'eof)
;;   Read the next raw CSS token from an input port.

(provide css-raw-token?
         css-raw-token-kind
         css-raw-token-text
         css-raw-token-start
         css-raw-token-end
         read-css-raw-token)

(require parser-tools/lex
         "parser-tools-compat.rkt")

;; A raw CSS token with a spec-oriented kind, token text, and source positions.
(struct css-raw-token (kind text start end) #:transparent)

;; A small set of single-character punctuation tokens for the scaffold.
(define css-delimiter-characters
  '(#\{ #\} #\( #\) #\[ #\] #\: #\; #\,))

(define start-paren (string-ref "(" 0))

;; comment-start : char?
;;   The leading slash in a CSS block comment.
(define comment-start #\/)

;; hash-start : char?
;;   The leading hash in a CSS hash token.
(define hash-start #\#)

;; dot-char : char?
;;   The decimal point character for leading-dot numbers.
(define dot-char #\.)

;; css-ident-start? : char? -> boolean?
;;   Recognize a simplified CSS identifier start character.
(define (css-ident-start? ch)
  (or (char-alphabetic? ch)
      (char=? ch #\_)
      (char=? ch #\-)))

;; css-ident-char? : char? -> boolean?
;;   Recognize a simplified CSS identifier character.
(define (css-ident-char? ch)
  (or (css-ident-start? ch)
      (char-numeric? ch)))

;; valid-css-escape-start? : input-port? -> boolean?
;;   Determine whether the next input begins a simple CSS escape.
(define (valid-css-escape-start? in)
  (define next  (peek-char in 0))
  (define after (peek-char in 1))
  (and (char? next)
       (char? after)
       (char=? next #\\)
       (not (char=? after #\newline))))

;; css-name-start? : input-port? -> boolean?
;;   Determine whether the next input begins a simplified CSS name.
(define (css-name-start? in)
  (define next (peek-char in 0))
  (or (and (char? next)
           (css-ident-start? next))
      (valid-css-escape-start? in)))

;; css-name-char-start? : input-port? -> boolean?
;;   Determine whether the next input begins a simplified CSS name character.
(define (css-name-char-start? in)
  (define next (peek-char in 0))
  (or (and (char? next)
           (css-ident-char? next))
      (valid-css-escape-start? in)))

;; hash-body-start? : char? char? -> boolean?
;;   Determine whether the next two characters can begin the body of a hash token.
(define (hash-body-start? next after)
  (or (and (char? next)
           (or (css-ident-char? next)
               (char=? next hash-start)))
      (and (char? next)
           (char? after)
           (char=? next #\\)
           (not (char=? after #\newline)))))

;; delimiter-token-kind : char? -> (or/c symbol? #f)
;;   Map one-character punctuation to a more precise raw token kind.
(define (delimiter-token-kind ch)
  (case ch
    [(#\{) 'open-brace-token]
    [(#\}) 'close-brace-token]
    [(#\() 'open-paren-token]
    [(#\)) 'close-paren-token]
    [(#\[) 'open-bracket-token]
    [(#\]) 'close-bracket-token]
    [(#\:) 'colon-token]
    [(#\;) 'semicolon-token]
    [(#\,) 'comma-token]
    [else   #f]))

;; attribute-matcher-token-kind : input-port? -> (or/c symbol? #f)
;;   Recognize the common two-character CSS attribute matcher operators.
(define (attribute-matcher-token-kind in)
  (define c0 (peek-char in 0))
  (define c1 (peek-char in 1))
  (cond
    [(and (char? c0) (char? c1) (char=? c0 #\~) (char=? c1 #\=))
     'include-match-token]
    [(and (char? c0) (char? c1) (char=? c0 #\|) (char=? c1 #\=))
     'dash-match-token]
    [(and (char? c0) (char? c1) (char=? c0 #\^) (char=? c1 #\=))
     'prefix-match-token]
    [(and (char? c0) (char? c1) (char=? c0 #\$) (char=? c1 #\=))
     'suffix-match-token]
    [(and (char? c0) (char? c1) (char=? c0 #\*) (char=? c1 #\=))
     'substring-match-token]
    [else
     #f]))

;; hex-digit? : char? -> boolean?
;;   Recognize a hexadecimal digit.
(define (hex-digit? ch)
  (or (char-numeric? ch)
      (and (char-ci>=? ch #\a)
           (char-ci<=? ch #\f))))

;; starts-cdo? : input-port? -> boolean?
;;   Determine whether the next input begins a CDO token.
(define (starts-cdo? in)
  (define c0 (peek-char in 0))
  (define c1 (peek-char in 1))
  (define c2 (peek-char in 2))
  (define c3 (peek-char in 3))
  (and (char? c0)
       (char? c1)
       (char? c2)
       (char? c3)
       (char=? c0 #\<)
       (char=? c1 #\!)
       (char=? c2 #\-)
       (char=? c3 #\-)))

;; starts-cdc? : input-port? -> boolean?
;;   Determine whether the next input begins a CDC token.
(define (starts-cdc? in)
  (define c0 (peek-char in 0))
  (define c1 (peek-char in 1))
  (define c2 (peek-char in 2))
  (and (char? c0)
       (char? c1)
       (char? c2)
       (char=? c0 #\-)
       (char=? c1 #\-)
       (char=? c2 #\>)))

;; starts-number? : input-port? -> boolean?
;;   Determine whether the next input begins a numeric token in the scaffold.
(define (starts-number? in)
  (define next  (peek-char in 0))
  (define after (peek-char in 1))
  (define third (peek-char in 2))
  (cond
    [(and (char? next)
          (char-numeric? next))
     #t]
    [(and (char? next)
          (member next '(#\+ #\-))
          (or (and (char? after)
                   (char-numeric? after))
              (and (char? after)
                   (char? third)
                   (char=? after dot-char)
                   (char-numeric? third))))
     #t]
    [(and (char? next)
          (char? after)
          (char=? next dot-char)
          (char-numeric? after))
     #t]
    [else
     #f]))

;; starts-unicode-range? : input-port? -> boolean?
;;   Determine whether the next input begins a simplified unicode-range token.
(define (starts-unicode-range? in)
  (define c0 (peek-char in 0))
  (define c1 (peek-char in 1))
  (define c2 (peek-char in 2))
  (and (char? c0)
       (char? c1)
       (char? c2)
       (or (char=? c0 #\U)
           (char=? c0 #\u))
       (char=? c1 #\+)
       (or (hex-digit? c2)
           (char=? c2 #\?))))

;; current-stream-position : input-port? -> position?
;;   Read the current parser-tools-compatible source position from a port.
(define (current-stream-position in)
  (let-values ([(line col offset) (port-next-location in)])
    (define safe-line   (cond [(exact-positive-integer? line)   line]   [else 1]))
    (define safe-col    (cond [(exact-nonnegative-integer? col) col]    [else 0]))
    (define safe-offset (cond [(exact-positive-integer? offset) offset] [else 1]))
    (make-stream-position safe-offset safe-line safe-col)))

;; read-while! : input-port? (char? -> boolean?) -> string?
;;   Consume characters while the predicate holds.
(define (read-while! in pred?)
  (define out (open-output-string))
  (let loop ()
    (define next (peek-char in))
    (cond
      [(and (char? next) (pred? next))
       (write-char (read-char in) out)
       (loop)]
      [else
       (get-output-string out)])))

;; read-css-escape! : input-port? -> string?
;;   Consume a simplified CSS escape, preserving its original spelling.
(define (read-css-escape! in)
  (define out (open-output-string))
  (write-char (read-char in) out)
  (define next (peek-char in))
  (cond
    [(and (char? next)
          (hex-digit? next))
     (display (read-while! in hex-digit?) out)
     (define trailing (peek-char in))
     (when (and (char? trailing)
                (char-whitespace? trailing))
       (write-char (read-char in) out))]
    [(char? next)
     (write-char (read-char in) out)])
  (get-output-string out))

;; read-comment! : input-port? -> string?
;;   Consume a CSS block comment, including its delimiters.
(define (read-comment! in)
  (define out (open-output-string))
  (write-char (read-char in) out)
  (write-char (read-char in) out)
  (let loop ([previous #f])
    (define next (read-char in))
    (cond
      [(eof-object? next) (get-output-string out)]
      [else
       (write-char next out)
       (cond
         [(and (char? previous)
               (char=? previous #\*)
               (char=? next #\/))
          (get-output-string out)]
         [else
          (loop next)])])))

;; read-string-literal! : input-port? -> string?
;;   Consume a quoted string literal, including delimiters, and report whether
;;   it terminated cleanly.
(define (read-string-literal! in)
  (define out (open-output-string))
  (define quote-char (read-char in))
  (write-char quote-char out)
  (let loop ([escaped? #f])
    (define next (read-char in))
    (cond
      [(eof-object? next) (values (get-output-string out) #f)]
      [else
       (write-char next out)
       (cond
         [escaped?                 (loop #f)]
         [(char=? next #\\)        (loop #t)]
         [(char=? next quote-char) (values (get-output-string out) #t)]
         [(char=? next #\newline)  (values (get-output-string out) #f)]
         [else                     (loop #f)])])))

;; read-url-tail! : input-port? -> (values string? boolean? boolean?)
;;   Consume the remainder of a simplified bare url(...) form, reporting
;;   whether a closing parenthesis was found and whether malformed content was
;;   seen along the way.
(define (read-url-tail! in)
  (define out (open-output-string))
  (let loop ([escaped? #f]
             [malformed? #f])
    (define next (read-char in))
    (cond
      [(eof-object? next)
       (values (get-output-string out) #f malformed?)]
      [else
       (write-char next out)
       (cond
         [escaped?                 (loop #f malformed?)]
         [(char=? next #\\)        (loop #t malformed?)]
         [(char=? next #\))        (values (get-output-string out) #t malformed?)]
         [(or (char=? next #\")
              (char=? next #\')
              (char=? next #\newline)
              (char-whitespace? next))
          (loop #f #t)]
         [else
          (loop #f malformed?)])])))

;; read-number-literal! : input-port? -> string?
;;   Consume a small numeric literal subset for the scaffold.
(define (read-number-literal! in)
  (define out (open-output-string))
  (define first (peek-char in))
  (when (and (char? first)
             (member first '(#\+ #\-)))
    (write-char (read-char in) out))
  (display (read-while! in char-numeric?) out)
  (define dot (peek-char in))
  (when (and (char? dot)
             (char=? dot dot-char))
    (write-char (read-char in) out)
    (display (read-while! in char-numeric?) out))
  (define suffix
    (read-while! in
                 (lambda (ch)
                   (or (char-alphabetic? ch)
                       (char=? ch #\%)
                       (char=? ch #\-)))))
  (display suffix out)
  (get-output-string out))

;; read-unicode-range! : input-port? -> string?
;;   Consume a simplified CSS unicode-range token.
(define (read-unicode-range! in)
  (define out (open-output-string))
  (write-char (read-char in) out)
  (write-char (read-char in) out)
  (display
   (read-while! in
                (lambda (ch)
                  (or (hex-digit? ch)
                      (char=? ch #\?))))
   out)
  (define dash (peek-char in))
  (when (and (char? dash)
             (char=? dash #\-))
    (write-char (read-char in) out)
    (display (read-while! in hex-digit?) out))
  (get-output-string out))

;; identifier-token-text! : input-port? -> string?
;;   Consume a simplified CSS identifier, including simple escapes.
(define (identifier-token-text! in)
  (define out (open-output-string))
  (let loop ()
    (cond
      [(css-name-char-start? in)
       (define next (peek-char in))
       (cond
         [(and (char? next)
               (css-ident-char? next))
          (write-char (read-char in) out)]
         [else
          (display (read-css-escape! in) out)])
       (loop)]
      [else
       (get-output-string out)])))

;; raw-token : input-port? position? symbol? string? -> css-raw-token?
;;   Construct a raw CSS token with positions.
(define (raw-token in start-pos kind text)
  (css-raw-token kind text start-pos (current-stream-position in)))

;; read-css-raw-token : input-port? -> (or/c css-raw-token? 'eof)
;;   Read the next raw CSS token from an input port.
(define (read-css-raw-token in)
  (unless (input-port? in)
    (raise-argument-error 'read-css-raw-token "input-port?" 0 in))
  (port-count-lines! in)
  (define start-pos (current-stream-position in))
  (define next (peek-char in))
  (cond
    [(eof-object? next) 'eof]
    [(and (char? next)
          (char-whitespace? next))
     (raw-token in start-pos 'whitespace-token (read-while! in char-whitespace?))]
    [(and (char? next)
          (char=? next comment-start)
          (let ([after (peek-char in 1)])
            (and (char? after)
                 (char=? after #\*))))
     (raw-token in start-pos 'comment-token (read-comment! in))]
    [(and (char? next)
          (starts-cdo? in))
     (read-char in)
     (read-char in)
     (read-char in)
     (read-char in)
     (raw-token in start-pos 'CDO-token "<!--")]
    [(and (char? next)
          (starts-cdc? in))
     (read-char in)
     (read-char in)
     (read-char in)
     (raw-token in start-pos 'CDC-token "-->")]
    [(and (char? next)
          (or (char=? next #\")
              (char=? next #\')))
     (define-values (text terminated?) (read-string-literal! in))
     (raw-token in
                start-pos
                (if terminated?
                    'string-token
                    'bad-string-token)
                text)]
    [(and (char? next)
          (char=? next #\@))
     (read-char in)
     (raw-token in
                start-pos
                'at-keyword-token
                (string-append "@"
                               (identifier-token-text! in)))]
    [(and (char? next)
          (char=? next hash-start)
          (let ([after (peek-char in 1)]
                [third (peek-char in 2)])
            (hash-body-start? after third)))
     (read-char in)
     (raw-token in
                start-pos
                'hash-token
                (string-append "#"
                               (identifier-token-text! in)))]
    [(and (char? next)
          (starts-unicode-range? in))
     (raw-token in
                start-pos
                'unicode-range-token
                (read-unicode-range! in))]
    [(and (char? next)
          (starts-number? in))
     (define text (read-number-literal! in))
     (define kind
       (cond
         [(regexp-match? #px"%$" text)          'percentage-token]
         [(regexp-match? #px"[a-zA-Z-]+$" text) 'dimension-token]
         [else                                  'number-token]))
     (raw-token in start-pos kind text)]
    [(css-name-start? in)
     (define text (identifier-token-text! in))
     (define after-ident (peek-char in))
     (define maybe-function?
       (and (char? after-ident)
            (char=? after-ident start-paren)))
       (cond
         [(and maybe-function?
               (string-ci=? text "url"))
        (define after-open-paren (peek-char in 1))
        (cond
          [(and (char? after-open-paren)
                (or (char=? after-open-paren #\")
                    (char=? after-open-paren #\')))
           (raw-token in
                      start-pos
                      'function-token
                      text)]
          [else
           (read-char in)
           (define-values (tail terminated? malformed?) (read-url-tail! in))
           (raw-token in
                      start-pos
                      (if (and terminated?
                               (not malformed?))
                          'url-token
                          'bad-url-token)
                      (string-append text "(" tail))])]
       [else
        (raw-token in
                   start-pos
                   (if maybe-function?
                       'function-token
                       'ident-token)
                   text)])]
    [(and (char? next)
          (delimiter-token-kind next))
     (define ch (read-char in))
     (raw-token in
                start-pos
                (delimiter-token-kind ch)
                (string ch))]
    [(attribute-matcher-token-kind in)
     (define kind (attribute-matcher-token-kind in))
     (define first (read-char in))
     (define second (read-char in))
     (raw-token in
                start-pos
                kind
                (string first second))]
    [else
     (raw-token in start-pos 'delim-token (string (read-char in)))]))
