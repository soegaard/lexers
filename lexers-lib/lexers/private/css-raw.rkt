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

;; hex-digit? : char? -> boolean?
;;   Recognize a hexadecimal digit.
(define (hex-digit? ch)
  (or (char-numeric? ch)
      (and (char-ci>=? ch #\a)
           (char-ci<=? ch #\f))))

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
         [else                     (loop #f)])])))

;; read-url-tail! : input-port? -> (values string? boolean?)
;;   Consume the remainder of a simple url(...) form, reporting whether a
;;   closing parenthesis was found.
(define (read-url-tail! in)
  (define out (open-output-string))
  (let loop ()
    (define next (read-char in))
    (cond
      [(eof-object? next)
       (values (get-output-string out) #f)]
      [else
       (write-char next out)
       (cond
         [(char=? next #\)) (values (get-output-string out) #t)]
         [else              (loop)])])))

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
             (char=? dot #\.))
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

;; identifier-token-text! : input-port? -> string?
;;   Consume a simplified CSS identifier.
(define (identifier-token-text! in)
  (read-while! in css-ident-char?))

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
          (char=? next #\/)
          (let ([after (peek-char in 1)])
            (and (char? after)
                 (char=? after #\*))))
     (raw-token in start-pos 'comment-token (read-comment! in))]
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
          (char=? next #\#)
          (let ([after (peek-char in 1)])
            (and (char? after)
                 (hex-digit? after))))
     (read-char in)
     (raw-token in
                start-pos
                'hash-token
                (string-append "#"
                               (read-while! in hex-digit?)))]
    [(and (char? next)
          (or (char-numeric? next)
              (and (member next '(#\+ #\-))
                   (let ([after (peek-char in 1)])
                     (and (char? after)
                          (or (char-numeric? after)
                              (char=? after #\.)))))))
     (define text (read-number-literal! in))
     (define kind
       (cond
         [(regexp-match? #px"%$" text)          'percentage-token]
         [(regexp-match? #px"[a-zA-Z-]+$" text) 'dimension-token]
         [else                                  'number-token]))
     (raw-token in start-pos kind text)]
    [(and (char? next)
          (css-ident-start? next))
     (define text (identifier-token-text! in))
     (define after-ident (peek-char in))
     (define maybe-function?
       (and (char? after-ident)
            (char=? after-ident start-paren)))
     (cond
       [(and maybe-function?
             (string-ci=? text "url"))
        (read-char in)
        (define-values (tail terminated?) (read-url-tail! in))
        (raw-token in
                   start-pos
                   (if terminated?
                       'url-token
                       'bad-url-token)
                   (string-append text "(" tail))]
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
    [else
     (raw-token in start-pos 'delim-token (string (read-char in)))]))
