#lang racket/base

;;;
;;; JavaScript Raw Tokens
;;;
;;
;; Minimal JavaScript raw tokenization for the initial subset.

;; javascript-raw-token?      : any/c -> boolean?
;;   Recognize a raw JavaScript token.
;; javascript-raw-token-kind  : javascript-raw-token? -> symbol?
;;   Extract the raw token kind.
;; javascript-raw-token-text  : javascript-raw-token? -> string?
;;   Extract the token text.
;; javascript-raw-token-start : javascript-raw-token? -> position?
;;   Extract the starting source position.
;; javascript-raw-token-end   : javascript-raw-token? -> position?
;;   Extract the ending source position.
;; read-javascript-raw-token  : input-port? -> (or/c javascript-raw-token? 'eof)
;;   Read the next raw JavaScript token from an input port.

(provide javascript-raw-token?
         javascript-raw-token-kind
         javascript-raw-token-text
         javascript-raw-token-start
         javascript-raw-token-end
         read-javascript-raw-token)

(require parser-tools/lex
         "parser-tools-compat.rkt")

(struct javascript-raw-token (kind text start end) #:transparent)

(define js-delimiter-characters
  '(#\( #\) #\[ #\] #\{ #\} #\, #\; #\.))

(define js-operator-characters
  '(#\= #\+ #\- #\* #\/))

;; js-ident-start? : char? -> boolean?
;;   Recognize a simplified JavaScript identifier start character.
(define (js-ident-start? ch)
  (or (char-alphabetic? ch)
      (char=? ch #\_)
      (char=? ch #\$)))

;; js-ident-char? : char? -> boolean?
;;   Recognize a simplified JavaScript identifier character.
(define (js-ident-char? ch)
  (or (js-ident-start? ch)
      (char-numeric? ch)))

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

;; raw-token : input-port? position? symbol? string? -> javascript-raw-token?
;;   Construct a raw JavaScript token with positions.
(define (raw-token in start-pos kind text)
  (javascript-raw-token kind text start-pos (current-stream-position in)))

;; read-line-comment! : input-port? -> string?
;;   Consume a JavaScript line comment.
(define (read-line-comment! in)
  (define out (open-output-string))
  (write-char (read-char in) out)
  (write-char (read-char in) out)
  (let loop ()
    (define next (peek-char in))
    (cond
      [(or (eof-object? next)
           (and (char? next) (char=? next #\newline)))
       (get-output-string out)]
      [else
       (write-char (read-char in) out)
       (loop)])))

;; read-block-comment! : input-port? -> string?
;;   Consume a JavaScript block comment, including delimiters.
(define (read-block-comment! in)
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

;; read-string-literal! : input-port? -> (values string? boolean?)
;;   Consume a quoted string literal and report whether it terminated cleanly.
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

;; read-number-literal! : input-port? -> string?
;;   Consume a small JavaScript numeric literal subset.
(define (read-number-literal! in)
  (define out (open-output-string))
  (display (read-while! in char-numeric?) out)
  (define dot (peek-char in))
  (when (and (char? dot)
             (char=? dot #\.))
    (write-char (read-char in) out)
    (display (read-while! in char-numeric?) out))
  (get-output-string out))

;; identifier-token-text! : input-port? -> string?
;;   Consume a simplified JavaScript identifier.
(define (identifier-token-text! in)
  (read-while! in js-ident-char?))

;; read-javascript-raw-token : input-port? -> (or/c javascript-raw-token? 'eof)
;;   Read the next raw JavaScript token from an input port.
(define (read-javascript-raw-token in)
  (unless (input-port? in)
    (raise-argument-error 'read-javascript-raw-token "input-port?" 0 in))
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
                 (char=? after #\/))))
     (raw-token in start-pos 'line-comment-token (read-line-comment! in))]
    [(and (char? next)
          (char=? next #\/)
          (let ([after (peek-char in 1)])
            (and (char? after)
                 (char=? after #\*))))
     (raw-token in start-pos 'block-comment-token (read-block-comment! in))]
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
          (char-numeric? next))
     (raw-token in start-pos 'number-token (read-number-literal! in))]
    [(and (char? next)
          (js-ident-start? next))
     (raw-token in start-pos 'identifier-token (identifier-token-text! in))]
    [(and (char? next)
          (member next js-delimiter-characters))
     (raw-token in start-pos 'delimiter-token (string (read-char in)))]
    [(and (char? next)
          (member next js-operator-characters))
     (raw-token in start-pos 'operator-token (string (read-char in)))]
    [else
     (raw-token in start-pos 'unknown-raw-token (string (read-char in)))]))

