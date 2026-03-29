#lang racket/base

;;;
;;; JavaScript Raw Tokens
;;;
;;
;; JavaScript raw tokenization for the current shared subset.

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
;; make-javascript-raw-reader : -> (input-port? -> (or/c javascript-raw-token? 'eof))
;;   Construct a stateful JavaScript raw-token reader.

(provide javascript-raw-token?
         javascript-raw-token-kind
         javascript-raw-token-text
         javascript-raw-token-start
         javascript-raw-token-end
         read-javascript-raw-token
         make-javascript-raw-reader)

(require parser-tools/lex
         "parser-tools-compat.rkt")

(struct javascript-raw-token (kind text start end) #:transparent)

;; One-character delimiter tokens.
(define js-delimiter-characters
  '(#\( #\) #\[ #\] #\{ #\} #\, #\; #\. #\:))

(define start-paren (string-ref "(" 0))

;; Longest-match-first JavaScript operator spellings for the current subset.
(define js-operators
  '(">>>=" "<<=" ">>=" "&&=" "||=" "??=" "**=" "===" "!=="
    ">>>" "<<" ">>" "==" "!=" "<=" ">=" "&&" "||" "??"
    "++" "--" "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "=>"
    "**" "?." "=" "<" ">" "!" "~" "+" "-" "*" "/" "%" "&"
    "|" "^" "?" "@"))

(define js-regex-context-keywords
  '("return" "throw" "case" "delete" "void" "typeof" "new" "in" "instanceof"
    "do" "else" "if" "while" "for" "switch" "catch" "await" "yield"))

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

;; string-prefix-at? : input-port? exact-nonnegative-integer? string? -> boolean?
;;   Determine whether the input has the given prefix at the current offset.
(define (string-prefix-at? in offset prefix)
  (define n (string-length prefix))
  (let loop ([i 0])
    (cond
      [(= i n) #t]
      [else
       (define next (peek-char in (+ offset i)))
       (and (char? next)
            (char=? next (string-ref prefix i))
            (loop (add1 i)))])))

;; peek-next-nonspace-char : input-port? -> (or/c char? #f)
;;   Peek the next non-whitespace character without consuming input.
(define (peek-next-nonspace-char in)
  (let loop ([offset 0])
    (define next (peek-char in offset))
    (cond
      [(eof-object? next) #f]
      [(and (char? next) (char-whitespace? next))
       (loop (add1 offset))]
      [(char? next) next]
      [else #f])))

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

;; read-exactly! : input-port? exact-nonnegative-integer? -> string?
;;   Consume exactly n characters from an input port.
(define (read-exactly! in n)
  (define out (open-output-string))
  (for ([i (in-range n)])
    (define next (read-char in))
    (when (char? next)
      (write-char next out)))
  (get-output-string out))

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

;; read-js-regex-literal! : input-port? -> (values string? boolean?)
;;   Consume a JavaScript regex literal and report whether it terminated cleanly.
(define (read-js-regex-literal! in)
  (define out (open-output-string))
  (write-char (read-char in) out)
  (let loop ([escaped? #f] [in-class? #f])
    (define next (read-char in))
    (cond
      [(eof-object? next) (values (get-output-string out) #f)]
      [else
       (write-char next out)
       (cond
         [(or (char=? next #\newline)
              (char=? next #\return))
          (values (get-output-string out) #f)]
         [escaped?
          (loop #f in-class?)]
         [(char=? next #\\)
          (loop #t in-class?)]
         [(char=? next #\[)
          (loop #f #t)]
         [(and in-class? (char=? next #\]))
          (loop #f #f)]
         [(and (not in-class?) (char=? next #\/))
          (display (read-while! in char-alphabetic?) out)
          (values (get-output-string out) #t)]
         [else
          (loop #f in-class?)])])))

;; read-template-chunk! : input-port? -> (values string? symbol?)
;;   Consume one template chunk and report how it ended.
(define (read-template-chunk! in)
  (define out (open-output-string))
  (let loop ([escaped? #f])
    (define next (peek-char in))
    (cond
      [(eof-object? next)
       (values (get-output-string out) 'eof)]
      [(and (not escaped?)
            (char? next)
            (char=? next #\`))
       (values (get-output-string out) 'template-end)]
      [(and (not escaped?)
            (char? next)
            (char=? next #\$)
            (let ([after (peek-char in 1)])
              (and (char? after)
                   (char=? after #\{))))
       (values (get-output-string out) 'interpolation-start)]
      [else
       (define ch (read-char in))
       (write-char ch out)
       (cond
         [escaped?          (loop #f)]
         [(char=? ch #\\)   (loop #t)]
         [else              (loop #f)])])))

;; read-private-name! : input-port? -> string?
;;   Consume a JavaScript private name beginning with #.
(define (read-private-name! in)
  (define out (open-output-string))
  (write-char (read-char in) out)
  (display (read-while! in js-ident-char?) out)
  (get-output-string out))

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

;; read-js-operator! : input-port? -> (or/c string? #f)
;;   Consume the longest matching JavaScript operator in the current subset.
(define (read-js-operator! in)
  (for/or ([op (in-list js-operators)])
    (and (string-prefix-at? in 0 op)
         (read-exactly! in (string-length op)))))

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
          (char=? next #\#)
          (let ([after (peek-char in 1)])
            (and (char? after)
                 (js-ident-start? after))))
     (raw-token in start-pos 'private-name-token (read-private-name! in))]
    [(and (char? next)
          (js-ident-start? next))
     (raw-token in start-pos 'identifier-token (identifier-token-text! in))]
    [(and (char? next)
          (member next js-delimiter-characters))
     (raw-token in start-pos 'delimiter-token (string (read-char in)))]
    [(char? next)
     (define maybe-operator
       (read-js-operator! in))
     (cond
       [maybe-operator
        (raw-token in start-pos 'operator-token maybe-operator)]
       [else
        (raw-token in start-pos 'unknown-raw-token (string (read-char in)))])]
    [else
     (raw-token in start-pos 'unknown-raw-token (string (read-char in)))]))

;; make-javascript-raw-reader : -> (input-port? -> (or/c javascript-raw-token? 'eof))
;;   Construct a stateful JavaScript raw-token reader.
(define (make-javascript-raw-reader)
  (define can-start-regex? #t)
  (define previous-significant-token #f)
  (define pending-raw-tokens '())
  (define template-mode 'normal)
  (define template-brace-depth 0)
  (define (update-regex-state! raw-token)
    (define kind (javascript-raw-token-kind raw-token))
    (define text (javascript-raw-token-text raw-token))
    (unless (memq kind '(whitespace-token line-comment-token block-comment-token))
      (set! can-start-regex?
            (case kind
              [(identifier-token)
               (member text js-regex-context-keywords)]
              [(private-name-token string-token number-token regex-token)
               #f]
              [(delimiter-token)
               (member text '("(" "[" "{" "," ";" ":" "?"))]
              [(operator-token)
               (not (member text '("++" "--")))]
              [else
               #f]))))
  (define (enqueue-raw-token! token)
    (set! pending-raw-tokens (append pending-raw-tokens (list token))))
  (define (dequeue-raw-token!)
    (define next-token (car pending-raw-tokens))
    (set! pending-raw-tokens (cdr pending-raw-tokens))
    next-token)
  (define (decorate-raw-token in raw-token)
    (define kind (javascript-raw-token-kind raw-token))
    (define text (javascript-raw-token-text raw-token))
    (cond
      [(and (memq kind '(identifier-token private-name-token))
            previous-significant-token
            (eq? (javascript-raw-token-kind previous-significant-token) 'delimiter-token)
            (string=? (javascript-raw-token-text previous-significant-token) ".")
            (let ([next (peek-next-nonspace-char in)])
              (and (char? next)
                   (char=? next start-paren))))
       (javascript-raw-token 'method-name-token
                             text
                             (javascript-raw-token-start raw-token)
                             (javascript-raw-token-end raw-token))]
      [else
       raw-token]))
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-javascript-raw-reader "input-port?" in))
    (port-count-lines! in)
    (define raw-result
      (cond
        [(pair? pending-raw-tokens)
         (dequeue-raw-token!)]
        [(eq? template-mode 'template)
         (define start-pos (current-stream-position in))
         (define-values (chunk-text chunk-end)
           (read-template-chunk! in))
         (cond
           [(positive? (string-length chunk-text))
            (case chunk-end
              [(template-end)
               (define boundary-start (current-stream-position in))
               (define boundary-text (string (read-char in)))
               (set! template-mode 'normal)
               (enqueue-raw-token!
                (raw-token in boundary-start 'template-end-token boundary-text))]
              [(interpolation-start)
               (define boundary-start (current-stream-position in))
               (define boundary-text (read-exactly! in 2))
               (set! template-mode 'template-expr)
               (set! template-brace-depth 0)
               (enqueue-raw-token!
                (raw-token in boundary-start 'template-interpolation-start-token boundary-text))]
              [(eof)
               (void)])
            (raw-token in start-pos
                       (if (eq? chunk-end 'eof)
                           'unknown-raw-token
                           'template-chunk-token)
                       chunk-text)]
           [(eq? chunk-end 'template-end)
            (define boundary-text (string (read-char in)))
            (set! template-mode 'normal)
            (raw-token in start-pos 'template-end-token boundary-text)]
           [(eq? chunk-end 'interpolation-start)
            (define boundary-text (read-exactly! in 2))
            (set! template-mode 'template-expr)
            (set! template-brace-depth 0)
            (raw-token in start-pos 'template-interpolation-start-token boundary-text)]
           [else
            'eof])]
        [else
         (define start-pos (current-stream-position in))
         (define next (peek-char in))
         (cond
           [(and (eq? template-mode 'normal)
                 (char? next)
                 (char=? next #\`))
            (define text (string (read-char in)))
            (set! template-mode 'template)
            (raw-token in start-pos 'template-start-token text)]
           [(and (eq? template-mode 'template-expr)
                 (char? next)
                 (char=? next #\})
                 (zero? template-brace-depth))
            (define text (string (read-char in)))
            (set! template-mode 'template)
            (raw-token in start-pos 'template-interpolation-end-token text)]
           [(and can-start-regex?
                 (char? next)
                 (char=? next #\/)
                 (let ([after (peek-char in 1)])
                   (and (char? after)
                        (not (member after '(#\/ #\*))))))
            (define-values (text terminated?) (read-js-regex-literal! in))
            (raw-token in start-pos (if terminated? 'regex-token 'unknown-raw-token) text)]
           [else
            (read-javascript-raw-token in)])]))
    (define decorated-result
      (if (eq? raw-result 'eof)
          'eof
          (decorate-raw-token in raw-result)))
    (when (and (eq? template-mode 'template-expr)
               (not (eq? decorated-result 'eof))
               (eq? (javascript-raw-token-kind decorated-result) 'delimiter-token))
      (define delimiter-text (javascript-raw-token-text decorated-result))
      (cond
        [(string=? delimiter-text "{")
         (set! template-brace-depth (add1 template-brace-depth))]
        [(and (string=? delimiter-text "}")
              (positive? template-brace-depth))
         (set! template-brace-depth (sub1 template-brace-depth))]))
    (unless (eq? decorated-result 'eof)
      (update-regex-state! decorated-result))
    (unless (or (eq? decorated-result 'eof)
                (memq (javascript-raw-token-kind decorated-result)
                      '(whitespace-token line-comment-token block-comment-token)))
      (set! previous-significant-token decorated-result))
    decorated-result))
