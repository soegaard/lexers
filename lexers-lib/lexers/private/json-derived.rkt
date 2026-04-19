#lang racket/base

;;;
;;; JSON Derived Tokens
;;;
;;
;; Stateful JSON tokenization and reusable JSON-specific classifications.

;; json-derived-token?         : any/c -> boolean?
;;   Recognize a derived JSON token.
;; json-derived-token-text     : json-derived-token? -> string?
;;   Extract the source text for one derived JSON token.
;; json-derived-token-start    : json-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; json-derived-token-end      : json-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; json-derived-token-tags     : json-derived-token? -> (listof symbol?)
;;   Extract reusable JSON classification tags.
;; json-derived-token-has-tag? : json-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-json-derived-reader    : -> (input-port? -> (or/c json-derived-token? 'eof))
;;   Construct a stateful JSON derived-token reader.

(provide json-derived-token?
         json-derived-token-text
         json-derived-token-start
         json-derived-token-end
         json-derived-token-tags
         json-derived-token-has-tag?
         make-json-derived-reader)

(require parser-tools/lex
         racket/list
         racket/string
         "parser-tools-compat.rkt")

;; A JSON token plus reusable tags.
(struct json-derived-token (kind text start end tags) #:transparent)

;; JSON parser-lite context states.
(define context-root-value          'root-value)
(define context-root-done           'root-done)
(define context-object-key-or-end   'object-key-or-end)
(define context-object-colon        'object-colon)
(define context-object-value        'object-value)
(define context-object-comma-or-end 'object-comma-or-end)
(define context-array-value-or-end  'array-value-or-end)
(define context-array-comma-or-end  'array-comma-or-end)

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

;; make-token-from-text : position? position? string? (listof symbol?) -> json-derived-token?
;;   Construct one derived token from explicit positions, text, and tags.
(define (make-token-from-text start-pos end-pos text tags)
  (define kind
    (cond
      [(member 'whitespace tags)      'whitespace]
      [(member 'malformed-token tags) 'malformed]
      [(member 'delimiter tags)       'delimiter]
      [(member 'operator tags)        'operator]
      [(member 'identifier tags)      'identifier]
      [(member 'literal tags)         'literal]
      [else                           'unknown]))
  (json-derived-token kind
                      text
                      start-pos
                      end-pos
                      (remove-duplicates tags)))

;; whitespace-char? : char? -> boolean?
;;   Recognize JSON whitespace.
(define (whitespace-char? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\newline)
      (char=? ch #\return)))

;; json-number-char? : char? -> boolean?
;;   Recognize a character that can appear in one JSON number token candidate.
(define (json-number-char? ch)
  (or (char-numeric? ch)
      (char=? ch #\-)
      (char=? ch #\+)
      (char=? ch #\.)
      (char=? ch #\e)
      (char=? ch #\E)))

;; json-number-valid? : string? -> boolean?
;;   Check whether a consumed number candidate matches the JSON grammar.
(define (json-number-valid? text)
  (regexp-match? #px"^-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?$"
                 text))

;; read-json-string! : input-port? output-port? -> boolean?
;;   Consume one JSON string literal and report whether it terminated cleanly.
(define (read-json-string! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [else
       (define ch
         next)
       (write-one! in out)
       (cond
         [(char=? ch #\")
          #t]
         [(or (char=? ch #\newline)
              (char=? ch #\return))
          #f]
         [(char=? ch #\\)
          (define escaped
            (peek-next in))
          (cond
            [(eof-object? escaped)
             #f]
            [else
             (write-one! in out)
             (define esc
               escaped)
             (cond
               [(member esc '(#\" #\\ #\/ #\b #\f #\n #\r #\t))
                (loop)]
               [(char=? esc #\u)
                (define hex-ok?
                  (for/and ([i (in-range 4)])
                    (define h
                      (peek-next in i))
                    (and (char? h)
                         (or (char-numeric? h)
                             (member (char-downcase h)
                                     '(#\a #\b #\c #\d #\e #\f))))))
                (cond
                  [hex-ok?
                   (for ([i (in-range 4)])
                     (write-one! in out))
                   (loop)]
                  [else
                   #f])]
               [else
                #f])])]
         [(char<? ch #\space)
          #f]
         [else
          (loop)])])))

;; read-json-number! : input-port? output-port? -> string?
;;   Consume one JSON number token candidate.
(define (read-json-number! in out)
  (read-while! in out json-number-char?)
  (get-output-string out))

;; read-json-word! : input-port? output-port? -> string?
;;   Consume one lowercase JSON keyword-like word.
(define (read-json-word! in out)
  (read-while! in out char-alphabetic?)
  (get-output-string out))

;; top-state : (listof symbol?) -> symbol?
;;   Read the current parser-lite JSON context state.
(define (top-state stack)
  (cond
    [(pair? stack)
     (car stack)]
    [else
     context-root-done]))

;; replace-top : (listof symbol?) symbol? -> (listof symbol?)
;;   Replace the current parser-lite JSON context state.
(define (replace-top stack new-top)
  (cond
    [(pair? stack)
     (cons new-top (cdr stack))]
    [else
     (list new-top)]))

;; value-finished : (listof symbol?) -> (listof symbol?)
;;   Update the parser-lite context after reading one complete JSON value.
(define (value-finished stack)
  (case (top-state stack)
    [(root-value)          (replace-top stack context-root-done)]
    [(object-value)        (replace-top stack context-object-comma-or-end)]
    [(array-value-or-end)  (replace-top stack context-array-comma-or-end)]
    [else                  stack]))

;; make-delimiter-token! : input-port? position? output-port? (listof symbol?) -> json-derived-token?
;;   Consume one delimiter character and build the matching token.
(define (make-delimiter-token! in start-pos out tags)
  (write-one! in out)
  (make-token-from-text start-pos
                        (current-stream-position in)
                        (get-output-string out)
                        tags))

;; make-json-derived-reader : -> (input-port? -> (or/c json-derived-token? 'eof))
;;   Construct a stateful JSON derived-token reader.
(define (make-json-derived-reader)
  (define stack
    (list context-root-value))
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-json-derived-reader "input-port?" in))
    (port-count-lines! in)
    (define start-pos
      (current-stream-position in))
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       'eof]
      [(whitespace-char? next)
       (define out
         (open-output-string))
       (read-while! in out whitespace-char?)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(whitespace json-whitespace))]
      [else
       (define ch
         next)
       (define out
         (open-output-string))
       (cond
         [(char=? ch #\{)
          (set! stack
                (cons context-object-key-or-end
                      (value-finished stack)))
          (make-delimiter-token! in start-pos out
                                 '(delimiter json-object-start))]
         [(char=? ch #\})
          (set! stack
                (value-finished
                 (cond
                   [(eq? (top-state stack) context-object-key-or-end)
                    (cdr stack)]
                   [(eq? (top-state stack) context-object-comma-or-end)
                    (cdr stack)]
                   [else
                    stack])))
          (make-delimiter-token! in start-pos out
                                 '(delimiter json-object-end))]
         [(char=? ch #\[)
          (set! stack
                (cons context-array-value-or-end
                      (value-finished stack)))
          (make-delimiter-token! in start-pos out
                                 '(delimiter json-array-start))]
         [(char=? ch #\])
          (set! stack
                (value-finished
                 (cond
                   [(eq? (top-state stack) context-array-value-or-end)
                    (cdr stack)]
                   [(eq? (top-state stack) context-array-comma-or-end)
                    (cdr stack)]
                   [else
                    stack])))
          (make-delimiter-token! in start-pos out
                                 '(delimiter json-array-end))]
         [(char=? ch #\,)
          (set! stack
                (case (top-state stack)
                  [(object-comma-or-end) (replace-top stack context-object-key-or-end)]
                  [(array-comma-or-end)  (replace-top stack context-array-value-or-end)]
                  [else                  stack]))
          (make-delimiter-token! in start-pos out
                                 '(delimiter json-comma))]
         [(char=? ch #\:)
          (set! stack
                (case (top-state stack)
                  [(object-colon) (replace-top stack context-object-value)]
                  [else           stack]))
          (make-delimiter-token! in start-pos out
                                 '(operator json-colon))]
         [(char=? ch #\")
          (define terminated?
            (read-json-string! in out))
          (define text
            (get-output-string out))
          (define state
            (top-state stack))
          (cond
            [terminated?
             (define tags
               (cond
                 [(eq? state context-object-key-or-end)
                  '(identifier json-object-key json-string)]
                 [else
                  '(literal json-string)]))
             (when (eq? state context-object-key-or-end)
               (set! stack
                     (replace-top stack context-object-colon)))
             (when (not (eq? state context-object-key-or-end))
               (set! stack
                     (value-finished stack)))
             (make-token-from-text start-pos
                                   (current-stream-position in)
                                   text
                                   tags)]
            [else
             (make-token-from-text start-pos
                                   (current-stream-position in)
                                   text
                                   '(malformed-token json-string json-error))])]
         [(or (char=? ch #\-)
              (char-numeric? ch))
          (define text
            (read-json-number! in out))
          (cond
            [(json-number-valid? text)
             (set! stack
                   (value-finished stack))
             (make-token-from-text start-pos
                                   (current-stream-position in)
                                   text
                                   '(literal json-number))]
            [else
             (make-token-from-text start-pos
                                   (current-stream-position in)
                                   text
                                   '(malformed-token json-number json-error))])]
         [(char-alphabetic? ch)
          (define text
            (read-json-word! in out))
          (define tags
            (case (string->symbol text)
              [(true)  '(literal json-true json-keyword)]
              [(false) '(literal json-false json-keyword)]
              [(null)  '(literal json-null json-keyword)]
              [else    '(malformed-token json-error)]))
          (when (not (member 'malformed-token tags))
            (set! stack
                  (value-finished stack)))
          (make-token-from-text start-pos
                                (current-stream-position in)
                                text
                                tags)]
         [else
          (write-one! in out)
          (make-token-from-text start-pos
                                (current-stream-position in)
                                (get-output-string out)
                                '(malformed-token json-error))])])))

;; json-derived-token-has-tag? : json-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (json-derived-token-has-tag? token tag)
  (and (member tag (json-derived-token-tags token))
       #t))
