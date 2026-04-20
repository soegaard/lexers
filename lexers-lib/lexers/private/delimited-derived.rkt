#lang racket/base

;;;
;;; Delimited Derived Tokens
;;;
;;
;; Shared streaming tokenization for delimiter-separated text formats such as
;; CSV and TSV.

;; delimited-derived-token?         : any/c -> boolean?
;;   Recognize a shared derived token for one delimited-text lexer.
;; delimited-derived-token-text     : delimited-derived-token? -> string?
;;   Extract the exact source text for one derived token.
;; delimited-derived-token-start    : delimited-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; delimited-derived-token-end      : delimited-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; delimited-derived-token-tags     : delimited-derived-token? -> (listof symbol?)
;;   Extract reusable classification tags for one derived token.
;; delimited-derived-token-has-tag? : delimited-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-delimited-derived-reader    : keyword-arguments -> (input-port? -> (or/c delimited-derived-token? 'eof))
;;   Construct a stateful derived-token reader for one delimited-text dialect.

(provide delimited-derived-token?
         delimited-derived-token-text
         delimited-derived-token-start
         delimited-derived-token-end
         delimited-derived-token-tags
         delimited-derived-token-has-tag?
         make-delimited-derived-reader)

(require parser-tools/lex
         racket/list
         racket/string
         "parser-tools-compat.rkt")

;; A shared derived token for one delimited-text lexer.
(struct delimited-derived-token (kind text start end tags) #:transparent)

;; Shared parser-lite states.
(define state-idle             'idle)
(define state-after-field      'after-field)
(define state-after-separator  'after-separator)

;; write-one! : input-port? output-port? -> void?
;;   Consume one character and append it to the output accumulator.
(define (write-one! in out)
  (write-char (read-char in) out))

;; peek-next : input-port? [exact-nonnegative-integer?] -> (or/c char? eof-object?)
;;   Peek one character ahead in the input stream.
(define (peek-next in [skip 0])
  (peek-char in skip))

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

;; dialect-symbol : symbol? string? -> symbol?
;;   Build one dialect-specific tag name.
(define (dialect-symbol dialect suffix)
  (string->symbol
   (string-append (symbol->string dialect) suffix)))

;; make-token-from-text : position? position? symbol? string? (listof symbol?) -> delimited-derived-token?
;;   Construct one derived token from explicit positions, text, and tags.
(define (make-token-from-text start-pos end-pos kind text tags)
  (delimited-derived-token kind
                           text
                           start-pos
                           end-pos
                           (remove-duplicates tags)))

;; delimited-derived-token-has-tag? : delimited-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (delimited-derived-token-has-tag? token tag)
  (member tag (delimited-derived-token-tags token)))

;; make-field-token : position? position? string? boolean? boolean? symbol? -> delimited-derived-token?
;;   Construct one field token, optionally malformed or quoted.
(define (make-field-token start-pos end-pos text quoted? empty? dialect)
  (define dialect-field-tag
    (dialect-symbol dialect "-field"))
  (define tags
    (append '(literal delimited-field)
            (list dialect-field-tag)
            (cond
              [quoted? '(delimited-quoted-field)]
              [empty?  '(delimited-empty-field)]
              [else    '(delimited-unquoted-field)])))
  (make-token-from-text start-pos end-pos 'literal text tags))

;; make-malformed-field-token : position? position? string? boolean? symbol? -> delimited-derived-token?
;;   Construct one malformed field token.
(define (make-malformed-field-token start-pos end-pos text quoted? dialect)
  (define dialect-field-tag
    (dialect-symbol dialect "-field"))
  (make-token-from-text start-pos
                        end-pos
                        'malformed
                        text
                        (append '(malformed-token delimited-error delimited-field)
                                (list dialect-field-tag)
                                (cond
                                  [quoted? '(delimited-quoted-field)]
                                  [else    '(delimited-unquoted-field)]))))

;; make-separator-token! : input-port? position? output-port? symbol? char? -> delimited-derived-token?
;;   Consume one field separator and return its derived token.
(define (make-separator-token! in start-pos out dialect separator)
  (write-char separator out)
  (read-char in)
  (make-token-from-text start-pos
                        (current-stream-position in)
                        'delimiter
                        (get-output-string out)
                        (list 'delimiter
                              'delimited-separator
                              (dialect-symbol dialect "-separator"))))

;; read-row-separator! : input-port? output-port? -> void?
;;   Consume one row separator, preserving CRLF when present.
(define (read-row-separator! in out)
  (define first
    (peek-next in))
  (cond
    [(char=? first #\return)
     (write-one! in out)
     (cond
       [(char=? (peek-next in) #\newline)
        (write-one! in out)]
       [else
        (void)])]
    [else
     (write-one! in out)]))

;; make-row-separator-token! : input-port? position? output-port? symbol? -> delimited-derived-token?
;;   Consume one row separator and return its derived token.
(define (make-row-separator-token! in start-pos out dialect)
  (read-row-separator! in out)
  (make-token-from-text start-pos
                        (current-stream-position in)
                        'delimiter
                        (get-output-string out)
                        (list 'delimiter
                              'delimited-row-separator
                              (dialect-symbol dialect "-row-separator"))))

;; row-separator-start? : input-port? -> boolean?
;;   Determine whether the next character starts a row separator.
(define (row-separator-start? in)
  (define next
    (peek-next in))
  (or (char=? next #\newline)
      (char=? next #\return)))

;; stop-char? : input-port? char? char? -> boolean?
;;   Determine whether a character ends one unquoted field.
(define (stop-char? in ch separator)
  (or (char=? ch separator)
      (row-separator-start? in)))

;; read-unquoted-field! : input-port? output-port? char? -> string?
;;   Consume one unquoted field.
(define (read-unquoted-field! in out separator)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       (get-output-string out)]
      [else
       (define ch
         next)
       (cond
         [(stop-char? in ch separator)
          (get-output-string out)]
         [else
          (write-one! in out)
          (loop)])])))

;; read-quoted-field! : input-port? output-port? char? -> (values string? boolean?)
;;   Consume one quoted field and report whether it is malformed.
(define (read-quoted-field! in out separator)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       (values (get-output-string out) #t)]
      [else
       (define ch
         next)
       (write-one! in out)
       (cond
         [(char=? ch #\")
          (cond
            [(char=? (peek-next in) #\")
             (write-one! in out)
             (loop)]
            [else
             (let tail-loop ([malformed? #f])
               (define tail
                 (peek-next in))
               (cond
                 [(eof-object? tail)
                  (values (get-output-string out) malformed?)]
                 [else
                  (define tail-ch
                    tail)
                  (cond
                    [(stop-char? in tail-ch separator)
                     (values (get-output-string out) malformed?)]
                    [else
                     (write-one! in out)
                     (tail-loop #t)])]))])]
         [else
          (loop)])])))

;; make-empty-field-token : input-port? symbol? -> delimited-derived-token?
;;   Construct one zero-length empty field token at the current position.
(define (make-empty-field-token in dialect)
  (define pos
    (current-stream-position in))
  (make-field-token pos pos "" #f #t dialect))

;; make-delimited-derived-reader : keyword-arguments -> (input-port? -> (or/c delimited-derived-token? 'eof))
;;   Construct a shared streaming derived-token reader for one delimited-text dialect.
(define (make-delimited-derived-reader #:separator separator
                                       #:dialect   dialect)
  (define state
    state-idle)
  (define pending
    '())
  (define (enqueue! token)
    (set! pending (append pending (list token))))
  (define (dequeue!)
    (define token
      (car pending))
    (set! pending (cdr pending))
    token)
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-delimited-derived-reader "input-port?" in))
    (port-count-lines! in)
    (cond
      [(pair? pending)
       (dequeue!)]
      [else
       (define start-pos
         (current-stream-position in))
       (define next
         (peek-next in))
       (cond
         [(eof-object? next)
          (cond
            [(eq? state state-after-separator)
             (set! state state-after-field)
             (make-empty-field-token in dialect)]
            [else
             'eof])]
         [(char=? next separator)
          (cond
            [(or (eq? state state-idle)
                 (eq? state state-after-separator))
             (define separator-token
               (make-separator-token! in start-pos (open-output-string) dialect separator))
             (enqueue! separator-token)
             (set! state state-after-separator)
             (make-empty-field-token in dialect)]
            [else
             (set! state state-after-separator)
             (make-separator-token! in start-pos (open-output-string) dialect separator)])]
         [(row-separator-start? in)
          (cond
            [(eq? state state-after-separator)
             (define row-token
               (make-row-separator-token! in start-pos (open-output-string) dialect))
             (enqueue! row-token)
             (set! state state-idle)
             (make-empty-field-token in dialect)]
            [else
             (set! state state-idle)
             (make-row-separator-token! in start-pos (open-output-string) dialect)])]
         [else
          (define out
            (open-output-string))
          (cond
            [(char=? next #\")
             (define-values (text malformed?)
               (read-quoted-field! in out separator))
             (set! state state-after-field)
             (cond
               [malformed?
                (make-malformed-field-token start-pos
                                            (current-stream-position in)
                                            text
                                            #t
                                            dialect)]
               [else
                (make-field-token start-pos
                                  (current-stream-position in)
                                  text
                                  #t
                                  #f
                                  dialect)])]
            [else
             (define text
               (read-unquoted-field! in out separator))
             (set! state state-after-field)
             (make-field-token start-pos
                               (current-stream-position in)
                               text
                               #f
                               #f
                               dialect)])])])))
