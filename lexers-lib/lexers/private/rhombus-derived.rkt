#lang racket/base

;;;
;;; Rhombus Derived Tokens
;;;
;;
;; Adapter-backed Rhombus tokenization and reusable Rhombus-specific
;; classifications based on rhombus/private/syntax-color.

;; rhombus-derived-token?         : any/c -> boolean?
;;   Recognize a derived Rhombus token.
;; rhombus-derived-token-text     : rhombus-derived-token? -> string?
;;   Extract the source text for one derived Rhombus token.
;; rhombus-derived-token-start    : rhombus-derived-token? -> position?
;;   Extract the starting source position for one derived Rhombus token.
;; rhombus-derived-token-end      : rhombus-derived-token? -> position?
;;   Extract the ending source position for one derived Rhombus token.
;; rhombus-derived-token-tags     : rhombus-derived-token? -> (listof symbol?)
;;   Extract the reusable classification tags for one derived Rhombus token.
;; rhombus-derived-token-has-tag? : rhombus-derived-token? symbol? -> boolean?
;;   Determine whether a derived Rhombus token has a given classification tag.
;; make-rhombus-derived-reader    : -> (input-port? -> (or/c rhombus-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived Rhombus tokens.

(provide rhombus-derived-token?
         rhombus-derived-token-text
         rhombus-derived-token-start
         rhombus-derived-token-end
         rhombus-derived-token-tags
         rhombus-derived-token-has-tag?
         make-rhombus-derived-reader)

(require parser-tools/lex
         racket/list
         racket/match
         racket/port
         racket/promise
         "parser-tools-compat.rkt")

;; A derived Rhombus token with reusable tags and source positions.
(struct rhombus-derived-token (type text start end tags) #:transparent)

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

;; normalized-token-class : (or/c symbol? immutable-hash?) -> symbol?
;;   Extract the underlying syntax-color token class.
(define (normalized-token-class cls)
  (match cls
    [(? symbol?)                     cls]
    [(and (? hash?) (? immutable?))  (hash-ref cls 'type 'other)]
    [_                               'other]))

;; rhombus-type : (or/c symbol? immutable-hash?) -> symbol?
;;   Extract the Rhombus-specific token classification, if available.
(define (rhombus-type cls)
  (match cls
    [(and (? hash?) (? immutable?)) (hash-ref cls 'rhombus-type #f)]
    [_                              #f]))

;; semantic-type-guess : (or/c symbol? immutable-hash?) -> symbol?
;;   Extract one optional syntax-color semantic-type guess.
(define (semantic-type-guess cls)
  (match cls
    [(and (? hash?) (? immutable?)) (hash-ref cls 'semantic-type-guess #f)]
    [_                              #f]))

;; base-tags-for-class : symbol? -> (listof symbol?)
;;   Choose reusable tags for one syntax-color token class.
(define (base-tags-for-class cls)
  (case cls
    [(comment)     '(comment rhombus-comment)]
    [(white-space) '(whitespace rhombus-whitespace)]
    [(string)      '(literal rhombus-string rhombus-literal)]
    [(constant)    '(literal rhombus-constant rhombus-literal)]
    [(symbol)      '(identifier rhombus-identifier)]
    [(parenthesis) '(delimiter rhombus-parenthesis)]
    [(separator)   '(delimiter rhombus-separator)]
    [(operator)    '(operator rhombus-operator)]
    [(block-operator) '(operator rhombus-block-operator)]
    [(at)          '(operator rhombus-at)]
    [(error)       '(malformed-token rhombus-error)]
    [else          '()]))

;; rhombus-type-tags : symbol? -> (listof symbol?)
;;   Add reusable tags from Rhombus's own type classification.
(define (rhombus-type-tags type)
  (case type
    [(identifier)      '(rhombus-identifier)]
    [(whitespace)      '(rhombus-whitespace)]
    [(literal)         '(rhombus-literal)]
    [(operator)        '(operator rhombus-operator)]
    [(block-operator)  '(operator rhombus-block-operator)]
    [(comma-operator)  '(operator rhombus-comma-operator)]
    [(opener)          '(delimiter rhombus-opener)]
    [(closer)          '(delimiter rhombus-closer)]
    [(at)              '(operator rhombus-at)]
    [(fail)            '(malformed-token rhombus-fail)]
    [else              '()]))

;; semantic-tags : symbol? -> (listof symbol?)
;;   Add reusable tags from Rhombus keyword and builtin guesses.
(define (semantic-tags guess)
  (case guess
    [(keyword) '(keyword rhombus-keyword)]
    [(builtin) '(rhombus-builtin)]
    [else      '()]))

;; string-logical-length : string? -> exact-nonnegative-integer?
;;   Count source characters in syntax-color's logical newline space.
(define (string-logical-length text)
  (define len
    (string-length text))
  (let loop ([i 0]
             [count 0])
    (cond
      [(= i len)
       count]
      [(and (char=? (string-ref text i) #\return)
            (< (add1 i) len)
            (char=? (string-ref text (add1 i)) #\newline))
       (loop (+ i 2) (add1 count))]
      [else
       (loop (add1 i) (add1 count))])))

;; Cached result of dynamically resolving the Rhombus syntax-color constructor.
(define rhombus-lexer-factory-result
  (delay/sync
   (with-handlers ([exn:fail? values])
     (dynamic-require 'rhombus/private/syntax-color
                      'make-rhombus-lexer))))

;; resolve-rhombus-lexer-factory : symbol? -> procedure?
;;   Resolve the Rhombus syntax-color constructor or raise a clear error when
;;   Rhombus support is unavailable in this Racket installation.
(define (resolve-rhombus-lexer-factory who)
  (define result
    (force rhombus-lexer-factory-result))
  (cond
    [(procedure? result)
     result]
    [else
     (raise-arguments-error who
                            "Rhombus support is unavailable"
                            "requires"
                            "rhombus-lib on Racket base >= 8.14"
                            "details"
                            (exn-message result))]))

;; make-rhombus-derived-reader : -> (input-port? -> (or/c rhombus-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived Rhombus tokens.
(define (make-rhombus-derived-reader)
  (struct source-chunk (actual-start logical-start text logical-length)
    #:transparent)

  (define lexer-port         #f)
  (define lexer              #f)
  (define offset             0)
  (define mode               #f)
  (define chunks             '())
  (define chunk-lock         (make-semaphore 1))
  (define next-logical-start 0)

  ;; append-chunk! : exact-nonnegative-integer? string? -> void?
  ;;   Record one streamed source chunk for later exact-slice recovery.
  (define (append-chunk! start chunk)
    (call-with-semaphore
     chunk-lock
     (lambda ()
       (define logical-length
         (string-logical-length chunk))
       (set! chunks
             (append chunks
                     (list (source-chunk start
                                         next-logical-start
                                         chunk
                                         logical-length))))
       (set! next-logical-start
             (+ next-logical-start logical-length)))))

  ;; slice-text : exact-nonnegative-integer? exact-nonnegative-integer? -> (or/c string? #f)
  ;;   Recover one source slice from the incrementally buffered chunks.
  (define (slice-text start end)
    (call-with-semaphore
     chunk-lock
     (lambda ()
       (define pieces '())
       (for ([chunk (in-list chunks)])
         (define chunk-start
           (source-chunk-actual-start chunk))
         (define chunk-text
           (source-chunk-text chunk))
         (define chunk-end
           (+ chunk-start (string-length chunk-text)))
         (when (and (< start chunk-end)
                    (< chunk-start end))
           (define piece-start
             (max start chunk-start))
           (define piece-end
             (min end chunk-end))
           (set! pieces
                 (cons (substring chunk-text
                                  (- piece-start chunk-start)
                                  (- piece-end chunk-start))
                       pieces))))
       (define slice
         (apply string-append (reverse pieces)))
       (cond
         [(= (string-length slice) (- end start))
          slice]
         [else
          #f]))))

  ;; logical-index->actual-index : exact-nonnegative-integer? -> (or/c exact-nonnegative-integer? #f)
  ;;   Convert one logical syntax-color offset into one exact source index.
  (define (logical-index->actual-index logical-index)
    (call-with-semaphore
     chunk-lock
     (lambda ()
       (define found
         (for/first ([chunk (in-list chunks)]
                     #:when (<= (source-chunk-logical-start chunk)
                                logical-index
                                (+ (source-chunk-logical-start chunk)
                                   (source-chunk-logical-length chunk))))
           chunk))
       (cond
         [(not found)
          #f]
         [else
          (define text
            (source-chunk-text found))
          (define target
            (- logical-index
               (source-chunk-logical-start found)))
          (let loop ([actual-delta 0]
                     [logical-delta 0])
            (cond
              [(= logical-delta target)
               (+ (source-chunk-actual-start found) actual-delta)]
              [(>= actual-delta (string-length text))
               (+ (source-chunk-actual-start found) actual-delta)]
              [(and (char=? (string-ref text actual-delta) #\return)
                    (< (add1 actual-delta) (string-length text))
                    (char=? (string-ref text (add1 actual-delta)) #\newline))
               (loop (+ actual-delta 2) (add1 logical-delta))]
              [else
               (loop (add1 actual-delta) (add1 logical-delta))]))]))))

  ;; buffered-token-text : any/c any/c any/c position? position? -> string?
  ;;   Recover exact token text from the incremental buffer when possible.
  (define (buffered-token-text raw-text raw-start raw-end start-pos end-pos)
    (define start-index
      (max 0 (sub1 (position-offset start-pos))))
    (define end-index
      (max start-index (sub1 (position-offset end-pos))))
    (define logical-start
      (cond
        [(exact-integer? raw-start)
         (max 0 (sub1 raw-start))]
        [else
         start-index]))
    (define logical-end
      (cond
        [(exact-integer? raw-end)
         (max logical-start (sub1 raw-end))]
        [else
         end-index]))
    (define actual-start
      (or (logical-index->actual-index logical-start)
          start-index))
    (define actual-end
      (or (logical-index->actual-index logical-end)
          end-index))
    (cond
      [(and (exact-integer? actual-start)
            (exact-integer? actual-end)
            (<= 0 actual-start actual-end)
            (slice-text actual-start actual-end))
       => values]
      [(string? raw-text)
       raw-text]
      [else
       (format "~a" raw-text)]))

  ;; initialize-source! : input-port? -> void?
  ;;   Stream the original input into a pipe and an exact-text buffer.
  (define (initialize-source! in)
    (when (not lexer-port)
      (let-values ([(line col raw-offset) (port-next-location in)])
        (define base-line
          (cond
            [(exact-positive-integer? line)   line]
            [else                             1]))
        (define base-col
          (cond
            [(exact-nonnegative-integer? col) col]
            [else                             0]))
        (define base-offset
          (cond
            [(exact-positive-integer? raw-offset) raw-offset]
            [else                                 1]))
        (define-values (pipe-in pipe-out)
          (make-pipe))
        (port-count-lines! pipe-in)
        (set-port-next-location! pipe-in
                                 base-line
                                 base-col
                                 base-offset)
        (thread
         (lambda ()
           (let loop ([local-start 0])
             (define first-char
               (read-char in))
             (cond
               [(eof-object? first-char)
                (close-output-port pipe-out)]
               [else
                (define out
                  (open-output-string))
                (write-char first-char out)
                (let fill ()
                  (when (char-ready? in)
                    (define next-char
                      (read-char in))
                    (unless (eof-object? next-char)
                      (write-char next-char out)
                      (fill))))
                (define chunk
                  (get-output-string out))
                (append-chunk! local-start chunk)
                (write-string chunk pipe-out)
                (flush-output pipe-out)
                (loop (+ local-start (string-length chunk)))]))))
        (set! lexer-port pipe-in)
        (set! lexer
              ((resolve-rhombus-lexer-factory 'make-rhombus-derived-reader))))))

  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-rhombus-derived-reader "input-port?" in))
    (initialize-source! in)
    (define start-pos
      (current-stream-position lexer-port))
    (define-values (raw-text cls _paren raw-start raw-end next-offset next-mode)
      (lexer lexer-port offset mode))
    (set! offset next-offset)
    (set! mode   next-mode)
    (cond
      [(eof-object? raw-text)
       'eof]
      [else
       (define end-pos
         (current-stream-position lexer-port))
       (define text
         (buffered-token-text raw-text
                              raw-start
                              raw-end
                              start-pos
                              end-pos))
       (define base-class
         (normalized-token-class cls))
       (define tags
         (remove-duplicates
          (append (base-tags-for-class base-class)
                  (rhombus-type-tags (rhombus-type cls))
                  (semantic-tags (semantic-type-guess cls)))))
       (rhombus-derived-token cls
                              text
                              start-pos
                              end-pos
                              tags)])))

;; rhombus-derived-token-has-tag? : rhombus-derived-token? symbol? -> boolean?
;;   Determine whether a derived Rhombus token has a given classification tag.
(define (rhombus-derived-token-has-tag? token tag)
  (and (member tag (rhombus-derived-token-tags token))
       #t))
