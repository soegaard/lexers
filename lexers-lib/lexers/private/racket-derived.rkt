#lang racket/base

;;;
;;; Racket Derived Tokens
;;;
;;
;; Adapter-backed Racket tokenization and reusable Racket-specific
;; classifications based on syntax-color/racket-lexer.

;; racket-derived-token?         : any/c -> boolean?
;;   Recognize a derived Racket token.
;; racket-derived-token-text     : racket-derived-token? -> string?
;;   Extract the source text for one derived Racket token.
;; racket-derived-token-start    : racket-derived-token? -> position?
;;   Extract the starting source position for one derived Racket token.
;; racket-derived-token-end      : racket-derived-token? -> position?
;;   Extract the ending source position for one derived Racket token.
;; racket-derived-token-tags     : racket-derived-token? -> (listof symbol?)
;;   Extract the reusable classification tags for one derived Racket token.
;; racket-derived-token-has-tag? : racket-derived-token? symbol? -> boolean?
;;   Determine whether a derived Racket token has a given classification tag.
;; make-racket-derived-reader    : -> (input-port? -> (or/c racket-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived Racket tokens.

(provide racket-derived-token?
         racket-derived-token-text
         racket-derived-token-start
         racket-derived-token-end
         racket-derived-token-tags
         racket-derived-token-has-tag?
         make-racket-derived-reader)

(require parser-tools/lex
         racket/list
         racket/match
         racket/port
         syntax-color/racket-lexer
         "parser-tools-compat.rkt")

;; A derived Racket token with reusable tags and source positions.
(struct racket-derived-token (type text start end tags) #:transparent)

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
    [(? symbol?)          cls]
    [(and (? hash?) (? immutable?)) (hash-ref cls 'type 'other)]))

;; commented-out-token-class? : (or/c symbol? immutable-hash?) -> boolean?
;;   Determine whether syntax-color marks the token as commented out by #;.
(define (commented-out-token-class? cls)
  (match cls
    [(and (? hash?) (? immutable?)) (hash-ref cls 'comment? #f)]
    [_                              #f]))

;; base-tags-for-class : symbol? -> (listof symbol?)
;;   Choose reusable tags for one syntax-color token class.
(define (base-tags-for-class cls)
  (case cls
    [(comment)            '(comment racket-comment)]
    [(sexp-comment)       '(comment racket-sexp-comment)]
    [(white-space)        '(whitespace racket-whitespace)]
    [(constant)           '(literal racket-constant)]
    [(string)             '(literal racket-string)]
    [(symbol)             '(identifier racket-symbol)]
    [(hash-colon-keyword) '(literal racket-hash-colon-keyword)]
    [(parenthesis)        '(delimiter racket-parenthesis)]
    [(no-color)           '(identifier racket-no-color)]
    [(other)              '(identifier racket-other)]
    [(error)              '(malformed-token racket-error)]
    [else                 '()]))

;; status-tags : any/c -> (listof symbol?)
;;   Map syntax-color datum status to reusable Racket tags.
(define (status-tags status)
  (case status
    [(datum)    '(racket-datum)]
    [(open)     '(racket-open)]
    [(close)    '(racket-close)]
    [(continue) '(racket-continue)]
    [(bad)      '(racket-bad)]
    [else       '()]))

;; Usual special-form name tables used as a small heuristic layer.
(define usual-definition-forms
  '("define"
    "define-values"
    "define-syntax"
    "define-syntaxes"
    "define-for-syntax"
    "define-module-boundary-contract"
    "struct"))

(define usual-binding-forms
  '("lambda"
    "let"
    "let*"
    "letrec"
    "let-values"
    "let*-values"
    "letrec-values"
    "parameterize"))

(define usual-conditional-forms
  '("if"
    "when"
    "unless"
    "cond"
    "case"
    "and"
    "or"))

(define usual-special-forms
  (append usual-definition-forms
          usual-binding-forms
          usual-conditional-forms
          '("begin"
            "begin0"
            "set!"
            "quote"
            "quasiquote"
            "syntax"
            "quasisyntax"
            "module"
            "module*"
            "require"
            "provide")))

;; heuristic-form-tags : string? (listof symbol?) -> (listof symbol?)
;;   Add clearly heuristic tags for usual Racket special forms.
(define (heuristic-form-tags text base-tags)
  (cond
    [(or (not (member 'racket-symbol base-tags))
         (not (member 'racket-datum base-tags)))
     '()]
    [(member text usual-definition-forms)
     '(racket-usual-special-form racket-definition-form)]
    [(member text usual-binding-forms)
     '(racket-usual-special-form racket-binding-form)]
    [(member text usual-conditional-forms)
     '(racket-usual-special-form racket-conditional-form)]
    [(member text usual-special-forms)
     '(racket-usual-special-form)]
    [else
     '()]))

;; token-text : string? any/c any/c any/c any/c any/c -> string?
;;   Recover the exact token text from the raw syntax-color offsets whenever
;;   possible, falling back to source-port indices and finally the raw text.
(define (token-text source raw-text raw-start raw-end start-index end-index)
  (define (valid-slice? start end)
    (and (exact-integer? start)
         (exact-integer? end)
         (<= 0 start end (string-length source))))
  (cond
    [(and (exact-integer? raw-start)
          (exact-integer? raw-end)
          (valid-slice? (sub1 raw-start) (sub1 raw-end)))
     (substring source (sub1 raw-start) (sub1 raw-end))]
    [(valid-slice? start-index end-index)
     (substring source start-index end-index)]
    [(string? raw-text)
     raw-text]
    [else
     (format "~a" raw-text)]))

;; derived-token-from-result : ... -> racket-derived-token?
;;   Construct a derived token from one syntax-color token result.
(define (derived-token-from-result text cls start-pos end-pos paren status)
  (define normalized-class
    (normalized-token-class cls))
  (define commented-out?
    (commented-out-token-class? cls))
  (define base-tags
    (base-tags-for-class normalized-class))
  (define extra-tags
    (append (status-tags status)
            (if paren '(delimiter) '())
            (if commented-out?
                '(comment racket-commented-out)
                '())))
  (define heuristic-tags
    (heuristic-form-tags text
                         (append base-tags extra-tags)))
  (racket-derived-token normalized-class
                        text
                        start-pos
                        end-pos
                        (remove-duplicates
                         (append base-tags extra-tags heuristic-tags))))

;; make-racket-derived-reader : -> (input-port? -> (or/c racket-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived Racket tokens.
(define (make-racket-derived-reader)
  (define lexer-port  #f)
  (define offset      0)
  (define mode        #f)
  (define base-offset 1)
  (define chunks      '())
  (define chunk-lock  (make-semaphore 1))

  ;; append-chunk! : exact-nonnegative-integer? string? -> void?
  ;;   Record one streamed source chunk for later exact-slice recovery.
  (define (append-chunk! start chunk)
    (call-with-semaphore
     chunk-lock
     (lambda ()
       (set! chunks
             (append chunks
                     (list (cons start chunk)))))))

  ;; slice-text : exact-nonnegative-integer? exact-nonnegative-integer? -> (or/c string? #f)
  ;;   Recover a source slice from the incrementally buffered chunks.
  (define (slice-text start end)
    (call-with-semaphore
     chunk-lock
     (lambda ()
       (define pieces '())
       (for ([chunk (in-list chunks)])
         (define chunk-start
           (car chunk))
         (define chunk-text
           (cdr chunk))
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

  ;; source-text : any/c any/c any/c exact-nonnegative-integer? exact-nonnegative-integer? -> string?
  ;;   Recover exact token text from the incremental buffer when possible.
  (define (source-text raw-text raw-start raw-end start-index end-index)
    (define (valid-local-range? start end)
      (and (exact-integer? start)
           (exact-integer? end)
           (<= 0 start end)))
    (define local-start
      (cond
        [(exact-integer? raw-start)
         (- raw-start base-offset)]
        [else
         start-index]))
    (define local-end
      (cond
        [(exact-integer? raw-end)
         (- raw-end base-offset)]
        [else
         end-index]))
    (cond
      [(and (valid-local-range? local-start local-end)
            (slice-text local-start local-end))
       => values]
      [(string? raw-text)
       raw-text]
      [else
       (format "~a" raw-text)]))

  ;; initialize-source! : input-port? -> void?
  ;;   Stream the original input into a pipe and a small exact-text buffer.
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
        (define initial-offset
          (cond
            [(exact-positive-integer? raw-offset) raw-offset]
            [else                                 1]))
        (set! base-offset initial-offset)
        (define-values (pipe-in pipe-out)
          (make-pipe))
        (port-count-lines! pipe-in)
        (set-port-next-location! pipe-in
                                 base-line
                                 base-col
                                 initial-offset)
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
        (set! lexer-port pipe-in))))

  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-racket-derived-reader "input-port?" in))
    (initialize-source! in)
    (define start-pos
      (current-stream-position lexer-port))
    (define start-index
      (file-position lexer-port))
    (define-values (text cls paren raw-start raw-end next-offset next-mode status)
      (racket-lexer*/status lexer-port offset mode))
    (set! offset next-offset)
    (set! mode   next-mode)
    (cond
      [(eof-object? text)
       'eof]
      [else
       (define end-index
         (file-position lexer-port))
       (derived-token-from-result (source-text text
                                               raw-start
                                               raw-end
                                               start-index
                                               end-index)
                                  cls
                                  start-pos
                                  (current-stream-position lexer-port)
                                  paren
                                  status)])))

;; racket-derived-token-has-tag? : racket-derived-token? symbol? -> boolean?
;;   Determine whether a derived Racket token has a given classification tag.
(define (racket-derived-token-has-tag? token tag)
  (member tag (racket-derived-token-tags token)))
