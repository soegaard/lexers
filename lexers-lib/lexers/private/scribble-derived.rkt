#lang racket/base

;;;
;;; Scribble Derived Tokens
;;;
;;
;; Adapter-backed Scribble tokenization and reusable Scribble-specific
;; classifications based on syntax-color/scribble-lexer.

;; scribble-derived-token?         : any/c -> boolean?
;;   Recognize a derived Scribble token.
;; scribble-derived-token-text     : scribble-derived-token? -> string?
;;   Extract the source text for one derived Scribble token.
;; scribble-derived-token-start    : scribble-derived-token? -> position?
;;   Extract the starting source position for one derived Scribble token.
;; scribble-derived-token-end      : scribble-derived-token? -> position?
;;   Extract the ending source position for one derived Scribble token.
;; scribble-derived-token-tags     : scribble-derived-token? -> (listof symbol?)
;;   Extract the reusable classification tags for one derived Scribble token.
;; scribble-derived-token-has-tag? : scribble-derived-token? symbol? -> boolean?
;;   Determine whether a derived Scribble token has a given classification tag.
;; make-scribble-derived-reader    : -> (input-port? -> (or/c scribble-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived Scribble tokens.

(provide scribble-derived-token?
         scribble-derived-token-text
         scribble-derived-token-start
         scribble-derived-token-end
         scribble-derived-token-tags
         scribble-derived-token-has-tag?
         make-scribble-derived-reader)

(require parser-tools/lex
         racket/list
         racket/port
         syntax-color/scribble-lexer
         "parser-tools-compat.rkt")

;; A derived Scribble token with reusable tags and source positions.
(struct scribble-derived-token (type text start end tags) #:transparent)

;; line-starts : string? -> (vectorof exact-nonnegative-integer?)
;;   Compute the starting index of each line in a buffered source string.
(define (line-starts source)
  (list->vector
   (let loop ([i 0] [starts '(0)])
     (cond
       [(>= i (string-length source))
        (reverse starts)]
       [(char=? (string-ref source i) #\newline)
        (loop (add1 i) (cons (add1 i) starts))]
       [else
        (loop (add1 i) starts)]))))

;; position-at : (vectorof exact-nonnegative-integer?) exact-nonnegative-integer? exact-positive-integer? exact-nonnegative-integer? exact-positive-integer? -> position?
;;   Convert a buffered-source index into a parser-tools-compatible position.
(define (position-at starts index base-line base-col base-offset)
  (define len (vector-length starts))
  (let loop ([line-idx 0])
    (cond
      [(= line-idx (sub1 len))
       (define line-start
         (vector-ref starts line-idx))
       (make-stream-position (+ base-offset index)
                             (+ base-line line-idx)
                             (cond
                               [(zero? line-idx)
                                (+ base-col index)]
                               [else
                                (- index line-start)]))]
      [(< index (vector-ref starts (add1 line-idx)))
       (define line-start
         (vector-ref starts line-idx))
       (make-stream-position (+ base-offset index)
                             (+ base-line line-idx)
                             (cond
                               [(zero? line-idx)
                                (+ base-col index)]
                               [else
                                (- index line-start)]))]
      [else
       (loop (add1 line-idx))])))

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

;; base-tags-for-class : symbol? -> (listof symbol?)
;;   Choose reusable tags for one syntax-color token class.
(define (base-tags-for-class cls)
  (case cls
    [(comment)     '(comment scribble-comment)]
    [(white-space) '(whitespace scribble-whitespace)]
    [(text)        '(literal scribble-text)]
    [(string)      '(literal scribble-string)]
    [(constant)    '(literal scribble-constant)]
    [(symbol)      '(identifier scribble-symbol)]
    [(parenthesis) '(delimiter scribble-parenthesis)]
    [(other)       '(identifier scribble-other)]
    [(error)       '(malformed-token scribble-error)]
    [else          '()]))

;; token-text : string? any/c any/c any/c exact-nonnegative-integer? exact-nonnegative-integer? -> string?
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

;; command-char-token? : scribble-derived-token? -> boolean?
;;   Determine whether a token is the ordinary Scribble command character.
(define (command-char-token? token)
  (and (scribble-derived-token-has-tag? token 'scribble-parenthesis)
       (string=? (scribble-derived-token-text token) "@")))

;; structure-tags : scribble-derived-token? boolean? exact-nonnegative-integer? -> (listof symbol?)
;;   Add parser-lite Scribble structure tags based on nearby context.
(define (structure-tags token pending-command? bracket-depth)
  (define text
    (scribble-derived-token-text token))
  (append
   (cond
     [(command-char-token? token)
      '(scribble-command-char)]
     [else
      '()])
   (cond
     [(and pending-command?
           (scribble-derived-token-has-tag? token 'scribble-symbol))
      '(scribble-command)]
     [else
      '()])
   (cond
     [(or (string=? text "{")
          (string=? text "}"))
      '(scribble-body-delimiter)]
     [else
      '()])
   (cond
     [(or (string=? text "[")
          (string=? text "]"))
      '(scribble-optional-delimiter)]
     [else
      '()])
   (cond
     [(positive? bracket-depth)
      '(scribble-racket-escape)]
     [else
      '()])))

;; update-command-state : scribble-derived-token? boolean? -> boolean?
;;   Advance the small command-name state machine.
(define (update-command-state token pending-command?)
  (cond
    [(command-char-token? token)
     #t]
    [pending-command?
     #f]
    [else
     #f]))

;; update-command-bracket-state : scribble-derived-token? -> boolean?
;;   Determine whether the next token may begin a @command[...] escape.
(define (update-command-bracket-state token)
  (scribble-derived-token-has-tag? token 'scribble-command))

;; update-bracket-depth : scribble-derived-token? boolean? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Track bracket-delimited @command[...] escapes.
(define (update-bracket-depth token after-command? bracket-depth)
  (define text
    (scribble-derived-token-text token))
  (cond
    [(and after-command? (string=? text "["))
     1]
    [(and (positive? bracket-depth)
          (string=? text "["))
     (add1 bracket-depth)]
    [(and (positive? bracket-depth)
          (string=? text "]"))
     (sub1 bracket-depth)]
    [else
     bracket-depth]))

;; make-scribble-derived-reader : -> (input-port? -> (or/c scribble-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived Scribble tokens.
(define (make-scribble-derived-reader)
  (define lexer-port       #f)
  (define lexer            #f)
  (define base-offset      1)
  (define offset           0)
  (define mode             #f)
  (define pending-command? #f)
  (define after-command?   #f)
  (define bracket-depth    0)
  (define chunks           '())
  (define chunk-lock       (make-semaphore 1))

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
  ;;   Recover one source slice from the incrementally buffered chunks.
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

  ;; buffered-token-text : any/c any/c any/c exact-nonnegative-integer? exact-nonnegative-integer? -> string?
  ;;   Recover exact token text from the incremental buffer when possible.
  (define (buffered-token-text raw-text raw-start raw-end start-index end-index)
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
        (set! base-offset
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
        (set! lexer (make-scribble-inside-lexer)))))

  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-scribble-derived-reader "input-port?" in))
    (initialize-source! in)
    (define start-pos
      (current-stream-position lexer-port))
    (define start-index
      (file-position lexer-port))
    (define-values (raw-text cls _paren raw-start raw-end next-offset next-mode)
      (lexer lexer-port offset mode))
    (set! offset next-offset)
    (set! mode   next-mode)
    (define end-index
      (file-position lexer-port))
    (cond
      [(eof-object? raw-text)
       'eof]
      [else
       (define text
         (buffered-token-text raw-text
                              raw-start
                              raw-end
                              start-index
                              end-index))
       (define end-pos
         (current-stream-position lexer-port))
       (define base-token
         (scribble-derived-token cls
                                 text
                                 start-pos
                                 end-pos
                                 (base-tags-for-class cls)))
       (define token
         (scribble-derived-token cls
                                 text
                                 start-pos
                                 end-pos
                                 (remove-duplicates
                                  (append (scribble-derived-token-tags base-token)
                                          (structure-tags base-token
                                                          pending-command?
                                                          bracket-depth)))))
       (define next-pending-command?
         (update-command-state token pending-command?))
       (define next-after-command?
         (update-command-bracket-state token))
       (define next-bracket-depth
         (update-bracket-depth token after-command? bracket-depth))
       (set! pending-command? next-pending-command?)
       (set! after-command?   next-after-command?)
       (set! bracket-depth    next-bracket-depth)
       token])))

;; scribble-derived-token-has-tag? : scribble-derived-token? symbol? -> boolean?
;;   Determine whether a derived Scribble token has a given classification tag.
(define (scribble-derived-token-has-tag? token tag)
  (member tag (scribble-derived-token-tags token)))
