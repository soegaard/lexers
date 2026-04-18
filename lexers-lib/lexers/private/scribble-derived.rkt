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
  (struct source-chunk (actual-start logical-start text logical-length)
    #:transparent)

  (define lexer-port       #f)
  (define lexer            #f)
  (define offset           0)
  (define mode             #f)
  (define pending-command? #f)
  (define after-command?   #f)
  (define bracket-depth    0)
  (define chunks           '())
  (define chunk-lock       (make-semaphore 1))
  (define next-logical-start 0)

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
        (set! lexer (make-scribble-inside-lexer)))))

  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-scribble-derived-reader "input-port?" in))
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
