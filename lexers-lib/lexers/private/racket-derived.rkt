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
         syntax-color/scribble-lexer
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
    [(text)               '(literal scribble-text)]
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

;; at-exp-command-char? : symbol? string? -> boolean?
;;   Determine whether one token is the Scribble command character in at-exp mode.
(define (at-exp-command-char? cls text)
  (and (eq? cls 'parenthesis)
       (string=? text "@")))

;; at-exp-structure-tags : symbol? string? boolean? exact-nonnegative-integer? -> (listof symbol?)
;;   Add Scribble structure tags when lexing a #lang at-exp source.
(define (at-exp-structure-tags cls text pending-command? bracket-depth)
  (append
   (cond
     [(at-exp-command-char? cls text)
      '(scribble-command-char)]
     [else
      '()])
   (cond
     [(and pending-command?
           (eq? cls 'symbol))
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

;; update-at-exp-command-state : symbol? string? boolean? -> boolean?
;;   Advance the small @command state machine in at-exp mode.
(define (update-at-exp-command-state cls text pending-command?)
  (cond
    [(at-exp-command-char? cls text)
     #t]
    [pending-command?
     #f]
    [else
     #f]))

;; update-at-exp-command-bracket-state : symbol? string? -> boolean?
;;   Determine whether the next token may begin a @command[...] escape.
(define (update-at-exp-command-bracket-state cls text)
  (and (eq? cls 'symbol)
       (not (string=? text "@"))))

;; update-at-exp-bracket-depth : string? boolean? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Track bracket-delimited @command[...] escapes in at-exp mode.
(define (update-at-exp-bracket-depth text after-command? bracket-depth)
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
(define (derived-token-from-result text cls start-pos end-pos paren status
                                   #:at-exp?            [at-exp? #f]
                                   #:pending-command?   [pending-command? #f]
                                   #:bracket-depth      [bracket-depth 0])
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
  (define at-exp-tags
    (cond
      [at-exp?
       (at-exp-structure-tags normalized-class
                              text
                              pending-command?
                              bracket-depth)]
      [else
       '()]))
  (racket-derived-token normalized-class
                        text
                        start-pos
                        end-pos
                        (remove-duplicates
                         (append base-tags extra-tags heuristic-tags at-exp-tags))))

;; at-exp-source-prefix? : string? -> boolean?
;;   Recognize a #lang at-exp language line at the start of a source file.
(define (at-exp-source-prefix? prefix)
  (regexp-match? #px"^#lang[ \t]+at-exp(?:[ \t]|$)"
                 prefix))

;; make-racket-derived-reader : -> (input-port? -> (or/c racket-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived Racket tokens.
(define (make-racket-derived-reader)
  (struct source-chunk (actual-start logical-start text logical-length)
    #:transparent)

  (define lexer-port  #f)
  (define lexer-kind  'racket)
  (define offset      0)
  (define mode        #f)
  (define chunks      '())
  (define chunk-lock  (make-semaphore 1))
  (define next-logical-start 0)
  (define pending-command? #f)
  (define after-command?   #f)
  (define bracket-depth    0)

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
  ;;   Recover a source slice from the incrementally buffered chunks.
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

  ;; source-char : exact-nonnegative-integer? -> (or/c char? #f)
  ;;   Read one exact source character from the buffered chunks.
  (define (source-char index)
    (define piece
      (slice-text index (add1 index)))
    (cond
      [(and piece (= (string-length piece) 1))
       (string-ref piece 0)]
      [else
       #f]))

  ;; source-prefix=? : exact-nonnegative-integer? string? -> boolean?
  ;;   Determine whether the buffered source matches one string at one index.
  (define (source-prefix=? index text)
    (define piece
      (slice-text index (+ index (string-length text))))
    (and piece
         (string=? piece text)))

  ;; next-newline-unit-length : exact-nonnegative-integer? -> exact-positive-integer?
  ;;   Measure one logical newline unit from the buffered exact source.
  (define (next-newline-unit-length index)
    (define ch
      (source-char index))
    (cond
      [(not ch) 1]
      [(char=? ch #\return)
       (cond
         [(equal? (source-char (add1 index)) #\newline) 2]
         [else                                          1])]
      [else 1]))

  ;; consume-line-comment : exact-nonnegative-integer? -> string?
  ;;   Recover one source-faithful ; line comment without its trailing newline.
  (define (consume-line-comment index)
    (let loop ([end index])
      (define ch
        (source-char end))
      (cond
        [(or (not ch)
             (char=? ch #\newline))
         (or (slice-text index end) "")]
        [else
         (loop (add1 end))])))

  ;; consume-horizontal-whitespace : exact-nonnegative-integer? -> string?
  ;;   Recover one horizontal whitespace run from the buffered exact source.
  (define (consume-horizontal-whitespace index)
    (let loop ([end index])
      (define ch
        (source-char end))
      (cond
        [(and ch
              (char-whitespace? ch)
              (not (char=? ch #\newline))
              (not (char=? ch #\return)))
         (loop (add1 end))]
        [else
         (or (slice-text index end) "")])))

  ;; consume-newlines : exact-nonnegative-integer? exact-positive-integer? -> string?
  ;;   Recover one or more logical newline units from the buffered exact source.
  (define (consume-newlines index count)
    (let loop ([remaining count]
               [end       index])
      (cond
        [(zero? remaining)
         (or (slice-text index end) "")]
        [else
         (loop (sub1 remaining)
               (+ end (next-newline-unit-length end)))])))

  ;; source-text : any/c symbol? position? position? -> string?
  ;;   Recover exact token text from the incremental buffer using the current
  ;;   stream positions as the primary exact coordinate system.
  (define (source-text raw-text normalized-class start-pos end-pos)
    (define logical-start
      (max 0 (sub1 (position-offset start-pos))))
    (define logical-end
      (max logical-start (sub1 (position-offset end-pos))))
    (define start-index
      (or (logical-index->actual-index logical-start)
          logical-start))
    (define end-index
      (or (logical-index->actual-index logical-end)
          start-index))
    (cond
      [(and (< start-index end-index)
            (slice-text start-index end-index))
       => values]
      [(and (memq normalized-class '(comment sexp-comment))
            (equal? (source-char start-index) #\;))
       (consume-line-comment start-index)]
      [(eq? normalized-class 'white-space)
       (cond
         [(and (string? raw-text)
               (positive? (string-length raw-text))
               (source-prefix=? start-index raw-text))
          raw-text]
         [else
          (define ch
            (source-char start-index))
          (cond
            [(not ch) ""]
            [(or (char=? ch #\newline)
                 (char=? ch #\return))
             (define newline-count
               (for/sum ([raw-ch (in-string (if (string? raw-text) raw-text ""))]
                         #:when (char=? raw-ch #\newline))
                 1))
             (consume-newlines start-index
                               (max 1 newline-count))]
            [else
             (consume-horizontal-whitespace start-index)])])]
      [(and (string? raw-text)
            (source-prefix=? start-index raw-text))
       raw-text]
      [else
        (define ch
         (source-char start-index))
       (cond
         [ch  (string ch)]
         [else
          (format "~a" raw-text)])]))

  ;; read-initial-prefix : input-port? -> string?
  ;;   Read the initial #lang line eagerly so we can choose the right lexer.
  (define (read-initial-prefix in)
    (define out
      (open-output-string))
    (define first-char
      (read-char in))
    (cond
      [(eof-object? first-char)
       ""]
      [else
       (write-char first-char out)
       (let loop ()
         (cond
           [(char=? (string-ref (get-output-string out)
                                (sub1 (string-length (get-output-string out))))
                     #\newline)
            (get-output-string out)]
           [(char-ready? in)
            (define next-char
              (read-char in))
            (unless (eof-object? next-char)
              (write-char next-char out)
              (loop))
            (get-output-string out)]
           [else
            (get-output-string out)]))]))

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
        (define initial-prefix
          (read-initial-prefix in))
        (set! lexer-kind
              (cond
                [(at-exp-source-prefix? initial-prefix) 'at-exp]
                [else                                   'racket]))
        (define-values (pipe-in pipe-out)
          (make-pipe))
        (port-count-lines! pipe-in)
        (set-port-next-location! pipe-in
                                 base-line
                                 base-col
                                 initial-offset)
        (append-chunk! 0 initial-prefix)
        (write-string initial-prefix pipe-out)
        (flush-output pipe-out)
        (thread
         (lambda ()
           (let loop ([local-start (string-length initial-prefix)])
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
    (case lexer-kind
      [(at-exp)
       (define-values (text cls paren raw-start raw-end next-offset next-mode)
         (scribble-lexer lexer-port offset mode))
       (set! offset next-offset)
       (set! mode   next-mode)
       (cond
         [(eof-object? text)
          'eof]
         [else
          (define normalized-class
            (normalized-token-class cls))
          (define end-pos
            (current-stream-position lexer-port))
          (define exact-text
            (source-text text normalized-class start-pos end-pos))
          (define token
            (derived-token-from-result
             exact-text
             cls
             start-pos
             end-pos
             paren
             (cond
               [(eq? normalized-class 'white-space) 'continue]
               [(and (eq? normalized-class 'parenthesis)
                     (member paren '(|(| |[| |{|)))
                'open]
               [(and (eq? normalized-class 'parenthesis)
                     (member paren '(|)| |]| |}|)))
                'close]
               [(eq? normalized-class 'text) 'continue]
               [(eq? normalized-class 'comment) 'continue]
               [else 'datum])
             #:at-exp?          #t
             #:pending-command? pending-command?
             #:bracket-depth    bracket-depth))
          (define normalized-after-command?
            (update-at-exp-command-bracket-state normalized-class exact-text))
          (set! bracket-depth
                (update-at-exp-bracket-depth exact-text
                                            after-command?
                                            bracket-depth))
          (set! pending-command?
                (update-at-exp-command-state normalized-class
                                             exact-text
                                             pending-command?))
          (set! after-command? normalized-after-command?)
          token])]
      [else
       (define-values (text cls paren raw-start raw-end next-offset next-mode status)
         (racket-lexer*/status lexer-port offset mode))
       (set! offset next-offset)
       (set! mode   next-mode)
       (cond
         [(eof-object? text)
          'eof]
         [else
          (define normalized-class
            (normalized-token-class cls))
          (define end-pos
            (current-stream-position lexer-port))
          (define exact-text
            (source-text text normalized-class start-pos end-pos))
          (derived-token-from-result exact-text
                                     cls
                                     start-pos
                                     end-pos
                                     paren
                                     status)])])))

;; racket-derived-token-has-tag? : racket-derived-token? symbol? -> boolean?
;;   Determine whether a derived Racket token has a given classification tag.
(define (racket-derived-token-has-tag? token tag)
  (member tag (racket-derived-token-tags token)))
