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

;; token-text : string? any/c exact-nonnegative-integer? exact-nonnegative-integer? -> string?
;;   Recover the exact token text from the consumed source span whenever
;;   possible.
(define (token-text source raw-text start-index end-index)
  (cond
    [(<= 0 start-index end-index (string-length source))
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
  (define source           #f)
  (define source-port      #f)
  (define lexer            #f)
  (define offset           0)
  (define mode             #f)
  (define pending-command? #f)
  (define after-command?   #f)
  (define bracket-depth    0)

  ;; initialize-source! : input-port? -> void?
  ;;   Buffer the remaining input into an internal string port so the adapter
  ;;   can recover exact text spans for syntax-color's 'text results.
  (define (initialize-source! in)
    (when (not source-port)
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
        (set! source (port->string in))
        (set! source-port (open-input-string source))
        (port-count-lines! source-port)
        (set-port-next-location! source-port
                                 base-line
                                 base-col
                                 base-offset)
        (set! lexer (make-scribble-inside-lexer)))))

  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-scribble-derived-reader "input-port?" in))
    (initialize-source! in)
    (define start-pos
      (current-stream-position source-port))
    (define start-index
      (file-position source-port))
    (define-values (raw-text cls _paren _raw-start _raw-end next-offset next-mode)
      (lexer source-port offset mode))
    (set! offset next-offset)
    (set! mode   next-mode)
    (define end-index
      (file-position source-port))
    (cond
      [(eof-object? raw-text)
       'eof]
      [else
       (define text
         (token-text source raw-text start-index end-index))
       (define end-pos
         (current-stream-position source-port))
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
