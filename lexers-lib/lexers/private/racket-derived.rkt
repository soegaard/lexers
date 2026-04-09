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
  (define offset 0)
  (define mode   #f)
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-racket-derived-reader "input-port?" in))
    (port-count-lines! in)
    (define start-pos
      (current-stream-position in))
    (define-values (text cls paren _raw-start _raw-end next-offset next-mode status)
      (racket-lexer*/status in offset mode))
    (set! offset next-offset)
    (set! mode   next-mode)
    (cond
      [(eof-object? text)
       'eof]
      [else
       (derived-token-from-result text
                                  cls
                                  start-pos
                                  (current-stream-position in)
                                  paren
                                  status)])))

;; racket-derived-token-has-tag? : racket-derived-token? symbol? -> boolean?
;;   Determine whether a derived Racket token has a given classification tag.
(define (racket-derived-token-has-tag? token tag)
  (member tag (racket-derived-token-tags token)))
