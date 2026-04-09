#lang racket/base

;;;
;;; Racket Lexer
;;;
;;
;; Public entry points for the Racket lexer.

;; make-racket-lexer          : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Racket lexer.
;; make-racket-derived-lexer  : -> (input-port? -> (or/c racket-derived-token? 'eof))
;;   Construct a port-based Racket lexer that returns derived Racket token
;;   values.
;; racket-derived-token?      : any/c -> boolean?
;;   Recognize a derived Racket token value returned by the derived-token API.
;; racket-derived-token-tags  : racket-derived-token? -> (listof symbol?)
;;   Extract the Racket-specific classification tags for one derived token.
;; racket-derived-token-has-tag? : racket-derived-token? symbol? -> boolean?
;;   Determine whether a derived Racket token has a given classification tag.
;; racket-derived-token-text  : racket-derived-token? -> string?
;;   Extract the source text corresponding to one derived Racket token.
;; racket-derived-token-start : racket-derived-token? -> position?
;;   Extract the starting source position for one derived Racket token.
;; racket-derived-token-end   : racket-derived-token? -> position?
;;   Extract the ending source position for one derived Racket token.
;; racket-string->tokens      : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Racket string using the Racket lexer.
;; racket-string->derived-tokens : string? -> (listof racket-derived-token?)
;;   Tokenize an entire Racket string into derived Racket token values.
;; racket-profiles            : immutable-hash?
;;   Profile defaults for the public Racket lexer.

(provide make-racket-lexer
         make-racket-derived-lexer
         racket-derived-token?
         racket-derived-token-tags
         racket-derived-token-has-tag?
         racket-derived-token-text
         racket-derived-token-start
         racket-derived-token-end
         racket-string->tokens
         racket-string->derived-tokens
         racket-profiles)

(require parser-tools/lex
         "private/config.rkt"
         (rename-in "private/racket-derived.rkt"
                    [racket-derived-token? private-racket-derived-token?]
                    [racket-derived-token-tags private-racket-derived-token-tags]
                    [racket-derived-token-has-tag? private-racket-derived-token-has-tag?]
                    [racket-derived-token-text private-racket-derived-token-text]
                    [racket-derived-token-start private-racket-derived-token-start]
                    [racket-derived-token-end private-racket-derived-token-end]
                    [make-racket-derived-reader private-make-racket-derived-reader])
         "private/parser-tools-compat.rkt"
         "private/racket-tokenize.rkt")

(define racket-profiles racket-profile-defaults)

;; racket-derived-token? : any/c -> boolean?
;;   Recognize a derived Racket token value returned by the derived-token API.
(define (racket-derived-token? v)
  (private-racket-derived-token? v))

;; racket-derived-token-tags : racket-derived-token? -> (listof symbol?)
;;   Extract the Racket-specific classification tags for one derived token.
(define (racket-derived-token-tags token)
  (private-racket-derived-token-tags token))

;; racket-derived-token-has-tag? : racket-derived-token? symbol? -> boolean?
;;   Determine whether a derived Racket token has a given classification tag.
(define (racket-derived-token-has-tag? token tag)
  (private-racket-derived-token-has-tag? token tag))

;; racket-derived-token-text : racket-derived-token? -> string?
;;   Extract the source text corresponding to one derived Racket token.
(define (racket-derived-token-text token)
  (private-racket-derived-token-text token))

;; racket-derived-token-start : racket-derived-token? -> position?
;;   Extract the starting source position for one derived Racket token.
(define (racket-derived-token-start token)
  (private-racket-derived-token-start token))

;; racket-derived-token-end : racket-derived-token? -> position?
;;   Extract the ending source position for one derived Racket token.
(define (racket-derived-token-end token)
  (private-racket-derived-token-end token))

;; make-racket-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Racket lexer.
(define (make-racket-lexer #:profile          [profile 'coloring]
                           #:trivia           [trivia 'profile-default]
                           #:source-positions [source-positions 'profile-default])
  (define config
    (make-racket-config #:profile          profile
                        #:trivia           trivia
                        #:source-positions source-positions))
  (make-racket-token-reader config))

;; make-racket-derived-lexer : -> (input-port? -> (or/c racket-derived-token? 'eof))
;;   Construct a port-based Racket lexer that returns derived token values.
(define (make-racket-derived-lexer)
  (private-make-racket-derived-reader))

;; racket-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Racket string using the Racket lexer.
(define (racket-string->tokens source
                               #:profile          [profile 'coloring]
                               #:trivia           [trivia 'profile-default]
                               #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-racket-lexer #:profile          profile
                       #:trivia           trivia
                       #:source-positions source-positions))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (cond
      [(eof-token? token) (reverse (cons token tokens))]
      [else               (loop (cons token tokens))])))

;; racket-string->derived-tokens : string? -> (listof racket-derived-token?)
;;   Tokenize an entire Racket string into derived Racket token values.
(define (racket-string->derived-tokens source)
  (define lexer (make-racket-derived-lexer))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (cond
      [(eq? token 'eof)
       (reverse tokens)]
      [else
       (loop (cons token tokens))])))

(module+ test
  (require rackunit
           racket/list)

  (define coloring-tokens
    (racket-string->tokens "; hi\n#;(+ 1 2) #:x \"hi\""
                           #:profile 'coloring
                           #:source-positions #f))
  (define compiler-tokens
    (racket-string->tokens "; hi\n#;(+ 1 2) #:x \"hi\""
                           #:profile 'compiler
                           #:source-positions #f))
  (define no-position-tokens
    (racket-string->tokens "(define x 1)"
                           #:profile 'coloring
                           #:source-positions #f))
  (define malformed-coloring
    (racket-string->tokens "\""
                           #:profile 'coloring
                           #:source-positions #f))
  (define malformed-compiler-thunk
    (lambda ()
      (racket-string->tokens "\""
                             #:profile 'compiler
                             #:source-positions #f)))
  (define derived-tokens
    (racket-string->derived-tokens "; hi\n#;(+ 1 2) #:x \"hi\""))
  (define derived-comment
    (findf (lambda (token)
             (racket-derived-token-has-tag? token 'racket-comment))
           derived-tokens))
  (define derived-sexp-comment
    (findf (lambda (token)
             (racket-derived-token-has-tag? token 'racket-sexp-comment))
           derived-tokens))
  (define derived-commented-out
    (findf (lambda (token)
             (racket-derived-token-has-tag? token 'racket-commented-out))
           derived-tokens))
  (define derived-hash-colon
    (findf (lambda (token)
             (racket-derived-token-has-tag? token 'racket-hash-colon-keyword))
           derived-tokens))
  (define derived-string
    (findf (lambda (token)
             (racket-derived-token-has-tag? token 'racket-string))
           derived-tokens))
  (define derived-open
    (findf (lambda (token)
             (racket-derived-token-has-tag? token 'racket-open))
           derived-tokens))
  (define derived-close
    (findf (lambda (token)
             (racket-derived-token-has-tag? token 'racket-close))
           derived-tokens))
  (define derived-continue
    (findf (lambda (token)
             (racket-derived-token-has-tag? token 'racket-continue))
           derived-tokens))

  (check-equal? (map stream-token-name coloring-tokens)
                '(comment whitespace comment comment comment comment comment
                  comment comment comment whitespace literal whitespace
                  literal eof))
  (check-equal? (map stream-token-name compiler-tokens)
                '(literal literal eof))
  (check-equal? (map stream-token-name no-position-tokens)
                '(delimiter identifier whitespace identifier whitespace literal delimiter eof))
  (check-equal? (stream-token-name (car malformed-coloring))
                'unknown)
  (check-exn exn:fail:read?
             malformed-compiler-thunk)
  (check-not-false derived-comment)
  (check-not-false derived-sexp-comment)
  (check-not-false derived-commented-out)
  (check-not-false derived-hash-colon)
  (check-not-false derived-string)
  (check-not-false derived-open)
  (check-not-false derived-close)
  (check-not-false derived-continue)
  (check-equal? (racket-derived-token-text derived-hash-colon)
                "#:x")
  (check-true (< (position-offset (racket-derived-token-start derived-string))
                 (position-offset (racket-derived-token-end derived-string))))
  (check-true (position-token? (car (racket-string->tokens "(x)"
                                                           #:profile 'coloring))))
  (check-false (position-token? (car no-position-tokens))))
