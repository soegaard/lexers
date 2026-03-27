#lang racket/base

;;;
;;; JavaScript Lexer
;;;
;;
;; Public entry points for the JavaScript lexer.

;; make-javascript-lexer      : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based JavaScript lexer.
;; make-javascript-derived-lexer : -> (input-port? -> (or/c javascript-derived-token? 'eof))
;;   Construct a port-based JavaScript lexer that returns derived token values.
;; javascript-derived-token? : any/c -> boolean?
;;   Recognize a derived JavaScript token value returned by the derived-token API.
;; javascript-derived-token-tags : javascript-derived-token? -> (listof symbol?)
;;   Extract the JavaScript-specific classification tags for one derived token.
;; javascript-derived-token-has-tag? : javascript-derived-token? symbol? -> boolean?
;;   Determine whether a derived JavaScript token has a given classification tag.
;; javascript-derived-token-text : javascript-derived-token? -> string?
;;   Extract the source text corresponding to one derived JavaScript token.
;; javascript-derived-token-start : javascript-derived-token? -> position?
;;   Extract the starting source position for one derived JavaScript token.
;; javascript-derived-token-end : javascript-derived-token? -> position?
;;   Extract the ending source position for one derived JavaScript token.
;; javascript-string->tokens  : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire JavaScript string using the JavaScript lexer.
;; javascript-string->derived-tokens : string? -> (listof javascript-derived-token?)
;;   Tokenize an entire JavaScript string into derived token values.
;; javascript-profiles        : immutable-hash?
;;   Profile defaults for the public JavaScript lexer.

(provide make-javascript-lexer
         make-javascript-derived-lexer
         javascript-derived-token?
         javascript-derived-token-tags
         javascript-derived-token-has-tag?
         javascript-derived-token-text
         javascript-derived-token-start
         javascript-derived-token-end
         javascript-string->tokens
         javascript-string->derived-tokens
         javascript-profiles)

(require parser-tools/lex
         "private/config.rkt"
         (rename-in "private/javascript-derived.rkt"
                    [javascript-derived-token? private-javascript-derived-token?]
                    [javascript-derived-token-raw private-javascript-derived-token-raw]
                    [javascript-derived-token-tags private-javascript-derived-token-tags]
                    [javascript-derived-token-has-tag? private-javascript-derived-token-has-tag?])
         "private/javascript-raw.rkt"
         "private/javascript-tokenize.rkt"
         "private/parser-tools-compat.rkt")

(define javascript-profiles javascript-profile-defaults)

;; javascript-derived-token? : any/c -> boolean?
;;   Recognize a derived JavaScript token value returned by the derived-token API.
(define (javascript-derived-token? v)
  (private-javascript-derived-token? v))

;; javascript-derived-token-tags : javascript-derived-token? -> (listof symbol?)
;;   Extract the JavaScript-specific classification tags for one derived token.
(define (javascript-derived-token-tags token)
  (private-javascript-derived-token-tags token))

;; javascript-derived-token-has-tag? : javascript-derived-token? symbol? -> boolean?
;;   Determine whether a derived JavaScript token has a given classification tag.
(define (javascript-derived-token-has-tag? token tag)
  (private-javascript-derived-token-has-tag? token tag))

;; javascript-derived-token-text : javascript-derived-token? -> string?
;;   Extract the source text corresponding to one derived JavaScript token.
(define (javascript-derived-token-text token)
  (javascript-raw-token-text (private-javascript-derived-token-raw token)))

;; javascript-derived-token-start : javascript-derived-token? -> position?
;;   Extract the starting source position for one derived JavaScript token.
(define (javascript-derived-token-start token)
  (javascript-raw-token-start (private-javascript-derived-token-raw token)))

;; javascript-derived-token-end : javascript-derived-token? -> position?
;;   Extract the ending source position for one derived JavaScript token.
(define (javascript-derived-token-end token)
  (javascript-raw-token-end (private-javascript-derived-token-raw token)))

;; make-javascript-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based JavaScript lexer.
(define (make-javascript-lexer #:profile          [profile 'coloring]
                               #:trivia           [trivia 'profile-default]
                               #:source-positions [source-positions 'profile-default])
  (define config
    (make-javascript-config #:profile          profile
                            #:trivia           trivia
                            #:source-positions source-positions))
  (lambda (in)
    (read-javascript-token in config)))

;; make-javascript-derived-lexer : -> (input-port? -> (or/c javascript-derived-token? 'eof))
;;   Construct a port-based JavaScript lexer that returns derived token values.
(define (make-javascript-derived-lexer)
  (lambda (in)
    (define raw-token (read-javascript-raw-token in))
    (cond
      [(eq? raw-token 'eof) 'eof]
      [else                 (derive-javascript-token raw-token)])))

;; javascript-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire JavaScript string using the JavaScript lexer.
(define (javascript-string->tokens source
                                   #:profile          [profile 'coloring]
                                   #:trivia           [trivia 'profile-default]
                                   #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-javascript-lexer #:profile          profile
                           #:trivia           trivia
                           #:source-positions source-positions))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (cond
      [(eof-token? token) (reverse (cons token tokens))]
      [else               (loop (cons token tokens))])))

;; javascript-string->derived-tokens : string? -> (listof javascript-derived-token?)
;;   Tokenize an entire JavaScript string into derived token values.
(define (javascript-string->derived-tokens source)
  (define lexer (make-javascript-derived-lexer))
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
    (javascript-string->tokens "// c\nconst x = 1;" #:profile 'coloring))
  (define compiler-tokens
    (javascript-string->tokens "// c\nconst x = 1;" #:profile 'compiler))
  (define string-tokens
    (javascript-string->tokens "\"hello\"" #:profile 'coloring))
  (define bad-string-tokens
    (javascript-string->tokens "\"unterminated" #:profile 'coloring))
  (define no-position-tokens
    (javascript-string->tokens "const" #:profile 'coloring #:source-positions #f))
  (define compiler-no-trivia-tokens
    (javascript-string->tokens "   const" #:profile 'compiler))
  (define coloring-with-trivia-tokens
    (javascript-string->tokens "   const" #:profile 'coloring))
  (define operator-tokens
    (javascript-string->tokens "=" #:profile 'compiler))
  (define derived-lexer
    (make-javascript-derived-lexer))
  (define derived-tokens
    (javascript-string->derived-tokens "const name = 1"))
  (define (find-derived-token tag)
    (findf (lambda (token)
             (javascript-derived-token-has-tag? token tag))
           derived-tokens))
  (define derived-keyword-token
    (find-derived-token 'keyword))
  (define derived-identifier-token
    (find-derived-token 'identifier))
  (define derived-number-token
    (findf (lambda (token)
             (javascript-derived-token-has-tag? token 'numeric-literal))
           derived-tokens))

  (check-true (pair? coloring-tokens))
  (check-true (position-token? (car coloring-tokens)))
  (check-equal? (stream-token-name (car coloring-tokens)) 'comment)
  (check-equal? (stream-token-name (car compiler-tokens)) 'keyword)
  (check-equal? (stream-token-name (car string-tokens)) 'literal)
  (check-equal? (stream-token-name (car bad-string-tokens)) 'unknown)
  (check-false (position-token? (car no-position-tokens)))
  (check-equal? (stream-token-name (car compiler-no-trivia-tokens)) 'keyword)
  (check-equal? (stream-token-name (car coloring-with-trivia-tokens)) 'whitespace)
  (check-equal? (stream-token-name (car operator-tokens)) 'operator)
  (check-exn exn:fail:read?
             (lambda ()
               (javascript-string->tokens "\"unterminated" #:profile 'compiler)))
  (check-false (eq? (derived-lexer (open-input-string "const")) 'eof))
  (check-not-false (javascript-derived-token-has-tag? derived-keyword-token 'keyword))
  (check-equal? (javascript-derived-token-tags derived-keyword-token)
                '(keyword))
  (check-equal? (javascript-derived-token-text derived-keyword-token)
                "const")
  (check-equal? (position-offset (javascript-derived-token-start derived-keyword-token))
                1)
  (check-equal? (position-offset (javascript-derived-token-end derived-keyword-token))
                6)
  (check-not-false (javascript-derived-token-has-tag? derived-identifier-token 'identifier))
  (check-not-false (javascript-derived-token-has-tag? derived-number-token 'numeric-literal)))
