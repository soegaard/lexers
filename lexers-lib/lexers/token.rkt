#lang racket/base

;;;
;;; Token Helpers
;;;
;;
;; Public helpers for inspecting wrapped or unwrapped lexer token values.

;; lexer-token-name   : token-like? -> symbol?
;;   Extract the effective token category from a wrapped or unwrapped token.
;; lexer-token-value  : token-like? -> any/c
;;   Extract the effective token payload from a wrapped or unwrapped token.
;; lexer-token-eof?   : token-like? -> boolean?
;;   Determine whether a wrapped or unwrapped token is an end-of-file result.

(provide lexer-token-name
         lexer-token-value
         lexer-token-eof?)

(require "private/parser-tools-compat.rkt")

;; lexer-token-name : token-like? -> symbol?
;;   Extract the effective token category from a wrapped or unwrapped token.
(define (lexer-token-name token)
  (stream-token-name token))

;; lexer-token-value : token-like? -> any/c
;;   Extract the effective token payload from a wrapped or unwrapped token.
(define (lexer-token-value token)
  (stream-token-value token))

;; lexer-token-eof? : token-like? -> boolean?
;;   Determine whether a wrapped or unwrapped token is an end-of-file result.
(define (lexer-token-eof? token)
  (eof-token? token))

(module+ test
  (require rackunit
           parser-tools/lex
           "private/parser-tools-compat.rkt")

  (define bare-token 'eof)
  (define plain-token (make-stream-token 'identifier "color"))
  (define wrapped-token
    (make-position-token plain-token
                         (make-position 1 1 0)
                         (make-position 6 1 5)))

  (check-equal? (lexer-token-name bare-token) 'eof)
  (check-false  (lexer-token-value bare-token))
  (check-true   (lexer-token-eof? bare-token))
  (check-equal? (lexer-token-name plain-token) 'identifier)
  (check-equal? (lexer-token-value plain-token) "color")
  (check-false  (lexer-token-eof? plain-token))
  (check-equal? (lexer-token-name wrapped-token) 'identifier)
  (check-equal? (lexer-token-value wrapped-token) "color")
  (check-false  (lexer-token-eof? wrapped-token)))
