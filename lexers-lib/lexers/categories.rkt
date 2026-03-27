#lang racket/base

;;;
;;; Token Categories
;;;
;;
;; Public category constants for the projected lexer token stream.

;; lexer-category-whitespace : symbol?
;;   Public category name for whitespace tokens.
;; lexer-category-comment    : symbol?
;;   Public category name for comment tokens.
;; lexer-category-identifier : symbol?
;;   Public category name for identifier tokens.
;; lexer-category-keyword    : symbol?
;;   Public category name for keyword tokens.
;; lexer-category-literal    : symbol?
;;   Public category name for literal tokens.
;; lexer-category-operator   : symbol?
;;   Public category name for operator tokens.
;; lexer-category-delimiter  : symbol?
;;   Public category name for delimiter tokens.
;; lexer-category-unknown    : symbol?
;;   Public category name for recoverable unknown tokens.

(provide lexer-category-whitespace
         lexer-category-comment
         lexer-category-identifier
         lexer-category-keyword
         lexer-category-literal
         lexer-category-operator
         lexer-category-delimiter
         lexer-category-unknown)

(require "private/stream.rkt")

;; lexer-category-whitespace : symbol?
;;   Public category name for whitespace tokens.
(define lexer-category-whitespace stream-category-whitespace)

;; lexer-category-comment : symbol?
;;   Public category name for comment tokens.
(define lexer-category-comment stream-category-comment)

;; lexer-category-identifier : symbol?
;;   Public category name for identifier tokens.
(define lexer-category-identifier stream-category-identifier)

;; lexer-category-keyword : symbol?
;;   Public category name for keyword tokens.
(define lexer-category-keyword stream-category-keyword)

;; lexer-category-literal : symbol?
;;   Public category name for literal tokens.
(define lexer-category-literal stream-category-literal)

;; lexer-category-operator : symbol?
;;   Public category name for operator tokens.
(define lexer-category-operator stream-category-operator)

;; lexer-category-delimiter : symbol?
;;   Public category name for delimiter tokens.
(define lexer-category-delimiter stream-category-delimiter)

;; lexer-category-unknown : symbol?
;;   Public category name for recoverable unknown tokens.
(define lexer-category-unknown stream-category-unknown)

(module+ test
  (require rackunit)

  (check-equal? lexer-category-whitespace 'whitespace)
  (check-equal? lexer-category-comment    'comment)
  (check-equal? lexer-category-identifier 'identifier)
  (check-equal? lexer-category-keyword    'keyword)
  (check-equal? lexer-category-literal    'literal)
  (check-equal? lexer-category-operator   'operator)
  (check-equal? lexer-category-delimiter  'delimiter)
  (check-equal? lexer-category-unknown    'unknown))
