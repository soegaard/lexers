#lang racket/base

;;;
;;; Reusable Stream Categories
;;;
;;
;; Shared category names for the reusable token stream model.

;; stream-category-whitespace : symbol?
;;   Common reusable-stream category for whitespace.
;; stream-category-comment    : symbol?
;;   Common reusable-stream category for comments.
;; stream-category-identifier : symbol?
;;   Common reusable-stream category for identifiers.
;; stream-category-keyword    : symbol?
;;   Common reusable-stream category for keywords.
;; stream-category-literal    : symbol?
;;   Common reusable-stream category for literals.
;; stream-category-operator   : symbol?
;;   Common reusable-stream category for operators.
;; stream-category-delimiter  : symbol?
;;   Common reusable-stream category for delimiters.
;; stream-category-unknown    : symbol?
;;   Common reusable-stream category for recoverable unknown input.

(provide stream-category-whitespace
         stream-category-comment
         stream-category-identifier
         stream-category-keyword
         stream-category-literal
         stream-category-operator
         stream-category-delimiter
         stream-category-unknown)

;; Common reusable-stream categories.
(define stream-category-whitespace 'whitespace)
(define stream-category-comment    'comment)
(define stream-category-identifier 'identifier)
(define stream-category-keyword    'keyword)
(define stream-category-literal    'literal)
(define stream-category-operator   'operator)
(define stream-category-delimiter  'delimiter)
(define stream-category-unknown    'unknown)

