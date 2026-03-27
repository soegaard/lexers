#lang racket/base

;;;
;;; Parser Tools Compatibility
;;;
;;
;; Helpers for constructing and inspecting token values that use the actual
;; structures exported by parser-tools.

;; make-stream-token     : symbol? any/c -> token?
;;   Construct a parser-tools token carrying a reusable-stream category.
;; wrap-token-with-pos   : any/c position? position? boolean? -> any/c
;;   Conditionally wrap a token-like value in a parser-tools position token.
;; make-stream-position  : exact-integer? exact-integer? exact-integer? -> position?
;;   Construct a parser-tools position value.
;; stream-token-name     : token-like? -> symbol?
;;   Extract the token category from a wrapped or unwrapped token value.
;; eof-token?            : token-like? -> boolean?
;;   Recognize wrapped or unwrapped eof results.

(provide make-stream-token
         wrap-token-with-pos
         make-stream-position
         stream-token-name
         eof-token?)

(require parser-tools/lex
         parser-tools/private-lex/token)

;; make-stream-token : symbol? any/c -> token?
;;   Construct a parser-tools token carrying a reusable-stream category.
(define (make-stream-token category value)
  (make-token category value))

;; wrap-token-with-pos : any/c position? position? boolean? -> any/c
;;   Conditionally wrap a token-like value in a parser-tools position token.
(define (wrap-token-with-pos token start-pos end-pos include-positions?)
  (cond
    [include-positions? (make-position-token token start-pos end-pos)]
    [else               token]))

;; make-stream-position : exact-integer? exact-integer? exact-integer? -> position?
;;   Construct a parser-tools position value.
(define (make-stream-position offset line col)
  (make-position offset line col))

;; stream-token-name : token-like? -> symbol?
;;   Extract the token category from a wrapped or unwrapped token value.
(define (stream-token-name token)
  (cond
    [(position-token? token) (stream-token-name (position-token-token token))]
    [(symbol? token)         token]
    [else                    (token-name token)]))

;; eof-token? : token-like? -> boolean?
;;   Recognize wrapped or unwrapped eof results.
(define (eof-token? token)
  (eq? (stream-token-name token) 'eof))
