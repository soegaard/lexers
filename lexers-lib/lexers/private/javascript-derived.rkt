#lang racket/base

;;;
;;; JavaScript Derived Classifications
;;;
;;
;; Derived JavaScript-specific classifications layered on top of raw tokens.

;; javascript-derived-token?             : any/c -> boolean?
;;   Recognize a derived JavaScript token.
;; javascript-derived-token-raw          : javascript-derived-token? -> javascript-raw-token?
;;   Extract the underlying raw token.
;; javascript-derived-token-tags         : javascript-derived-token? -> (listof symbol?)
;;   Extract the derived classification tags.
;; derive-javascript-token               : javascript-raw-token? -> javascript-derived-token?
;;   Attach JavaScript-specific derived classifications to one raw token.
;; javascript-derived-token-has-tag?     : javascript-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a classification tag.

(provide javascript-derived-token?
         javascript-derived-token-raw
         javascript-derived-token-tags
         derive-javascript-token
         javascript-derived-token-has-tag?)

(require racket/list
         "javascript-raw.rkt")

(struct javascript-derived-token (raw tags) #:transparent)

(define javascript-keywords
  '("const" "let" "var" "function" "return" "if" "else" "for" "while"))

;; javascript-keyword? : string? -> boolean?
;;   Recognize a keyword in the small initial JavaScript subset.
(define (javascript-keyword? text)
  (member text javascript-keywords))

;; raw-token->derived-tags : javascript-raw-token? -> (listof symbol?)
;;   Compute JavaScript-specific derived classification tags for one raw token.
(define (raw-token->derived-tags raw-token)
  (define kind (javascript-raw-token-kind raw-token))
  (define text (javascript-raw-token-text raw-token))
  (remove-duplicates
   (append
    (case kind
      [(identifier-token)
       (cond
         [(javascript-keyword? text) '(keyword)]
         [else                       '(identifier)])]
      [(string-token) '(string-literal)]
      [(number-token) '(numeric-literal)]
      [(line-comment-token block-comment-token) '(comment)]
      [else '()])
    (case kind
      [(bad-string-token unknown-raw-token) '(malformed-token)]
      [else                                 '()]))))

;; derive-javascript-token : javascript-raw-token? -> javascript-derived-token?
;;   Attach JavaScript-specific derived classifications to one raw token.
(define (derive-javascript-token raw-token)
  (javascript-derived-token raw-token
                            (raw-token->derived-tags raw-token)))

;; javascript-derived-token-has-tag? : javascript-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a classification tag.
(define (javascript-derived-token-has-tag? derived-token tag)
  (member tag (javascript-derived-token-tags derived-token)))

