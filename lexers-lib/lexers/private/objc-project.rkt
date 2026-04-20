#lang racket/base

;;;
;;; Objective-C Projection
;;;
;;
;; Project derived Objective-C tokens into the reusable stream model.

;; project-objc-derived-token : (or/c objc-derived-token? 'eof) objc-config? -> token-like?
;;   Convert a derived Objective-C token into a reusable-stream token-like value.

(provide project-objc-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "objc-derived.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; skip-trivia? : objc-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (objc-config-trivia config) 'skip))

;; derived->stream-category : objc-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(objc-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(objc-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(objc-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(objc-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(objc-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(objc-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(objc-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : objc-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (objc-config-source-positions config)))

;; malformed-token->result : objc-derived-token? objc-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (objc-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (objc-derived-token-text derived-token))
      (objc-derived-token-start derived-token)
      (objc-derived-token-end derived-token)
      (objc-config-source-positions config))]
    [(raise)
     (define start-pos
       (objc-derived-token-start derived-token))
     (define end-pos
       (objc-derived-token-end derived-token))
     (raise-read-error "unknown Objective-C input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-objc-derived-token
            "unsupported Objective-C error policy: ~a"
            (objc-config-errors config))]))

;; visible-derived-token? : objc-derived-token? objc-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : objc-derived-token? objc-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (objc-derived-token-text derived-token))
   (objc-derived-token-start derived-token)
   (objc-derived-token-end derived-token)
   (objc-config-source-positions config)))

;; project-objc-derived-token : (or/c objc-derived-token? 'eof) objc-config? -> token-like?
;;   Convert a derived Objective-C token into the reusable stream model.
(define (project-objc-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(objc-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
