#lang racket/base

;;;
;;; Rhombus Projection
;;;
;;
;; Project derived Rhombus tokens into the reusable stream model.

;; project-rhombus-derived-token : (or/c rhombus-derived-token? 'eof) rhombus-config? -> token-like?
;;   Convert a derived Rhombus token into a reusable-stream token-like value.

(provide project-rhombus-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "rhombus-derived.rkt"
         "stream.rkt")

;; skip-trivia? : rhombus-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (rhombus-config-trivia config) 'skip))

;; derived->stream-category : rhombus-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(rhombus-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(rhombus-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(rhombus-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(rhombus-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(rhombus-derived-token-has-tag? derived-token 'rhombus-builtin)
     stream-category-keyword]
    [(rhombus-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(rhombus-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [(rhombus-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(rhombus-derived-token-has-tag? derived-token 'identifier)
     stream-category-identifier]
    [else
     stream-category-unknown]))

;; raw-eof->token : rhombus-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (rhombus-config-source-positions config)))

;; malformed-token->result : rhombus-derived-token? rhombus-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (rhombus-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (rhombus-derived-token-text derived-token))
      (rhombus-derived-token-start derived-token)
      (rhombus-derived-token-end derived-token)
      (rhombus-config-source-positions config))]
    [(raise)
     (define start-pos
       (rhombus-derived-token-start derived-token))
     (define end-pos
       (rhombus-derived-token-end derived-token))
     (raise-read-error "unknown Rhombus input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-rhombus-derived-token
            "unsupported Rhombus error policy: ~a"
            (rhombus-config-errors config))]))

;; visible-derived-token? : rhombus-derived-token? rhombus-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : rhombus-derived-token? rhombus-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (rhombus-derived-token-text derived-token))
   (rhombus-derived-token-start derived-token)
   (rhombus-derived-token-end derived-token)
   (rhombus-config-source-positions config)))

;; project-rhombus-derived-token : (or/c rhombus-derived-token? 'eof) rhombus-config? -> token-like?
;;   Convert a derived Rhombus token into a reusable stream token-like value.
(define (project-rhombus-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(rhombus-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
