#lang racket/base

;;;
;;; Racket Projection
;;;
;;
;; Project derived Racket tokens into the reusable stream model.

;; project-racket-derived-token : (or/c racket-derived-token? 'eof) racket-config? -> token-like?
;;   Convert a derived Racket token into a reusable-stream token-like value.

(provide project-racket-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "racket-derived.rkt"
         "stream.rkt")

;; skip-trivia? : racket-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (racket-config-trivia config) 'skip))

;; derived->stream-category : racket-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(racket-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(racket-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(racket-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(racket-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(racket-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [(racket-derived-token-has-tag? derived-token 'identifier)
     stream-category-identifier]
    [else
     stream-category-unknown]))

;; raw-eof->token : racket-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (racket-config-source-positions config)))

;; malformed-token->result : racket-derived-token? racket-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (racket-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (racket-derived-token-text derived-token))
      (racket-derived-token-start derived-token)
      (racket-derived-token-end derived-token)
      (racket-config-source-positions config))]
    [(raise)
     (define start-pos (racket-derived-token-start derived-token))
     (define end-pos   (racket-derived-token-end derived-token))
     (raise-read-error "unknown Racket input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-racket-derived-token
            "unsupported Racket error policy: ~a"
            (racket-config-errors config))]))

;; visible-derived-token? : racket-derived-token? racket-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : racket-derived-token? racket-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (racket-derived-token-text derived-token))
   (racket-derived-token-start derived-token)
   (racket-derived-token-end derived-token)
   (racket-config-source-positions config)))

;; project-racket-derived-token : (or/c racket-derived-token? 'eof) racket-config? -> token-like?
;;   Convert a derived Racket token into a reusable-stream token-like value.
(define (project-racket-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(racket-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
