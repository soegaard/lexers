#lang racket/base

;;;
;;; C Projection
;;;
;;
;; Project derived C tokens into the reusable stream model.

;; project-c-derived-token : (or/c c-derived-token? 'eof) c-config? -> token-like?
;;   Convert a derived C token into a reusable-stream token-like value.

(provide project-c-derived-token)

(require parser-tools/lex
         syntax/readerr
         "c-derived.rkt"
         "config.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; skip-trivia? : c-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (c-config-trivia config) 'skip))

;; derived->stream-category : c-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(c-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(c-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(c-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(c-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(c-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(c-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(c-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : c-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (c-config-source-positions config)))

;; malformed-token->result : c-derived-token? c-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (c-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (c-derived-token-text derived-token))
      (c-derived-token-start derived-token)
      (c-derived-token-end derived-token)
      (c-config-source-positions config))]
    [(raise)
     (define start-pos
       (c-derived-token-start derived-token))
     (define end-pos
       (c-derived-token-end derived-token))
     (raise-read-error "unknown C input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-c-derived-token
            "unsupported C error policy: ~a"
            (c-config-errors config))]))

;; visible-derived-token? : c-derived-token? c-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : c-derived-token? c-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (c-derived-token-text derived-token))
   (c-derived-token-start derived-token)
   (c-derived-token-end derived-token)
   (c-config-source-positions config)))

;; project-c-derived-token : (or/c c-derived-token? 'eof) c-config? -> token-like?
;;   Convert a derived C token into the reusable stream model.
(define (project-c-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(c-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
