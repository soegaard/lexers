#lang racket/base

;;;
;;; JSON Projection
;;;
;;
;; Project derived JSON tokens into the reusable stream model.

;; project-json-derived-token : (or/c json-derived-token? 'eof) json-config? -> token-like?
;;   Convert a derived JSON token into a reusable-stream token-like value.

(provide project-json-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "json-derived.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; skip-trivia? : json-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (json-config-trivia config) 'skip))

;; derived->stream-category : json-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(json-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(json-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(json-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(json-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [(json-derived-token-has-tag? derived-token 'identifier)
     stream-category-identifier]
    [(json-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [else
     stream-category-unknown]))

;; raw-eof->token : json-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (json-config-source-positions config)))

;; malformed-token->result : json-derived-token? json-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (json-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (json-derived-token-text derived-token))
      (json-derived-token-start derived-token)
      (json-derived-token-end derived-token)
      (json-config-source-positions config))]
    [(raise)
     (define start-pos
       (json-derived-token-start derived-token))
     (define end-pos
       (json-derived-token-end derived-token))
     (raise-read-error "unknown JSON input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-json-derived-token
            "unsupported JSON error policy: ~a"
            (json-config-errors config))]))

;; visible-derived-token? : json-derived-token? json-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : json-derived-token? json-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (json-derived-token-text derived-token))
   (json-derived-token-start derived-token)
   (json-derived-token-end derived-token)
   (json-config-source-positions config)))

;; project-json-derived-token : (or/c json-derived-token? 'eof) json-config? -> token-like?
;;   Convert a derived JSON token into the reusable stream model.
(define (project-json-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(json-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
