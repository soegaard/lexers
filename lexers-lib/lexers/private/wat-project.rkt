#lang racket/base

;;;
;;; WAT Projection
;;;
;;
;; Project derived WAT tokens into the reusable stream model.

;; project-wat-derived-token : (or/c wat-derived-token? 'eof) wat-config? -> token-like?
;;   Convert a derived WAT token into a reusable-stream token-like value.

(provide project-wat-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt"
         "wat-derived.rkt")

;; skip-trivia? : wat-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (wat-config-trivia config) 'skip))

;; derived->stream-category : wat-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(wat-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(wat-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(wat-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(wat-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(wat-derived-token-has-tag? derived-token 'identifier)
     stream-category-identifier]
    [(wat-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(wat-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-unknown]))

;; raw-eof->token : wat-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (wat-config-source-positions config)))

;; malformed-token->result : wat-derived-token? wat-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (wat-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (wat-derived-token-text derived-token))
      (wat-derived-token-start derived-token)
      (wat-derived-token-end derived-token)
      (wat-config-source-positions config))]
    [(raise)
     (define start-pos (wat-derived-token-start derived-token))
     (define end-pos   (wat-derived-token-end derived-token))
     (raise-read-error "unknown WAT input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-wat-derived-token
            "unsupported WAT error policy: ~a"
            (wat-config-errors config))]))

;; visible-derived-token? : wat-derived-token? wat-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : wat-derived-token? wat-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (wat-derived-token-text derived-token))
   (wat-derived-token-start derived-token)
   (wat-derived-token-end derived-token)
   (wat-config-source-positions config)))

;; project-wat-derived-token : (or/c wat-derived-token? 'eof) wat-config? -> token-like?
;;   Convert a derived WAT token into the reusable stream model.
(define (project-wat-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(wat-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
