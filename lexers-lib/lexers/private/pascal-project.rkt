#lang racket/base

;;;
;;; Pascal Projection
;;;
;;
;; Project derived Pascal tokens into the reusable stream model.

;; project-pascal-derived-token : (or/c pascal-derived-token? 'eof) pascal-config? -> token-like?
;;   Convert a derived Pascal token into a reusable-stream token-like value.

(provide project-pascal-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "pascal-derived.rkt"
         "stream.rkt")

;; skip-trivia? : pascal-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (pascal-config-trivia config) 'skip))

;; derived->stream-category : pascal-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(pascal-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(pascal-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(pascal-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(pascal-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(pascal-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(pascal-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(pascal-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : pascal-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (pascal-config-source-positions config)))

;; malformed-token->result : pascal-derived-token? pascal-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (pascal-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (pascal-derived-token-text derived-token))
      (pascal-derived-token-start derived-token)
      (pascal-derived-token-end derived-token)
      (pascal-config-source-positions config))]
    [(raise)
     (define start-pos
       (pascal-derived-token-start derived-token))
     (define end-pos
       (pascal-derived-token-end derived-token))
     (raise-read-error "unknown Pascal input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-pascal-derived-token
            "unsupported Pascal error policy: ~a"
            (pascal-config-errors config))]))

;; visible-derived-token? : pascal-derived-token? pascal-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : pascal-derived-token? pascal-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (pascal-derived-token-text derived-token))
   (pascal-derived-token-start derived-token)
   (pascal-derived-token-end derived-token)
   (pascal-config-source-positions config)))

;; project-pascal-derived-token : (or/c pascal-derived-token? 'eof) pascal-config? -> token-like?
;;   Convert a derived Pascal token into a reusable stream token-like value.
(define (project-pascal-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(pascal-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
