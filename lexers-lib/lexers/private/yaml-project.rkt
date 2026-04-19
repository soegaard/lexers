#lang racket/base

;;;
;;; YAML Projection
;;;
;;
;; Project derived YAML tokens into the reusable stream model.

;; project-yaml-derived-token : (or/c yaml-derived-token? 'eof) yaml-config? -> token-like?
;;   Convert a derived YAML token into a reusable-stream token-like value.

(provide project-yaml-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt"
         "yaml-derived.rkt")

;; skip-trivia? : yaml-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (yaml-config-trivia config) 'skip))

;; derived->stream-category : yaml-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(yaml-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(yaml-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(yaml-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(yaml-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(yaml-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(yaml-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(yaml-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : yaml-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (yaml-config-source-positions config)))

;; malformed-token->result : yaml-derived-token? yaml-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (yaml-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (yaml-derived-token-text derived-token))
      (yaml-derived-token-start derived-token)
      (yaml-derived-token-end derived-token)
      (yaml-config-source-positions config))]
    [(raise)
     (define start-pos
       (yaml-derived-token-start derived-token))
     (define end-pos
       (yaml-derived-token-end derived-token))
     (raise-read-error "unknown YAML input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-yaml-derived-token
            "unsupported YAML error policy: ~a"
            (yaml-config-errors config))]))

;; visible-derived-token? : yaml-derived-token? yaml-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : yaml-derived-token? yaml-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (yaml-derived-token-text derived-token))
   (yaml-derived-token-start derived-token)
   (yaml-derived-token-end derived-token)
   (yaml-config-source-positions config)))

;; project-yaml-derived-token : (or/c yaml-derived-token? 'eof) yaml-config? -> token-like?
;;   Convert a derived YAML token into the reusable stream model.
(define (project-yaml-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(yaml-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
