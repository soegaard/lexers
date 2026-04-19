#lang racket/base

;;;
;;; Shell Projection
;;;
;;
;; Project derived shell tokens into the reusable stream model.

;; project-shell-derived-token : (or/c shell-derived-token? 'eof) shell-config? -> token-like?
;;   Convert a derived shell token into a reusable-stream token-like value.

(provide project-shell-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "shell-derived.rkt"
         "stream.rkt")

;; skip-trivia? : shell-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (shell-config-trivia config) 'skip))

;; derived->stream-category : shell-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(shell-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(shell-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(shell-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(shell-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(shell-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(shell-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : shell-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (shell-config-source-positions config)))

;; malformed-token->result : shell-derived-token? shell-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (shell-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (shell-derived-token-text derived-token))
      (shell-derived-token-start derived-token)
      (shell-derived-token-end derived-token)
      (shell-config-source-positions config))]
    [(raise)
     (define start-pos
       (shell-derived-token-start derived-token))
     (define end-pos
       (shell-derived-token-end derived-token))
     (raise-read-error "unknown shell input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-shell-derived-token
            "unsupported shell error policy: ~a"
            (shell-config-errors config))]))

;; visible-derived-token? : shell-derived-token? shell-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : shell-derived-token? shell-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (shell-derived-token-text derived-token))
   (shell-derived-token-start derived-token)
   (shell-derived-token-end derived-token)
   (shell-config-source-positions config)))

;; project-shell-derived-token : (or/c shell-derived-token? 'eof) shell-config? -> token-like?
;;   Convert a derived shell token into a reusable-stream token-like value.
(define (project-shell-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(shell-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
