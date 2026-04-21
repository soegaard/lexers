#lang racket/base

;;;
;;; TeX Projection
;;;
;;
;; Project derived TeX tokens into the reusable stream model.

;; project-tex-derived-token : (or/c tex-derived-token? 'eof) tex-config? -> token-like?
;;   Convert a derived TeX token into a reusable-stream token-like value.

(provide project-tex-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt"
         "tex-derived.rkt")

;; skip-trivia? : tex-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (tex-config-trivia config) 'skip))

;; derived->stream-category : tex-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(tex-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(tex-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(tex-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(tex-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(tex-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(tex-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : tex-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (tex-config-source-positions config)))

;; malformed-token->result : tex-derived-token? tex-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (tex-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (tex-derived-token-text derived-token))
      (tex-derived-token-start derived-token)
      (tex-derived-token-end derived-token)
      (tex-config-source-positions config))]
    [(raise)
     (define start-pos
       (tex-derived-token-start derived-token))
     (define end-pos
       (tex-derived-token-end derived-token))
     (raise-read-error "unknown TeX input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-tex-derived-token
            "unsupported TeX error policy: ~a"
            (tex-config-errors config))]))

;; visible-derived-token? : tex-derived-token? tex-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : tex-derived-token? tex-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (tex-derived-token-text derived-token))
   (tex-derived-token-start derived-token)
   (tex-derived-token-end derived-token)
   (tex-config-source-positions config)))

;; project-tex-derived-token : (or/c tex-derived-token? 'eof) tex-config? -> token-like?
;;   Convert a derived TeX token into a reusable stream token-like value.
(define (project-tex-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(tex-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
