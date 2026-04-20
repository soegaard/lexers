#lang racket/base

;;;
;;; Makefile Projection
;;;
;;
;; Project derived Makefile tokens into the reusable stream model.

;; project-makefile-derived-token : (or/c makefile-derived-token? 'eof) makefile-config? -> token-like?
;;   Convert a derived Makefile token into a reusable-stream token-like value.

(provide project-makefile-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "makefile-derived.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; skip-trivia? : makefile-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (makefile-config-trivia config) 'skip))

;; derived->stream-category : makefile-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(makefile-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(makefile-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(makefile-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(makefile-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(makefile-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(makefile-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(makefile-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : makefile-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (makefile-config-source-positions config)))

;; malformed-token->result : makefile-derived-token? makefile-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (makefile-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (makefile-derived-token-text derived-token))
      (makefile-derived-token-start derived-token)
      (makefile-derived-token-end derived-token)
      (makefile-config-source-positions config))]
    [(raise)
     (define start-pos
       (makefile-derived-token-start derived-token))
     (define end-pos
       (makefile-derived-token-end derived-token))
     (raise-read-error "unknown Makefile input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-makefile-derived-token
            "unsupported Makefile error policy: ~a"
            (makefile-config-errors config))]))

;; visible-derived-token? : makefile-derived-token? makefile-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : makefile-derived-token? makefile-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (makefile-derived-token-text derived-token))
   (makefile-derived-token-start derived-token)
   (makefile-derived-token-end derived-token)
   (makefile-config-source-positions config)))

;; project-makefile-derived-token : (or/c makefile-derived-token? 'eof) makefile-config? -> token-like?
;;   Convert a derived Makefile token into the reusable stream model.
(define (project-makefile-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(makefile-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
