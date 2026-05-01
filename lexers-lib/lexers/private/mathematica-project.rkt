#lang racket/base

;;;
;;; Mathematica Projection
;;;
;;
;; Project derived Mathematica tokens into the reusable stream model.

;; project-mathematica-derived-token : (or/c mathematica-derived-token? 'eof) mathematica-config? -> token-like?
;;   Convert a derived Mathematica token into a reusable-stream token-like value.

(provide project-mathematica-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "mathematica-derived.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; skip-trivia? : mathematica-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (mathematica-config-trivia config) 'skip))

;; derived->stream-category : mathematica-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(mathematica-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(mathematica-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(mathematica-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(mathematica-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(mathematica-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(mathematica-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : mathematica-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (mathematica-config-source-positions config)))

;; malformed-token->result : mathematica-derived-token? mathematica-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (mathematica-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (mathematica-derived-token-text derived-token))
      (mathematica-derived-token-start derived-token)
      (mathematica-derived-token-end derived-token)
      (mathematica-config-source-positions config))]
    [(raise)
     (define start-pos
       (mathematica-derived-token-start derived-token))
     (define end-pos
       (mathematica-derived-token-end derived-token))
     (raise-read-error "unknown Mathematica input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-mathematica-derived-token
            "unsupported Mathematica error policy: ~a"
            (mathematica-config-errors config))]))

;; visible-derived-token? : mathematica-derived-token? mathematica-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : mathematica-derived-token? mathematica-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (mathematica-derived-token-text derived-token))
   (mathematica-derived-token-start derived-token)
   (mathematica-derived-token-end derived-token)
   (mathematica-config-source-positions config)))

;; project-mathematica-derived-token : (or/c mathematica-derived-token? 'eof) mathematica-config? -> token-like?
;;   Convert a derived Mathematica token into a reusable stream token-like value.
(define (project-mathematica-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(mathematica-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
