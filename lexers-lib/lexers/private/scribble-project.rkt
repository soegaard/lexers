#lang racket/base

;;;
;;; Scribble Projection
;;;
;;
;; Project derived Scribble tokens into the reusable stream model.

;; project-scribble-derived-token : (or/c scribble-derived-token? 'eof) scribble-config? -> token-like?
;;   Convert a derived Scribble token into a reusable-stream token-like value.

(provide project-scribble-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "scribble-derived.rkt"
         "stream.rkt")

;; skip-trivia? : scribble-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (scribble-config-trivia config) 'skip))

;; derived->stream-category : scribble-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(scribble-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(scribble-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(scribble-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(scribble-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(scribble-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [(scribble-derived-token-has-tag? derived-token 'identifier)
     stream-category-identifier]
    [else
     stream-category-unknown]))

;; raw-eof->token : scribble-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (scribble-config-source-positions config)))

;; malformed-token->result : scribble-derived-token? scribble-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (scribble-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (scribble-derived-token-text derived-token))
      (scribble-derived-token-start derived-token)
      (scribble-derived-token-end derived-token)
      (scribble-config-source-positions config))]
    [(raise)
     (define start-pos (scribble-derived-token-start derived-token))
     (define end-pos   (scribble-derived-token-end derived-token))
     (raise-read-error "unknown Scribble input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-scribble-derived-token
            "unsupported Scribble error policy: ~a"
            (scribble-config-errors config))]))

;; visible-derived-token? : scribble-derived-token? scribble-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : scribble-derived-token? scribble-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (scribble-derived-token-text derived-token))
   (scribble-derived-token-start derived-token)
   (scribble-derived-token-end derived-token)
   (scribble-config-source-positions config)))

;; project-scribble-derived-token : (or/c scribble-derived-token? 'eof) scribble-config? -> token-like?
;;   Convert a derived Scribble token into a reusable stream token-like value.
(define (project-scribble-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(scribble-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
