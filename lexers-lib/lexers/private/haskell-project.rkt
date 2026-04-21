#lang racket/base

;;;
;;; Haskell Projection
;;;
;;
;; Project derived Haskell tokens into the reusable stream model.

;; project-haskell-derived-token : (or/c haskell-derived-token? 'eof) haskell-config? -> token-like?
;;   Convert a derived Haskell token into a reusable-stream token-like value.

(provide project-haskell-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "haskell-derived.rkt"
         "stream.rkt")

;; skip-trivia? : haskell-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (haskell-config-trivia config) 'skip))

;; derived->stream-category : haskell-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(haskell-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(haskell-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(haskell-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(haskell-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(haskell-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(haskell-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(haskell-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : haskell-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (haskell-config-source-positions config)))

;; malformed-token->result : haskell-derived-token? haskell-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (haskell-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (haskell-derived-token-text derived-token))
      (haskell-derived-token-start derived-token)
      (haskell-derived-token-end derived-token)
      (haskell-config-source-positions config))]
    [(raise)
     (define start-pos
       (haskell-derived-token-start derived-token))
     (define end-pos
       (haskell-derived-token-end derived-token))
     (raise-read-error "unknown Haskell input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-haskell-derived-token
            "unsupported Haskell error policy: ~a"
            (haskell-config-errors config))]))

;; visible-derived-token? : haskell-derived-token? haskell-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : haskell-derived-token? haskell-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (haskell-derived-token-text derived-token))
   (haskell-derived-token-start derived-token)
   (haskell-derived-token-end derived-token)
   (haskell-config-source-positions config)))

;; project-haskell-derived-token : (or/c haskell-derived-token? 'eof) haskell-config? -> token-like?
;;   Convert a derived Haskell token into a reusable stream token-like value.
(define (project-haskell-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(haskell-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
