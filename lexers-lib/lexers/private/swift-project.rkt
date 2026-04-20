#lang racket/base

;;;
;;; Swift Projection
;;;
;;
;; Project derived Swift tokens into the reusable stream model.

;; project-swift-derived-token : (or/c swift-derived-token? 'eof) swift-config? -> token-like?
;;   Convert a derived Swift token into a reusable-stream token-like value.

(provide project-swift-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt"
         "swift-derived.rkt")

;; skip-trivia? : swift-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (swift-config-trivia config) 'skip))

;; derived->stream-category : swift-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(swift-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(swift-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(swift-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(swift-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(swift-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(swift-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(swift-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : swift-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (swift-config-source-positions config)))

;; malformed-token->result : swift-derived-token? swift-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (swift-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (swift-derived-token-text derived-token))
      (swift-derived-token-start derived-token)
      (swift-derived-token-end derived-token)
      (swift-config-source-positions config))]
    [(raise)
     (define start-pos
       (swift-derived-token-start derived-token))
     (define end-pos
       (swift-derived-token-end derived-token))
     (raise-read-error "unknown Swift input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-swift-derived-token
            "unsupported Swift error policy: ~a"
            (swift-config-errors config))]))

;; visible-derived-token? : swift-derived-token? swift-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : swift-derived-token? swift-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (swift-derived-token-text derived-token))
   (swift-derived-token-start derived-token)
   (swift-derived-token-end derived-token)
   (swift-config-source-positions config)))

;; project-swift-derived-token : (or/c swift-derived-token? 'eof) swift-config? -> token-like?
;;   Convert a derived Swift token into a reusable stream token-like value.
(define (project-swift-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(swift-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
