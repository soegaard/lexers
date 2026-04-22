#lang racket/base

;;;
;;; Go Projection
;;;
;;
;; Project derived Go tokens into the reusable stream model.

;; project-go-derived-token : (or/c go-derived-token? 'eof) go-config? -> token-like?
;;   Convert a derived Go token into a reusable-stream token-like value.

(provide project-go-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "go-derived.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; skip-trivia? : go-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (go-config-trivia config) 'skip))

;; derived->stream-category : go-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(go-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(go-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(go-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(go-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(go-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(go-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(go-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : go-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (go-config-source-positions config)))

;; malformed-token->result : go-derived-token? go-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (go-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (go-derived-token-text derived-token))
      (go-derived-token-start derived-token)
      (go-derived-token-end derived-token)
      (go-config-source-positions config))]
    [(raise)
     (define start-pos
       (go-derived-token-start derived-token))
     (define end-pos
       (go-derived-token-end derived-token))
     (raise-read-error "unknown Go input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-go-derived-token
            "unsupported Go error policy: ~a"
            (go-config-errors config))]))

;; visible-derived-token? : go-derived-token? go-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : go-derived-token? go-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (go-derived-token-text derived-token))
   (go-derived-token-start derived-token)
   (go-derived-token-end derived-token)
   (go-config-source-positions config)))

;; project-go-derived-token : (or/c go-derived-token? 'eof) go-config? -> token-like?
;;   Convert a derived Go token into a reusable stream token-like value.
(define (project-go-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(go-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
