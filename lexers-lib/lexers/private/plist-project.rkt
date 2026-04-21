#lang racket/base

;;;
;;; Plist Projection
;;;
;;
;; Project derived plist tokens into the reusable stream model.

;; project-plist-derived-token : (or/c plist-derived-token? 'eof) plist-config? -> token-like?
;;   Convert a derived plist token into a reusable-stream token-like value.

(provide project-plist-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "plist-derived.rkt"
         "stream.rkt")

;; skip-trivia? : plist-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (plist-config-trivia config) 'skip))

;; derived->stream-category : plist-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(plist-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(plist-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(plist-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(plist-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(plist-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(plist-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(plist-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : plist-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (plist-config-source-positions config)))

;; malformed-token->result : plist-derived-token? plist-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (plist-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (plist-derived-token-text derived-token))
      (plist-derived-token-start derived-token)
      (plist-derived-token-end derived-token)
      (plist-config-source-positions config))]
    [(raise)
     (define start-pos
       (plist-derived-token-start derived-token))
     (define end-pos
       (plist-derived-token-end derived-token))
     (raise-read-error "unknown plist input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-plist-derived-token
            "unsupported plist error policy: ~a"
            (plist-config-errors config))]))

;; visible-derived-token? : plist-derived-token? plist-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : plist-derived-token? plist-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (plist-derived-token-text derived-token))
   (plist-derived-token-start derived-token)
   (plist-derived-token-end derived-token)
   (plist-config-source-positions config)))

;; project-plist-derived-token : (or/c plist-derived-token? 'eof) plist-config? -> token-like?
;;   Convert a derived plist token into the reusable stream model.
(define (project-plist-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(plist-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
