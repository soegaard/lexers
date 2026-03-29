#lang racket/base

;;;
;;; JavaScript Projection
;;;
;;
;; Project raw JavaScript tokens into the reusable stream model.

;; project-javascript-derived-token : (or/c javascript-derived-token? 'eof) javascript-config? -> token-like?
;;   Convert a derived JavaScript token into a reusable-stream token-like value.

(provide project-javascript-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "javascript-derived.rkt"
         "javascript-raw.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; skip-trivia? : javascript-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (javascript-config-trivia config) 'skip))

;; raw->default-stream-category : symbol? -> symbol?
;;   Project a raw JavaScript token kind to its default reusable-stream category.
(define (raw->default-stream-category raw-kind)
  (case raw-kind
    [(whitespace-token)                   stream-category-whitespace]
    [(line-comment-token block-comment-token) stream-category-comment]
    [(identifier-token
      jsx-tag-name-token
      jsx-closing-tag-name-token
      jsx-attribute-name-token)
     stream-category-identifier]
    [(string-token
      bad-string-token
      number-token
      regex-token
      template-chunk-token
      jsx-text-token)
     stream-category-literal]
    [(delimiter-token
      template-start-token
      template-end-token
      template-interpolation-start-token
      template-interpolation-end-token
      jsx-fragment-boundary-token
      jsx-interpolation-start-token
      jsx-interpolation-end-token)
     stream-category-delimiter]
    [(operator-token)                     stream-category-operator]
    [(unknown-raw-token)                  stream-category-unknown]
    [else                                 stream-category-unknown]))

;; derived->stream-category : javascript-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags when available.
(define (derived->stream-category derived-token)
  (cond
    [(javascript-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(javascript-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(javascript-derived-token-has-tag? derived-token 'identifier)
     stream-category-identifier]
    [(or (javascript-derived-token-has-tag? derived-token 'string-literal)
         (javascript-derived-token-has-tag? derived-token 'numeric-literal)
         (javascript-derived-token-has-tag? derived-token 'regex-literal)
         (javascript-derived-token-has-tag? derived-token 'template-chunk)
         (javascript-derived-token-has-tag? derived-token 'jsx-text))
     stream-category-literal]
    [(javascript-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [else
     (raw->default-stream-category
      (javascript-raw-token-kind (javascript-derived-token-raw derived-token)))]))

;; raw-eof->token : javascript-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (javascript-config-source-positions config)))

;; malformed-token->result : javascript-derived-token? javascript-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (define raw-token
    (javascript-derived-token-raw derived-token))
  (case (javascript-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (javascript-raw-token-text raw-token))
      (javascript-raw-token-start raw-token)
      (javascript-raw-token-end raw-token)
      (javascript-config-source-positions config))]
    [(raise)
     (define start-pos (javascript-raw-token-start raw-token))
     (define end-pos   (javascript-raw-token-end raw-token))
     (raise-read-error "unknown JavaScript input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos) (position-offset start-pos)))]
    [else
     (error 'project-javascript-raw-token
            "unsupported JavaScript error policy: ~a"
            (javascript-config-errors config))]))

;; visible-derived-token? : javascript-derived-token? javascript-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (javascript-raw-token-kind (javascript-derived-token-raw derived-token))
    [(whitespace-token line-comment-token block-comment-token)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : javascript-derived-token? javascript-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (define raw-token
    (javascript-derived-token-raw derived-token))
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (javascript-raw-token-text raw-token))
   (javascript-raw-token-start raw-token)
   (javascript-raw-token-end raw-token)
   (javascript-config-source-positions config)))

;; project-javascript-derived-token : (or/c javascript-derived-token? 'eof) javascript-config? -> token-like?
;;   Convert a derived JavaScript token into a reusable-stream token-like value.
(define (project-javascript-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(javascript-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
