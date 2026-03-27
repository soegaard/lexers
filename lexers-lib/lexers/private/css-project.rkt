#lang racket/base

;;;
;;; CSS Projection
;;;
;;
;; Project raw CSS tokens into the reusable stream model.

;; project-css-derived-token : (or/c css-derived-token? 'eof) css-config? -> token-like?
;;   Convert a derived CSS token into a reusable-stream token-like value.

(provide project-css-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "css-derived.rkt"
         "css-raw.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; skip-trivia? : css-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (css-config-trivia config) 'skip))

;; raw->default-stream-category : symbol? -> symbol?
;;   Project a raw CSS token kind to its default reusable-stream category.
(define (raw->default-stream-category raw-kind)
  (case raw-kind
    [(whitespace-token)  stream-category-whitespace]
    [(comment-token)     stream-category-comment]
    [(ident-token)       stream-category-identifier]
    [(at-keyword-token)  stream-category-keyword]
    [(string-token
      bad-string-token
      function-token
      hash-token
      url-token
      bad-url-token
      unicode-range-token
      number-token
      percentage-token
      dimension-token)   stream-category-literal]
    [(open-brace-token
      close-brace-token
      open-paren-token
      close-paren-token
      open-bracket-token
      close-bracket-token
      CDO-token
      CDC-token
      include-match-token
      dash-match-token
      prefix-match-token
      suffix-match-token
      substring-match-token
      colon-token
      semicolon-token
      comma-token
      delim-token)       stream-category-delimiter]
    [(unknown-raw-token) stream-category-unknown]
    [else                stream-category-unknown]))

;; derived->stream-category : css-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags when available.
(define (derived->stream-category derived-token)
  (cond
    [(css-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(or (css-derived-token-has-tag? derived-token 'color-literal)
         (css-derived-token-has-tag? derived-token 'color-function)
         (css-derived-token-has-tag? derived-token 'gradient-function)
         (css-derived-token-has-tag? derived-token 'length-dimension))
     stream-category-literal]
    [(or (css-derived-token-has-tag? derived-token 'custom-property-name)
         (css-derived-token-has-tag? derived-token 'property-name-candidate))
     stream-category-identifier]
    [else
     (raw->default-stream-category
      (css-raw-token-kind (css-derived-token-raw derived-token)))]))

;; raw-eof->token : css-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (css-config-source-positions config)))

;; malformed-token->result : css-derived-token? css-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (define raw-token (css-derived-token-raw derived-token))
  (case (css-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (css-raw-token-text raw-token))
      (css-raw-token-start raw-token)
      (css-raw-token-end raw-token)
      (css-config-source-positions config))]
    [(raise)
     (define start-pos (css-raw-token-start raw-token))
     (define end-pos   (css-raw-token-end raw-token))
     (raise-read-error "unknown CSS input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos) (position-offset start-pos)))]
    [else
     (error 'project-css-raw-token
            "unsupported CSS error policy: ~a"
            (css-config-errors config))]))

;; visible-derived-token? : css-derived-token? css-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (css-raw-token-kind (css-derived-token-raw derived-token))
    [(whitespace-token comment-token) (not (skip-trivia? config))]
    [else                             #t]))

;; plain-derived-token->result : css-derived-token? css-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (define raw-token (css-derived-token-raw derived-token))
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (css-raw-token-text raw-token))
   (css-raw-token-start raw-token)
   (css-raw-token-end raw-token)
   (css-config-source-positions config)))

;; project-css-derived-token : (or/c css-derived-token? 'eof) css-config? -> token-like?
;;   Convert a derived CSS token into a reusable-stream token-like value.
(define (project-css-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(css-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
