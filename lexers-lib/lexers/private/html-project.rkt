#lang racket/base

;;;
;;; HTML Projection
;;;
;;
;; Project derived HTML tokens into the reusable stream model.

;; project-html-derived-token : (or/c html-derived-token? 'eof) html-config? -> token-like?
;;   Convert a derived HTML token into a reusable-stream token-like value.

(provide project-html-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "html-derived.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; skip-trivia? : html-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (html-config-trivia config) 'skip))

;; derived->stream-category : html-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(html-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(html-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(html-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(or (html-derived-token-has-tag? derived-token 'keyword)
         (html-derived-token-has-tag? derived-token 'html-doctype))
     stream-category-keyword]
    [(or (html-derived-token-has-tag? derived-token 'operator)
         (html-derived-token-has-tag? derived-token 'delimiter))
     (if (html-derived-token-has-tag? derived-token 'operator)
         stream-category-operator
         stream-category-delimiter)]
    [(or (html-derived-token-has-tag? derived-token 'identifier)
         (html-derived-token-has-tag? derived-token 'html-tag-name)
         (html-derived-token-has-tag? derived-token 'html-closing-tag-name)
         (html-derived-token-has-tag? derived-token 'html-attribute-name))
     stream-category-identifier]
    [(or (html-derived-token-has-tag? derived-token 'literal)
         (html-derived-token-has-tag? derived-token 'html-attribute-value)
         (html-derived-token-has-tag? derived-token 'html-text)
         (html-derived-token-has-tag? derived-token 'html-entity))
     stream-category-literal]
    [else
     stream-category-unknown]))

;; raw-eof->token : html-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (html-config-source-positions config)))

;; malformed-token->result : html-derived-token? html-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (html-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (html-derived-token-text derived-token))
      (html-derived-token-start derived-token)
      (html-derived-token-end derived-token)
      (html-config-source-positions config))]
    [(raise)
     (define start-pos (html-derived-token-start derived-token))
     (define end-pos   (html-derived-token-end derived-token))
     (raise-read-error "unknown HTML input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos) (position-offset start-pos)))]
    [else
     (error 'project-html-derived-token
            "unsupported HTML error policy: ~a"
            (html-config-errors config))]))

;; visible-derived-token? : html-derived-token? html-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (cond
    [(string=? (html-derived-token-text derived-token) "")
     #f]
    [(and (eq? (derived->stream-category derived-token) stream-category-whitespace)
          (skip-trivia? config))
     #f]
    [(and (eq? (derived->stream-category derived-token) stream-category-comment)
          (skip-trivia? config))
     #f]
    [(and (eq? (derived->stream-category derived-token) stream-category-literal)
          (html-derived-token-has-tag? derived-token 'html-text)
          (regexp-match? #px"^\\s+$" (html-derived-token-text derived-token))
          (skip-trivia? config))
     #f]
    [else
     #t]))

;; plain-derived-token->result : html-derived-token? html-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (html-derived-token-text derived-token))
   (html-derived-token-start derived-token)
   (html-derived-token-end derived-token)
   (html-config-source-positions config)))

;; project-html-derived-token : (or/c html-derived-token? 'eof) html-config? -> token-like?
;;   Convert a derived HTML token into a reusable-stream token-like value.
(define (project-html-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(html-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
