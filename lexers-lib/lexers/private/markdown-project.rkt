#lang racket/base

;;;
;;; Markdown Projection
;;;
;;
;; Project derived Markdown tokens into the reusable stream model.

;; project-markdown-derived-token : (or/c markdown-derived-token? 'eof) markdown-config? -> token-like?
;;   Convert a derived Markdown token into a reusable-stream token-like value.

(provide project-markdown-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "markdown-derived.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; skip-trivia? : markdown-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (markdown-config-trivia config) 'skip))

;; derived->stream-category : markdown-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(markdown-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(markdown-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(markdown-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(markdown-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [(markdown-derived-token-has-tag? derived-token 'identifier)
     stream-category-identifier]
    [(markdown-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [else
     stream-category-unknown]))

;; raw-eof->token : markdown-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (markdown-config-source-positions config)))

;; malformed-token->result : markdown-derived-token? markdown-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (markdown-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (markdown-derived-token-text derived-token))
      (markdown-derived-token-start derived-token)
      (markdown-derived-token-end derived-token)
      (markdown-config-source-positions config))]
    [(raise)
     (define start-pos (markdown-derived-token-start derived-token))
     (define end-pos   (markdown-derived-token-end derived-token))
     (raise-read-error "unknown Markdown input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-markdown-derived-token
            "unsupported Markdown error policy: ~a"
            (markdown-config-errors config))]))

;; visible-derived-token? : markdown-derived-token? markdown-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : markdown-derived-token? markdown-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (markdown-derived-token-text derived-token))
   (markdown-derived-token-start derived-token)
   (markdown-derived-token-end derived-token)
   (markdown-config-source-positions config)))

;; project-markdown-derived-token : (or/c markdown-derived-token? 'eof) markdown-config? -> token-like?
;;   Convert a derived Markdown token into a reusable stream token-like value.
(define (project-markdown-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(markdown-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
