#lang racket/base

;;;
;;; Rust Projection
;;;
;;
;; Project derived Rust tokens into the reusable stream model.

;; project-rust-derived-token : (or/c rust-derived-token? 'eof) rust-config? -> token-like?
;;   Convert a derived Rust token into a reusable-stream token-like value.

(provide project-rust-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "rust-derived.rkt"
         "stream.rkt")

;; skip-trivia? : rust-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (rust-config-trivia config) 'skip))

;; derived->stream-category : rust-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(rust-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(rust-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(rust-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(rust-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(rust-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(rust-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(rust-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : rust-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (rust-config-source-positions config)))

;; malformed-token->result : rust-derived-token? rust-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (rust-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (rust-derived-token-text derived-token))
      (rust-derived-token-start derived-token)
      (rust-derived-token-end derived-token)
      (rust-config-source-positions config))]
    [(raise)
     (define start-pos
       (rust-derived-token-start derived-token))
     (define end-pos
       (rust-derived-token-end derived-token))
     (raise-read-error "unknown Rust input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-rust-derived-token
            "unsupported Rust error policy: ~a"
            (rust-config-errors config))]))

;; visible-derived-token? : rust-derived-token? rust-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : rust-derived-token? rust-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (rust-derived-token-text derived-token))
   (rust-derived-token-start derived-token)
   (rust-derived-token-end derived-token)
   (rust-config-source-positions config)))

;; project-rust-derived-token : (or/c rust-derived-token? 'eof) rust-config? -> token-like?
;;   Convert a derived Rust token into a reusable stream token-like value.
(define (project-rust-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(rust-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
