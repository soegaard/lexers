#lang racket/base

;;;
;;; Ruby Projection
;;;
;;
;; Project derived Ruby tokens into the reusable stream model.

;; project-ruby-derived-token : (or/c ruby-derived-token? 'eof) ruby-config? -> token-like?
;;   Convert a derived Ruby token into the reusable-stream model.

(provide project-ruby-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "ruby-derived.rkt"
         "stream.rkt")

;; skip-trivia? : ruby-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (ruby-config-trivia config) 'skip))

;; derived->stream-category : ruby-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(ruby-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(ruby-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(ruby-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(ruby-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(ruby-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(ruby-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(ruby-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : ruby-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (ruby-config-source-positions config)))

;; malformed-token->result : ruby-derived-token? ruby-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (ruby-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (ruby-derived-token-text derived-token))
      (ruby-derived-token-start derived-token)
      (ruby-derived-token-end derived-token)
      (ruby-config-source-positions config))]
    [(raise)
     (define start-pos
       (ruby-derived-token-start derived-token))
     (define end-pos
       (ruby-derived-token-end derived-token))
     (raise-read-error "unknown Ruby input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-ruby-derived-token
            "unsupported Ruby error policy: ~a"
            (ruby-config-errors config))]))

;; visible-derived-token? : ruby-derived-token? ruby-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : ruby-derived-token? ruby-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (ruby-derived-token-text derived-token))
   (ruby-derived-token-start derived-token)
   (ruby-derived-token-end derived-token)
   (ruby-config-source-positions config)))

;; project-ruby-derived-token : (or/c ruby-derived-token? 'eof) ruby-config? -> token-like?
;;   Convert a derived Ruby token into the reusable stream model.
(define (project-ruby-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(ruby-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
