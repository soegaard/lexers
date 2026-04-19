#lang racket/base

;;;
;;; Python Projection
;;;
;;
;; Project derived Python tokens into the reusable stream model.

;; project-python-derived-token : (or/c python-derived-token? 'eof) python-config? -> token-like?
;;   Convert a derived Python token into a reusable-stream token-like value.

(provide project-python-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "parser-tools-compat.rkt"
         "python-derived.rkt"
         "stream.rkt")

;; skip-trivia? : python-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (python-config-trivia config) 'skip))

;; derived->stream-category : python-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(python-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(python-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(python-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(python-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(python-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(python-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(python-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : python-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (python-config-source-positions config)))

;; malformed-token->result : python-derived-token? python-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (python-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (python-derived-token-text derived-token))
      (python-derived-token-start derived-token)
      (python-derived-token-end derived-token)
      (python-config-source-positions config))]
    [(raise)
     (define start-pos
       (python-derived-token-start derived-token))
     (define end-pos
       (python-derived-token-end derived-token))
     (raise-read-error "unknown Python input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-python-derived-token
            "unsupported Python error policy: ~a"
            (python-config-errors config))]))

;; visible-derived-token? : python-derived-token? python-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : python-derived-token? python-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (python-derived-token-text derived-token))
   (python-derived-token-start derived-token)
   (python-derived-token-end derived-token)
   (python-config-source-positions config)))

;; project-python-derived-token : (or/c python-derived-token? 'eof) python-config? -> token-like?
;;   Convert a derived Python token into the reusable stream model.
(define (project-python-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(python-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
