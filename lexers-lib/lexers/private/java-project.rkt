#lang racket/base

;;;
;;; Java Projection
;;;
;;
;; Project derived Java tokens into the reusable stream model.

;; project-java-derived-token : (or/c java-derived-token? 'eof) java-config? -> token-like?
;;   Convert a derived Java token into a reusable-stream token-like value.

(provide project-java-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "java-derived.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; skip-trivia? : java-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (java-config-trivia config) 'skip))

;; derived->stream-category : java-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(java-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(java-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(java-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(java-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(java-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(java-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(java-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : java-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (java-config-source-positions config)))

;; malformed-token->result : java-derived-token? java-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (java-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (java-derived-token-text derived-token))
      (java-derived-token-start derived-token)
      (java-derived-token-end derived-token)
      (java-config-source-positions config))]
    [(raise)
     (define start-pos
       (java-derived-token-start derived-token))
     (define end-pos
       (java-derived-token-end derived-token))
     (raise-read-error "unknown Java input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-java-derived-token
            "unsupported Java error policy: ~a"
            (java-config-errors config))]))

;; visible-derived-token? : java-derived-token? java-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : java-derived-token? java-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (java-derived-token-text derived-token))
   (java-derived-token-start derived-token)
   (java-derived-token-end derived-token)
   (java-config-source-positions config)))

;; project-java-derived-token : (or/c java-derived-token? 'eof) java-config? -> token-like?
;;   Convert a derived Java token into a reusable stream token-like value.
(define (project-java-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(java-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
