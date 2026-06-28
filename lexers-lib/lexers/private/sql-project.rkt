#lang racket/base

;;;
;;; SQL Projection
;;;
;;
;; Project derived SQL tokens into the reusable stream model.

;; project-sql-derived-token : (or/c sql-derived-token? 'eof) sql-config? -> token-like?
;;   Convert a derived SQL token into a reusable-stream token-like value.

(provide project-sql-derived-token)

(require parser-tools/lex
         syntax/readerr
         "config.rkt"
         "sql-derived.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; skip-trivia? : sql-config? -> boolean?
;;   Determine whether trivia should be skipped.
(define (skip-trivia? config)
  (eq? (sql-config-trivia config) 'skip))

;; derived->stream-category : sql-derived-token? -> symbol?
;;   Choose a reusable-stream category using derived tags.
(define (derived->stream-category derived-token)
  (cond
    [(sql-derived-token-has-tag? derived-token 'malformed-token)
     stream-category-unknown]
    [(sql-derived-token-has-tag? derived-token 'comment)
     stream-category-comment]
    [(sql-derived-token-has-tag? derived-token 'whitespace)
     stream-category-whitespace]
    [(sql-derived-token-has-tag? derived-token 'keyword)
     stream-category-keyword]
    [(sql-derived-token-has-tag? derived-token 'literal)
     stream-category-literal]
    [(sql-derived-token-has-tag? derived-token 'operator)
     stream-category-operator]
    [(sql-derived-token-has-tag? derived-token 'delimiter)
     stream-category-delimiter]
    [else
     stream-category-identifier]))

;; raw-eof->token : sql-config? -> token-like?
;;   Convert raw eof to the public token-like result.
(define (raw-eof->token config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (sql-config-source-positions config)))

;; malformed-token->result : sql-derived-token? sql-config? -> token-like?
;;   Project malformed input or raise in strict mode.
(define (malformed-token->result derived-token config)
  (case (sql-config-errors config)
    [(emit-unknown)
     (wrap-token-with-pos
      (make-stream-token stream-category-unknown
                         (sql-derived-token-text derived-token))
      (sql-derived-token-start derived-token)
      (sql-derived-token-end derived-token)
      (sql-config-source-positions config))]
    [(raise)
     (define start-pos
       (sql-derived-token-start derived-token))
     (define end-pos
       (sql-derived-token-end derived-token))
     (raise-read-error "unknown SQL input"
                       #f
                       (position-line start-pos)
                       (position-col start-pos)
                       (position-offset start-pos)
                       (- (position-offset end-pos)
                          (position-offset start-pos)))]
    [else
     (error 'project-sql-derived-token
            "unsupported SQL error policy: ~a"
            (sql-config-errors config))]))

;; visible-derived-token? : sql-derived-token? sql-config? -> boolean?
;;   Determine whether a derived token should be emitted in the current profile.
(define (visible-derived-token? derived-token config)
  (case (derived->stream-category derived-token)
    [(whitespace comment)
     (not (skip-trivia? config))]
    [else
     #t]))

;; plain-derived-token->result : sql-derived-token? sql-config? -> token-like?
;;   Project a non-error derived token to the reusable stream model.
(define (plain-derived-token->result derived-token config)
  (wrap-token-with-pos
   (make-stream-token (derived->stream-category derived-token)
                      (sql-derived-token-text derived-token))
   (sql-derived-token-start derived-token)
   (sql-derived-token-end derived-token)
   (sql-config-source-positions config)))

;; project-sql-derived-token : (or/c sql-derived-token? 'eof) sql-config? -> token-like?
;;   Convert a derived SQL token into a reusable stream token-like value.
(define (project-sql-derived-token derived-token config)
  (cond
    [(eq? derived-token 'eof)
     (raw-eof->token config)]
    [(sql-derived-token-has-tag? derived-token 'malformed-token)
     (malformed-token->result derived-token config)]
    [(visible-derived-token? derived-token config)
     (plain-derived-token->result derived-token config)]
    [else
     #f]))
