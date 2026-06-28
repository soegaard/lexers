#lang racket/base

;;;
;;; SQL Token Reader
;;;
;;
;; Bridge the derived SQL tokenizer to the projected reusable token stream.

;; make-sql-token-reader : sql-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public SQL API.

(provide make-sql-token-reader)

(require "config.rkt"
         "sql-derived.rkt"
         "sql-project.rkt")

;; make-sql-token-reader : sql-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public SQL API.
(define (make-sql-token-reader config)
  (define next-derived-token
    (make-sql-derived-reader (sql-config-dialect config)))
  (lambda (in)
    (let loop ()
      (define result
        (project-sql-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
