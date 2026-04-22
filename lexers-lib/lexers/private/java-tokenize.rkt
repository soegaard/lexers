#lang racket/base

;;;
;;; Java Token Reader
;;;
;;
;; Bridge the derived Java tokenizer to the projected reusable token stream.

;; make-java-token-reader : java-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Java API.

(provide make-java-token-reader)

(require "config.rkt"
         "java-derived.rkt"
         "java-project.rkt")

;; make-java-token-reader : java-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Java API.
(define (make-java-token-reader config)
  (define next-derived-token
    (make-java-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-java-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
