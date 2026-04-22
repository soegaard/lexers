#lang racket/base

;;;
;;; Go Token Reader
;;;
;;
;; Bridge the derived Go tokenizer to the projected reusable token stream.

;; make-go-token-reader : go-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Go API.

(provide make-go-token-reader)

(require "config.rkt"
         "go-derived.rkt"
         "go-project.rkt")

;; make-go-token-reader : go-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Go API.
(define (make-go-token-reader config)
  (define next-derived-token
    (make-go-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-go-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
