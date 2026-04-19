#lang racket/base

;;;
;;; Python Token Reader
;;;
;;
;; Bridge the derived Python tokenizer to the projected reusable token stream.

;; make-python-token-reader : python-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Python API.

(provide make-python-token-reader)

(require "config.rkt"
         "python-derived.rkt"
         "python-project.rkt")

;; make-python-token-reader : python-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Python API.
(define (make-python-token-reader config)
  (define next-derived-token
    (make-python-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-python-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
