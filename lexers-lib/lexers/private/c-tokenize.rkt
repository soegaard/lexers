#lang racket/base

;;;
;;; C Token Reader
;;;
;;
;; Bridge the derived C tokenizer to the projected reusable token stream.

;; make-c-token-reader : c-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public C API.

(provide make-c-token-reader)

(require "c-derived.rkt"
         "c-project.rkt"
         "config.rkt")

;; make-c-token-reader : c-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public C API.
(define (make-c-token-reader config)
  (define next-derived-token
    (make-c-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-c-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
