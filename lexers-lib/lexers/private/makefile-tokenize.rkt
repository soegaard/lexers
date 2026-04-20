#lang racket/base

;;;
;;; Makefile Token Reader
;;;
;;
;; Bridge the derived Makefile tokenizer to the projected reusable token
;; stream.

;; make-makefile-token-reader : makefile-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Makefile API.

(provide make-makefile-token-reader)

(require "config.rkt"
         "makefile-derived.rkt"
         "makefile-project.rkt")

;; make-makefile-token-reader : makefile-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Makefile API.
(define (make-makefile-token-reader config)
  (define next-derived-token
    (make-makefile-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-makefile-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
