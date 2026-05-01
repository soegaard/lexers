#lang racket/base

;;;
;;; Mathematica Token Reader
;;;
;;
;; Bridge the derived Mathematica tokenizer to the projected reusable token
;; stream.

;; make-mathematica-token-reader : mathematica-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Mathematica API.

(provide make-mathematica-token-reader)

(require "config.rkt"
         "mathematica-derived.rkt"
         "mathematica-project.rkt")

;; make-mathematica-token-reader : mathematica-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Mathematica API.
(define (make-mathematica-token-reader config)
  (define next-derived-token
    (make-mathematica-derived-reader))
  (lambda (in)
    (let loop ()
      (define derived-token
        (next-derived-token in))
      (define projected
        (project-mathematica-derived-token derived-token config))
      (cond
        [projected projected]
        [else      (loop)]))))
