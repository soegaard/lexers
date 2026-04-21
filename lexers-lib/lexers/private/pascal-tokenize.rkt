#lang racket/base

;;;
;;; Pascal Token Reader
;;;
;;
;; Bridge the derived Pascal tokenizer to the projected reusable token stream.

;; make-pascal-token-reader : pascal-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Pascal API.

(provide make-pascal-token-reader)

(require "config.rkt"
         "pascal-derived.rkt"
         "pascal-project.rkt")

;; make-pascal-token-reader : pascal-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Pascal API.
(define (make-pascal-token-reader config)
  (define next-derived-token
    (make-pascal-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-pascal-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
