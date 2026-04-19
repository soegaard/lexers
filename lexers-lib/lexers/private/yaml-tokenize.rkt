#lang racket/base

;;;
;;; YAML Token Reader
;;;
;;
;; Bridge the derived YAML tokenizer to the projected reusable token stream.

;; make-yaml-token-reader : yaml-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public YAML API.

(provide make-yaml-token-reader)

(require "config.rkt"
         "yaml-derived.rkt"
         "yaml-project.rkt")

;; make-yaml-token-reader : yaml-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public YAML API.
(define (make-yaml-token-reader config)
  (define next-derived-token
    (make-yaml-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-yaml-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
