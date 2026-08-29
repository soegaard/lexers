#lang racket/base

;;;
;;; TOML Token Reader
;;;

;; Bridge derived TOML tokens to the projected reusable token stream.

;; make-toml-token-reader : toml-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public TOML API.

(provide make-toml-token-reader)

(require "toml-derived.rkt"
         "toml-project.rkt")

;; make-toml-token-reader : toml-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public TOML API.
(define (make-toml-token-reader config)
  (define next-derived-token (make-toml-derived-reader))
  (lambda (in)
    (let loop ()
      (define token (project-toml-derived-token (next-derived-token in) config))
      (if token token (loop)))))
