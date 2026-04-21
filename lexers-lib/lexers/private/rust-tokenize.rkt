#lang racket/base

;;;
;;; Rust Token Reader
;;;
;;
;; Bridge the derived Rust tokenizer to the projected reusable token stream.

;; make-rust-token-reader : rust-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Rust API.

(provide make-rust-token-reader)

(require "config.rkt"
         "rust-derived.rkt"
         "rust-project.rkt")

;; make-rust-token-reader : rust-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Rust API.
(define (make-rust-token-reader config)
  (define next-derived-token
    (make-rust-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-rust-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
