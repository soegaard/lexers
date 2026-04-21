#lang racket/base

;;;
;;; Haskell Token Reader
;;;
;;
;; Bridge the derived Haskell tokenizer to the projected reusable token stream.

;; make-haskell-token-reader : haskell-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Haskell API.

(provide make-haskell-token-reader)

(require "config.rkt"
         "haskell-derived.rkt"
         "haskell-project.rkt")

;; make-haskell-token-reader : haskell-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Haskell API.
(define (make-haskell-token-reader config)
  (define next-derived-token
    (make-haskell-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-haskell-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
