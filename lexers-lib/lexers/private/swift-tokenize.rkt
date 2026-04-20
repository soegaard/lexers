#lang racket/base

;;;
;;; Swift Token Reader
;;;
;;
;; Bridge the derived Swift tokenizer to the projected reusable token stream.

;; make-swift-token-reader : swift-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Swift API.

(provide make-swift-token-reader)

(require "config.rkt"
         "swift-derived.rkt"
         "swift-project.rkt")

;; make-swift-token-reader : swift-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Swift API.
(define (make-swift-token-reader config)
  (define next-derived-token
    (make-swift-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-swift-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
