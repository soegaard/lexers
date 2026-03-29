#lang racket/base

;;;
;;; HTML Tokenization
;;;
;;
;; Public HTML token reading implemented as derived tokenization plus projection.

;; make-html-token-reader : html-config? -> (input-port? -> token-like?)
;;   Construct a stateful HTML token reader from derived tokenization plus projection.

(provide make-html-token-reader)

(require "config.rkt"
         "html-derived.rkt"
         "html-project.rkt")

;; make-html-token-reader : html-config? -> (input-port? -> token-like?)
;;   Construct a stateful HTML token reader from derived tokenization plus projection.
(define (make-html-token-reader config)
  (define read-html-derived
    (make-html-derived-reader))
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-html-token-reader "input-port?" in))
    (let loop ()
      (define projected
        (project-html-derived-token
         (read-html-derived in)
         config))
      (cond
        [projected projected]
        [else      (loop)]))))
