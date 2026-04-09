#lang racket/base

;;;
;;; Racket Tokenization
;;;
;;
;; Public Racket token reading implemented as adapter-backed tokenization plus
;; projection.

;; make-racket-token-reader : racket-config? -> (input-port? -> token-like?)
;;   Construct a stateful Racket token reader from derived tokenization plus
;;   projection.

(provide make-racket-token-reader)

(require "config.rkt"
         "racket-derived.rkt"
         "racket-project.rkt")

;; make-racket-token-reader : racket-config? -> (input-port? -> token-like?)
;;   Construct a stateful Racket token reader from derived tokenization plus
;;   projection.
(define (make-racket-token-reader config)
  (define read-racket-derived
    (make-racket-derived-reader))
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-racket-token-reader "input-port?" in))
    (let loop ()
      (define projected
        (project-racket-derived-token
         (read-racket-derived in)
         config))
      (cond
        [projected projected]
        [else      (loop)]))))
