#lang racket/base

;;;
;;; Scribble Tokenization
;;;
;;
;; Public Scribble token reading implemented as adapter-backed tokenization plus
;; projection.

;; make-scribble-token-reader : scribble-config? -> (input-port? -> token-like?)
;;   Construct a stateful Scribble token reader from derived tokenization plus
;;   projection.

(provide make-scribble-token-reader)

(require "config.rkt"
         "scribble-derived.rkt"
         "scribble-project.rkt")

;; make-scribble-token-reader : scribble-config? -> (input-port? -> token-like?)
;;   Construct a stateful Scribble token reader from derived tokenization plus
;;   projection.
(define (make-scribble-token-reader config)
  (define read-scribble-derived
    (make-scribble-derived-reader))
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-scribble-token-reader "input-port?" in))
    (let loop ()
      (define projected
        (project-scribble-derived-token
         (read-scribble-derived in)
         config))
      (cond
        [projected projected]
        [else      (loop)]))))
