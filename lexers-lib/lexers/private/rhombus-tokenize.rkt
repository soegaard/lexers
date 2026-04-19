#lang racket/base

;;;
;;; Rhombus Tokenization
;;;
;;
;; Public Rhombus token reading implemented as adapter-backed tokenization plus
;; projection.

;; make-rhombus-token-reader : rhombus-config? -> (input-port? -> token-like?)
;;   Construct a stateful Rhombus token reader from derived tokenization plus
;;   projection.

(provide make-rhombus-token-reader)

(require "config.rkt"
         "rhombus-derived.rkt"
         "rhombus-project.rkt")

;; make-rhombus-token-reader : rhombus-config? -> (input-port? -> token-like?)
;;   Construct a stateful Rhombus token reader from derived tokenization plus
;;   projection.
(define (make-rhombus-token-reader config)
  (define read-rhombus-derived
    (make-rhombus-derived-reader))
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-rhombus-token-reader "input-port?" in))
    (let loop ()
      (define projected
        (project-rhombus-derived-token
         (read-rhombus-derived in)
         config))
      (cond
        [projected projected]
        [else      (loop)]))))
