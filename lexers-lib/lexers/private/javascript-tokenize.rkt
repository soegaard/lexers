#lang racket/base

;;;
;;; JavaScript Tokenization
;;;
;;
;; Public JavaScript token reading implemented as raw-tokenization plus projection.

;; make-javascript-token-reader : javascript-config? -> (input-port? -> token-like?)
;;   Construct a stateful JavaScript token reader from raw tokenization plus projection.

(provide make-javascript-token-reader)

(require "config.rkt"
         "javascript-derived.rkt"
         "javascript-project.rkt"
         "javascript-raw.rkt")

;; make-javascript-token-reader : javascript-config? -> (input-port? -> token-like?)
;;   Construct a stateful JavaScript token reader from raw tokenization plus projection.
(define (make-javascript-token-reader config)
  (define classify-javascript-token
    (make-javascript-derived-classifier))
  (define read-javascript-raw
    (make-javascript-raw-reader))
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-javascript-token-reader "input-port?" in))
    (let loop ()
      (define raw-token
        (read-javascript-raw in))
      (define projected
        (project-javascript-derived-token
         (cond
           [(eq? raw-token 'eof) 'eof]
           [else                 (classify-javascript-token raw-token)])
         config))
      (cond
      [projected projected]
      [else      (loop)]))))
