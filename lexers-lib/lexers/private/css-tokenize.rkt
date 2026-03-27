#lang racket/base

;;;
;;; CSS Tokenization
;;;
;;
;; Public CSS token reading implemented as raw-tokenization plus projection.

;; make-css-token-reader : css-config? -> (input-port? -> token-like?)
;;   Construct a stateful CSS token reader from raw tokenization plus projection.

(provide make-css-token-reader)

(require "config.rkt"
         "css-derived.rkt"
         "css-project.rkt"
         "css-raw.rkt")

;; make-css-token-reader : css-config? -> (input-port? -> token-like?)
;;   Construct a stateful CSS token reader from raw tokenization plus projection.
(define (make-css-token-reader config)
  (define classify-css-token
    (make-css-derived-classifier))
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-css-token-reader "input-port?" in))
    (let loop ()
      (define raw-token
        (read-css-raw-token in))
      (define projected
        (project-css-derived-token
         (cond
           [(eq? raw-token 'eof) 'eof]
           [else                 (classify-css-token raw-token)])
         config))
      (cond
        [projected projected]
        [else      (loop)]))))
