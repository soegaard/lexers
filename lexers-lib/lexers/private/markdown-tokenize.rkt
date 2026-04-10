#lang racket/base

;;;
;;; Markdown Tokenization
;;;
;;
;; Public Markdown token reading implemented as handwritten tokenization plus
;; projection.

;; make-markdown-token-reader : markdown-config? -> (input-port? -> token-like?)
;;   Construct a stateful Markdown token reader from derived tokenization plus
;;   projection.

(provide make-markdown-token-reader)

(require "config.rkt"
         "markdown-derived.rkt"
         "markdown-project.rkt")

;; make-markdown-token-reader : markdown-config? -> (input-port? -> token-like?)
;;   Construct a stateful Markdown token reader from derived tokenization plus
;;   projection.
(define (make-markdown-token-reader config)
  (define read-markdown-derived
    (make-markdown-derived-reader))
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-markdown-token-reader "input-port?" in))
    (let loop ()
      (define projected
        (project-markdown-derived-token
         (read-markdown-derived in)
         config))
      (cond
        [projected projected]
        [else      (loop)]))))
