#lang racket/base

;;;
;;; Ruby Token Reader
;;;
;;
;; Bridge the derived Ruby tokenizer to the projected reusable token stream.

;; make-ruby-token-reader : ruby-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Ruby API.

(provide make-ruby-token-reader)

(require "config.rkt"
         "ruby-derived.rkt"
         "ruby-project.rkt")

;; make-ruby-token-reader : ruby-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Ruby API.
(define (make-ruby-token-reader config)
  (define next-derived-token
    (make-ruby-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-ruby-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
