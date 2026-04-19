#lang racket/base

;;;
;;; JSON Token Reader
;;;
;;
;; Bridge the derived JSON tokenizer to the projected reusable token stream.

;; make-json-token-reader : json-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public JSON API.

(provide make-json-token-reader)

(require "config.rkt"
         "json-derived.rkt"
         "json-project.rkt")

;; make-json-token-reader : json-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public JSON API.
(define (make-json-token-reader config)
  (define next-derived-token
    (make-json-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-json-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
