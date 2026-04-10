#lang racket/base

;;;
;;; WAT Token Reader
;;;
;;
;; Bridge the derived WAT tokenizer to the projected reusable token stream.

;; make-wat-token-reader : wat-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public WAT API.

(provide make-wat-token-reader)

(require "config.rkt"
         "wat-derived.rkt"
         "wat-project.rkt")

;; make-wat-token-reader : wat-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public WAT API.
(define (make-wat-token-reader config)
  (define next-derived-token
    (make-wat-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-wat-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
