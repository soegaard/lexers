#lang racket/base

;;;
;;; Shell Token Reader
;;;
;;
;; Bridge the derived shell tokenizer to the projected reusable token stream.

;; make-shell-token-reader : shell-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public shell API.

(provide make-shell-token-reader)

(require "config.rkt"
         "shell-derived.rkt"
         "shell-project.rkt")

;; make-shell-token-reader : shell-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public shell API.
(define (make-shell-token-reader config)
  (define next-derived-token
    (make-shell-derived-reader (shell-config-shell config)))
  (lambda (in)
    (let loop ()
      (define result
        (project-shell-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
