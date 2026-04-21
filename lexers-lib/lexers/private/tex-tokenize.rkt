#lang racket/base

;;;
;;; TeX Token Reader
;;;
;;
;; Bridge the derived TeX tokenizer to the projected reusable token stream.

;; make-tex-token-reader : tex-config? [symbol?] -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public TeX and LaTeX APIs.

(provide make-tex-token-reader)

(require "config.rkt"
         "tex-derived.rkt"
         "tex-project.rkt")

;; make-tex-token-reader : tex-config? [symbol?] -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public TeX and LaTeX APIs.
(define (make-tex-token-reader config [mode 'tex])
  (define next-derived-token
    (make-tex-derived-reader mode))
  (lambda (in)
    (let loop ()
      (define result
        (project-tex-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
