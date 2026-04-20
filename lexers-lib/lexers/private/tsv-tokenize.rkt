#lang racket/base

;;;
;;; TSV Token Reader
;;;
;;
;; Bridge the derived TSV tokenizer to the projected reusable token stream.

;; make-tsv-token-reader : tsv-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public TSV API.

(provide make-tsv-token-reader)

(require "config.rkt"
         "delimited-derived.rkt"
         "tsv-project.rkt")

;; make-tsv-token-reader : tsv-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public TSV API.
(define (make-tsv-token-reader config)
  (define next-derived-token
    (make-delimited-derived-reader #:separator #\tab #:dialect 'tsv))
  (lambda (in)
    (let loop ()
      (define result
        (project-tsv-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
