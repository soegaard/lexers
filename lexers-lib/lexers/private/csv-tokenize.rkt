#lang racket/base

;;;
;;; CSV Token Reader
;;;
;;
;; Bridge the derived CSV tokenizer to the projected reusable token stream.

;; make-csv-token-reader : csv-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public CSV API.

(provide make-csv-token-reader)

(require "config.rkt"
         "csv-project.rkt"
         "delimited-derived.rkt")

;; make-csv-token-reader : csv-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public CSV API.
(define (make-csv-token-reader config)
  (define next-derived-token
    (make-delimited-derived-reader #:separator #\, #:dialect 'csv))
  (lambda (in)
    (let loop ()
      (define result
        (project-csv-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
