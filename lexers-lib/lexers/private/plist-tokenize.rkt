#lang racket/base

;;;
;;; Plist Token Reader
;;;
;;
;; Bridge the derived plist tokenizer to the projected reusable token stream.

;; make-plist-token-reader : plist-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public plist API.

(provide make-plist-token-reader)

(require "config.rkt"
         "plist-derived.rkt"
         "plist-project.rkt")

;; make-plist-token-reader : plist-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public plist API.
(define (make-plist-token-reader config)
  (define next-derived-token
    (make-plist-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-plist-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
