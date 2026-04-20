#lang racket/base

;;;
;;; Objective-C Token Reader
;;;
;;
;; Bridge the derived Objective-C tokenizer to the projected reusable token
;; stream.

;; make-objc-token-reader : objc-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Objective-C API.

(provide make-objc-token-reader)

(require "config.rkt"
         "objc-derived.rkt"
         "objc-project.rkt")

;; make-objc-token-reader : objc-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Objective-C API.
(define (make-objc-token-reader config)
  (define next-derived-token
    (make-objc-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-objc-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
