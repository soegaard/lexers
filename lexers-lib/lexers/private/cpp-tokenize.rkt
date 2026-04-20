#lang racket/base

;;;
;;; C++ Token Reader
;;;
;;
;; Bridge the derived C++ tokenizer to the projected reusable token stream.

;; make-cpp-token-reader : cpp-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public C++ API.

(provide make-cpp-token-reader)

(require "config.rkt"
         "cpp-derived.rkt"
         "cpp-project.rkt")

;; make-cpp-token-reader : cpp-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public C++ API.
(define (make-cpp-token-reader config)
  (define next-derived-token
    (make-cpp-derived-reader))
  (lambda (in)
    (let loop ()
      (define result
        (project-cpp-derived-token (next-derived-token in) config))
      (cond
        [result result]
        [else   (loop)]))))
