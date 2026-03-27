#lang racket/base

;;;
;;; JavaScript Tokenization
;;;
;;
;; Public JavaScript token reading implemented as raw-tokenization plus projection.

;; read-javascript-token : input-port? javascript-config? -> token-like?
;;   Read the next JavaScript token-like value from an input port.

(provide read-javascript-token)

(require "config.rkt"
         "javascript-project.rkt"
         "javascript-raw.rkt")

;; read-javascript-token : input-port? javascript-config? -> token-like?
;;   Read the next JavaScript token-like value from an input port.
(define (read-javascript-token in config)
  (unless (input-port? in)
    (raise-argument-error 'read-javascript-token "input-port?" 0 in config))
  (let loop ()
    (define projected
      (project-javascript-raw-token (read-javascript-raw-token in) config))
    (cond
      [projected projected]
      [else      (loop)])))

