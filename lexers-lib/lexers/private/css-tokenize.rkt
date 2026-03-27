#lang racket/base

;;;
;;; CSS Tokenization
;;;
;;
;; Public CSS token reading implemented as raw-tokenization plus projection.

;; read-css-token : input-port? css-config? -> token-like?
;;   Read the next CSS token-like value from an input port.

(provide read-css-token)

(require "config.rkt"
         "css-project.rkt"
         "css-raw.rkt")

;; read-css-token : input-port? css-config? -> token-like?
;;   Read the next CSS token-like value from an input port.
(define (read-css-token in config)
  (unless (input-port? in)
    (raise-argument-error 'read-css-token "input-port?" 0 in config))
  (let loop ()
    (define projected
      (project-css-raw-token (read-css-raw-token in) config))
    (cond
      [projected projected]
      [else      (loop)])))
