#lang racket/base

;;;
;;; Lua Token Reader
;;;

;; Bridge derived Lua tokens to the projected reusable token stream.

(provide make-lua-token-reader)

(require "lua-derived.rkt" "lua-project.rkt")

;; make-lua-token-reader : lua-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Lua API.
(define (make-lua-token-reader config)
  (define next-token (make-lua-derived-reader))
  (lambda (in)
    (let loop ()
      (define token (project-lua-derived-token (next-token in) config))
      (if token token (loop)))))
