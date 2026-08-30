#lang racket/base
(provide make-scheme-token-reader)
(require "config.rkt"
         "scheme-derived.rkt"
         "scheme-project.rkt")
(define (make-scheme-token-reader config)
  (define next-token (make-scheme-derived-reader (scheme-config-dialect config)))
  (lambda (in) (let loop () (define token (project-scheme-derived-token (next-token in) config)) (if token token (loop)))))
