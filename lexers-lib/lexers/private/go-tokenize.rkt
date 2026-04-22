#lang racket/base

;;;
;;; Go Token Reader
;;;
;;
;; Bridge the derived Go tokenizer to the projected reusable token stream.

;; make-go-token-reader : go-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Go API.

(provide make-go-token-reader)

(require "config.rkt"
         "go-derived.rkt"
         "go-project.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; compiler-profile? : go-config? -> boolean?
;;   Determine whether Go semicolon insertion should be active.
(define (compiler-profile? config)
  (eq? (go-config-profile config) 'compiler))

;; semicolon-insertion-trigger? : go-derived-token? -> boolean?
;;   Determine whether a token can trigger automatic semicolon insertion.
(define (semicolon-insertion-trigger? token)
  (or (go-derived-token-has-tag? token 'go-identifier)
      (go-derived-token-has-tag? token 'literal)
      (and (go-derived-token-has-tag? token 'go-keyword)
           (member (go-derived-token-text token)
                   '("break" "continue" "fallthrough" "return")))
      (and (go-derived-token-has-tag? token 'go-operator)
           (member (go-derived-token-text token)
                   '("++" "--")))
      (and (go-derived-token-has-tag? token 'go-delimiter)
           (member (go-derived-token-text token)
                   '(")" "]" "}")))))

;; newline-whitespace-token? : go-derived-token? -> boolean?
;;   Determine whether a whitespace token contains a line terminator.
(define (newline-whitespace-token? token)
  (and (go-derived-token-has-tag? token 'go-whitespace)
       (regexp-match? #px"\r\n|\r|\n"
                      (go-derived-token-text token))))

;; inserted-semicolon-token : go-derived-token? go-config? -> token-like?
;;   Construct one synthesized semicolon token at the end of the previous token.
(define (inserted-semicolon-token previous-token config)
  (define pos
    (go-derived-token-end previous-token))
  (wrap-token-with-pos
   (make-stream-token stream-category-delimiter ";")
   pos
   pos
   (go-config-source-positions config)))

;; eof-token/result : go-config? -> token-like?
;;   Construct the public eof result for the current projection configuration.
(define (eof-token/result config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (go-config-source-positions config)))

;; make-go-token-reader : go-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Go API.
(define (make-go-token-reader config)
  (define next-derived-token
    (make-go-derived-reader))
  (define previous-significant-token
    #f)
  (define queued-results
    '())
  (define (enqueue! token-like)
    (when token-like
      (set! queued-results
            (append queued-results (list token-like)))))
  (define (dequeue!)
    (define result
      (car queued-results))
    (set! queued-results
          (cdr queued-results))
    result)
  (lambda (in)
    (let loop ()
      (cond
        [(pair? queued-results)
         (dequeue!)]
        [else
         (define derived-token
           (next-derived-token in))
         (cond
           [(eq? derived-token 'eof)
            (cond
              [(and (compiler-profile? config)
                    previous-significant-token
                    (semicolon-insertion-trigger? previous-significant-token))
               (define previous-token
                 previous-significant-token)
               (enqueue! (eof-token/result config))
               (set! previous-significant-token #f)
               (inserted-semicolon-token previous-token config)]
              [else
               (eof-token/result config)])]
           [else
            (define projected
              (project-go-derived-token derived-token config))
            (cond
              [(and (compiler-profile? config)
                    previous-significant-token
                    (semicolon-insertion-trigger? previous-significant-token)
                    (newline-whitespace-token? derived-token))
               (define previous-token
                 previous-significant-token)
               (enqueue! projected)
               (set! previous-significant-token #f)
               (inserted-semicolon-token previous-token config)]
              [else
               (unless (or (go-derived-token-has-tag? derived-token 'go-whitespace)
                           (go-derived-token-has-tag? derived-token 'go-comment))
                 (set! previous-significant-token derived-token))
               (cond
                 [projected projected]
                 [else      (loop)])])])]))))
