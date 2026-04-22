#lang racket/base

;;;
;;; Haskell Token Reader
;;;
;;
;; Bridge the derived Haskell tokenizer to the projected reusable token stream.

;; make-haskell-token-reader : haskell-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Haskell API.

(provide make-haskell-token-reader)

(require parser-tools/lex
         "config.rkt"
         "haskell-derived.rkt"
         "haskell-project.rkt"
         "parser-tools-compat.rkt"
         "stream.rkt")

;; compiler-profile? : haskell-config? -> boolean?
;;   Determine whether layout insertion should be active.
(define (compiler-profile? config)
  (eq? (haskell-config-profile config) 'compiler))

;; layout-keyword? : haskell-derived-token? -> boolean?
;;   Determine whether a token introduces a layout context when no explicit brace follows.
(define (layout-keyword? token)
  (and (haskell-derived-token-has-tag? token 'haskell-keyword)
       (member (haskell-derived-token-text token)
               '("let" "where" "do" "of"))))

;; in-keyword? : haskell-derived-token? -> boolean?
;;   Determine whether a token is the keyword @tt{in}.
(define (in-keyword? token)
  (and (haskell-derived-token-has-tag? token 'haskell-keyword)
       (string=? (haskell-derived-token-text token) "in")))

;; trivia-token? : haskell-derived-token? -> boolean?
;;   Determine whether a derived token is trivia.
(define (trivia-token? token)
  (or (haskell-derived-token-has-tag? token 'haskell-whitespace)
      (haskell-derived-token-has-tag? token 'haskell-comment)))

;; newline-whitespace-token? : haskell-derived-token? -> boolean?
;;   Determine whether a whitespace token contains a line terminator.
(define (newline-whitespace-token? token)
  (and (haskell-derived-token-has-tag? token 'haskell-whitespace)
       (regexp-match? #px"\r\n|\r|\n"
                      (haskell-derived-token-text token))))

;; token-column : haskell-derived-token? -> exact-integer?
;;   Extract the starting column for one token.
(define (token-column token)
  (position-col (haskell-derived-token-start token)))

;; virtual-layout-token : string? position? haskell-config? -> token-like?
;;   Construct one synthesized layout delimiter token.
(define (virtual-layout-token text pos config)
  (wrap-token-with-pos
   (make-stream-token stream-category-delimiter text)
   pos
   pos
   (haskell-config-source-positions config)))

;; eof-token/result : haskell-config? -> token-like?
;;   Construct the public eof result for the current projection configuration.
(define (eof-token/result config)
  (wrap-token-with-pos 'eof
                       (make-stream-position 1 1 0)
                       (make-stream-position 1 1 0)
                       (haskell-config-source-positions config)))

;; make-haskell-token-reader : haskell-config? -> (input-port? -> token-like?)
;;   Construct a projected token reader for the public Haskell API.
(define (make-haskell-token-reader config)
  (define next-derived-token
    (make-haskell-derived-reader))
  (define queued-results
    '())
  (define layout-stack
    '())
  (define pending-layout-keyword
    #f)
  (define saw-newline?
    #f)
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
  (define (close-layouts-to-column! column pos)
    (let loop ()
      (when (and (pair? layout-stack)
                 (> (car layout-stack) column))
        (enqueue! (virtual-layout-token "}" pos config))
        (set! layout-stack (cdr layout-stack))
        (loop))))
  (define (handle-significant-token! token projected)
    (define start-pos
      (haskell-derived-token-start token))
    (define column
      (token-column token))
    (cond
      [pending-layout-keyword
       (cond
         [(and (haskell-derived-token-has-tag? token 'haskell-delimiter)
               (string=? (haskell-derived-token-text token) "{"))
          (set! pending-layout-keyword #f)
          (set! saw-newline? #f)
          projected]
         [else
          (close-layouts-to-column! column start-pos)
          (set! layout-stack
                (cons column layout-stack))
          (set! pending-layout-keyword #f)
          (set! saw-newline? #f)
          (enqueue! projected)
          (cond
            [(pair? queued-results)
             (virtual-layout-token "{" start-pos config)]
            [else
             (virtual-layout-token "{" start-pos config)])])]
      [(in-keyword? token)
       (close-layouts-to-column! -1 start-pos)
       (set! saw-newline? #f)
       (cond
         [(pair? queued-results)
          (enqueue! projected)
          (dequeue!)]
         [else
          projected])]
      [saw-newline?
       (close-layouts-to-column! column start-pos)
       (cond
         [(pair? queued-results)
          (set! saw-newline? #f)
          (enqueue! projected)
          (dequeue!)]
         [(and (pair? layout-stack)
               (= (car layout-stack) column))
          (set! saw-newline? #f)
          (enqueue! projected)
          (virtual-layout-token ";" start-pos config)]
         [else
          (set! saw-newline? #f)
          projected])]
      [else
       projected]))
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
                    (pair? layout-stack))
               (for ([indent (in-list layout-stack)])
                 (enqueue! (virtual-layout-token "}"
                                                 (make-stream-position 1 1 0)
                                                 config)))
               (set! layout-stack '())
               (enqueue! (eof-token/result config))
               (dequeue!)]
              [else
               (eof-token/result config)])]
           [else
            (define projected
              (project-haskell-derived-token derived-token config))
            (cond
              [(and (compiler-profile? config)
                    (trivia-token? derived-token))
               (when (newline-whitespace-token? derived-token)
                 (set! saw-newline? #t))
               (cond
                 [projected projected]
                 [else      (loop)])]
              [(compiler-profile? config)
               (define result
                 (handle-significant-token! derived-token projected))
               (set! pending-layout-keyword
                     (layout-keyword? derived-token))
               result]
              [projected
               projected]
              [else
               (loop)])])]))))
