#lang racket/base

;;;
;;; CSS Lexer
;;;
;;
;; Public entry points for the CSS lexer.

;; make-css-lexer      : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based CSS lexer.
;; make-css-derived-lexer : -> (input-port? -> (or/c css-derived-token? 'eof))
;;   Construct a port-based CSS lexer that returns derived CSS token values.
;; css-string->tokens  : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire CSS string using the CSS lexer.
;; css-string->derived-tokens : string? -> (listof css-derived-token?)
;;   Tokenize an entire CSS string into derived CSS token values.
;; css-profiles        : immutable-hash?
;;   Profile defaults for the public CSS lexer.

(provide make-css-lexer
         make-css-derived-lexer
         css-string->tokens
         css-string->derived-tokens
         css-profiles)

(require parser-tools/lex
         "private/config.rkt"
         "private/css-derived.rkt"
         "private/css-raw.rkt"
         "private/css-tokenize.rkt"
         "private/parser-tools-compat.rkt")

(define css-profiles css-profile-defaults)

;; make-css-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based CSS lexer.
(define (make-css-lexer #:profile          [profile 'coloring]
                        #:trivia           [trivia 'profile-default]
                        #:source-positions [source-positions 'profile-default])
  (define config
    (make-css-config #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions))
  (lambda (in)
    (read-css-token in config)))

;; make-css-derived-lexer : -> (input-port? -> (or/c css-derived-token? 'eof))
;;   Construct a port-based CSS lexer that returns derived CSS token values.
(define (make-css-derived-lexer)
  (lambda (in)
    (define raw-token (read-css-raw-token in))
    (cond
      [(eq? raw-token 'eof) 'eof]
      [else                 (derive-css-token raw-token)])))

;; css-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire CSS string using the CSS lexer.
(define (css-string->tokens source
                            #:profile          [profile 'coloring]
                            #:trivia           [trivia 'profile-default]
                            #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-css-lexer #:profile          profile
                    #:trivia           trivia
                    #:source-positions source-positions))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (cond
      [(eof-token? token) (reverse (cons token tokens))]
      [else               (loop (cons token tokens))])))

;; css-string->derived-tokens : string? -> (listof css-derived-token?)
;;   Tokenize an entire CSS string into derived CSS token values.
(define (css-string->derived-tokens source)
  (define lexer (make-css-derived-lexer))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (cond
      [(eq? token 'eof)
       (reverse tokens)]
      [else
       (loop (cons token tokens))])))

(module+ test
  (require rackunit)
  (require racket/list)

  (define coloring-tokens
    (css-string->tokens "/* c */ color: #fff;" #:profile 'coloring))
  (define compiler-tokens
    (css-string->tokens "/* c */ color: #fff;" #:profile 'compiler))
  (define function-tokens
    (css-string->tokens "rgb(10 20 30)" #:profile 'coloring))
  (define url-tokens
    (css-string->tokens "url(foo.png)" #:profile 'coloring))
  (define bad-url-tokens
    (css-string->tokens "url(foo.png" #:profile 'coloring))
  (define bad-string-tokens
    (css-string->tokens "\"unterminated" #:profile 'coloring))
  (define no-position-tokens
    (css-string->tokens "color" #:profile 'coloring #:source-positions #f))
  (define compiler-no-trivia-tokens
    (css-string->tokens "   color" #:profile 'compiler))
  (define coloring-with-trivia-tokens
    (css-string->tokens "   color" #:profile 'coloring))
  (define delim-tokens
    (css-string->tokens "~" #:profile 'compiler))
  (define derived-lexer
    (make-css-derived-lexer))
  (define derived-tokens
    (css-string->derived-tokens "#fff rgb( --brand-color"))
  (define (find-derived-token tag)
    (findf (lambda (token)
             (css-derived-token-has-tag? token tag))
           derived-tokens))
  (define derived-color-token
    (find-derived-token 'color-literal))
  (define derived-function-token
    (find-derived-token 'color-function))
  (define derived-custom-property-token
    (find-derived-token 'custom-property-name))

  (check-true (pair? coloring-tokens))
  (check-true (position-token? (car coloring-tokens)))
  (check-equal? (stream-token-name (car coloring-tokens)) 'comment)
  (check-equal? (stream-token-name (car compiler-tokens)) 'identifier)
  (check-equal? (stream-token-name (car function-tokens)) 'literal)
  (check-equal? (stream-token-name (car url-tokens)) 'literal)
  (check-equal? (stream-token-name (car bad-url-tokens)) 'unknown)
  (check-equal? (stream-token-name (car bad-string-tokens)) 'unknown)
  (check-false (position-token? (car no-position-tokens)))
  (check-equal? (stream-token-name (car compiler-no-trivia-tokens)) 'identifier)
  (check-equal? (stream-token-name (car coloring-with-trivia-tokens)) 'whitespace)
  (check-equal? (stream-token-name (car delim-tokens)) 'delimiter)
  (check-exn exn:fail:read?
             (lambda ()
               (css-string->tokens "\"unterminated" #:profile 'compiler)))
  (check-exn exn:fail:read?
             (lambda ()
               (css-string->tokens "url(foo.png" #:profile 'compiler)))
  (check-false (eq? (derived-lexer (open-input-string "#fff")) 'eof))
  (check-equal? (length derived-tokens) 6)
  (check-not-false (css-derived-token-has-tag? derived-color-token 'color-literal))
  (check-not-false (css-derived-token-has-tag? derived-function-token 'color-function))
  (check-not-false (css-derived-token-has-tag? derived-custom-property-token 'custom-property-name)))
