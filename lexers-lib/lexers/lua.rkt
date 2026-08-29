#lang racket/base

;;;
;;; Lua Lexer
;;;

;; Public entry points for the Lua lexer.

(provide make-lua-lexer make-lua-derived-lexer
         lua-derived-token? lua-derived-token-tags lua-derived-token-has-tag?
         lua-derived-token-text lua-derived-token-start lua-derived-token-end
         lua-string->tokens lua-string->derived-tokens lua-profiles)

(require parser-tools/lex racket/list "private/config.rkt"
         (rename-in "private/lua-derived.rkt"
                    [lua-derived-token? private-token?]
                    [lua-derived-token-tags private-tags]
                    [lua-derived-token-has-tag? private-has-tag?]
                    [lua-derived-token-text private-text]
                    [lua-derived-token-start private-start]
                    [lua-derived-token-end private-end]
                    [make-lua-derived-reader private-reader])
         "private/lua-tokenize.rkt" "token.rkt")

(define lua-profiles lua-profile-defaults)
(define (lua-derived-token? value) (private-token? value))
(define (lua-derived-token-tags token) (private-tags token))
(define (lua-derived-token-has-tag? token tag) (private-has-tag? token tag))
(define (lua-derived-token-text token) (private-text token))
(define (lua-derived-token-start token) (private-start token))
(define (lua-derived-token-end token) (private-end token))

;; make-lua-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Lua lexer.
(define (make-lua-lexer #:profile [profile 'coloring] #:trivia [trivia 'profile-default]
                        #:source-positions [source-positions 'profile-default])
  (make-lua-token-reader (make-lua-config #:profile profile #:trivia trivia #:source-positions source-positions)))

;; make-lua-derived-lexer : -> (input-port? -> (or/c lua-derived-token? 'eof))
;;   Construct a port-based Lua lexer returning derived tokens.
(define (make-lua-derived-lexer) (private-reader))

;; lua-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize all source through the projected Lua API.
(define (lua-string->tokens source #:profile [profile 'coloring] #:trivia [trivia 'profile-default]
                            #:source-positions [source-positions 'profile-default])
  (define lexer (make-lua-lexer #:profile profile #:trivia trivia #:source-positions source-positions))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (if (lexer-token-eof? token) (reverse (cons token tokens)) (loop (cons token tokens)))))

;; lua-string->derived-tokens : string? -> (listof lua-derived-token?)
;;   Tokenize all source into derived Lua tokens.
(define (lua-string->derived-tokens source)
  (define lexer (make-lua-derived-lexer))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (if (eq? token 'eof) (reverse tokens) (loop (cons token tokens)))))

(module+ test
  (require rackunit)
  (define source "--[=[ comment ]=]\r\nlocal message = [[hello\nworld]]\r\nlocal n = 0x1.fp2 .. \"!\"\r\nif n >= 1 then return true end\r\n")
  (define derived (lua-string->derived-tokens source))
  (define projected (lua-string->tokens source #:source-positions #f))
  (check-equal? (apply string-append (map lua-derived-token-text derived)) source)
  (check-equal? (apply string-append (for/list ([token (in-list (drop-right projected 1))]) (lexer-token-value token))) source)
  (check-not-false (ormap (lambda (token) (lua-derived-token-has-tag? token 'lua-long-string)) derived))
  (check-not-false (ormap (lambda (token) (lua-derived-token-has-tag? token 'lua-number)) derived))
  (check-true (ormap (lambda (token) (and (lua-derived-token-has-tag? token 'lua-operator)
                                           (string=? (lua-derived-token-text token) "...")))
                     (lua-string->derived-tokens "local function f(...) end")))
  (check-equal? (map lua-derived-token-text (lua-string->derived-tokens "return 3-4, 0xe+1"))
                '("return" " " "3" "-" "4" "," " " "0xe" "+" "1"))
  (check-exn exn:fail:read? (lambda () (lua-string->tokens "local x = [[oops" #:profile 'compiler))))
