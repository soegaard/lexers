#lang racket/base

;;;
;;; Scheme Lexer
;;;

;; Public entry points for standards-aware and implementation-aware Scheme lexing.

(provide make-scheme-lexer             ; Projected Scheme token reader.
         make-scheme-derived-lexer     ; Scheme-specific derived token reader.
         scheme-derived-token?         ; Recognize a derived Scheme token.
         scheme-derived-token-tags     ; Return a derived token's tags.
         scheme-derived-token-has-tag? ; Check a derived token tag.
         scheme-derived-token-text     ; Return a derived token's source slice.
         scheme-derived-token-start    ; Return a derived token's start position.
         scheme-derived-token-end      ; Return a derived token's end position.
         scheme-string->tokens         ; Tokenize a complete Scheme string.
         scheme-string->derived-tokens ; Derive tokens from a complete Scheme string.
         scheme-profiles               ; Available public lexer profiles.
         scheme-dialects)              ; Supported report and implementation dialects.

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         (rename-in "private/scheme-derived.rkt"
                    [scheme-derived-token? private-token?] [scheme-derived-token-tags private-tags]
                    [scheme-derived-token-has-tag? private-has-tag?] [scheme-derived-token-text private-text]
                    [scheme-derived-token-start private-start] [scheme-derived-token-end private-end]
                    [make-scheme-derived-reader private-reader])
         "private/scheme-tokenize.rkt" "token.rkt")

;; Supported lexer profiles and reader dialects.
(define scheme-profiles scheme-profile-defaults)
(define scheme-dialects '(r5rs r6rs r7rs chez guile chicken gambit))

;; scheme-derived-token? : any/c -> boolean?
;;   Recognize a Scheme derived token.
(define (scheme-derived-token? value) (private-token? value))

;; scheme-derived-token-tags : scheme-derived-token? -> (listof symbol?)
;;   Return a Scheme derived token's reusable tags.
(define (scheme-derived-token-tags token) (private-tags token))

;; scheme-derived-token-has-tag? : scheme-derived-token? symbol? -> boolean?
;;   Determine whether a Scheme derived token has a tag.
(define (scheme-derived-token-has-tag? token tag) (private-has-tag? token tag))

;; scheme-derived-token-text : scheme-derived-token? -> string?
;;   Return the exact source text covered by a token.
(define (scheme-derived-token-text token) (private-text token))

;; scheme-derived-token-start : scheme-derived-token? -> position?
;;   Return a token's starting source position.
(define (scheme-derived-token-start token) (private-start token))

;; scheme-derived-token-end : scheme-derived-token? -> position?
;;   Return a token's ending source position.
(define (scheme-derived-token-end token) (private-end token))

;; make-scheme-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a projected Scheme lexer for the selected dialect.
(define (make-scheme-lexer #:profile [profile 'coloring] #:trivia [trivia 'profile-default]
                           #:source-positions [source-positions 'profile-default] #:dialect [dialect 'r5rs])
  (make-scheme-token-reader
   (make-scheme-config #:profile profile #:trivia trivia
                       #:source-positions source-positions #:dialect dialect)))

;; make-scheme-derived-lexer : symbol? -> (input-port? -> (or/c scheme-derived-token? 'eof))
;;   Construct a derived Scheme lexer for the selected dialect.
(define (make-scheme-derived-lexer #:dialect [dialect 'r5rs])
  (unless (member dialect scheme-dialects)
    (raise-arguments-error 'make-scheme-derived-lexer "unknown Scheme dialect" "dialect" dialect))
  (private-reader dialect))

;; scheme-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize a complete Scheme source string into projected tokens.
(define (scheme-string->tokens source #:profile [profile 'coloring] #:trivia [trivia 'profile-default]
                               #:source-positions [source-positions 'profile-default] #:dialect [dialect 'r5rs])
  (define lexer
    (make-scheme-lexer #:profile profile #:trivia trivia
                       #:source-positions source-positions #:dialect dialect))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (if (lexer-token-eof? token)
        (reverse (cons token tokens))
        (loop (cons token tokens)))))

;; scheme-string->derived-tokens : string? symbol? -> (listof scheme-derived-token?)
;;   Tokenize a complete Scheme source string into derived tokens.
(define (scheme-string->derived-tokens source #:dialect [dialect 'r5rs])
  (define lexer (make-scheme-derived-lexer #:dialect dialect))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (if (eq? token 'eof)
        (reverse tokens)
        (loop (cons token tokens)))))
(module+ test
  (require rackunit)
  (define source "#| nested #| comment |# |#\r\n(define x #;(+ 1 2) #:key \"ok\")\r\n")
  (define tokens (scheme-string->derived-tokens source #:dialect 'chicken))
  (check-equal? (apply string-append (map scheme-derived-token-text tokens)) source)
  (check-not-false (ormap (lambda (token) (scheme-derived-token-has-tag? token 'scheme-comment)) tokens))
  (for ([dialect (in-list scheme-dialects)])
    (define dialect-tokens
      (scheme-string->derived-tokens "(display #\\space #u8(1 2) 'symbol)\r\n" #:dialect dialect))
    (check-equal? (apply string-append (map scheme-derived-token-text dialect-tokens))
                  "(display #\\space #u8(1 2) 'symbol)\r\n"))
  (define projected
    (scheme-string->tokens "(display \"CRLF\")\r\n" #:source-positions #f))
  (check-equal?
   (apply string-append
          (for/list ([token (in-list (drop-right projected 1))])
            (lexer-token-value token)))
   "(display \"CRLF\")\r\n")
  (check-not-false
   (ormap (lambda (token) (scheme-derived-token-has-tag? token 'scheme-prefix-keyword))
          (scheme-string->derived-tokens "#:name" #:dialect 'guile)))
  (check-not-false
   (ormap (lambda (token) (scheme-derived-token-has-tag? token 'scheme-suffix-keyword))
          (scheme-string->derived-tokens "name:" #:dialect 'gambit))))
