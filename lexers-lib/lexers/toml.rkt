#lang racket/base

;;;
;;; TOML Lexer
;;;

;; Public entry points for the TOML lexer.

;; make-toml-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based TOML lexer.
;; make-toml-derived-lexer : -> (input-port? -> (or/c toml-derived-token? 'eof))
;;   Construct a port-based TOML lexer that returns derived token values.
;; toml-derived-token?     : any/c -> boolean?
;;   Recognize a derived TOML token value.
;; toml-derived-token-tags : toml-derived-token? -> (listof symbol?)
;;   Extract TOML-specific classification tags.
;; toml-derived-token-has-tag? : toml-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a classification tag.
;; toml-derived-token-text : toml-derived-token? -> string?
;;   Extract exact source text for one derived token.
;; toml-derived-token-start : toml-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; toml-derived-token-end  : toml-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; toml-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire TOML string using projected tokens.
;; toml-string->derived-tokens : string? -> (listof toml-derived-token?)
;;   Tokenize an entire TOML string into derived token values.
;; toml-profiles           : immutable-hash?
;;   Profile defaults for the public TOML lexer.

(provide make-toml-lexer
         make-toml-derived-lexer
         toml-derived-token?
         toml-derived-token-tags
         toml-derived-token-has-tag?
         toml-derived-token-text
         toml-derived-token-start
         toml-derived-token-end
         toml-string->tokens
         toml-string->derived-tokens
         toml-profiles)

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         (rename-in "private/toml-derived.rkt"
                    [toml-derived-token? private-toml-derived-token?]
                    [toml-derived-token-tags private-toml-derived-token-tags]
                    [toml-derived-token-has-tag? private-toml-derived-token-has-tag?]
                    [toml-derived-token-text private-toml-derived-token-text]
                    [toml-derived-token-start private-toml-derived-token-start]
                    [toml-derived-token-end private-toml-derived-token-end]
                    [make-toml-derived-reader private-make-toml-derived-reader])
         "private/toml-tokenize.rkt"
         "token.rkt")

(define toml-profiles toml-profile-defaults)

;; toml-derived-token? : any/c -> boolean?
;;   Recognize a derived TOML token value.
(define (toml-derived-token? value) (private-toml-derived-token? value))

;; toml-derived-token-tags : toml-derived-token? -> (listof symbol?)
;;   Extract TOML-specific classification tags.
(define (toml-derived-token-tags token) (private-toml-derived-token-tags token))

;; toml-derived-token-has-tag? : toml-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a classification tag.
(define (toml-derived-token-has-tag? token tag)
  (private-toml-derived-token-has-tag? token tag))

;; toml-derived-token-text : toml-derived-token? -> string?
;;   Extract exact source text for one derived token.
(define (toml-derived-token-text token) (private-toml-derived-token-text token))

;; toml-derived-token-start : toml-derived-token? -> position?
;;   Extract the starting source position for one derived token.
(define (toml-derived-token-start token) (private-toml-derived-token-start token))

;; toml-derived-token-end : toml-derived-token? -> position?
;;   Extract the ending source position for one derived token.
(define (toml-derived-token-end token) (private-toml-derived-token-end token))

;; make-toml-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based TOML lexer.
(define (make-toml-lexer #:profile          [profile 'coloring]
                         #:trivia           [trivia 'profile-default]
                         #:source-positions [source-positions 'profile-default])
  (make-toml-token-reader
   (make-toml-config #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions)))

;; make-toml-derived-lexer : -> (input-port? -> (or/c toml-derived-token? 'eof))
;;   Construct a port-based TOML lexer that returns derived token values.
(define (make-toml-derived-lexer) (private-make-toml-derived-reader))

;; toml-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire TOML string using projected tokens.
(define (toml-string->tokens source
                             #:profile          [profile 'coloring]
                             #:trivia           [trivia 'profile-default]
                             #:source-positions [source-positions 'profile-default])
  (define lexer (make-toml-lexer #:profile profile #:trivia trivia #:source-positions source-positions))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (if (lexer-token-eof? token)
        (reverse (cons token tokens))
        (loop (cons token tokens)))))

;; toml-string->derived-tokens : string? -> (listof toml-derived-token?)
;;   Tokenize an entire TOML string into derived token values.
(define (toml-string->derived-tokens source)
  (define lexer (make-toml-derived-lexer))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (if (eq? token 'eof)
        (reverse tokens)
        (loop (cons token tokens)))))

(module+ test
  (require rackunit)

  ;; tokens->text : (listof token-like?) -> string?
  ;;   Reconstruct source from projected tokens, excluding eof.
  (define (tokens->text tokens)
    (apply string-append
           (for/list ([token (in-list (drop-right tokens 1))])
             (lexer-token-value token))))

  (define source
    "# Cargo-like manifest\r\n[package]\r\nname = \"lexers\"\r\nversion = '1.0.0'\r\nwhen = 1979-05-27T07:32:00Z\r\nvalues = [1, 0xDEAD, true, -inf]\r\n[[bin]]\r\nname = \"tool\"\r\n")
  (define derived (toml-string->derived-tokens source))
  (define projected (toml-string->tokens source #:source-positions #f))
  (define strict-thunk
    (lambda () (toml-string->tokens "name = \"unterminated" #:profile 'compiler)))

  (check-equal? (apply string-append (map toml-derived-token-text derived)) source)
  (check-equal? (tokens->text projected) source)
  (check-true (for/and ([left (in-list derived)] [right (in-list (cdr derived))])
                (= (position-offset (toml-derived-token-end left))
                   (position-offset (toml-derived-token-start right)))))
  (check-not-false (ormap (lambda (token) (toml-derived-token-has-tag? token 'toml-table-key)) derived))
  (check-not-false (ormap (lambda (token) (toml-derived-token-has-tag? token 'toml-number)) derived))
  (check-not-false (ormap (lambda (token) (toml-derived-token-has-tag? token 'toml-date-time)) derived))
  (check-not-false (ormap (lambda (token) (toml-derived-token-has-tag? token 'malformed-token))
                           (toml-string->derived-tokens "name = \"unterminated")))
  (check-exn exn:fail:read? strict-thunk))
