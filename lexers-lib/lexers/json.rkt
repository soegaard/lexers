#lang racket/base

;;;
;;; JSON Lexer
;;;
;;
;; Public entry points for the JSON lexer.

;; make-json-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based JSON lexer.
;; make-json-derived-lexer : -> (input-port? -> (or/c json-derived-token? 'eof))
;;   Construct a port-based JSON lexer that returns derived JSON token values.
;; json-derived-token?     : any/c -> boolean?
;;   Recognize a derived JSON token value returned by the derived-token API.
;; json-derived-token-tags : json-derived-token? -> (listof symbol?)
;;   Extract the JSON-specific classification tags for one derived token.
;; json-derived-token-has-tag? : json-derived-token? symbol? -> boolean?
;;   Determine whether a derived JSON token has a given classification tag.
;; json-derived-token-text : json-derived-token? -> string?
;;   Extract the source text corresponding to one derived JSON token.
;; json-derived-token-start : json-derived-token? -> position?
;;   Extract the starting source position for one derived JSON token.
;; json-derived-token-end  : json-derived-token? -> position?
;;   Extract the ending source position for one derived JSON token.
;; json-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire JSON string using the JSON lexer.
;; json-string->derived-tokens : string? -> (listof json-derived-token?)
;;   Tokenize an entire JSON string into derived JSON token values.
;; json-profiles           : immutable-hash?
;;   Profile defaults for the public JSON lexer.

(provide make-json-lexer
         make-json-derived-lexer
         json-derived-token?
         json-derived-token-tags
         json-derived-token-has-tag?
         json-derived-token-text
         json-derived-token-start
         json-derived-token-end
         json-string->tokens
         json-string->derived-tokens
         json-profiles)

(require parser-tools/lex
         "private/config.rkt"
         (rename-in "private/json-derived.rkt"
                    [json-derived-token? private-json-derived-token?]
                    [json-derived-token-tags private-json-derived-token-tags]
                    [json-derived-token-has-tag? private-json-derived-token-has-tag?]
                    [json-derived-token-text private-json-derived-token-text]
                    [json-derived-token-start private-json-derived-token-start]
                    [json-derived-token-end private-json-derived-token-end]
                    [make-json-derived-reader private-make-json-derived-reader])
         "private/json-tokenize.rkt"
         "private/parser-tools-compat.rkt"
         "token.rkt")

(define json-profiles
  json-profile-defaults)

;; json-derived-token? : any/c -> boolean?
;;   Recognize a derived JSON token value returned by the derived-token API.
(define (json-derived-token? v)
  (private-json-derived-token? v))

;; json-derived-token-tags : json-derived-token? -> (listof symbol?)
;;   Extract the JSON-specific classification tags for one derived token.
(define (json-derived-token-tags token)
  (private-json-derived-token-tags token))

;; json-derived-token-has-tag? : json-derived-token? symbol? -> boolean?
;;   Determine whether a derived JSON token has a given classification tag.
(define (json-derived-token-has-tag? token tag)
  (private-json-derived-token-has-tag? token tag))

;; json-derived-token-text : json-derived-token? -> string?
;;   Extract the source text corresponding to one derived JSON token.
(define (json-derived-token-text token)
  (private-json-derived-token-text token))

;; json-derived-token-start : json-derived-token? -> position?
;;   Extract the starting source position for one derived JSON token.
(define (json-derived-token-start token)
  (private-json-derived-token-start token))

;; json-derived-token-end : json-derived-token? -> position?
;;   Extract the ending source position for one derived JSON token.
(define (json-derived-token-end token)
  (private-json-derived-token-end token))

;; make-json-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based JSON lexer.
(define (make-json-lexer #:profile          [profile 'coloring]
                         #:trivia           [trivia 'profile-default]
                         #:source-positions [source-positions 'profile-default])
  (define config
    (make-json-config #:profile          profile
                      #:trivia           trivia
                      #:source-positions source-positions))
  (make-json-token-reader config))

;; make-json-derived-lexer : -> (input-port? -> (or/c json-derived-token? 'eof))
;;   Construct a port-based JSON lexer that returns derived JSON token values.
(define (make-json-derived-lexer)
  (private-make-json-derived-reader))

;; json-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire JSON string using the projected token API.
(define (json-string->tokens source
                             #:profile          [profile 'coloring]
                             #:trivia           [trivia 'profile-default]
                             #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-json-lexer #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions))
  (define in
    (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token
      (lexer in))
    (cond
      [(eof-token? token)
       (reverse (cons token tokens))]
      [else
       (loop (cons token tokens))])))

;; json-string->derived-tokens : string? -> (listof json-derived-token?)
;;   Tokenize an entire JSON string into derived JSON token values.
(define (json-string->derived-tokens source)
  (define lexer
    (make-json-derived-lexer))
  (define in
    (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token
      (lexer in))
    (cond
      [(eq? token 'eof)
       (reverse tokens)]
      [else
       (loop (cons token tokens))])))

(module+ test
  (require rackunit
           racket/list
           racket/string)

  ;; contiguous-derived-stream? : (listof json-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (json-derived-token-end left))
         (position-offset (json-derived-token-start right)))))

  (define sample-source
    "{\"x\": [1, true, null, \"hi\"]}\n")
  (define sample-tokens
    (json-string->tokens sample-source
                         #:profile 'coloring
                         #:source-positions #f))
  (define compiler-tokens
    (json-string->tokens sample-source
                         #:profile 'compiler
                         #:source-positions #f))
  (define derived-tokens
    (json-string->derived-tokens sample-source))
  (define key-token
    (findf (lambda (token)
             (json-derived-token-has-tag? token 'json-object-key))
           derived-tokens))
  (define true-token
    (findf (lambda (token)
             (json-derived-token-has-tag? token 'json-true))
           derived-tokens))
  (define null-token
    (findf (lambda (token)
             (json-derived-token-has-tag? token 'json-null))
           derived-tokens))
  (define number-token
    (findf (lambda (token)
             (json-derived-token-has-tag? token 'json-number))
           derived-tokens))
  (define malformed-coloring
    (json-string->tokens "{\"x\": tru}"
                         #:profile 'coloring
                         #:source-positions #f))
  (define malformed-compiler-thunk
    (lambda ()
      (json-string->tokens "{\"x\": tru}"
                           #:profile 'compiler
                           #:source-positions #f)))
  (define crlf-source
    "{\r\n  \"x\": 1\r\n}\r\n")
  (define crlf-derived-tokens
    (json-string->derived-tokens crlf-source))

  (check-equal? (map lexer-token-name sample-tokens)
                '(delimiter
                  identifier
                  operator
                  whitespace
                  delimiter
                  literal
                  delimiter
                  whitespace
                  literal
                  delimiter
                  whitespace
                  literal
                  delimiter
                  whitespace
                  literal
                  delimiter
                  delimiter
                  whitespace
                  eof))
  (check-equal? (map lexer-token-name compiler-tokens)
                '(delimiter
                  identifier
                  operator
                  delimiter
                  literal
                  delimiter
                  literal
                  delimiter
                  literal
                  delimiter
                  literal
                  delimiter
                  delimiter
                  eof))
  (check-equal? (json-derived-token-text key-token)
                "\"x\"")
  (check-true (json-derived-token-has-tag? key-token 'identifier))
  (check-equal? (json-derived-token-text true-token)
                "true")
  (check-equal? (json-derived-token-text null-token)
                "null")
  (check-equal? (json-derived-token-text number-token)
                "1")
  (check-true (contiguous-derived-stream? derived-tokens))
  (check-equal? (apply string-append (map json-derived-token-text derived-tokens))
                sample-source)
  (check-equal? (map lexer-token-name malformed-coloring)
                '(delimiter identifier operator whitespace unknown delimiter eof))
  (check-exn exn:fail:read?
             malformed-compiler-thunk)
  (check-equal? (apply string-append (map json-derived-token-text crlf-derived-tokens))
                crlf-source))
