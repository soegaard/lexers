#lang racket/base

;;;
;;; Rhombus Lexer
;;;
;;
;; Public entry points for the Rhombus lexer.

;; make-rhombus-lexer          : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Rhombus lexer.
;; make-rhombus-derived-lexer  : -> (input-port? -> (or/c rhombus-derived-token? 'eof))
;;   Construct a port-based Rhombus lexer that returns derived Rhombus token
;;   values.
;; rhombus-derived-token?      : any/c -> boolean?
;;   Recognize a derived Rhombus token value returned by the derived-token API.
;; rhombus-derived-token-tags  : rhombus-derived-token? -> (listof symbol?)
;;   Extract the Rhombus-specific classification tags for one derived token.
;; rhombus-derived-token-has-tag? : rhombus-derived-token? symbol? -> boolean?
;;   Determine whether a derived Rhombus token has a given classification tag.
;; rhombus-derived-token-text  : rhombus-derived-token? -> string?
;;   Extract the source text corresponding to one derived Rhombus token.
;; rhombus-derived-token-start : rhombus-derived-token? -> position?
;;   Extract the starting source position for one derived Rhombus token.
;; rhombus-derived-token-end   : rhombus-derived-token? -> position?
;;   Extract the ending source position for one derived Rhombus token.
;; rhombus-string->tokens      : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Rhombus string using the Rhombus lexer.
;; rhombus-string->derived-tokens : string? -> (listof rhombus-derived-token?)
;;   Tokenize an entire Rhombus string into derived Rhombus token values.
;; rhombus-profiles            : immutable-hash?
;;   Profile defaults for the public Rhombus lexer.

(provide make-rhombus-lexer
         make-rhombus-derived-lexer
         rhombus-derived-token?
         rhombus-derived-token-tags
         rhombus-derived-token-has-tag?
         rhombus-derived-token-text
         rhombus-derived-token-start
         rhombus-derived-token-end
         rhombus-string->tokens
         rhombus-string->derived-tokens
         rhombus-profiles)

(require parser-tools/lex
         "private/config.rkt"
         (rename-in "private/rhombus-derived.rkt"
                    [rhombus-derived-token? private-rhombus-derived-token?]
                    [rhombus-derived-token-tags private-rhombus-derived-token-tags]
                    [rhombus-derived-token-has-tag? private-rhombus-derived-token-has-tag?]
                    [rhombus-derived-token-text private-rhombus-derived-token-text]
                    [rhombus-derived-token-start private-rhombus-derived-token-start]
                    [rhombus-derived-token-end private-rhombus-derived-token-end]
                    [make-rhombus-derived-reader private-make-rhombus-derived-reader])
         "private/parser-tools-compat.rkt"
         "private/rhombus-tokenize.rkt")

(define rhombus-profiles
  rhombus-profile-defaults)

;; rhombus-derived-token? : any/c -> boolean?
;;   Recognize a derived Rhombus token value returned by the derived-token API.
(define (rhombus-derived-token? v)
  (private-rhombus-derived-token? v))

;; rhombus-derived-token-tags : rhombus-derived-token? -> (listof symbol?)
;;   Extract the Rhombus-specific classification tags for one derived token.
(define (rhombus-derived-token-tags token)
  (private-rhombus-derived-token-tags token))

;; rhombus-derived-token-has-tag? : rhombus-derived-token? symbol? -> boolean?
;;   Determine whether a derived Rhombus token has a given classification tag.
(define (rhombus-derived-token-has-tag? token tag)
  (private-rhombus-derived-token-has-tag? token tag))

;; rhombus-derived-token-text : rhombus-derived-token? -> string?
;;   Extract the source text corresponding to one derived Rhombus token.
(define (rhombus-derived-token-text token)
  (private-rhombus-derived-token-text token))

;; rhombus-derived-token-start : rhombus-derived-token? -> position?
;;   Extract the starting source position for one derived Rhombus token.
(define (rhombus-derived-token-start token)
  (private-rhombus-derived-token-start token))

;; rhombus-derived-token-end : rhombus-derived-token? -> position?
;;   Extract the ending source position for one derived Rhombus token.
(define (rhombus-derived-token-end token)
  (private-rhombus-derived-token-end token))

;; make-rhombus-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Rhombus lexer.
(define (make-rhombus-lexer #:profile          [profile 'coloring]
                            #:trivia           [trivia 'profile-default]
                            #:source-positions [source-positions 'profile-default])
  (define config
    (make-rhombus-config #:profile          profile
                         #:trivia           trivia
                         #:source-positions source-positions))
  (make-rhombus-token-reader config))

;; make-rhombus-derived-lexer : -> (input-port? -> (or/c rhombus-derived-token? 'eof))
;;   Construct a port-based Rhombus lexer that returns derived token values.
(define (make-rhombus-derived-lexer)
  (private-make-rhombus-derived-reader))

;; rhombus-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Rhombus string using the Rhombus lexer.
(define (rhombus-string->tokens source
                                #:profile          [profile 'coloring]
                                #:trivia           [trivia 'profile-default]
                                #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-rhombus-lexer #:profile          profile
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

;; rhombus-string->derived-tokens : string? -> (listof rhombus-derived-token?)
;;   Tokenize an entire Rhombus string into derived Rhombus token values.
(define (rhombus-string->derived-tokens source)
  (define lexer
    (make-rhombus-derived-lexer))
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
           "token.rkt")

  ;; rhombus-runtime-available? : boolean?
  ;;   Determine whether the current installation provides the optional
  ;;   Rhombus syntax-color layer.
  (define rhombus-runtime-available?
    (with-handlers ([exn:fail? (lambda (_exn) #f)])
      (dynamic-require 'rhombus/private/syntax-color
                       'make-rhombus-lexer)
      #t))

  (cond
    [rhombus-runtime-available?
     (define sample-source
       "fun f(x): println(x + 1)\n")
     (define coloring-tokens
       (rhombus-string->tokens sample-source
                               #:profile 'coloring
                               #:source-positions #f))
     (define compiler-tokens
       (rhombus-string->tokens sample-source
                               #:profile 'compiler
                               #:source-positions #f))
     (define derived-tokens
       (rhombus-string->derived-tokens sample-source))
     (define crlf-source
       "fun f():\r\n  println(1)\r\n")
     (define crlf-derived-tokens
       (rhombus-string->derived-tokens crlf-source))
     (define keyword-token
       (findf (lambda (token)
                (rhombus-derived-token-has-tag? token 'rhombus-keyword))
              derived-tokens))
     (define builtin-token
       (findf (lambda (token)
                (rhombus-derived-token-has-tag? token 'rhombus-builtin))
              derived-tokens))
     (define block-operator-token
       (findf (lambda (token)
                (rhombus-derived-token-has-tag? token 'rhombus-block-operator))
              derived-tokens))

     (check-equal? (map lexer-token-name coloring-tokens)
                   '(keyword
                     whitespace
                     identifier
                     delimiter
                     identifier
                     delimiter
                     operator
                     whitespace
                     keyword
                     delimiter
                     identifier
                     whitespace
                     operator
                     whitespace
                     literal
                     delimiter
                     whitespace
                     eof))
     (check-equal? (map lexer-token-name compiler-tokens)
                   '(keyword
                     identifier
                     delimiter
                     identifier
                     delimiter
                     operator
                     keyword
                     delimiter
                     identifier
                     operator
                     literal
                     delimiter
                     eof))
     (check-equal? (rhombus-derived-token-text keyword-token)
                   "fun")
     (check-equal? (rhombus-derived-token-text builtin-token)
                   "println")
     (check-equal? (rhombus-derived-token-text block-operator-token)
                   ":")
     (check-true (rhombus-derived-token-has-tag? keyword-token 'keyword))
     (check-true (rhombus-derived-token-has-tag? builtin-token 'rhombus-builtin))
     (check-equal? (apply string-append (map rhombus-derived-token-text derived-tokens))
                   sample-source)
     (check-equal? (apply string-append (map rhombus-derived-token-text crlf-derived-tokens))
                   crlf-source)]
    [else
     (check-exn exn:fail:contract?
                (lambda ()
                  (rhombus-string->tokens "fun f(): 1\n")))]))
