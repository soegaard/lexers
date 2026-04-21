#lang racket/base

;;;
;;; Haskell Lexer
;;;
;;
;; Public entry points for the Haskell lexer.

;; make-haskell-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Haskell lexer.
;; make-haskell-derived-lexer : -> (input-port? -> (or/c haskell-derived-token? 'eof))
;;   Construct a port-based Haskell lexer that returns derived token values.
;; haskell-derived-token?     : any/c -> boolean?
;;   Recognize a derived Haskell token value returned by the derived-token API.
;; haskell-derived-token-tags : haskell-derived-token? -> (listof symbol?)
;;   Extract the Haskell-specific classification tags for one derived token.
;; haskell-derived-token-has-tag? : haskell-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; haskell-derived-token-text : haskell-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; haskell-derived-token-start : haskell-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; haskell-derived-token-end  : haskell-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; haskell-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Haskell string using the Haskell lexer.
;; haskell-string->derived-tokens : string? -> (listof haskell-derived-token?)
;;   Tokenize an entire Haskell string into derived Haskell token values.
;; haskell-profiles           : immutable-hash?
;;   Profile defaults for the public Haskell lexer.

(provide make-haskell-lexer
         make-haskell-derived-lexer
         haskell-derived-token?
         haskell-derived-token-tags
         haskell-derived-token-has-tag?
         haskell-derived-token-text
         haskell-derived-token-start
         haskell-derived-token-end
         haskell-string->tokens
         haskell-string->derived-tokens
         haskell-profiles)

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         (rename-in "private/haskell-derived.rkt"
                    [haskell-derived-token? private-haskell-derived-token?]
                    [haskell-derived-token-tags private-haskell-derived-token-tags]
                    [haskell-derived-token-has-tag? private-haskell-derived-token-has-tag?]
                    [haskell-derived-token-text private-haskell-derived-token-text]
                    [haskell-derived-token-start private-haskell-derived-token-start]
                    [haskell-derived-token-end private-haskell-derived-token-end]
                    [make-haskell-derived-reader private-make-haskell-derived-reader])
         "private/haskell-tokenize.rkt"
         "token.rkt")

(define haskell-profiles
  haskell-profile-defaults)

(define (haskell-derived-token? v)
  (private-haskell-derived-token? v))

(define (haskell-derived-token-tags token)
  (private-haskell-derived-token-tags token))

(define (haskell-derived-token-has-tag? token tag)
  (private-haskell-derived-token-has-tag? token tag))

(define (haskell-derived-token-text token)
  (private-haskell-derived-token-text token))

(define (haskell-derived-token-start token)
  (private-haskell-derived-token-start token))

(define (haskell-derived-token-end token)
  (private-haskell-derived-token-end token))

;; make-haskell-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Haskell lexer.
(define (make-haskell-lexer #:profile          [profile 'coloring]
                            #:trivia           [trivia 'profile-default]
                            #:source-positions [source-positions 'profile-default])
  (define config
    (make-haskell-config #:profile          profile
                         #:trivia           trivia
                         #:source-positions source-positions))
  (make-haskell-token-reader config))

;; make-haskell-derived-lexer : -> (input-port? -> (or/c haskell-derived-token? 'eof))
;;   Construct a port-based Haskell lexer that returns derived token values.
(define (make-haskell-derived-lexer)
  (private-make-haskell-derived-reader))

(define (haskell-string->tokens source
                                #:profile          [profile 'coloring]
                                #:trivia           [trivia 'profile-default]
                                #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-haskell-lexer #:profile          profile
                        #:trivia           trivia
                        #:source-positions source-positions))
  (define in
    (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token
      (lexer in))
    (cond
      [(lexer-token-eof? token)
       (reverse (cons token tokens))]
      [else
       (loop (cons token tokens))])))

(define (haskell-string->derived-tokens source)
  (define lexer
    (make-haskell-derived-lexer))
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
  (require rackunit)

  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (haskell-derived-token-end left))
         (position-offset (haskell-derived-token-start right)))))

  (define (first-token-before-rest? make-lexer first-chunk rest-chunk)
    (define lexer
      (make-lexer))
    (define-values (in out)
      (make-pipe))
    (write-string first-chunk out)
    (flush-output out)
    (define result-channel
      (make-channel))
    (thread
     (lambda ()
       (channel-put result-channel (lexer in))))
    (define token
      (sync/timeout 1 result-channel))
    (write-string rest-chunk out)
    (close-output-port out)
    token)

  (define sample-source
    "{-# LANGUAGE OverloadedStrings #-}\nmodule Main where\n\nmain = do\n  putStrLn \"hi\"\n  print 'x'\n  let x = 0x10\n  -- comment\n")
  (define sample-derived
    (haskell-string->derived-tokens sample-source))
  (define sample-tokens
    (haskell-string->tokens sample-source
                            #:profile 'coloring
                            #:source-positions #f))
  (define compiler-tokens
    (haskell-string->tokens sample-source
                            #:profile 'compiler
                            #:source-positions #f))
  (define crlf-source
    "module Main where\r\nmain = putStrLn \"hi\"\r\n"
    )
  (define crlf-derived
    (haskell-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-haskell-derived-lexer
                              "module "
                              "Main where\n"))
  (define pragma-token
    (findf (lambda (token)
             (haskell-derived-token-has-tag? token 'haskell-pragma))
           sample-derived))
  (define keyword-token
    (findf (lambda (token)
             (haskell-derived-token-has-tag? token 'haskell-keyword))
           sample-derived))
  (define string-token
    (findf (lambda (token)
             (haskell-derived-token-has-tag? token 'haskell-string-literal))
           sample-derived))
  (define char-token
    (findf (lambda (token)
             (haskell-derived-token-has-tag? token 'haskell-char-literal))
           sample-derived))
  (define numeric-token
    (findf (lambda (token)
             (haskell-derived-token-has-tag? token 'haskell-numeric-literal))
           sample-derived))
  (define comment-token
    (findf (lambda (token)
             (haskell-derived-token-has-tag? token 'haskell-line-comment))
           sample-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 8)
                '(comment whitespace keyword whitespace identifier whitespace keyword whitespace))
  (check-equal? (last (map lexer-token-name compiler-tokens))
                'eof)
  (check-not-false pragma-token)
  (check-not-false keyword-token)
  (check-not-false string-token)
  (check-not-false char-token)
  (check-not-false numeric-token)
  (check-not-false comment-token)
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map haskell-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map haskell-derived-token-text crlf-derived))
                crlf-source)
  (check-not-false first-streaming-token)
  (check-not-false (haskell-derived-token-has-tag? first-streaming-token
                                                   'haskell-keyword)))
