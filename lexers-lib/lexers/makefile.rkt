#lang racket/base

;;;
;;; Makefile Lexer
;;;
;;
;; Public entry points for the Makefile lexer.

;; make-makefile-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Makefile lexer.
;; make-makefile-derived-lexer : -> (input-port? -> (or/c makefile-derived-token? 'eof))
;;   Construct a port-based Makefile lexer that returns derived token values.
;; makefile-derived-token?     : any/c -> boolean?
;;   Recognize a derived Makefile token value returned by the derived-token API.
;; makefile-derived-token-tags : makefile-derived-token? -> (listof symbol?)
;;   Extract the Makefile-specific classification tags for one derived token.
;; makefile-derived-token-has-tag? : makefile-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; makefile-derived-token-text : makefile-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; makefile-derived-token-start : makefile-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; makefile-derived-token-end  : makefile-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; makefile-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Makefile string using the Makefile lexer.
;; makefile-string->derived-tokens : string? -> (listof makefile-derived-token?)
;;   Tokenize an entire Makefile string into derived Makefile token values.
;; makefile-profiles           : immutable-hash?
;;   Profile defaults for the public Makefile lexer.

(provide make-makefile-lexer
         make-makefile-derived-lexer
         makefile-derived-token?
         makefile-derived-token-tags
         makefile-derived-token-has-tag?
         makefile-derived-token-text
         makefile-derived-token-start
         makefile-derived-token-end
         makefile-string->tokens
         makefile-string->derived-tokens
         makefile-profiles)

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         (rename-in "private/makefile-derived.rkt"
                    [makefile-derived-token? private-makefile-derived-token?]
                    [makefile-derived-token-tags private-makefile-derived-token-tags]
                    [makefile-derived-token-has-tag? private-makefile-derived-token-has-tag?]
                    [makefile-derived-token-text private-makefile-derived-token-text]
                    [makefile-derived-token-start private-makefile-derived-token-start]
                    [makefile-derived-token-end private-makefile-derived-token-end]
                    [make-makefile-derived-reader private-make-makefile-derived-reader])
         "private/makefile-tokenize.rkt"
         "token.rkt")

(define makefile-profiles
  makefile-profile-defaults)

(define (makefile-derived-token? v)
  (private-makefile-derived-token? v))

(define (makefile-derived-token-tags token)
  (private-makefile-derived-token-tags token))

(define (makefile-derived-token-has-tag? token tag)
  (private-makefile-derived-token-has-tag? token tag))

(define (makefile-derived-token-text token)
  (private-makefile-derived-token-text token))

(define (makefile-derived-token-start token)
  (private-makefile-derived-token-start token))

(define (makefile-derived-token-end token)
  (private-makefile-derived-token-end token))

(define (make-makefile-lexer #:profile          [profile 'coloring]
                             #:trivia           [trivia 'profile-default]
                             #:source-positions [source-positions 'profile-default])
  (define config
    (make-makefile-config #:profile          profile
                          #:trivia           trivia
                          #:source-positions source-positions))
  (make-makefile-token-reader config))

(define (make-makefile-derived-lexer)
  (private-make-makefile-derived-reader))

(define (makefile-string->tokens source
                                 #:profile          [profile 'coloring]
                                 #:trivia           [trivia 'profile-default]
                                 #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-makefile-lexer #:profile          profile
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

(define (makefile-string->derived-tokens source)
  (define lexer
    (make-makefile-derived-lexer))
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
      (= (position-offset (makefile-derived-token-end left))
         (position-offset (makefile-derived-token-start right)))))

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
    "CC := gcc\nall: main.o util.o\n\t$(CC) -o app main.o util.o\ninclude local.mk\n")
  (define sample-derived
    (makefile-string->derived-tokens sample-source))
  (define sample-tokens
    (makefile-string->tokens sample-source
                             #:profile 'coloring
                             #:source-positions #f))
  (define compiler-tokens
    (makefile-string->tokens sample-source
                             #:profile 'compiler
                             #:source-positions #f))
  (define crlf-source
    "VAR = value\r\nall: target\r\n\t@echo $(VAR)\r\n")
  (define crlf-derived
    (makefile-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-makefile-derived-lexer
                              "CC := gcc\n"
                              "all:\n\t@echo hi\n"))
  (define directive-token
    (findf (lambda (token)
             (makefile-derived-token-has-tag? token 'makefile-directive))
           sample-derived))
  (define assignment-token
    (findf (lambda (token)
             (makefile-derived-token-has-tag? token 'makefile-assignment-operator))
           sample-derived))
  (define target-token
    (findf (lambda (token)
             (makefile-derived-token-has-tag? token 'makefile-rule-target))
           sample-derived))
  (define recipe-ref-token
    (findf (lambda (token)
             (makefile-derived-token-has-tag? token 'makefile-variable-reference))
           sample-derived))
  (define recipe-shell-token
    (findf (lambda (token)
             (and (makefile-derived-token-has-tag? token 'makefile-recipe)
                  (makefile-derived-token-has-tag? token 'shell-option)
                  (string=? (makefile-derived-token-text token) "-o")))
           sample-derived))
  (define shell-recipe-source
    "APP = scribble-tools\n.PHONY: docs test\ndocs:\n\traco scribble +m --html --dest html scribblings/scribble-tools.scrbl\n\ntest:\n\traco test private/lang-code.rkt\n")
  (define shell-recipe-derived
    (makefile-string->derived-tokens shell-recipe-source))
  (define shell-recipe-raco-token
    (findf (lambda (token)
             (and (makefile-derived-token-has-tag? token 'makefile-recipe)
                  (makefile-derived-token-has-tag? token 'identifier)
                  (string=? (makefile-derived-token-text token) "raco")))
           shell-recipe-derived))
  (define shell-recipe-command-token
    (findf (lambda (token)
             (and (makefile-derived-token-has-tag? token 'makefile-recipe)
                  (makefile-derived-token-has-tag? token 'shell-builtin)
                  (string=? (makefile-derived-token-text token) "test")))
           shell-recipe-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 8)
                '(identifier whitespace operator whitespace identifier whitespace identifier delimiter))
  (check-equal? (last (map lexer-token-name compiler-tokens))
                'eof)
  (check-not-false directive-token)
  (check-not-false assignment-token)
  (check-not-false target-token)
  (check-not-false recipe-ref-token)
  (check-not-false recipe-shell-token)
  (check-not-false shell-recipe-raco-token)
  (check-not-false shell-recipe-command-token)
  (check-equal? (makefile-derived-token-text directive-token)
                "include")
  (check-equal? (makefile-derived-token-text assignment-token)
                ":=")
  (check-equal? (makefile-derived-token-text target-token)
                "all")
  (check-equal? (makefile-derived-token-text recipe-ref-token)
                "$(CC)")
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map makefile-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map makefile-derived-token-text crlf-derived))
                crlf-source)
  (check-not-false first-streaming-token))
