#lang racket/base

;;;
;;; HTML Lexer
;;;
;;
;; Public entry points for the HTML lexer.

;; make-html-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based HTML lexer.
;; make-html-derived-lexer : -> (input-port? -> (or/c html-derived-token? 'eof))
;;   Construct a port-based HTML lexer that returns derived HTML token values.
;; html-derived-token?     : any/c -> boolean?
;;   Recognize a derived HTML token value returned by the derived-token API.
;; html-derived-token-tags : html-derived-token? -> (listof symbol?)
;;   Extract the HTML-specific classification tags for one derived token.
;; html-derived-token-has-tag? : html-derived-token? symbol? -> boolean?
;;   Determine whether a derived HTML token has a given classification tag.
;; html-derived-token-text : html-derived-token? -> string?
;;   Extract the source text corresponding to one derived HTML token.
;; html-derived-token-start : html-derived-token? -> position?
;;   Extract the starting source position for one derived HTML token.
;; html-derived-token-end : html-derived-token? -> position?
;;   Extract the ending source position for one derived HTML token.
;; html-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire HTML string using the HTML lexer.
;; html-string->derived-tokens : string? -> (listof html-derived-token?)
;;   Tokenize an entire HTML string into derived HTML token values.
;; html-profiles           : immutable-hash?
;;   Profile defaults for the public HTML lexer.

(provide make-html-lexer
         make-html-derived-lexer
         html-derived-token?
         html-derived-token-tags
         html-derived-token-has-tag?
         html-derived-token-text
         html-derived-token-start
         html-derived-token-end
         html-string->tokens
         html-string->derived-tokens
         html-profiles)

(require parser-tools/lex
         "private/config.rkt"
         (rename-in "private/html-derived.rkt"
                    [html-derived-token? private-html-derived-token?]
                    [html-derived-token-tags private-html-derived-token-tags]
                    [html-derived-token-has-tag? private-html-derived-token-has-tag?]
                    [html-derived-token-text private-html-derived-token-text]
                    [html-derived-token-start private-html-derived-token-start]
                    [html-derived-token-end private-html-derived-token-end]
                    [make-html-derived-reader private-make-html-derived-reader])
         "private/html-tokenize.rkt"
         "private/parser-tools-compat.rkt")

(define html-profiles html-profile-defaults)

;; html-derived-token? : any/c -> boolean?
;;   Recognize a derived HTML token value returned by the derived-token API.
(define (html-derived-token? v)
  (private-html-derived-token? v))

;; html-derived-token-tags : html-derived-token? -> (listof symbol?)
;;   Extract the HTML-specific classification tags for one derived token.
(define (html-derived-token-tags token)
  (private-html-derived-token-tags token))

;; html-derived-token-has-tag? : html-derived-token? symbol? -> boolean?
;;   Determine whether a derived HTML token has a given classification tag.
(define (html-derived-token-has-tag? token tag)
  (private-html-derived-token-has-tag? token tag))

;; html-derived-token-text : html-derived-token? -> string?
;;   Extract the source text corresponding to one derived HTML token.
(define (html-derived-token-text token)
  (private-html-derived-token-text token))

;; html-derived-token-start : html-derived-token? -> position?
;;   Extract the starting source position for one derived HTML token.
(define (html-derived-token-start token)
  (private-html-derived-token-start token))

;; html-derived-token-end : html-derived-token? -> position?
;;   Extract the ending source position for one derived HTML token.
(define (html-derived-token-end token)
  (private-html-derived-token-end token))

;; make-html-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based HTML lexer.
(define (make-html-lexer #:profile          [profile 'coloring]
                         #:trivia           [trivia 'profile-default]
                         #:source-positions [source-positions 'profile-default])
  (define config
    (make-html-config #:profile          profile
                      #:trivia           trivia
                      #:source-positions source-positions))
  (make-html-token-reader config))

;; make-html-derived-lexer : -> (input-port? -> (or/c html-derived-token? 'eof))
;;   Construct a port-based HTML lexer that returns derived HTML token values.
(define (make-html-derived-lexer)
  (private-make-html-derived-reader))

;; html-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire HTML string using the HTML lexer.
(define (html-string->tokens source
                             #:profile          [profile 'coloring]
                             #:trivia           [trivia 'profile-default]
                             #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-html-lexer #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (cond
      [(eof-token? token) (reverse (cons token tokens))]
      [else               (loop (cons token tokens))])))

;; html-string->derived-tokens : string? -> (listof html-derived-token?)
;;   Tokenize an entire HTML string into derived HTML token values.
(define (html-string->derived-tokens source)
  (define lexer (make-html-derived-lexer))
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
  (require rackunit
           racket/list)

  (define basic-tokens
    (html-string->tokens "<section id=main class=\"card\">Hi</section>"
                         #:profile 'coloring
                         #:source-positions #f))
  (define comment-tokens
    (html-string->tokens "<!-- note -->" #:profile 'coloring #:source-positions #f))
  (define doctype-tokens
    (html-string->tokens "<!doctype html>" #:profile 'compiler #:source-positions #f))
  (define entity-tokens
    (html-string->tokens "Hi &amp; bye" #:profile 'compiler #:source-positions #f))
  (define broken-tag-tokens
    (html-string->tokens "<div class=\"unterminated\ntext" #:profile 'coloring #:source-positions #f))
  (define style-tokens
    (html-string->tokens "<style>.hero { color: #c33; }</style>"
                         #:profile 'compiler
                         #:source-positions #f))
  (define script-tokens
    (html-string->tokens "<script>const msg = `</script not closed`; // ok\nx = 1;</script>"
                         #:profile 'compiler
                         #:source-positions #f))
  (define mixed-tokens
    (html-string->tokens "<!doctype html><main id=\"app\">Hi &amp; bye<style>.x { color: #fff; }</style><script>const root = document.querySelector(\"#app\");</script><!-- note --></main>"
                         #:profile 'coloring
                         #:source-positions #f))

  (define derived-tokens
    (html-string->derived-tokens
     "<!doctype html><section id=main class=\"card\">Hi &amp; bye<style>.hero { color: #c33; }</style><script>const msg = `</script not closed`; const x = 1;</script><!-- note --></section>"))
  (define (find-derived-token tag)
    (findf (lambda (token)
             (html-derived-token-has-tag? token tag))
           derived-tokens))
  (define derived-tag-token
    (findf (lambda (token)
             (and (html-derived-token-has-tag? token 'html-tag-name)
                  (string=? (html-derived-token-text token) "section")))
           derived-tokens))
  (define derived-closing-tag-token
    (findf (lambda (token)
             (and (html-derived-token-has-tag? token 'html-closing-tag-name)
                  (string=? (html-derived-token-text token) "section")))
           derived-tokens))
  (define derived-attribute-token
    (findf (lambda (token)
             (and (html-derived-token-has-tag? token 'html-attribute-name)
                  (string=? (html-derived-token-text token) "class")))
           derived-tokens))
  (define derived-attribute-value-token
    (findf (lambda (token)
             (and (html-derived-token-has-tag? token 'html-attribute-value)
                  (string=? (html-derived-token-text token) "\"card\"")))
           derived-tokens))
  (define derived-text-token
    (findf (lambda (token)
             (and (html-derived-token-has-tag? token 'html-text)
                  (string=? (html-derived-token-text token) "Hi ")))
           derived-tokens))
  (define derived-entity-token
    (findf (lambda (token)
             (html-derived-token-has-tag? token 'html-entity))
           derived-tokens))
  (define derived-doctype-token
    (find-derived-token 'html-doctype))
  (define derived-comment-token
    (find-derived-token 'comment))
  (define derived-css-token
    (findf (lambda (token)
             (and (html-derived-token-has-tag? token 'embedded-css)
                  (html-derived-token-has-tag? token 'color-literal)))
           derived-tokens))
  (define derived-js-token
    (findf (lambda (token)
             (and (html-derived-token-has-tag? token 'embedded-javascript)
                  (html-derived-token-has-tag? token 'template-literal)))
           derived-tokens))
  (define derived-js-keyword-token
    (findf (lambda (token)
             (and (html-derived-token-has-tag? token 'embedded-javascript)
                  (html-derived-token-has-tag? token 'keyword)
                  (string=? (html-derived-token-text token) "const")))
           derived-tokens))

  (check-equal? (map stream-token-name basic-tokens)
                '(delimiter identifier whitespace identifier operator literal
                  whitespace identifier operator literal delimiter literal
                  delimiter identifier delimiter eof))
  (check-equal? (map stream-token-name comment-tokens)
                '(comment eof))
  (check-equal? (stream-token-name (car doctype-tokens)) 'keyword)
  (check-equal? (map stream-token-name entity-tokens)
                '(literal literal literal eof))
  (check-equal? (stream-token-name (car broken-tag-tokens)) 'delimiter)
  (check-equal? (stream-token-name (caddr broken-tag-tokens)) 'whitespace)
  (check-equal? (stream-token-name (findf (lambda (token)
                                            (equal? (stream-token-value token) "hero"))
                                          style-tokens))
                'identifier)
  (check-equal? (stream-token-name (findf (lambda (token)
                                            (equal? (stream-token-value token) "#c33"))
                                          style-tokens))
                'literal)
  (check-equal? (stream-token-name (findf (lambda (token)
                                            (equal? (stream-token-value token) "const"))
                                          script-tokens))
                'keyword)
  (check-equal? (stream-token-name (findf (lambda (token)
                                            (equal? (stream-token-value token) "</script not closed"))
                                          script-tokens))
                'literal)
  (check-equal? (stream-token-name (last mixed-tokens)) 'eof)
  (check-not-false derived-tag-token)
  (check-not-false derived-closing-tag-token)
  (check-not-false derived-attribute-token)
  (check-not-false derived-attribute-value-token)
  (check-not-false derived-text-token)
  (check-not-false derived-entity-token)
  (check-not-false derived-doctype-token)
  (check-not-false derived-comment-token)
  (check-not-false derived-css-token)
  (check-not-false derived-js-token)
  (check-not-false derived-js-keyword-token)
  (check-equal? (position-offset (html-derived-token-start derived-doctype-token))
                1)
  (check-true (< (position-offset (html-derived-token-start derived-css-token))
                 (position-offset (html-derived-token-end derived-css-token)))))
