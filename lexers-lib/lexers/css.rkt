#lang racket/base

;;;
;;; CSS Lexer
;;;
;;
;; Public entry points for the CSS lexer.

;; make-css-lexer      : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based CSS lexer.
;; make-css-raw-lexer  : -> (input-port? -> (or/c css-raw-token? 'eof))
;;   Construct a port-based CSS lexer that returns raw CSS token values.
;; make-css-derived-lexer : -> (input-port? -> (or/c css-derived-token? 'eof))
;;   Construct a port-based CSS lexer that returns derived CSS token values.
;; css-raw-token? : any/c -> boolean?
;;   Recognize a raw CSS token value returned by the raw-token API.
;; css-raw-token-kind : css-raw-token? -> symbol?
;;   Extract the CSS Syntax-oriented raw token kind for one raw token.
;; css-raw-token-text : css-raw-token? -> string?
;;   Extract the exact source text corresponding to one raw CSS token.
;; css-raw-token-start : css-raw-token? -> position?
;;   Extract the starting source position for one raw CSS token.
;; css-raw-token-end : css-raw-token? -> position?
;;   Extract the ending source position for one raw CSS token.
;; css-derived-token? : any/c -> boolean?
;;   Recognize a derived CSS token value returned by the derived-token API.
;; css-derived-token-raw : css-derived-token? -> css-raw-token?
;;   Extract the raw CSS token wrapped by one derived CSS token.
;; css-derived-token-raw-kind : css-derived-token? -> symbol?
;;   Extract the CSS Syntax-oriented raw token kind from one derived CSS token.
;; css-derived-token-tags : css-derived-token? -> (listof symbol?)
;;   Extract the CSS-specific classification tags for one derived token.
;; css-derived-token-has-tag? : css-derived-token? symbol? -> boolean?
;;   Determine whether a derived CSS token has a given classification tag.
;; css-derived-token-text : css-derived-token? -> string?
;;   Extract the source text corresponding to one derived CSS token.
;; css-derived-token-start : css-derived-token? -> position?
;;   Extract the starting source position for one derived CSS token.
;; css-derived-token-end : css-derived-token? -> position?
;;   Extract the ending source position for one derived CSS token.
;; css-string->tokens  : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire CSS string using the CSS lexer.
;; css-string->raw-tokens : string? -> (listof css-raw-token?)
;;   Tokenize an entire CSS string into raw CSS token values.
;; css-string->derived-tokens : string? -> (listof css-derived-token?)
;;   Tokenize an entire CSS string into derived CSS token values.
;; css-profiles        : immutable-hash?
;;   Profile defaults for the public CSS lexer.

(provide make-css-lexer
         make-css-raw-lexer
         make-css-derived-lexer
         css-raw-token?
         css-raw-token-kind
         css-raw-token-text
         css-raw-token-start
         css-raw-token-end
         css-derived-token?
         css-derived-token-raw
         css-derived-token-raw-kind
         css-derived-token-tags
         css-derived-token-has-tag?
         css-derived-token-text
         css-derived-token-start
         css-derived-token-end
         css-string->tokens
         css-string->raw-tokens
         css-string->derived-tokens
         css-profiles)

(require parser-tools/lex
         "private/config.rkt"
         (rename-in "private/css-derived.rkt"
                    [css-derived-token? private-css-derived-token?]
                    [css-derived-token-raw private-css-derived-token-raw]
                    [css-derived-token-tags private-css-derived-token-tags]
                    [make-css-derived-classifier private-make-css-derived-classifier]
                    [css-derived-token-has-tag? private-css-derived-token-has-tag?])
         (rename-in "private/css-raw.rkt"
                    [css-raw-token? private-css-raw-token?]
                    [css-raw-token-kind private-css-raw-token-kind]
                    [css-raw-token-text private-css-raw-token-text]
                    [css-raw-token-start private-css-raw-token-start]
                    [css-raw-token-end private-css-raw-token-end]
                    [read-css-raw-token private-read-css-raw-token])
         "private/css-tokenize.rkt"
         "private/parser-tools-compat.rkt")

(define css-profiles css-profile-defaults)

;; css-raw-token? : any/c -> boolean?
;;   Recognize a raw CSS token value returned by the raw-token API.
(define (css-raw-token? v)
  (private-css-raw-token? v))

;; css-raw-token-kind : css-raw-token? -> symbol?
;;   Extract the CSS Syntax-oriented raw token kind for one raw token.
(define (css-raw-token-kind token)
  (private-css-raw-token-kind token))

;; css-raw-token-text : css-raw-token? -> string?
;;   Extract the exact source text corresponding to one raw CSS token.
(define (css-raw-token-text token)
  (private-css-raw-token-text token))

;; css-raw-token-start : css-raw-token? -> position?
;;   Extract the starting source position for one raw CSS token.
(define (css-raw-token-start token)
  (private-css-raw-token-start token))

;; css-raw-token-end : css-raw-token? -> position?
;;   Extract the ending source position for one raw CSS token.
(define (css-raw-token-end token)
  (private-css-raw-token-end token))

;; css-derived-token? : any/c -> boolean?
;;   Recognize a derived CSS token value returned by the derived-token API.
(define (css-derived-token? v)
  (private-css-derived-token? v))

;; css-derived-token-raw : css-derived-token? -> css-raw-token?
;;   Extract the raw CSS token wrapped by one derived CSS token.
(define (css-derived-token-raw token)
  (private-css-derived-token-raw token))

;; css-derived-token-raw-kind : css-derived-token? -> symbol?
;;   Extract the CSS Syntax-oriented raw token kind from one derived CSS token.
(define (css-derived-token-raw-kind token)
  (css-raw-token-kind (css-derived-token-raw token)))

;; css-derived-token-tags : css-derived-token? -> (listof symbol?)
;;   Extract the CSS-specific classification tags for one derived token.
(define (css-derived-token-tags token)
  (private-css-derived-token-tags token))

;; css-derived-token-has-tag? : css-derived-token? symbol? -> boolean?
;;   Determine whether a derived CSS token has a given classification tag.
(define (css-derived-token-has-tag? token tag)
  (private-css-derived-token-has-tag? token tag))

;; css-derived-token-text : css-derived-token? -> string?
;;   Extract the source text corresponding to one derived CSS token.
(define (css-derived-token-text token)
  (css-raw-token-text (css-derived-token-raw token)))

;; css-derived-token-start : css-derived-token? -> position?
;;   Extract the starting source position for one derived CSS token.
(define (css-derived-token-start token)
  (css-raw-token-start (css-derived-token-raw token)))

;; css-derived-token-end : css-derived-token? -> position?
;;   Extract the ending source position for one derived CSS token.
(define (css-derived-token-end token)
  (css-raw-token-end (css-derived-token-raw token)))

;; make-css-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based CSS lexer.
(define (make-css-lexer #:profile          [profile 'coloring]
                        #:trivia           [trivia 'profile-default]
                        #:source-positions [source-positions 'profile-default])
  (define config
    (make-css-config #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions))
  (make-css-token-reader config))

;; make-css-raw-lexer : -> (input-port? -> (or/c css-raw-token? 'eof))
;;   Construct a port-based CSS lexer that returns raw CSS token values.
(define (make-css-raw-lexer)
  private-read-css-raw-token)

;; make-css-derived-lexer : -> (input-port? -> (or/c css-derived-token? 'eof))
;;   Construct a port-based CSS lexer that returns derived CSS token values.
(define (make-css-derived-lexer)
  (define classify-css-token
    (private-make-css-derived-classifier))
  (lambda (in)
    (define raw-token (private-read-css-raw-token in))
    (cond
      [(eq? raw-token 'eof) 'eof]
      [else                 (classify-css-token raw-token)])))

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

;; css-string->raw-tokens : string? -> (listof css-raw-token?)
;;   Tokenize an entire CSS string into raw CSS token values.
(define (css-string->raw-tokens source)
  (define lexer
    (make-css-raw-lexer))
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
  (define quoted-url-tokens
    (css-string->tokens "url(\"foo.png\")" #:profile 'coloring #:source-positions #f))
  (define bad-url-tokens
    (css-string->tokens "url(foo.png" #:profile 'coloring))
  (define spaced-bad-url-tokens
    (css-string->tokens "url(foo bar)" #:profile 'coloring))
  (define escaped-url-paren-tokens
    (css-string->tokens "url(foo\\)bar)" #:profile 'coloring))
  (define bad-string-tokens
    (css-string->tokens "\"unterminated" #:profile 'coloring))
  (define bad-newline-string-tokens
    (css-string->tokens "\"line\nbreak\"" #:profile 'coloring))
  (define escaped-newline-string-tokens
    (css-string->tokens "\"line\\\nbreak\"" #:profile 'coloring))
  (define no-position-tokens
    (css-string->tokens "color" #:profile 'coloring #:source-positions #f))
  (define compiler-no-trivia-tokens
    (css-string->tokens "   color" #:profile 'compiler))
  (define coloring-with-trivia-tokens
    (css-string->tokens "   color" #:profile 'coloring))
  (define delim-tokens
    (css-string->tokens "~" #:profile 'compiler))
  (define hash-ident-tokens
    (css-string->tokens "#main" #:profile 'compiler #:source-positions #f))
  (define leading-dot-number-tokens
    (css-string->tokens ".5em" #:profile 'compiler #:source-positions #f))
  (define cdo-tokens
    (css-string->tokens "<!--" #:profile 'compiler #:source-positions #f))
  (define cdc-tokens
    (css-string->tokens "-->" #:profile 'compiler #:source-positions #f))
  (define include-match-tokens
    (css-string->tokens "~=" #:profile 'compiler #:source-positions #f))
  (define dash-match-tokens
    (css-string->tokens "|=" #:profile 'compiler #:source-positions #f))
  (define prefix-match-tokens
    (css-string->tokens "^=" #:profile 'compiler #:source-positions #f))
  (define suffix-match-tokens
    (css-string->tokens "$=" #:profile 'compiler #:source-positions #f))
  (define substring-match-tokens
    (css-string->tokens "*=" #:profile 'compiler #:source-positions #f))
  (define escaped-ident-tokens
    (css-string->tokens "\\66 oo" #:profile 'compiler #:source-positions #f))
  (define escaped-function-tokens
    (css-string->tokens "r\\67 b(" #:profile 'compiler #:source-positions #f))
  (define unicode-range-tokens
    (css-string->tokens "U+00A0-00FF" #:profile 'compiler #:source-positions #f))
  (define wildcard-unicode-range-tokens
    (css-string->tokens "u+4??" #:profile 'compiler #:source-positions #f))
  (define unicode-range-near-miss-tokens
    (css-string->tokens "u+" #:profile 'compiler #:source-positions #f))
  (define raw-tokens
    (css-string->raw-tokens "@media color 12px"))
  (define derived-lexer
    (make-css-derived-lexer))
  (define derived-tokens
    (css-string->derived-tokens "@media #fff rgb( --brand-color \"hi\" 12px 10% 42 U+00A0-00FF #main"))
  (define (find-derived-token tag)
    (findf (lambda (token)
             (css-derived-token-has-tag? token tag))
           derived-tokens))
  (define derived-at-rule-token
    (find-derived-token 'at-rule-name))
  (define derived-color-token
    (find-derived-token 'color-literal))
  (define derived-function-token
    (find-derived-token 'color-function))
  (define derived-custom-property-token
    (find-derived-token 'custom-property-name))
  (define derived-string-token
    (find-derived-token 'string-literal))
  (define derived-numeric-token
    (find-derived-token 'numeric-literal))
  (define contextual-derived-tokens
    (css-string->derived-tokens ".foo { color: red; }"))
  (define contextual-selector-token
    (findf (lambda (token)
             (and (css-derived-token-has-tag? token 'selector-token)
                  (string=? (css-derived-token-text token) "foo")))
           contextual-derived-tokens))
  (define contextual-property-token
    (findf (lambda (token)
             (and (css-derived-token-has-tag? token 'property-name)
                  (string=? (css-derived-token-text token) "color")))
           contextual-derived-tokens))
  (define contextual-value-token
    (findf (lambda (token)
             (and (css-derived-token-has-tag? token 'declaration-value-token)
                  (string=? (css-derived-token-text token) "red")))
           contextual-derived-tokens))
  (define derived-non-color-hash-token
    (findf (lambda (token)
             (string=? (css-derived-token-text token)
                       "#main"))
           derived-tokens))
  (define raw-at-token
    (car raw-tokens))
  (define raw-ident-token
    (caddr raw-tokens))
  (define raw-dimension-token
    (list-ref raw-tokens 4))

  (check-true (pair? coloring-tokens))
  (check-true (css-raw-token? raw-at-token))
  (check-equal? (css-raw-token-kind raw-at-token) 'at-keyword-token)
  (check-equal? (css-raw-token-text raw-ident-token) "color")
  (check-equal? (css-raw-token-kind raw-dimension-token) 'dimension-token)
  (check-true (position-token? (car coloring-tokens)))
  (check-equal? (stream-token-name (car coloring-tokens)) 'comment)
  (check-equal? (stream-token-name (car compiler-tokens)) 'identifier)
  (check-equal? (stream-token-name (car function-tokens)) 'literal)
  (check-equal? (stream-token-name (car url-tokens)) 'literal)
  (check-equal? (stream-token-name (car quoted-url-tokens)) 'literal)
  (check-equal? (stream-token-value (car quoted-url-tokens)) "url")
  (check-equal? (stream-token-name (cadr quoted-url-tokens)) 'delimiter)
  (check-equal? (stream-token-value (cadr quoted-url-tokens)) "(")
  (check-equal? (stream-token-name (car bad-url-tokens)) 'unknown)
  (check-equal? (stream-token-name (car spaced-bad-url-tokens)) 'unknown)
  (check-equal? (stream-token-name (car escaped-url-paren-tokens)) 'literal)
  (check-equal? (stream-token-value (car escaped-url-paren-tokens)) "url(foo\\)bar)")
  (check-equal? (stream-token-name (car bad-string-tokens)) 'unknown)
  (check-equal? (stream-token-name (car bad-newline-string-tokens)) 'unknown)
  (check-equal? (stream-token-name (car escaped-newline-string-tokens)) 'literal)
  (check-false (position-token? (car no-position-tokens)))
  (check-equal? (stream-token-name (car compiler-no-trivia-tokens)) 'identifier)
  (check-equal? (stream-token-name (car coloring-with-trivia-tokens)) 'whitespace)
  (check-equal? (stream-token-name (car delim-tokens)) 'delimiter)
  (check-equal? (stream-token-name (car hash-ident-tokens)) 'literal)
  (check-equal? (stream-token-value (car hash-ident-tokens)) "#main")
  (check-equal? (stream-token-name (car leading-dot-number-tokens)) 'literal)
  (check-equal? (stream-token-value (car leading-dot-number-tokens)) ".5em")
  (check-equal? (stream-token-name (car cdo-tokens)) 'delimiter)
  (check-equal? (stream-token-value (car cdo-tokens)) "<!--")
  (check-equal? (stream-token-name (car cdc-tokens)) 'delimiter)
  (check-equal? (stream-token-value (car cdc-tokens)) "-->")
  (check-equal? (stream-token-name (car include-match-tokens)) 'delimiter)
  (check-equal? (stream-token-value (car include-match-tokens)) "~=")
  (check-equal? (stream-token-name (car dash-match-tokens)) 'delimiter)
  (check-equal? (stream-token-value (car dash-match-tokens)) "|=")
  (check-equal? (stream-token-name (car prefix-match-tokens)) 'delimiter)
  (check-equal? (stream-token-value (car prefix-match-tokens)) "^=")
  (check-equal? (stream-token-name (car suffix-match-tokens)) 'delimiter)
  (check-equal? (stream-token-value (car suffix-match-tokens)) "$=")
  (check-equal? (stream-token-name (car substring-match-tokens)) 'delimiter)
  (check-equal? (stream-token-value (car substring-match-tokens)) "*=")
  (check-equal? (stream-token-name (car escaped-ident-tokens)) 'identifier)
  (check-equal? (stream-token-value (car escaped-ident-tokens)) "\\66 oo")
  (check-equal? (stream-token-name (car escaped-function-tokens)) 'literal)
  (check-equal? (stream-token-value (car escaped-function-tokens)) "r\\67 b")
  (check-equal? (stream-token-name (car unicode-range-tokens)) 'literal)
  (check-equal? (stream-token-value (car unicode-range-tokens)) "U+00A0-00FF")
  (check-equal? (stream-token-name (car wildcard-unicode-range-tokens)) 'literal)
  (check-equal? (stream-token-value (car wildcard-unicode-range-tokens)) "u+4??")
  (check-equal? (stream-token-name (car unicode-range-near-miss-tokens)) 'identifier)
  (check-equal? (stream-token-value (car unicode-range-near-miss-tokens)) "u")
  (check-exn exn:fail:read?
             (lambda ()
               (css-string->tokens "\"unterminated" #:profile 'compiler)))
  (check-exn exn:fail:read?
             (lambda ()
               (css-string->tokens "\"line\nbreak\"" #:profile 'compiler)))
  (check-exn exn:fail:read?
             (lambda ()
               (css-string->tokens "url(foo.png" #:profile 'compiler)))
  (check-exn exn:fail:read?
             (lambda ()
               (css-string->tokens "url(foo bar)" #:profile 'compiler)))
  (check-false (eq? (derived-lexer (open-input-string "#fff")) 'eof))
  (check-not-false (css-derived-token-has-tag? derived-at-rule-token 'at-rule-name))
  (check-equal? (css-derived-token-text derived-at-rule-token)
                "@media")
  (check-equal? (css-derived-token-raw-kind derived-at-rule-token)
                'at-keyword-token)
  (check-not-false (css-derived-token-has-tag? derived-color-token 'color-literal))
  (check-not-false
   (member 'color-literal
           (css-derived-token-tags derived-color-token)))
  (check-equal? (css-derived-token-text derived-color-token)
                "#fff")
  (check-equal? (position-offset (css-derived-token-start derived-color-token))
                8)
  (check-equal? (position-offset (css-derived-token-end derived-color-token))
                12)
  (check-not-false (css-derived-token-has-tag? derived-function-token 'color-function))
  (check-not-false (css-derived-token-has-tag? derived-function-token 'function-name))
  (check-not-false (css-derived-token-has-tag? derived-custom-property-token 'custom-property-name))
  (check-not-false (css-derived-token-has-tag? derived-string-token 'string-literal))
  (check-not-false (css-derived-token-has-tag? derived-numeric-token 'numeric-literal))
  (check-not-false contextual-selector-token)
  (check-not-false contextual-property-token)
  (check-not-false contextual-value-token)
  (check-not-false derived-non-color-hash-token)
  (check-false
   (css-derived-token-has-tag? derived-non-color-hash-token 'color-literal)))
