#lang racket/base

;;;
;;; Python Lexer
;;;
;;
;; Public entry points for the Python lexer.

;; make-python-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Python lexer.
;; make-python-derived-lexer : -> (input-port? -> (or/c python-derived-token? 'eof))
;;   Construct a port-based Python lexer that returns derived Python token values.
;; python-derived-token?     : any/c -> boolean?
;;   Recognize a derived Python token value returned by the derived-token API.
;; python-derived-token-tags : python-derived-token? -> (listof symbol?)
;;   Extract the Python-specific classification tags for one derived token.
;; python-derived-token-has-tag? : python-derived-token? symbol? -> boolean?
;;   Determine whether a derived Python token has a given classification tag.
;; python-derived-token-text : python-derived-token? -> string?
;;   Extract the source text corresponding to one derived Python token.
;; python-derived-token-start : python-derived-token? -> position?
;;   Extract the starting source position for one derived Python token.
;; python-derived-token-end  : python-derived-token? -> position?
;;   Extract the ending source position for one derived Python token.
;; python-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Python string using the Python lexer.
;; python-string->derived-tokens : string? -> (listof python-derived-token?)
;;   Tokenize an entire Python string into derived Python token values.
;; python-profiles           : immutable-hash?
;;   Profile defaults for the public Python lexer.

(provide make-python-lexer
         make-python-derived-lexer
         python-derived-token?
         python-derived-token-tags
         python-derived-token-has-tag?
         python-derived-token-text
         python-derived-token-start
         python-derived-token-end
         python-string->tokens
         python-string->derived-tokens
         python-profiles)

(require parser-tools/lex
         racket/list
         racket/string
         "private/config.rkt"
         "private/parser-tools-compat.rkt"
         (rename-in "private/python-derived.rkt"
                    [python-derived-token? private-python-derived-token?]
                    [python-derived-token-tags private-python-derived-token-tags]
                    [python-derived-token-has-tag? private-python-derived-token-has-tag?]
                    [python-derived-token-text private-python-derived-token-text]
                    [python-derived-token-start private-python-derived-token-start]
                    [python-derived-token-end private-python-derived-token-end]
                    [make-python-derived-reader private-make-python-derived-reader])
         "private/python-tokenize.rkt"
         "token.rkt")

(define python-profiles
  python-profile-defaults)

;; python-derived-token? : any/c -> boolean?
;;   Recognize a derived Python token value returned by the derived-token API.
(define (python-derived-token? v)
  (private-python-derived-token? v))

;; python-derived-token-tags : python-derived-token? -> (listof symbol?)
;;   Extract the Python-specific classification tags for one derived token.
(define (python-derived-token-tags token)
  (private-python-derived-token-tags token))

;; python-derived-token-has-tag? : python-derived-token? symbol? -> boolean?
;;   Determine whether a derived Python token has a given classification tag.
(define (python-derived-token-has-tag? token tag)
  (private-python-derived-token-has-tag? token tag))

;; python-derived-token-text : python-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
(define (python-derived-token-text token)
  (private-python-derived-token-text token))

;; python-derived-token-start : python-derived-token? -> position?
;;   Extract the starting source position for one derived token.
(define (python-derived-token-start token)
  (private-python-derived-token-start token))

;; python-derived-token-end : python-derived-token? -> position?
;;   Extract the ending source position for one derived token.
(define (python-derived-token-end token)
  (private-python-derived-token-end token))

;; make-python-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Python lexer.
(define (make-python-lexer #:profile          [profile 'coloring]
                           #:trivia           [trivia 'profile-default]
                           #:source-positions [source-positions 'profile-default])
  (define config
    (make-python-config #:profile          profile
                        #:trivia           trivia
                        #:source-positions source-positions))
  (make-python-token-reader config))

;; make-python-derived-lexer : -> (input-port? -> (or/c python-derived-token? 'eof))
;;   Construct a port-based Python lexer that returns derived token values.
(define (make-python-derived-lexer)
  (private-make-python-derived-reader))

;; python-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Python string using the projected token API.
(define (python-string->tokens source
                               #:profile          [profile 'coloring]
                               #:trivia           [trivia 'profile-default]
                               #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-python-lexer #:profile          profile
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

;; python-string->derived-tokens : string? -> (listof python-derived-token?)
;;   Tokenize an entire Python string into derived Python token values.
(define (python-string->derived-tokens source)
  (define lexer
    (make-python-derived-lexer))
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

  ;; contiguous-derived-stream? : (listof python-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (python-derived-token-end left))
         (position-offset (python-derived-token-start right)))))

  ;; first-token-before-rest? : (-> any) string? string? -> any
  ;;   Read the first token before the second chunk is written.
  (define (first-token-before-rest? make-lexer first-chunk rest-chunk)
    (define lexer
      (make-lexer))
    (define-values (in out)
      (make-pipe))
    (write-string first-chunk out)
    (flush-output out)
    (define result-channel
      (make-channel))
    (thread (lambda ()
              (channel-put result-channel (lexer in))))
    (define token
      (sync/timeout 1 result-channel))
    (write-string rest-chunk out)
    (close-output-port out)
    token)

  (define sample-source
    "def answer(x):\n    if x:\n        return 42\n    return None\n")
  (define sample-tokens
    (python-string->tokens sample-source
                           #:profile 'coloring
                           #:source-positions #f))
  (define compiler-tokens
    (python-string->tokens sample-source
                           #:profile 'compiler
                           #:source-positions #f))
  (define sample-derived
    (python-string->derived-tokens sample-source))
  (define triple-source
    "doc = \"\"\"hello\nworld\"\"\"\n")
  (define triple-derived
    (python-string->derived-tokens triple-source))
  (define joined-source
    "value = 1 + \\\n  2\n")
  (define joined-derived
    (python-string->derived-tokens joined-source))
  (define bracket-source
    "items = [\n    1,\n    2,\n]\n")
  (define bracket-derived
    (python-string->derived-tokens bracket-source))
  (define malformed-coloring
    (python-string->tokens "\"unterminated"
                           #:profile 'coloring
                           #:source-positions #f))
  (define malformed-compiler-thunk
    (lambda ()
      (python-string->tokens "\"unterminated"
                             #:profile 'compiler
                             #:source-positions #f)))
  (define prefixed-derived
    (python-string->derived-tokens
     "fr\"value {x}\"\nrb\"bytes\"\nt\"tmpl {x}\"\nbf\"oops\"\n"))
  (define crlf-source
    "def answer():\r\n    return 42\r\n")
  (define crlf-derived
    (python-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-python-derived-lexer
                              "def answer"
                              "(x):\n    return 1\n"))

  (define keyword-token
    (findf (lambda (token)
             (python-derived-token-has-tag? token 'python-keyword))
           sample-derived))
  (define indent-token
    (findf (lambda (token)
             (python-derived-token-has-tag? token 'python-indent))
           sample-derived))
  (define dedent-token
    (findf (lambda (token)
             (python-derived-token-has-tag? token 'python-dedent))
           sample-derived))
  (define number-token
    (findf (lambda (token)
             (python-derived-token-has-tag? token 'python-numeric-literal))
           sample-derived))
  (define triple-string-token
    (findf (lambda (token)
             (python-derived-token-has-tag? token 'python-string-literal))
           triple-derived))
  (define line-join-token
    (findf (lambda (token)
             (python-derived-token-has-tag? token 'python-line-join))
           joined-derived))
  (define nl-token
    (findf (lambda (token)
             (python-derived-token-has-tag? token 'python-nl))
           bracket-derived))
  (define formatted-string-token
    (findf (lambda (token)
             (python-derived-token-has-tag? token 'python-f-string-literal))
           prefixed-derived))
  (define raw-bytes-token
    (findf (lambda (token)
             (python-derived-token-has-tag? token 'python-bytes-literal))
           prefixed-derived))
  (define template-string-token
    (findf (lambda (token)
             (python-derived-token-has-tag? token 'python-t-string-literal))
           prefixed-derived))
  (define invalid-prefix-identifier
    (findf (lambda (token)
             (and (python-derived-token-has-tag? token 'python-identifier)
                  (string=? (python-derived-token-text token) "bf")))
           prefixed-derived))
  (define invalid-prefix-string
    (findf (lambda (token)
             (and (python-derived-token-has-tag? token 'python-string-literal)
                  (string=? (python-derived-token-text token) "\"oops\"")))
           prefixed-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 8)
                '(keyword whitespace identifier delimiter identifier delimiter delimiter whitespace))
  (check-equal? (map lexer-token-name compiler-tokens)
                '(keyword identifier delimiter identifier delimiter delimiter delimiter keyword identifier delimiter delimiter keyword literal delimiter keyword keyword delimiter eof))
  (check-not-false keyword-token)
  (check-not-false indent-token)
  (check-not-false dedent-token)
  (check-not-false number-token)
  (check-not-false triple-string-token)
  (check-not-false line-join-token)
  (check-not-false nl-token)
  (check-not-false formatted-string-token)
  (check-not-false raw-bytes-token)
  (check-not-false template-string-token)
  (check-not-false invalid-prefix-identifier)
  (check-not-false invalid-prefix-string)
  (check-not-false (python-derived-token-has-tag? formatted-string-token 'python-string-literal))
  (check-not-false (python-derived-token-has-tag? formatted-string-token 'python-raw-string-literal))
  (check-not-false (python-derived-token-has-tag? raw-bytes-token 'python-raw-string-literal))
  (check-not-false first-streaming-token)
  (check-not-false (python-derived-token-has-tag? first-streaming-token
                                                  'python-keyword))
  (check-equal? (python-derived-token-text triple-string-token)
                "\"\"\"hello\nworld\"\"\"")
  (check-true (contiguous-derived-stream? sample-derived))
  (check-true (contiguous-derived-stream? triple-derived))
  (check-equal? (apply string-append
                       (map python-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append
                       (map python-derived-token-text crlf-derived))
                crlf-source)
  (check-equal? (map lexer-token-name malformed-coloring)
                '(unknown eof))
  (check-exn exn:fail:read?
             malformed-compiler-thunk))
