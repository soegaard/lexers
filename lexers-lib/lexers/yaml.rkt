#lang racket/base

;;;
;;; YAML Lexer
;;;
;;
;; Public entry points for the YAML lexer.

;; make-yaml-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based YAML lexer.
;; make-yaml-derived-lexer : -> (input-port? -> (or/c yaml-derived-token? 'eof))
;;   Construct a port-based YAML lexer that returns derived YAML token values.
;; yaml-derived-token?     : any/c -> boolean?
;;   Recognize a derived YAML token value returned by the derived-token API.
;; yaml-derived-token-tags : yaml-derived-token? -> (listof symbol?)
;;   Extract the YAML-specific classification tags for one derived token.
;; yaml-derived-token-has-tag? : yaml-derived-token? symbol? -> boolean?
;;   Determine whether a derived YAML token has a given classification tag.
;; yaml-derived-token-text : yaml-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; yaml-derived-token-start : yaml-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; yaml-derived-token-end  : yaml-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; yaml-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire YAML string using the YAML lexer.
;; yaml-string->derived-tokens : string? -> (listof yaml-derived-token?)
;;   Tokenize an entire YAML string into derived YAML token values.
;; yaml-profiles           : immutable-hash?
;;   Profile defaults for the public YAML lexer.

(provide make-yaml-lexer
         make-yaml-derived-lexer
         yaml-derived-token?
         yaml-derived-token-tags
         yaml-derived-token-has-tag?
         yaml-derived-token-text
         yaml-derived-token-start
         yaml-derived-token-end
         yaml-string->tokens
         yaml-string->derived-tokens
         yaml-profiles)

(require parser-tools/lex
         racket/list
         racket/string
         "private/config.rkt"
         "private/parser-tools-compat.rkt"
         (rename-in "private/yaml-derived.rkt"
                    [yaml-derived-token? private-yaml-derived-token?]
                    [yaml-derived-token-tags private-yaml-derived-token-tags]
                    [yaml-derived-token-has-tag? private-yaml-derived-token-has-tag?]
                    [yaml-derived-token-text private-yaml-derived-token-text]
                    [yaml-derived-token-start private-yaml-derived-token-start]
                    [yaml-derived-token-end private-yaml-derived-token-end]
                    [make-yaml-derived-reader private-make-yaml-derived-reader])
         "private/yaml-tokenize.rkt"
         "token.rkt")

(define yaml-profiles
  yaml-profile-defaults)

;; yaml-derived-token? : any/c -> boolean?
;;   Recognize a derived YAML token value returned by the derived-token API.
(define (yaml-derived-token? v)
  (private-yaml-derived-token? v))

;; yaml-derived-token-tags : yaml-derived-token? -> (listof symbol?)
;;   Extract the YAML-specific classification tags for one derived token.
(define (yaml-derived-token-tags token)
  (private-yaml-derived-token-tags token))

;; yaml-derived-token-has-tag? : yaml-derived-token? symbol? -> boolean?
;;   Determine whether a derived YAML token has a given classification tag.
(define (yaml-derived-token-has-tag? token tag)
  (private-yaml-derived-token-has-tag? token tag))

;; yaml-derived-token-text : yaml-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
(define (yaml-derived-token-text token)
  (private-yaml-derived-token-text token))

;; yaml-derived-token-start : yaml-derived-token? -> position?
;;   Extract the starting source position for one derived token.
(define (yaml-derived-token-start token)
  (private-yaml-derived-token-start token))

;; yaml-derived-token-end : yaml-derived-token? -> position?
;;   Extract the ending source position for one derived token.
(define (yaml-derived-token-end token)
  (private-yaml-derived-token-end token))

;; make-yaml-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based YAML lexer.
(define (make-yaml-lexer #:profile          [profile 'coloring]
                         #:trivia           [trivia 'profile-default]
                         #:source-positions [source-positions 'profile-default])
  (define config
    (make-yaml-config #:profile          profile
                      #:trivia           trivia
                      #:source-positions source-positions))
  (make-yaml-token-reader config))

;; make-yaml-derived-lexer : -> (input-port? -> (or/c yaml-derived-token? 'eof))
;;   Construct a port-based YAML lexer that returns derived token values.
(define (make-yaml-derived-lexer)
  (private-make-yaml-derived-reader))

;; yaml-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire YAML string using the projected token API.
(define (yaml-string->tokens source
                             #:profile          [profile 'coloring]
                             #:trivia           [trivia 'profile-default]
                             #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-yaml-lexer #:profile          profile
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

;; yaml-string->derived-tokens : string? -> (listof yaml-derived-token?)
;;   Tokenize an entire YAML string into derived YAML token values.
(define (yaml-string->derived-tokens source)
  (define lexer
    (make-yaml-derived-lexer))
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

  ;; contiguous-derived-stream? : (listof yaml-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (yaml-derived-token-end left))
         (position-offset (yaml-derived-token-start right)))))

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
    (thread
     (lambda ()
       (channel-put result-channel (lexer in))))
    (define token
      (sync/timeout 1 result-channel))
    (write-string rest-chunk out)
    (close-output-port out)
    token)

  (define sample-source
    "name: Deploy\non:\n  push:\n    branches:\n      - main\n")
  (define sample-tokens
    (yaml-string->tokens sample-source
                         #:profile 'coloring
                         #:source-positions #f))
  (define compiler-tokens
    (yaml-string->tokens sample-source
                         #:profile 'compiler
                         #:source-positions #f))
  (define sample-derived
    (yaml-string->derived-tokens sample-source))
  (define block-source
    "run: |\n  echo hi\n  echo bye\nnext: done\n")
  (define block-derived
    (yaml-string->derived-tokens block-source))
  (define flow-source
    "with: {node-version: 25.2.1, stable: true}\n")
  (define flow-derived
    (yaml-string->derived-tokens flow-source))
  (define directive-source
    "%YAML 1.2\n---\nkey: value\n...\n")
  (define directive-derived
    (yaml-string->derived-tokens directive-source))
  (define quoted-source
    "good: \"line\\n\\u0041\"\nbad: \"\\q\"\n")
  (define quoted-derived
    (yaml-string->derived-tokens quoted-source))
  (define malformed-coloring
    (yaml-string->tokens "name: \"unterminated\n"
                         #:profile 'coloring
                         #:source-positions #f))
  (define malformed-compiler-thunk
    (lambda ()
      (yaml-string->tokens "name: \"unterminated\n"
                           #:profile 'compiler
                           #:source-positions #f)))
  (define crlf-source
    "name: Deploy\r\non:\r\n  push:\r\n")
  (define crlf-derived
    (yaml-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-yaml-derived-lexer
                              "name: Deploy\n"
                              "on:\n  push:\n"))

  (define key-token
    (findf (lambda (token)
             (yaml-derived-token-has-tag? token 'yaml-key-scalar))
           sample-derived))
  (define sequence-token
    (findf (lambda (token)
             (yaml-derived-token-has-tag? token 'yaml-sequence-indicator))
           sample-derived))
  (define block-header-token
    (findf (lambda (token)
             (yaml-derived-token-has-tag? token 'yaml-block-scalar-header))
           block-derived))
  (define block-content-token
    (findf (lambda (token)
             (yaml-derived-token-has-tag? token 'yaml-block-scalar-content))
           block-derived))
  (define bool-token
    (findf (lambda (token)
             (yaml-derived-token-has-tag? token 'yaml-boolean))
           flow-derived))
  (define directive-token
    (findf (lambda (token)
             (yaml-derived-token-has-tag? token 'yaml-directive))
           directive-derived))
  (define document-marker-token
    (findf (lambda (token)
             (yaml-derived-token-has-tag? token 'yaml-document-marker))
           directive-derived))
  (define valid-quoted-token
    (findf (lambda (token)
             (and (yaml-derived-token-has-tag? token 'yaml-string-literal)
                  (string=? (yaml-derived-token-text token) "\"line\\n\\u0041\"")))
           quoted-derived))
  (define malformed-quoted-token
    (findf (lambda (token)
             (and (yaml-derived-token-has-tag? token 'malformed-token)
                  (string=? (yaml-derived-token-text token) "\"\\q\"")))
           quoted-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 10)
                '(literal delimiter whitespace literal whitespace literal delimiter whitespace whitespace literal))
  (check-equal? (map lexer-token-name compiler-tokens)
                '(literal delimiter literal literal delimiter literal delimiter literal delimiter delimiter literal eof))
  (check-not-false key-token)
  (check-not-false sequence-token)
  (check-not-false block-header-token)
  (check-not-false block-content-token)
  (check-not-false bool-token)
  (check-not-false directive-token)
  (check-not-false document-marker-token)
  (check-not-false valid-quoted-token)
  (check-not-false malformed-quoted-token)
  (check-not-false first-streaming-token)
  (check-true (contiguous-derived-stream? sample-derived))
  (check-true (contiguous-derived-stream? block-derived))
  (check-equal? (apply string-append (map yaml-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map yaml-derived-token-text block-derived))
                block-source)
  (check-equal? (apply string-append (map yaml-derived-token-text crlf-derived))
                crlf-source)
  (check-equal? (yaml-derived-token-text key-token)
                "name")
  (check-equal? (yaml-derived-token-text block-header-token)
                "|")
  (check-equal? (yaml-derived-token-text directive-token)
                "%YAML")
  (check-equal? (map lexer-token-name malformed-coloring)
                '(literal delimiter whitespace unknown whitespace eof))
  (check-exn exn:fail:read?
             malformed-compiler-thunk))
