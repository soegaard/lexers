#lang racket/base

;;;
;;; Java Lexer
;;;
;;
;; Public entry points for the Java lexer.

;; make-java-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Java lexer.
;; make-java-derived-lexer : -> (input-port? -> (or/c java-derived-token? 'eof))
;;   Construct a port-based Java lexer that returns derived token values.
;; java-derived-token?     : any/c -> boolean?
;;   Recognize a derived Java token value returned by the derived-token API.
;; java-derived-token-tags : java-derived-token? -> (listof symbol?)
;;   Extract the Java-specific classification tags for one derived token.
;; java-derived-token-has-tag? : java-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; java-derived-token-text : java-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; java-derived-token-start : java-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; java-derived-token-end  : java-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; java-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Java string using the Java lexer.
;; java-string->derived-tokens : string? -> (listof java-derived-token?)
;;   Tokenize an entire Java string into derived Java token values.
;; java-profiles           : immutable-hash?
;;   Profile defaults for the public Java lexer.

(provide make-java-lexer
         make-java-derived-lexer
         java-derived-token?
         java-derived-token-tags
         java-derived-token-has-tag?
         java-derived-token-text
         java-derived-token-start
         java-derived-token-end
         java-string->tokens
         java-string->derived-tokens
         java-profiles)

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         (rename-in "private/java-derived.rkt"
                    [java-derived-token? private-java-derived-token?]
                    [java-derived-token-tags private-java-derived-token-tags]
                    [java-derived-token-has-tag? private-java-derived-token-has-tag?]
                    [java-derived-token-text private-java-derived-token-text]
                    [java-derived-token-start private-java-derived-token-start]
                    [java-derived-token-end private-java-derived-token-end]
                    [make-java-derived-reader private-make-java-derived-reader])
         "private/java-tokenize.rkt"
         "token.rkt")

(define java-profiles
  java-profile-defaults)

(define (java-derived-token? v)
  (private-java-derived-token? v))

(define (java-derived-token-tags token)
  (private-java-derived-token-tags token))

(define (java-derived-token-has-tag? token tag)
  (private-java-derived-token-has-tag? token tag))

(define (java-derived-token-text token)
  (private-java-derived-token-text token))

(define (java-derived-token-start token)
  (private-java-derived-token-start token))

(define (java-derived-token-end token)
  (private-java-derived-token-end token))

(define (make-java-lexer #:profile          [profile 'coloring]
                         #:trivia           [trivia 'profile-default]
                         #:source-positions [source-positions 'profile-default])
  (define config
    (make-java-config #:profile          profile
                      #:trivia           trivia
                      #:source-positions source-positions))
  (make-java-token-reader config))

(define (make-java-derived-lexer)
  (private-make-java-derived-reader))

(define (java-string->tokens source
                             #:profile          [profile 'coloring]
                             #:trivia           [trivia 'profile-default]
                             #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-java-lexer #:profile          profile
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

(define (java-string->derived-tokens source)
  (define lexer
    (make-java-derived-lexer))
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

  ;; contiguous-derived-stream? : (listof java-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (java-derived-token-end left))
         (position-offset (java-derived-token-start right)))))

  ;; first-token-before-rest? : (-> (input-port? -> any)) string? string? -> any
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
    "package demo;\n\nclass Example {\n    String s = \"hi\";\n    char c = 'a';\n    double x = .5e1;\n    String t = \"\"\"\nhello\n\"\"\";\n    // done\n}\n")
  (define sample-derived
    (java-string->derived-tokens sample-source))
  (define sample-tokens
    (java-string->tokens sample-source
                         #:profile 'coloring
                         #:source-positions #f))
  (define compiler-tokens
    (java-string->tokens sample-source
                         #:profile 'compiler
                         #:source-positions #f))
  (define crlf-source
    "class Example {\r\n    String s = \"hi\";\r\n}\r\n")
  (define crlf-derived
    (java-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-java-derived-lexer
                              "class "
                              "Example {}\n"))
  (define string-token
    (findf (lambda (token)
             (java-derived-token-has-tag? token 'java-string-literal))
           sample-derived))
  (define char-token
    (findf (lambda (token)
             (java-derived-token-has-tag? token 'java-char-literal))
           sample-derived))
  (define text-block-token
    (findf (lambda (token)
             (java-derived-token-has-tag? token 'java-text-block))
           sample-derived))
  (define numeric-token
    (findf (lambda (token)
             (java-derived-token-has-tag? token 'java-numeric-literal))
           sample-derived))
  (define comment-token
    (findf (lambda (token)
             (java-derived-token-has-tag? token 'java-line-comment))
           sample-derived))
  (define annotation-derived
    (java-string->derived-tokens "@Override\nclass Example {}\n"))
  (define annotation-name-token
    (findf (lambda (token)
             (java-derived-token-has-tag? token 'java-annotation-name))
           annotation-derived))
  (define doc-comment-derived
    (java-string->derived-tokens "/** docs */\nclass Example {}\n"))
  (define doc-comment-token
    (findf (lambda (token)
             (java-derived-token-has-tag? token 'java-doc-comment))
           doc-comment-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 8)
                '(keyword whitespace identifier delimiter whitespace keyword whitespace identifier))
  (check-equal? (last (map lexer-token-name compiler-tokens))
                'eof)
  (check-not-false string-token)
  (check-not-false char-token)
  (check-not-false text-block-token)
  (check-not-false numeric-token)
  (check-not-false comment-token)
  (check-not-false annotation-name-token)
  (check-not-false doc-comment-token)
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map java-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map java-derived-token-text crlf-derived))
                crlf-source)
  (check-not-false first-streaming-token))
