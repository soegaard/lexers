#lang racket/base

;;;
;;; Go Lexer
;;;
;;
;; Public entry points for the Go lexer.

;; make-go-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Go lexer.
;; make-go-derived-lexer : -> (input-port? -> (or/c go-derived-token? 'eof))
;;   Construct a port-based Go lexer that returns derived token values.
;; go-derived-token?     : any/c -> boolean?
;;   Recognize a derived Go token value returned by the derived-token API.
;; go-derived-token-tags : go-derived-token? -> (listof symbol?)
;;   Extract the Go-specific classification tags for one derived token.
;; go-derived-token-has-tag? : go-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; go-derived-token-text : go-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; go-derived-token-start : go-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; go-derived-token-end  : go-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; go-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Go string using the Go lexer.
;; go-string->derived-tokens : string? -> (listof go-derived-token?)
;;   Tokenize an entire Go string into derived Go token values.
;; go-profiles           : immutable-hash?
;;   Profile defaults for the public Go lexer.

(provide make-go-lexer
         make-go-derived-lexer
         go-derived-token?
         go-derived-token-tags
         go-derived-token-has-tag?
         go-derived-token-text
         go-derived-token-start
         go-derived-token-end
         go-string->tokens
         go-string->derived-tokens
         go-profiles)

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         (rename-in "private/go-derived.rkt"
                    [go-derived-token? private-go-derived-token?]
                    [go-derived-token-tags private-go-derived-token-tags]
                    [go-derived-token-has-tag? private-go-derived-token-has-tag?]
                    [go-derived-token-text private-go-derived-token-text]
                    [go-derived-token-start private-go-derived-token-start]
                    [go-derived-token-end private-go-derived-token-end]
                    [make-go-derived-reader private-make-go-derived-reader])
         "private/go-tokenize.rkt"
         "token.rkt")

(define go-profiles
  go-profile-defaults)

(define (go-derived-token? v)
  (private-go-derived-token? v))

(define (go-derived-token-tags token)
  (private-go-derived-token-tags token))

(define (go-derived-token-has-tag? token tag)
  (private-go-derived-token-has-tag? token tag))

(define (go-derived-token-text token)
  (private-go-derived-token-text token))

(define (go-derived-token-start token)
  (private-go-derived-token-start token))

(define (go-derived-token-end token)
  (private-go-derived-token-end token))

(define (make-go-lexer #:profile          [profile 'coloring]
                       #:trivia           [trivia 'profile-default]
                       #:source-positions [source-positions 'profile-default])
  (define config
    (make-go-config #:profile          profile
                    #:trivia           trivia
                    #:source-positions source-positions))
  (make-go-token-reader config))

(define (make-go-derived-lexer)
  (private-make-go-derived-reader))

(define (go-string->tokens source
                           #:profile          [profile 'coloring]
                           #:trivia           [trivia 'profile-default]
                           #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-go-lexer #:profile          profile
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

(define (go-string->derived-tokens source)
  (define lexer
    (make-go-derived-lexer))
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

  ;; contiguous-derived-stream? : (listof go-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (go-derived-token-end left))
         (position-offset (go-derived-token-start right)))))

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
    "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tvalue := 42i\n\ttext := `hi`\n\tr := 'a'\n\t/* block */\n\tfmt.Println(text)\n\t// done\n}\n")
  (define sample-derived
    (go-string->derived-tokens sample-source))
  (define sample-tokens
    (go-string->tokens sample-source
                       #:profile 'coloring
                       #:source-positions #f))
  (define compiler-tokens
    (go-string->tokens sample-source
                       #:profile 'compiler
                       #:source-positions #f))
  (define semicolon-source
    "package main\nfunc main() {\n\tprintln(\"hi\")\n\treturn\n}\n")
  (define semicolon-compiler-tokens
    (go-string->tokens semicolon-source
                       #:profile 'compiler
                       #:source-positions #f))
  (define crlf-source
    "package main\r\n\r\nfunc main() {\r\n\tprintln(\"hi\")\r\n}\r\n")
  (define crlf-derived
    (go-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-go-derived-lexer
                              "package "
                              "main\n"))
  (define raw-string-token
    (findf (lambda (token)
             (go-derived-token-has-tag? token 'go-raw-string-literal))
           sample-derived))
  (define rune-token
    (findf (lambda (token)
             (go-derived-token-has-tag? token 'go-rune-literal))
           sample-derived))
  (define numeric-token
    (findf (lambda (token)
             (go-derived-token-has-tag? token 'go-imaginary-literal))
           sample-derived))
  (define block-comment-token
    (findf (lambda (token)
             (go-derived-token-has-tag? token 'go-general-comment))
           sample-derived))
  (define line-comment-token
    (findf (lambda (token)
             (go-derived-token-has-tag? token 'go-line-comment))
           sample-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 8)
                '(keyword whitespace identifier whitespace keyword whitespace literal whitespace))
  (check-equal? (last (map lexer-token-name compiler-tokens))
                'eof)
  (check-equal? (map lexer-token-value semicolon-compiler-tokens)
                '("package" "main" ";" "func" "main" "(" ")" "{"
                  "println" "(" "\"hi\"" ")" ";" "return" ";" "}" ";" #f))
  (check-not-false raw-string-token)
  (check-not-false rune-token)
  (check-not-false numeric-token)
  (check-not-false block-comment-token)
  (check-not-false line-comment-token)
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map go-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map go-derived-token-text crlf-derived))
                crlf-source)
  (check-not-false first-streaming-token))
