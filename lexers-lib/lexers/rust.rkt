#lang racket/base

;;;
;;; Rust Lexer
;;;
;;
;; Public entry points for the Rust lexer.

;; make-rust-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Rust lexer.
;; make-rust-derived-lexer : -> (input-port? -> (or/c rust-derived-token? 'eof))
;;   Construct a port-based Rust lexer that returns derived token values.
;; rust-derived-token?     : any/c -> boolean?
;;   Recognize a derived Rust token value returned by the derived-token API.
;; rust-derived-token-tags : rust-derived-token? -> (listof symbol?)
;;   Extract the Rust-specific classification tags for one derived token.
;; rust-derived-token-has-tag? : rust-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; rust-derived-token-text : rust-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; rust-derived-token-start : rust-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; rust-derived-token-end  : rust-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; rust-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Rust string using the Rust lexer.
;; rust-string->derived-tokens : string? -> (listof rust-derived-token?)
;;   Tokenize an entire Rust string into derived Rust token values.
;; rust-profiles           : immutable-hash?
;;   Profile defaults for the public Rust lexer.

(provide make-rust-lexer
         make-rust-derived-lexer
         rust-derived-token?
         rust-derived-token-tags
         rust-derived-token-has-tag?
         rust-derived-token-text
         rust-derived-token-start
         rust-derived-token-end
         rust-string->tokens
         rust-string->derived-tokens
         rust-profiles)

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         (rename-in "private/rust-derived.rkt"
                    [rust-derived-token? private-rust-derived-token?]
                    [rust-derived-token-tags private-rust-derived-token-tags]
                    [rust-derived-token-has-tag? private-rust-derived-token-has-tag?]
                    [rust-derived-token-text private-rust-derived-token-text]
                    [rust-derived-token-start private-rust-derived-token-start]
                    [rust-derived-token-end private-rust-derived-token-end]
                    [make-rust-derived-reader private-make-rust-derived-reader])
         "private/rust-tokenize.rkt"
         "token.rkt")

(define rust-profiles
  rust-profile-defaults)

(define (rust-derived-token? v)
  (private-rust-derived-token? v))

(define (rust-derived-token-tags token)
  (private-rust-derived-token-tags token))

(define (rust-derived-token-has-tag? token tag)
  (private-rust-derived-token-has-tag? token tag))

(define (rust-derived-token-text token)
  (private-rust-derived-token-text token))

(define (rust-derived-token-start token)
  (private-rust-derived-token-start token))

(define (rust-derived-token-end token)
  (private-rust-derived-token-end token))

(define (make-rust-lexer #:profile          [profile 'coloring]
                         #:trivia           [trivia 'profile-default]
                         #:source-positions [source-positions 'profile-default])
  (define config
    (make-rust-config #:profile          profile
                      #:trivia           trivia
                      #:source-positions source-positions))
  (make-rust-token-reader config))

(define (make-rust-derived-lexer)
  (private-make-rust-derived-reader))

(define (rust-string->tokens source
                             #:profile          [profile 'coloring]
                             #:trivia           [trivia 'profile-default]
                             #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-rust-lexer #:profile          profile
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

(define (rust-string->derived-tokens source)
  (define lexer
    (make-rust-derived-lexer))
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
      (= (position-offset (rust-derived-token-end left))
         (position-offset (rust-derived-token-start right)))))

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
    "fn main() {\n    let r#type = \"hello\";\n    let x = 42u32;\n    let y = 'a';\n    let z = 'static;\n    // comment\n}\n")
  (define sample-derived
    (rust-string->derived-tokens sample-source))
  (define sample-tokens
    (rust-string->tokens sample-source
                         #:profile 'coloring
                         #:source-positions #f))
  (define compiler-tokens
    (rust-string->tokens sample-source
                         #:profile 'compiler
                         #:source-positions #f))
  (define crlf-source
    "fn main() {\r\n    let s = r#\"hi\"#;\r\n}\r\n")
  (define crlf-derived
    (rust-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-rust-derived-lexer
                              "fn "
                              "main() {}\n"))
  (define raw-identifier-token
    (findf (lambda (token)
             (rust-derived-token-has-tag? token 'rust-raw-identifier))
           sample-derived))
  (define string-token
    (findf (lambda (token)
             (rust-derived-token-has-tag? token 'rust-string-literal))
           sample-derived))
  (define char-token
    (findf (lambda (token)
             (rust-derived-token-has-tag? token 'rust-char-literal))
           sample-derived))
  (define lifetime-token
    (findf (lambda (token)
             (rust-derived-token-has-tag? token 'rust-lifetime))
           sample-derived))
  (define comment-token
    (findf (lambda (token)
             (rust-derived-token-has-tag? token 'rust-comment))
           sample-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 8)
                '(keyword whitespace identifier delimiter delimiter whitespace delimiter whitespace))
  (check-equal? (last (map lexer-token-name compiler-tokens))
                'eof)
  (check-not-false raw-identifier-token)
  (check-not-false string-token)
  (check-not-false char-token)
  (check-not-false lifetime-token)
  (check-not-false comment-token)
  (check-equal? (rust-derived-token-text raw-identifier-token)
                "r#type")
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map rust-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map rust-derived-token-text crlf-derived))
                crlf-source)
  (check-not-false first-streaming-token))
