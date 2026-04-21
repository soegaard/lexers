#lang racket/base

;;;
;;; Pascal Lexer
;;;
;;
;; Public entry points for the Pascal lexer.

;; make-pascal-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Pascal lexer.
;; make-pascal-derived-lexer : -> (input-port? -> (or/c pascal-derived-token? 'eof))
;;   Construct a port-based Pascal lexer that returns derived token values.
;; pascal-derived-token?     : any/c -> boolean?
;;   Recognize a derived Pascal token value returned by the derived-token API.
;; pascal-derived-token-tags : pascal-derived-token? -> (listof symbol?)
;;   Extract the Pascal-specific classification tags for one derived token.
;; pascal-derived-token-has-tag? : pascal-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; pascal-derived-token-text : pascal-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; pascal-derived-token-start : pascal-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; pascal-derived-token-end  : pascal-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; pascal-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Pascal string using the Pascal lexer.
;; pascal-string->derived-tokens : string? -> (listof pascal-derived-token?)
;;   Tokenize an entire Pascal string into derived Pascal token values.
;; pascal-profiles           : immutable-hash?
;;   Profile defaults for the public Pascal lexer.

(provide make-pascal-lexer
         make-pascal-derived-lexer
         pascal-derived-token?
         pascal-derived-token-tags
         pascal-derived-token-has-tag?
         pascal-derived-token-text
         pascal-derived-token-start
         pascal-derived-token-end
         pascal-string->tokens
         pascal-string->derived-tokens
         pascal-profiles)

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         (rename-in "private/pascal-derived.rkt"
                    [pascal-derived-token? private-pascal-derived-token?]
                    [pascal-derived-token-tags private-pascal-derived-token-tags]
                    [pascal-derived-token-has-tag? private-pascal-derived-token-has-tag?]
                    [pascal-derived-token-text private-pascal-derived-token-text]
                    [pascal-derived-token-start private-pascal-derived-token-start]
                    [pascal-derived-token-end private-pascal-derived-token-end]
                    [make-pascal-derived-reader private-make-pascal-derived-reader])
         "private/pascal-tokenize.rkt"
         "token.rkt")

(define pascal-profiles
  pascal-profile-defaults)

(define (pascal-derived-token? v)
  (private-pascal-derived-token? v))

(define (pascal-derived-token-tags token)
  (private-pascal-derived-token-tags token))

(define (pascal-derived-token-has-tag? token tag)
  (private-pascal-derived-token-has-tag? token tag))

(define (pascal-derived-token-text token)
  (private-pascal-derived-token-text token))

(define (pascal-derived-token-start token)
  (private-pascal-derived-token-start token))

(define (pascal-derived-token-end token)
  (private-pascal-derived-token-end token))

;; make-pascal-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Pascal lexer.
(define (make-pascal-lexer #:profile          [profile 'coloring]
                           #:trivia           [trivia 'profile-default]
                           #:source-positions [source-positions 'profile-default])
  (define config
    (make-pascal-config #:profile          profile
                        #:trivia           trivia
                        #:source-positions source-positions))
  (make-pascal-token-reader config))

;; make-pascal-derived-lexer : -> (input-port? -> (or/c pascal-derived-token? 'eof))
;;   Construct a port-based Pascal lexer that returns derived token values.
(define (make-pascal-derived-lexer)
  (private-make-pascal-derived-reader))

(define (pascal-string->tokens source
                               #:profile          [profile 'coloring]
                               #:trivia           [trivia 'profile-default]
                               #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-pascal-lexer #:profile          profile
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

(define (pascal-string->derived-tokens source)
  (define lexer
    (make-pascal-derived-lexer))
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
      (= (position-offset (pascal-derived-token-end left))
         (position-offset (pascal-derived-token-start right)))))

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
    "program Test;\nvar &do: Integer;\nbegin\n  { comment }\n  writeln('hi''s', #10, $FF, &17, %1010, 1.5E2);\nend.\n")
  (define sample-derived
    (pascal-string->derived-tokens sample-source))
  (define sample-tokens
    (pascal-string->tokens sample-source
                           #:profile 'coloring
                           #:source-positions #f))
  (define compiler-tokens
    (pascal-string->tokens sample-source
                           #:profile 'compiler
                           #:source-positions #f))
  (define crlf-source
    "program Test;\r\nbegin\r\n  writeln('hi');\r\nend.\r\n")
  (define crlf-derived
    (pascal-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-pascal-derived-lexer
                              "program "
                              "Test;\n"))
  (define keyword-token
    (findf (lambda (token)
             (pascal-derived-token-has-tag? token 'pascal-keyword))
           sample-derived))
  (define escaped-identifier-token
    (findf (lambda (token)
             (pascal-derived-token-has-tag? token 'pascal-escaped-identifier))
           sample-derived))
  (define string-token
    (findf (lambda (token)
             (pascal-derived-token-has-tag? token 'pascal-string-literal))
           sample-derived))
  (define control-token
    (findf (lambda (token)
             (pascal-derived-token-has-tag? token 'pascal-control-string))
           sample-derived))
  (define comment-token
    (findf (lambda (token)
             (pascal-derived-token-has-tag? token 'pascal-comment))
           sample-derived))
  (define numeric-token
    (findf (lambda (token)
             (and (pascal-derived-token-has-tag? token 'pascal-numeric-literal)
                  (string=? (pascal-derived-token-text token) "$FF")))
           sample-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 8)
                '(keyword whitespace identifier delimiter whitespace keyword whitespace identifier))
  (check-equal? (last (map lexer-token-name compiler-tokens))
                'eof)
  (check-not-false keyword-token)
  (check-not-false escaped-identifier-token)
  (check-not-false string-token)
  (check-not-false control-token)
  (check-not-false comment-token)
  (check-not-false numeric-token)
  (check-equal? (pascal-derived-token-text escaped-identifier-token)
                "&do")
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map pascal-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map pascal-derived-token-text crlf-derived))
                crlf-source)
  (check-not-false first-streaming-token)
  (check-not-false (pascal-derived-token-has-tag? first-streaming-token
                                                  'pascal-keyword)))
