#lang racket/base

;;;
;;; Objective-C Lexer
;;;
;;
;; Public entry points for the Objective-C lexer.

;; make-objc-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Objective-C lexer.
;; make-objc-derived-lexer : -> (input-port? -> (or/c objc-derived-token? 'eof))
;;   Construct a port-based Objective-C lexer that returns derived Objective-C token values.
;; objc-derived-token?     : any/c -> boolean?
;;   Recognize a derived Objective-C token value returned by the derived-token API.
;; objc-derived-token-tags : objc-derived-token? -> (listof symbol?)
;;   Extract the Objective-C-specific classification tags for one derived token.
;; objc-derived-token-has-tag? : objc-derived-token? symbol? -> boolean?
;;   Determine whether a derived Objective-C token has a given classification tag.
;; objc-derived-token-text : objc-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; objc-derived-token-start : objc-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; objc-derived-token-end  : objc-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; objc-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Objective-C string using the Objective-C lexer.
;; objc-string->derived-tokens : string? -> (listof objc-derived-token?)
;;   Tokenize an entire Objective-C string into derived Objective-C token values.
;; objc-profiles           : immutable-hash?
;;   Profile defaults for the public Objective-C lexer.

(provide make-objc-lexer
         make-objc-derived-lexer
         objc-derived-token?
         objc-derived-token-tags
         objc-derived-token-has-tag?
         objc-derived-token-text
         objc-derived-token-start
         objc-derived-token-end
         objc-string->tokens
         objc-string->derived-tokens
         objc-profiles)

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         "private/objc-tokenize.rkt"
         "private/parser-tools-compat.rkt"
         (rename-in "private/objc-derived.rkt"
                    [objc-derived-token? private-objc-derived-token?]
                    [objc-derived-token-tags private-objc-derived-token-tags]
                    [objc-derived-token-has-tag? private-objc-derived-token-has-tag?]
                    [objc-derived-token-text private-objc-derived-token-text]
                    [objc-derived-token-start private-objc-derived-token-start]
                    [objc-derived-token-end private-objc-derived-token-end]
                    [make-objc-derived-reader private-make-objc-derived-reader])
         "token.rkt")

(define objc-profiles
  objc-profile-defaults)

;; objc-derived-token? : any/c -> boolean?
;;   Recognize a derived Objective-C token value returned by the derived-token API.
(define (objc-derived-token? v)
  (private-objc-derived-token? v))

;; objc-derived-token-tags : objc-derived-token? -> (listof symbol?)
;;   Extract the Objective-C-specific classification tags for one derived token.
(define (objc-derived-token-tags token)
  (private-objc-derived-token-tags token))

;; objc-derived-token-has-tag? : objc-derived-token? symbol? -> boolean?
;;   Determine whether a derived Objective-C token has a given classification tag.
(define (objc-derived-token-has-tag? token tag)
  (private-objc-derived-token-has-tag? token tag))

;; objc-derived-token-text : objc-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
(define (objc-derived-token-text token)
  (private-objc-derived-token-text token))

;; objc-derived-token-start : objc-derived-token? -> position?
;;   Extract the starting source position for one derived token.
(define (objc-derived-token-start token)
  (private-objc-derived-token-start token))

;; objc-derived-token-end : objc-derived-token? -> position?
;;   Extract the ending source position for one derived token.
(define (objc-derived-token-end token)
  (private-objc-derived-token-end token))

;; make-objc-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Objective-C lexer.
(define (make-objc-lexer #:profile          [profile 'coloring]
                         #:trivia           [trivia 'profile-default]
                         #:source-positions [source-positions 'profile-default])
  (define config
    (make-objc-config #:profile          profile
                      #:trivia           trivia
                      #:source-positions source-positions))
  (make-objc-token-reader config))

;; make-objc-derived-lexer : -> (input-port? -> (or/c objc-derived-token? 'eof))
;;   Construct a port-based Objective-C lexer that returns derived token values.
(define (make-objc-derived-lexer)
  (private-make-objc-derived-reader))

;; objc-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Objective-C string using the projected token API.
(define (objc-string->tokens source
                             #:profile          [profile 'coloring]
                             #:trivia           [trivia 'profile-default]
                             #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-objc-lexer #:profile          profile
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

;; objc-string->derived-tokens : string? -> (listof objc-derived-token?)
;;   Tokenize an entire Objective-C string into derived Objective-C token values.
(define (objc-string->derived-tokens source)
  (define lexer
    (make-objc-derived-lexer))
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

  ;; contiguous-derived-stream? : (listof objc-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (objc-derived-token-end left))
         (position-offset (objc-derived-token-start right)))))

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
    "#import <Foundation/Foundation.h>\n@interface Foo : NSObject\n@property NSString *name;\n@end\n")
  (define sample-tokens
    (objc-string->tokens sample-source
                         #:profile 'coloring
                         #:source-positions #f))
  (define compiler-tokens
    (objc-string->tokens sample-source
                         #:profile 'compiler
                         #:source-positions #f))
  (define sample-derived
    (objc-string->derived-tokens sample-source))
  (define comment-source
    "/* one */\n// two\n")
  (define comment-derived
    (objc-string->derived-tokens comment-source))
  (define literal-source
    "NSString *s = @\"hi\";\nid x = @[];\n")
  (define literal-derived
    (objc-string->derived-tokens literal-source))
  (define malformed-literal-source
    "NSString *bad = @\"\\q\";\nchar badc = 'ab';\nchar good = '\\x41';\n")
  (define malformed-literal-derived
    (objc-string->derived-tokens malformed-literal-source))
  (define malformed-coloring
    (objc-string->tokens "NSString *s = @\"unterminated"
                         #:profile 'coloring
                         #:source-positions #f))
  (define malformed-compiler-thunk
    (lambda ()
      (objc-string->tokens "NSString *s = @\"unterminated"
                           #:profile 'compiler
                           #:source-positions #f)))
  (define crlf-source
    "#import \"Local.h\"\r\n@interface Foo\r\n@end\r\n")
  (define crlf-derived
    (objc-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-objc-derived-lexer
                              "#import "
                              "<Foundation/Foundation.h>\n"))

  (define directive-token
    (findf (lambda (token)
             (objc-derived-token-has-tag? token 'objc-preprocessor-directive))
           sample-derived))
  (define at-keyword-token
    (findf (lambda (token)
             (objc-derived-token-has-tag? token 'objc-at-keyword))
           sample-derived))
  (define string-token
    (findf (lambda (token)
             (objc-derived-token-has-tag? token 'objc-string-literal))
           literal-derived))
  (define literal-introducer-token
    (findf (lambda (token)
             (objc-derived-token-has-tag? token 'objc-literal-introducer))
           literal-derived))
  (define malformed-string-token
    (findf (lambda (token)
             (and (objc-derived-token-has-tag? token 'malformed-token)
                  (string=? (objc-derived-token-text token) "@\"\\q\"")))
           malformed-literal-derived))
  (define malformed-char-token
    (findf (lambda (token)
             (and (objc-derived-token-has-tag? token 'malformed-token)
                  (string=? (objc-derived-token-text token) "'ab'")))
           malformed-literal-derived))
  (define valid-hex-char-token
    (findf (lambda (token)
             (and (objc-derived-token-has-tag? token 'objc-char-literal)
                  (string=? (objc-derived-token-text token) "'\\x41'")))
           malformed-literal-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 12)
                '(delimiter keyword whitespace literal whitespace keyword whitespace identifier whitespace delimiter whitespace identifier))
  (check-not-false directive-token)
  (check-not-false at-keyword-token)
  (check-not-false string-token)
  (check-not-false literal-introducer-token)
  (check-not-false malformed-string-token)
  (check-not-false malformed-char-token)
  (check-not-false valid-hex-char-token)
  (check-not-false first-streaming-token)
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map objc-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map objc-derived-token-text comment-derived))
                comment-source)
  (check-equal? (apply string-append (map objc-derived-token-text literal-derived))
                literal-source)
  (check-equal? (apply string-append (map objc-derived-token-text crlf-derived))
                crlf-source)
  (check-equal? (map lexer-token-name malformed-coloring)
                '(identifier whitespace operator identifier whitespace operator whitespace unknown eof))
  (check-true (pair? compiler-tokens))
  (check-exn exn:fail:read?
             malformed-compiler-thunk))
