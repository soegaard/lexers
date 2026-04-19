#lang racket/base

;;;
;;; C Lexer
;;;
;;
;; Public entry points for the C lexer.

;; make-c-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based C lexer.
;; make-c-derived-lexer : -> (input-port? -> (or/c c-derived-token? 'eof))
;;   Construct a port-based C lexer that returns derived C token values.
;; c-derived-token?     : any/c -> boolean?
;;   Recognize a derived C token value returned by the derived-token API.
;; c-derived-token-tags : c-derived-token? -> (listof symbol?)
;;   Extract the C-specific classification tags for one derived token.
;; c-derived-token-has-tag? : c-derived-token? symbol? -> boolean?
;;   Determine whether a derived C token has a given classification tag.
;; c-derived-token-text : c-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; c-derived-token-start : c-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; c-derived-token-end  : c-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; c-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire C string using the C lexer.
;; c-string->derived-tokens : string? -> (listof c-derived-token?)
;;   Tokenize an entire C string into derived C token values.
;; c-profiles           : immutable-hash?
;;   Profile defaults for the public C lexer.

(provide make-c-lexer
         make-c-derived-lexer
         c-derived-token?
         c-derived-token-tags
         c-derived-token-has-tag?
         c-derived-token-text
         c-derived-token-start
         c-derived-token-end
         c-string->tokens
         c-string->derived-tokens
         c-profiles)

(require parser-tools/lex
         racket/list
         racket/string
         "private/c-tokenize.rkt"
         "private/config.rkt"
         "private/parser-tools-compat.rkt"
         (rename-in "private/c-derived.rkt"
                    [c-derived-token? private-c-derived-token?]
                    [c-derived-token-tags private-c-derived-token-tags]
                    [c-derived-token-has-tag? private-c-derived-token-has-tag?]
                    [c-derived-token-text private-c-derived-token-text]
                    [c-derived-token-start private-c-derived-token-start]
                    [c-derived-token-end private-c-derived-token-end]
                    [make-c-derived-reader private-make-c-derived-reader])
         "token.rkt")

(define c-profiles
  c-profile-defaults)

;; c-derived-token? : any/c -> boolean?
;;   Recognize a derived C token value returned by the derived-token API.
(define (c-derived-token? v)
  (private-c-derived-token? v))

;; c-derived-token-tags : c-derived-token? -> (listof symbol?)
;;   Extract the C-specific classification tags for one derived token.
(define (c-derived-token-tags token)
  (private-c-derived-token-tags token))

;; c-derived-token-has-tag? : c-derived-token? symbol? -> boolean?
;;   Determine whether a derived C token has a given classification tag.
(define (c-derived-token-has-tag? token tag)
  (private-c-derived-token-has-tag? token tag))

;; c-derived-token-text : c-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
(define (c-derived-token-text token)
  (private-c-derived-token-text token))

;; c-derived-token-start : c-derived-token? -> position?
;;   Extract the starting source position for one derived token.
(define (c-derived-token-start token)
  (private-c-derived-token-start token))

;; c-derived-token-end : c-derived-token? -> position?
;;   Extract the ending source position for one derived token.
(define (c-derived-token-end token)
  (private-c-derived-token-end token))

;; make-c-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based C lexer.
(define (make-c-lexer #:profile          [profile 'coloring]
                      #:trivia           [trivia 'profile-default]
                      #:source-positions [source-positions 'profile-default])
  (define config
    (make-c-config #:profile          profile
                   #:trivia           trivia
                   #:source-positions source-positions))
  (make-c-token-reader config))

;; make-c-derived-lexer : -> (input-port? -> (or/c c-derived-token? 'eof))
;;   Construct a port-based C lexer that returns derived token values.
(define (make-c-derived-lexer)
  (private-make-c-derived-reader))

;; c-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire C string using the projected token API.
(define (c-string->tokens source
                          #:profile          [profile 'coloring]
                          #:trivia           [trivia 'profile-default]
                          #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-c-lexer #:profile          profile
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

;; c-string->derived-tokens : string? -> (listof c-derived-token?)
;;   Tokenize an entire C string into derived C token values.
(define (c-string->derived-tokens source)
  (define lexer
    (make-c-derived-lexer))
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

  ;; contiguous-derived-stream? : (listof c-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (c-derived-token-end left))
         (position-offset (c-derived-token-start right)))))

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
    "#include <stdio.h>\n#define X 42\nint main(void) { return X; }\n")
  (define sample-tokens
    (c-string->tokens sample-source
                      #:profile 'coloring
                      #:source-positions #f))
  (define compiler-tokens
    (c-string->tokens sample-source
                      #:profile 'compiler
                      #:source-positions #f))
  (define sample-derived
    (c-string->derived-tokens sample-source))
  (define comment-source
    "/* one */\n// two\n")
  (define comment-derived
    (c-string->derived-tokens comment-source))
  (define literal-source
    "char ch = '\\n';\nchar *s = \"hi\";\n")
  (define literal-derived
    (c-string->derived-tokens literal-source))
  (define splice-source
    "#define VALUE(a, b) a + \\\n  b\n")
  (define splice-derived
    (c-string->derived-tokens splice-source))
  (define malformed-coloring
    (c-string->tokens "\"unterminated"
                      #:profile 'coloring
                      #:source-positions #f))
  (define malformed-compiler-thunk
    (lambda ()
      (c-string->tokens "\"unterminated"
                        #:profile 'compiler
                        #:source-positions #f)))
  (define crlf-source
    "#include \"local.h\"\r\nint x = 1;\r\n")
  (define crlf-derived
    (c-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-c-derived-lexer
                              "#include "
                              "<stdio.h>\nint main(void) { return 0; }\n"))

  (define directive-token
    (findf (lambda (token)
             (c-derived-token-has-tag? token 'c-preprocessor-directive))
           sample-derived))
  (define header-token
    (findf (lambda (token)
             (c-derived-token-has-tag? token 'c-header-name))
           sample-derived))
  (define string-token
    (findf (lambda (token)
             (c-derived-token-has-tag? token 'c-string-literal))
           literal-derived))
  (define char-token
    (findf (lambda (token)
             (c-derived-token-has-tag? token 'c-char-literal))
           literal-derived))
  (define splice-token
    (findf (lambda (token)
             (c-derived-token-has-tag? token 'c-line-splice))
           splice-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 12)
                '(delimiter keyword whitespace literal whitespace delimiter keyword whitespace identifier whitespace literal whitespace))
  (check-equal? (map lexer-token-name compiler-tokens)
                '(delimiter keyword literal delimiter keyword identifier literal keyword identifier delimiter keyword delimiter delimiter keyword identifier delimiter delimiter eof))
  (check-not-false directive-token)
  (check-not-false header-token)
  (check-not-false string-token)
  (check-not-false char-token)
  (check-not-false splice-token)
  (check-not-false first-streaming-token)
  (check-equal? (c-derived-token-text directive-token)
                "include")
  (check-equal? (c-derived-token-text header-token)
                "<stdio.h>")
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map c-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map c-derived-token-text comment-derived))
                comment-source)
  (check-equal? (apply string-append (map c-derived-token-text crlf-derived))
                crlf-source)
  (check-equal? (map lexer-token-value (take malformed-coloring 1))
                '("\"unterminated"))
  (check-equal? (map lexer-token-name malformed-coloring)
                '(unknown eof))
  (check-exn exn:fail:read?
             malformed-compiler-thunk))
