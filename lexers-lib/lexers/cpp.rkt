#lang racket/base

;;;
;;; C++ Lexer
;;;
;;
;; Public entry points for the C++ lexer.

;; make-cpp-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based C++ lexer.
;; make-cpp-derived-lexer : -> (input-port? -> (or/c cpp-derived-token? 'eof))
;;   Construct a port-based C++ lexer that returns derived C++ token values.
;; cpp-derived-token?     : any/c -> boolean?
;;   Recognize a derived C++ token value returned by the derived-token API.
;; cpp-derived-token-tags : cpp-derived-token? -> (listof symbol?)
;;   Extract the C++-specific classification tags for one derived token.
;; cpp-derived-token-has-tag? : cpp-derived-token? symbol? -> boolean?
;;   Determine whether a derived C++ token has a given classification tag.
;; cpp-derived-token-text : cpp-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; cpp-derived-token-start : cpp-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; cpp-derived-token-end  : cpp-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; cpp-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire C++ string using the C++ lexer.
;; cpp-string->derived-tokens : string? -> (listof cpp-derived-token?)
;;   Tokenize an entire C++ string into derived C++ token values.
;; cpp-profiles           : immutable-hash?
;;   Profile defaults for the public C++ lexer.

(provide make-cpp-lexer
         make-cpp-derived-lexer
         cpp-derived-token?
         cpp-derived-token-tags
         cpp-derived-token-has-tag?
         cpp-derived-token-text
         cpp-derived-token-start
         cpp-derived-token-end
         cpp-string->tokens
         cpp-string->derived-tokens
         cpp-profiles)

(require parser-tools/lex
         racket/list
         racket/string
         "private/config.rkt"
         "private/cpp-tokenize.rkt"
         "private/parser-tools-compat.rkt"
         (rename-in "private/cpp-derived.rkt"
                    [cpp-derived-token? private-cpp-derived-token?]
                    [cpp-derived-token-tags private-cpp-derived-token-tags]
                    [cpp-derived-token-has-tag? private-cpp-derived-token-has-tag?]
                    [cpp-derived-token-text private-cpp-derived-token-text]
                    [cpp-derived-token-start private-cpp-derived-token-start]
                    [cpp-derived-token-end private-cpp-derived-token-end]
                    [make-cpp-derived-reader private-make-cpp-derived-reader])
         "token.rkt")

(define cpp-profiles
  cpp-profile-defaults)

;; cpp-derived-token? : any/c -> boolean?
;;   Recognize a derived C++ token value returned by the derived-token API.
(define (cpp-derived-token? v)
  (private-cpp-derived-token? v))

;; cpp-derived-token-tags : cpp-derived-token? -> (listof symbol?)
;;   Extract the C++-specific classification tags for one derived token.
(define (cpp-derived-token-tags token)
  (private-cpp-derived-token-tags token))

;; cpp-derived-token-has-tag? : cpp-derived-token? symbol? -> boolean?
;;   Determine whether a derived C++ token has a given classification tag.
(define (cpp-derived-token-has-tag? token tag)
  (private-cpp-derived-token-has-tag? token tag))

;; cpp-derived-token-text : cpp-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
(define (cpp-derived-token-text token)
  (private-cpp-derived-token-text token))

;; cpp-derived-token-start : cpp-derived-token? -> position?
;;   Extract the starting source position for one derived token.
(define (cpp-derived-token-start token)
  (private-cpp-derived-token-start token))

;; cpp-derived-token-end : cpp-derived-token? -> position?
;;   Extract the ending source position for one derived token.
(define (cpp-derived-token-end token)
  (private-cpp-derived-token-end token))

;; make-cpp-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based C++ lexer.
(define (make-cpp-lexer #:profile          [profile 'coloring]
                        #:trivia           [trivia 'profile-default]
                        #:source-positions [source-positions 'profile-default])
  (define config
    (make-cpp-config #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions))
  (make-cpp-token-reader config))

;; make-cpp-derived-lexer : -> (input-port? -> (or/c cpp-derived-token? 'eof))
;;   Construct a port-based C++ lexer that returns derived token values.
(define (make-cpp-derived-lexer)
  (private-make-cpp-derived-reader))

;; cpp-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire C++ string using the projected token API.
(define (cpp-string->tokens source
                            #:profile          [profile 'coloring]
                            #:trivia           [trivia 'profile-default]
                            #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-cpp-lexer #:profile          profile
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

;; cpp-string->derived-tokens : string? -> (listof cpp-derived-token?)
;;   Tokenize an entire C++ string into derived C++ token values.
(define (cpp-string->derived-tokens source)
  (define lexer
    (make-cpp-derived-lexer))
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

  ;; contiguous-derived-stream? : (listof cpp-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (cpp-derived-token-end left))
         (position-offset (cpp-derived-token-start right)))))

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
    "#include <vector>\n#define X 42\nstd::string s = R\"cpp(hi)cpp\";\n")
  (define sample-tokens
    (cpp-string->tokens sample-source
                        #:profile 'coloring
                        #:source-positions #f))
  (define compiler-tokens
    (cpp-string->tokens sample-source
                        #:profile 'compiler
                        #:source-positions #f))
  (define sample-derived
    (cpp-string->derived-tokens sample-source))
  (define comment-source
    "/* one */\n// two\n")
  (define comment-derived
    (cpp-string->derived-tokens comment-source))
  (define literal-source
    "auto s = R\"tag(hello)tag\";\nchar c = '\\n';\nauto n = 12_km;\n")
  (define literal-derived
    (cpp-string->derived-tokens literal-source))
  (define splice-source
    "#define VALUE(a, b) a + \\\n  b\n")
  (define splice-derived
    (cpp-string->derived-tokens splice-source))
  (define malformed-coloring
    (cpp-string->tokens "auto s = R\"tag(hello)"
                        #:profile 'coloring
                        #:source-positions #f))
  (define malformed-compiler-thunk
    (lambda ()
      (cpp-string->tokens "auto s = R\"tag(hello)"
                          #:profile 'compiler
                          #:source-positions #f)))
  (define crlf-source
    "#include \"local.hpp\"\r\nstd::string s = \"x\";\r\n")
  (define crlf-derived
    (cpp-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-cpp-derived-lexer
                              "#include "
                              "<vector>\nstd::string s;\n"))

  (define directive-token
    (findf (lambda (token)
             (cpp-derived-token-has-tag? token 'cpp-preprocessor-directive))
           sample-derived))
  (define header-token
    (findf (lambda (token)
             (cpp-derived-token-has-tag? token 'cpp-header-name))
           sample-derived))
  (define raw-string-token
    (findf (lambda (token)
             (and (cpp-derived-token-has-tag? token 'cpp-string-literal)
                  (string-prefix? (cpp-derived-token-text token) "R\"")))
           literal-derived))
  (define char-token
    (findf (lambda (token)
             (cpp-derived-token-has-tag? token 'cpp-char-literal))
           literal-derived))
  (define numeric-token
    (findf (lambda (token)
             (and (cpp-derived-token-has-tag? token 'cpp-numeric-literal)
                  (string=? (cpp-derived-token-text token) "12_km")))
           literal-derived))
  (define splice-token
    (findf (lambda (token)
             (cpp-derived-token-has-tag? token 'cpp-line-splice))
           splice-derived))
  (define scope-token
    (findf (lambda (token)
             (and (cpp-derived-token-has-tag? token 'cpp-delimiter)
                  (string=? (cpp-derived-token-text token) "::")))
           sample-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 12)
                '(delimiter keyword whitespace literal whitespace delimiter keyword whitespace identifier whitespace literal whitespace))
  (check-equal? (map lexer-token-name compiler-tokens)
                '(delimiter keyword literal delimiter keyword identifier literal identifier delimiter identifier identifier operator literal delimiter eof))
  (check-not-false directive-token)
  (check-not-false header-token)
  (check-not-false raw-string-token)
  (check-not-false char-token)
  (check-not-false numeric-token)
  (check-not-false splice-token)
  (check-not-false scope-token)
  (check-not-false first-streaming-token)
  (check-equal? (cpp-derived-token-text directive-token)
                "include")
  (check-equal? (cpp-derived-token-text header-token)
                "<vector>")
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map cpp-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map cpp-derived-token-text comment-derived))
                comment-source)
  (check-equal? (apply string-append (map cpp-derived-token-text literal-derived))
                literal-source)
  (check-equal? (apply string-append (map cpp-derived-token-text crlf-derived))
                crlf-source)
  (check-equal? (map lexer-token-name malformed-coloring)
                '(keyword whitespace identifier whitespace operator whitespace unknown eof))
  (check-exn exn:fail:read?
             malformed-compiler-thunk))
