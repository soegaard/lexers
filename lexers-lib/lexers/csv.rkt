#lang racket/base

;;;
;;; CSV Lexer
;;;
;;
;; Public entry points for the CSV lexer.

;; make-csv-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based CSV lexer.
;; make-csv-derived-lexer : -> (input-port? -> (or/c csv-derived-token? 'eof))
;;   Construct a port-based CSV lexer that returns derived CSV token values.
;; csv-derived-token?     : any/c -> boolean?
;;   Recognize a derived CSV token value returned by the derived-token API.
;; csv-derived-token-tags : csv-derived-token? -> (listof symbol?)
;;   Extract the CSV-specific classification tags for one derived token.
;; csv-derived-token-has-tag? : csv-derived-token? symbol? -> boolean?
;;   Determine whether a derived CSV token has a given classification tag.
;; csv-derived-token-text : csv-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; csv-derived-token-start : csv-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; csv-derived-token-end  : csv-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; csv-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire CSV string using the CSV lexer.
;; csv-string->derived-tokens : string? -> (listof csv-derived-token?)
;;   Tokenize an entire CSV string into derived CSV token values.
;; csv-profiles           : immutable-hash?
;;   Profile defaults for the public CSV lexer.

(provide make-csv-lexer
         make-csv-derived-lexer
         csv-derived-token?
         csv-derived-token-tags
         csv-derived-token-has-tag?
         csv-derived-token-text
         csv-derived-token-start
         csv-derived-token-end
         csv-string->tokens
         csv-string->derived-tokens
         csv-profiles)

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         "private/csv-tokenize.rkt"
         (rename-in "private/delimited-derived.rkt"
                    [delimited-derived-token? csv-private-derived-token?]
                    [delimited-derived-token-tags csv-private-derived-token-tags]
                    [delimited-derived-token-has-tag? csv-private-derived-token-has-tag?]
                    [delimited-derived-token-text csv-private-derived-token-text]
                    [delimited-derived-token-start csv-private-derived-token-start]
                    [delimited-derived-token-end csv-private-derived-token-end]
                    [make-delimited-derived-reader private-make-delimited-derived-reader])
         "private/parser-tools-compat.rkt"
         "token.rkt")

(define csv-profiles
  csv-profile-defaults)

;; csv-derived-token? : any/c -> boolean?
;;   Recognize a derived CSV token value returned by the derived-token API.
(define (csv-derived-token? v)
  (csv-private-derived-token? v))

;; csv-derived-token-tags : csv-derived-token? -> (listof symbol?)
;;   Extract the CSV-specific classification tags for one derived token.
(define (csv-derived-token-tags token)
  (csv-private-derived-token-tags token))

;; csv-derived-token-has-tag? : csv-derived-token? symbol? -> boolean?
;;   Determine whether a derived CSV token has a given classification tag.
(define (csv-derived-token-has-tag? token tag)
  (csv-private-derived-token-has-tag? token tag))

;; csv-derived-token-text : csv-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
(define (csv-derived-token-text token)
  (csv-private-derived-token-text token))

;; csv-derived-token-start : csv-derived-token? -> position?
;;   Extract the starting source position for one derived token.
(define (csv-derived-token-start token)
  (csv-private-derived-token-start token))

;; csv-derived-token-end : csv-derived-token? -> position?
;;   Extract the ending source position for one derived token.
(define (csv-derived-token-end token)
  (csv-private-derived-token-end token))

;; make-csv-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based CSV lexer.
(define (make-csv-lexer #:profile          [profile 'coloring]
                        #:trivia           [trivia 'profile-default]
                        #:source-positions [source-positions 'profile-default])
  (define config
    (make-csv-config #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions))
  (make-csv-token-reader config))

;; make-csv-derived-lexer : -> (input-port? -> (or/c csv-derived-token? 'eof))
;;   Construct a port-based CSV lexer that returns derived CSV token values.
(define (make-csv-derived-lexer)
  (private-make-delimited-derived-reader #:separator #\, #:dialect 'csv))

;; csv-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire CSV string using the projected token API.
(define (csv-string->tokens source
                            #:profile          [profile 'coloring]
                            #:trivia           [trivia 'profile-default]
                            #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-csv-lexer #:profile          profile
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

;; csv-string->derived-tokens : string? -> (listof csv-derived-token?)
;;   Tokenize an entire CSV string into derived CSV token values.
(define (csv-string->derived-tokens source)
  (define lexer
    (make-csv-derived-lexer))
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
  (require rackunit
           racket/string)

  ;; contiguous-derived-stream? : (listof csv-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (csv-derived-token-end left))
         (position-offset (csv-derived-token-start right)))))

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
    "name,age,city\nAda,37,London\n")
  (define sample-tokens
    (csv-string->tokens sample-source
                        #:profile 'coloring
                        #:source-positions #f))
  (define compiler-tokens
    (csv-string->tokens sample-source
                        #:profile 'compiler
                        #:source-positions #f))
  (define sample-derived
    (csv-string->derived-tokens sample-source))
  (define quoted-source
    "\"last, first\",42,\"He said \"\"hi\"\"\"\n")
  (define quoted-derived
    (csv-string->derived-tokens quoted-source))
  (define empty-source
    "a,,c,\n")
  (define empty-derived
    (csv-string->derived-tokens empty-source))
  (define malformed-coloring
    (csv-string->tokens "\"unterminated\n"
                        #:profile 'coloring
                        #:source-positions #f))
  (define malformed-compiler-thunk
    (lambda ()
      (csv-string->tokens "\"unterminated\n"
                          #:profile 'compiler
                          #:source-positions #f)))
  (define crlf-source
    "name,age\r\nAda,37\r\n")
  (define crlf-derived
    (csv-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-csv-derived-lexer
                              "name,"
                              "age\n"))

  (define quoted-token
    (findf (lambda (token)
             (csv-derived-token-has-tag? token 'delimited-quoted-field))
           quoted-derived))
  (define empty-token
    (findf (lambda (token)
             (csv-derived-token-has-tag? token 'delimited-empty-field))
           empty-derived))
  (define separator-token
    (findf (lambda (token)
             (csv-derived-token-has-tag? token 'csv-separator))
           sample-derived))
  (define row-separator-token
    (findf (lambda (token)
             (csv-derived-token-has-tag? token 'csv-row-separator))
           sample-derived))

  (check-equal? (map lexer-token-name sample-tokens)
                '(literal delimiter literal delimiter literal delimiter literal delimiter literal delimiter literal delimiter eof))
  (check-equal? (map lexer-token-name compiler-tokens)
                '(literal delimiter literal delimiter literal delimiter literal delimiter literal delimiter literal delimiter eof))
  (check-not-false quoted-token)
  (check-not-false empty-token)
  (check-not-false separator-token)
  (check-not-false row-separator-token)
  (check-equal? (csv-derived-token-text quoted-token)
                "\"last, first\"")
  (check-equal? (csv-derived-token-text empty-token)
                "")
  (check-not-false (csv-derived-token-has-tag? separator-token 'delimiter))
  (check-not-false (csv-derived-token-has-tag? row-separator-token 'delimiter))
  (check-equal? (map lexer-token-name malformed-coloring)
                '(unknown eof))
  (check-exn exn:fail:read? malformed-compiler-thunk)
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map csv-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map csv-derived-token-text quoted-derived))
                quoted-source)
  (check-equal? (apply string-append (map csv-derived-token-text empty-derived))
                empty-source)
  (check-equal? (apply string-append (map csv-derived-token-text crlf-derived))
                crlf-source)
  (check-not-false first-streaming-token)
  (check-equal? (csv-derived-token-text first-streaming-token)
                "name"))
