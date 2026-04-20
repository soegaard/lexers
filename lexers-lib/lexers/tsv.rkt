#lang racket/base

;;;
;;; TSV Lexer
;;;
;;
;; Public entry points for the TSV lexer.

;; make-tsv-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based TSV lexer.
;; make-tsv-derived-lexer : -> (input-port? -> (or/c tsv-derived-token? 'eof))
;;   Construct a port-based TSV lexer that returns derived TSV token values.
;; tsv-derived-token?     : any/c -> boolean?
;;   Recognize a derived TSV token value returned by the derived-token API.
;; tsv-derived-token-tags : tsv-derived-token? -> (listof symbol?)
;;   Extract the TSV-specific classification tags for one derived token.
;; tsv-derived-token-has-tag? : tsv-derived-token? symbol? -> boolean?
;;   Determine whether a derived TSV token has a given classification tag.
;; tsv-derived-token-text : tsv-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; tsv-derived-token-start : tsv-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; tsv-derived-token-end  : tsv-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; tsv-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire TSV string using the TSV lexer.
;; tsv-string->derived-tokens : string? -> (listof tsv-derived-token?)
;;   Tokenize an entire TSV string into derived TSV token values.
;; tsv-profiles           : immutable-hash?
;;   Profile defaults for the public TSV lexer.

(provide make-tsv-lexer
         make-tsv-derived-lexer
         tsv-derived-token?
         tsv-derived-token-tags
         tsv-derived-token-has-tag?
         tsv-derived-token-text
         tsv-derived-token-start
         tsv-derived-token-end
         tsv-string->tokens
         tsv-string->derived-tokens
         tsv-profiles)

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         "private/tsv-tokenize.rkt"
         (rename-in "private/delimited-derived.rkt"
                    [delimited-derived-token? tsv-private-derived-token?]
                    [delimited-derived-token-tags tsv-private-derived-token-tags]
                    [delimited-derived-token-has-tag? tsv-private-derived-token-has-tag?]
                    [delimited-derived-token-text tsv-private-derived-token-text]
                    [delimited-derived-token-start tsv-private-derived-token-start]
                    [delimited-derived-token-end tsv-private-derived-token-end]
                    [make-delimited-derived-reader private-make-delimited-derived-reader])
         "private/parser-tools-compat.rkt"
         "token.rkt")

(define tsv-profiles
  tsv-profile-defaults)

;; tsv-derived-token? : any/c -> boolean?
;;   Recognize a derived TSV token value returned by the derived-token API.
(define (tsv-derived-token? v)
  (tsv-private-derived-token? v))

;; tsv-derived-token-tags : tsv-derived-token? -> (listof symbol?)
;;   Extract the TSV-specific classification tags for one derived token.
(define (tsv-derived-token-tags token)
  (tsv-private-derived-token-tags token))

;; tsv-derived-token-has-tag? : tsv-derived-token? symbol? -> boolean?
;;   Determine whether a derived TSV token has a given classification tag.
(define (tsv-derived-token-has-tag? token tag)
  (tsv-private-derived-token-has-tag? token tag))

;; tsv-derived-token-text : tsv-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
(define (tsv-derived-token-text token)
  (tsv-private-derived-token-text token))

;; tsv-derived-token-start : tsv-derived-token? -> position?
;;   Extract the starting source position for one derived token.
(define (tsv-derived-token-start token)
  (tsv-private-derived-token-start token))

;; tsv-derived-token-end : tsv-derived-token? -> position?
;;   Extract the ending source position for one derived token.
(define (tsv-derived-token-end token)
  (tsv-private-derived-token-end token))

;; make-tsv-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based TSV lexer.
(define (make-tsv-lexer #:profile          [profile 'coloring]
                        #:trivia           [trivia 'profile-default]
                        #:source-positions [source-positions 'profile-default])
  (define config
    (make-tsv-config #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions))
  (make-tsv-token-reader config))

;; make-tsv-derived-lexer : -> (input-port? -> (or/c tsv-derived-token? 'eof))
;;   Construct a port-based TSV lexer that returns derived TSV token values.
(define (make-tsv-derived-lexer)
  (private-make-delimited-derived-reader #:separator #\tab #:dialect 'tsv))

;; tsv-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire TSV string using the projected token API.
(define (tsv-string->tokens source
                            #:profile          [profile 'coloring]
                            #:trivia           [trivia 'profile-default]
                            #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-tsv-lexer #:profile          profile
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

;; tsv-string->derived-tokens : string? -> (listof tsv-derived-token?)
;;   Tokenize an entire TSV string into derived TSV token values.
(define (tsv-string->derived-tokens source)
  (define lexer
    (make-tsv-derived-lexer))
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

  ;; contiguous-derived-stream? : (listof tsv-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (tsv-derived-token-end left))
         (position-offset (tsv-derived-token-start right)))))

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
    "name\tage\tcity\nAda\t37\tLondon\n")
  (define sample-tokens
    (tsv-string->tokens sample-source
                        #:profile 'coloring
                        #:source-positions #f))
  (define compiler-tokens
    (tsv-string->tokens sample-source
                        #:profile 'compiler
                        #:source-positions #f))
  (define sample-derived
    (tsv-string->derived-tokens sample-source))
  (define quoted-source
    "\"a\tb\"\t42\t\"He said \"\"hi\"\"\"\n")
  (define quoted-derived
    (tsv-string->derived-tokens quoted-source))
  (define empty-source
    "a\t\tc\t\n")
  (define empty-derived
    (tsv-string->derived-tokens empty-source))
  (define malformed-coloring
    (tsv-string->tokens "\"unterminated\n"
                        #:profile 'coloring
                        #:source-positions #f))
  (define malformed-compiler-thunk
    (lambda ()
      (tsv-string->tokens "\"unterminated\n"
                          #:profile 'compiler
                          #:source-positions #f)))
  (define crlf-source
    "name\tage\r\nAda\t37\r\n")
  (define crlf-derived
    (tsv-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-tsv-derived-lexer
                              "name\t"
                              "age\n"))

  (define quoted-token
    (findf (lambda (token)
             (tsv-derived-token-has-tag? token 'delimited-quoted-field))
           quoted-derived))
  (define empty-token
    (findf (lambda (token)
             (tsv-derived-token-has-tag? token 'delimited-empty-field))
           empty-derived))
  (define separator-token
    (findf (lambda (token)
             (tsv-derived-token-has-tag? token 'tsv-separator))
           sample-derived))
  (define row-separator-token
    (findf (lambda (token)
             (tsv-derived-token-has-tag? token 'tsv-row-separator))
           sample-derived))

  (check-equal? (map lexer-token-name sample-tokens)
                '(literal delimiter literal delimiter literal delimiter literal delimiter literal delimiter literal delimiter eof))
  (check-equal? (map lexer-token-name compiler-tokens)
                '(literal delimiter literal delimiter literal delimiter literal delimiter literal delimiter literal delimiter eof))
  (check-not-false quoted-token)
  (check-not-false empty-token)
  (check-not-false separator-token)
  (check-not-false row-separator-token)
  (check-equal? (tsv-derived-token-text quoted-token)
                "\"a\tb\"")
  (check-equal? (tsv-derived-token-text empty-token)
                "")
  (check-not-false (tsv-derived-token-has-tag? separator-token 'delimiter))
  (check-not-false (tsv-derived-token-has-tag? row-separator-token 'delimiter))
  (check-equal? (map lexer-token-name malformed-coloring)
                '(unknown eof))
  (check-exn exn:fail:read? malformed-compiler-thunk)
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map tsv-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map tsv-derived-token-text quoted-derived))
                quoted-source)
  (check-equal? (apply string-append (map tsv-derived-token-text empty-derived))
                empty-source)
  (check-equal? (apply string-append (map tsv-derived-token-text crlf-derived))
                crlf-source)
  (check-not-false first-streaming-token)
  (check-equal? (tsv-derived-token-text first-streaming-token)
                "name"))
