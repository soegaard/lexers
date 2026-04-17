#lang racket/base

;;;
;;; Scribble Lexer
;;;
;;
;; Public entry points for the Scribble lexer.

;; make-scribble-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Scribble lexer.
;; make-scribble-derived-lexer : -> (input-port? -> (or/c scribble-derived-token? 'eof))
;;   Construct a port-based Scribble lexer that returns derived Scribble token
;;   values.
;; scribble-derived-token?     : any/c -> boolean?
;;   Recognize a derived Scribble token value returned by the derived-token API.
;; scribble-derived-token-tags : scribble-derived-token? -> (listof symbol?)
;;   Extract the Scribble-specific classification tags for one derived token.
;; scribble-derived-token-has-tag? : scribble-derived-token? symbol? -> boolean?
;;   Determine whether a derived Scribble token has a given classification tag.
;; scribble-derived-token-text : scribble-derived-token? -> string?
;;   Extract the source text corresponding to one derived Scribble token.
;; scribble-derived-token-start : scribble-derived-token? -> position?
;;   Extract the starting source position for one derived Scribble token.
;; scribble-derived-token-end  : scribble-derived-token? -> position?
;;   Extract the ending source position for one derived Scribble token.
;; scribble-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Scribble string using the Scribble lexer.
;; scribble-string->derived-tokens : string? -> (listof scribble-derived-token?)
;;   Tokenize an entire Scribble string into derived Scribble token values.
;; scribble-profiles           : immutable-hash?
;;   Profile defaults for the public Scribble lexer.

(provide make-scribble-lexer
         make-scribble-derived-lexer
         scribble-derived-token?
         scribble-derived-token-tags
         scribble-derived-token-has-tag?
         scribble-derived-token-text
         scribble-derived-token-start
         scribble-derived-token-end
         scribble-string->tokens
         scribble-string->derived-tokens
         scribble-profiles)

(require parser-tools/lex
         "private/config.rkt"
         (rename-in "private/scribble-derived.rkt"
                    [scribble-derived-token? private-scribble-derived-token?]
                    [scribble-derived-token-tags private-scribble-derived-token-tags]
                    [scribble-derived-token-has-tag? private-scribble-derived-token-has-tag?]
                    [scribble-derived-token-text private-scribble-derived-token-text]
                    [scribble-derived-token-start private-scribble-derived-token-start]
                    [scribble-derived-token-end private-scribble-derived-token-end]
                    [make-scribble-derived-reader private-make-scribble-derived-reader])
         "private/parser-tools-compat.rkt"
         "private/scribble-tokenize.rkt")

(define scribble-profiles scribble-profile-defaults)

;; scribble-derived-token? : any/c -> boolean?
;;   Recognize a derived Scribble token value returned by the derived-token API.
(define (scribble-derived-token? v)
  (private-scribble-derived-token? v))

;; scribble-derived-token-tags : scribble-derived-token? -> (listof symbol?)
;;   Extract the Scribble-specific classification tags for one derived token.
(define (scribble-derived-token-tags token)
  (private-scribble-derived-token-tags token))

;; scribble-derived-token-has-tag? : scribble-derived-token? symbol? -> boolean?
;;   Determine whether a derived Scribble token has a given classification tag.
(define (scribble-derived-token-has-tag? token tag)
  (private-scribble-derived-token-has-tag? token tag))

;; scribble-derived-token-text : scribble-derived-token? -> string?
;;   Extract the source text corresponding to one derived Scribble token.
(define (scribble-derived-token-text token)
  (private-scribble-derived-token-text token))

;; scribble-derived-token-start : scribble-derived-token? -> position?
;;   Extract the starting source position for one derived Scribble token.
(define (scribble-derived-token-start token)
  (private-scribble-derived-token-start token))

;; scribble-derived-token-end : scribble-derived-token? -> position?
;;   Extract the ending source position for one derived Scribble token.
(define (scribble-derived-token-end token)
  (private-scribble-derived-token-end token))

;; make-scribble-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Scribble lexer.
(define (make-scribble-lexer #:profile          [profile 'coloring]
                             #:trivia           [trivia 'profile-default]
                             #:source-positions [source-positions 'profile-default])
  (define config
    (make-scribble-config #:profile          profile
                          #:trivia           trivia
                          #:source-positions source-positions))
  (make-scribble-token-reader config))

;; make-scribble-derived-lexer : -> (input-port? -> (or/c scribble-derived-token? 'eof))
;;   Construct a port-based Scribble lexer that returns derived token values.
(define (make-scribble-derived-lexer)
  (private-make-scribble-derived-reader))

;; scribble-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Scribble string using the projected token API.
(define (scribble-string->tokens source
                                 #:profile          [profile 'coloring]
                                 #:trivia           [trivia 'profile-default]
                                 #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-scribble-lexer #:profile          profile
                         #:trivia           trivia
                         #:source-positions source-positions))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (cond
      [(eof-token? token) (reverse (cons token tokens))]
      [else               (loop (cons token tokens))])))

;; scribble-string->derived-tokens : string? -> (listof scribble-derived-token?)
;;   Tokenize an entire Scribble string into derived Scribble token values.
(define (scribble-string->derived-tokens source)
  (define lexer (make-scribble-derived-lexer))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (cond
      [(eq? token 'eof)
       (reverse tokens)]
      [else
       (loop (cons token tokens))])))

(module+ test
  (require rackunit
           racket/list
           lexers/token)

  ;; first-derived-token-before-rest? : (-> (input-port? -> any)) string? string? -> any
  ;;   Read the first derived token before the second chunk is written.
  (define (first-derived-token-before-rest? make-lexer first-chunk rest-chunk)
    (define lexer
      (make-lexer))
    (define-values (in out)
      (make-pipe))
    (write-string first-chunk out)
    (flush-output out)
    (define result-channel
      (make-channel))
    (thread (lambda ()
              (channel-put result-channel (lexer in))))
    (define token
      (sync/timeout 1 result-channel))
    (write-string rest-chunk out)
    (close-output-port out)
    token)

  ;; token-source-slice : string? (or/c symbol? token? position-token?) -> string?
  ;;   Extract the exact source slice covered by one projected token.
  (define (token-source-slice source token)
    (define start (lexer-token-start token))
    (define end   (lexer-token-end token))
    (substring source
               (sub1 (position-offset start))
               (sub1 (position-offset end))))

  (define simple-tokens
    (scribble-string->tokens "@title{Hi}\nText"
                             #:profile 'coloring
                             #:source-positions #f))
  (define repeated-tokens
    (scribble-string->tokens "@itemlist[@item{One} @item{Two}]"
                             #:profile 'compiler
                             #:source-positions #f))
  (define escape-tokens
    (scribble-string->tokens "@racket[(define x 1)]"
                             #:profile 'compiler
                             #:source-positions #f))
  (define no-position-tokens
    (scribble-string->tokens "@title{Hi}"
                             #:profile 'coloring
                             #:source-positions #f))
  (define compiler-no-trivia-tokens
    (scribble-string->tokens "@title{Hi}\nText"
                             #:profile 'compiler
                             #:source-positions #f))
  (define fidelity-source
    "#lang scribble/manual\n\n@title{peek Scribble Demo}\n\nThis is plain text.\n\nInline Racket: @racket[(define x 1)]\n")
  (define fidelity-projected-tokens
    (scribble-string->tokens fidelity-source
                             #:profile 'coloring
                             #:source-positions #t))
  (define fidelity-derived-tokens
    (scribble-string->derived-tokens fidelity-source))
  (define contracts-line-source
    "                opaque (e.g, @racket[new-∀/c]).\n")
  (define contracts-line-projected-tokens
    (scribble-string->tokens contracts-line-source
                             #:profile 'coloring
                             #:source-positions #t))
  (define contracts-line-derived-tokens
    (scribble-string->derived-tokens contracts-line-source))
  (define contracts-excerpt-source
    "@item{@deftech{Impersonator @tech{contracts}} may wrap values and do\n                not provide any guarantees. Impersonator contracts\n                may hide properties of values, or even make them completely\n                opaque (e.g, @racket[new-∀/c]).\n")
  (define contracts-excerpt-projected-tokens
    (scribble-string->tokens contracts-excerpt-source
                             #:profile 'coloring
                             #:source-positions #t))
  (define contracts-excerpt-derived-tokens
    (scribble-string->derived-tokens contracts-excerpt-source))
  (define projected-whitespace-token
    (findf (lambda (token)
             (and (eq? (lexer-token-name token) 'whitespace)
                  (> (- (position-line (lexer-token-end token))
                        (position-line (lexer-token-start token)))
                     0)))
           fidelity-projected-tokens))
  (define derived-whitespace-token
    (findf (lambda (token)
             (and (scribble-derived-token-has-tag? token 'whitespace)
                  (> (- (position-line (scribble-derived-token-end token))
                        (position-line (scribble-derived-token-start token)))
                     0)))
           fidelity-derived-tokens))
  (define derived-tokens
    (scribble-string->derived-tokens
     "@title{Hi}\nText\n@itemlist[@item{One} @item{Two}]\n@racket[(define x 1)]"))
  (define derived-command-char
    (findf (lambda (token)
             (scribble-derived-token-has-tag? token 'scribble-command-char))
           derived-tokens))
  (define derived-title
    (findf (lambda (token)
             (and (scribble-derived-token-has-tag? token 'scribble-command)
                  (string=? (scribble-derived-token-text token) "title")))
           derived-tokens))
  (define derived-item
    (findf (lambda (token)
             (and (scribble-derived-token-has-tag? token 'scribble-command)
                  (string=? (scribble-derived-token-text token) "item")))
           derived-tokens))
  (define derived-racket-command
    (findf (lambda (token)
             (and (scribble-derived-token-has-tag? token 'scribble-command)
                  (string=? (scribble-derived-token-text token) "racket")))
           derived-tokens))
  (define derived-text
    (findf (lambda (token)
             (and (scribble-derived-token-has-tag? token 'scribble-text)
                  (string=? (scribble-derived-token-text token) "Hi")))
           derived-tokens))
  (define derived-body-delimiter
    (findf (lambda (token)
             (and (scribble-derived-token-has-tag? token 'scribble-body-delimiter)
                  (string=? (scribble-derived-token-text token) "{")))
           derived-tokens))
  (define derived-optional-delimiter
    (findf (lambda (token)
             (and (scribble-derived-token-has-tag? token 'scribble-optional-delimiter)
                  (string=? (scribble-derived-token-text token) "[")))
           derived-tokens))
  (define derived-racket-escape-token
    (findf (lambda (token)
             (and (scribble-derived-token-has-tag? token 'scribble-racket-escape)
                  (string=? (scribble-derived-token-text token) "define")))
           derived-tokens))
  (define streaming-first-token
    (first-derived-token-before-rest? make-scribble-derived-lexer
                                      "@title{Hi}"
                                      "\n\nText\n"))

  (check-equal? (map stream-token-name simple-tokens)
                '(delimiter identifier delimiter literal delimiter whitespace literal eof))
  (check-equal? (map stream-token-name repeated-tokens)
                '(delimiter identifier delimiter delimiter identifier delimiter
                  literal delimiter delimiter identifier delimiter
                  literal delimiter delimiter eof))
  (check-equal? (map stream-token-name escape-tokens)
                '(delimiter identifier delimiter delimiter identifier identifier
                  literal delimiter delimiter eof))
  (check-equal? (map stream-token-name no-position-tokens)
                '(delimiter identifier delimiter literal delimiter eof))
  (check-equal? (map stream-token-name compiler-no-trivia-tokens)
                '(delimiter identifier delimiter literal delimiter literal eof))
  (check-true (stream-token-has-positions? (car (scribble-string->tokens "@title{Hi}"))))
  (check-not-false projected-whitespace-token)
  (check-not-false derived-whitespace-token)
  (check-equal? (lexer-token-value projected-whitespace-token)
                (token-source-slice fidelity-source projected-whitespace-token))
  (check-equal? (apply string-append
                       (for/list ([token (in-list contracts-line-projected-tokens)]
                                  #:unless (eof-token? token))
                         (lexer-token-value token)))
                contracts-line-source)
  (check-equal? (apply string-append
                       (map scribble-derived-token-text contracts-line-derived-tokens))
                contracts-line-source)
  (check-equal? (apply string-append
                       (for/list ([token (in-list contracts-excerpt-projected-tokens)]
                                  #:unless (eof-token? token))
                         (lexer-token-value token)))
                contracts-excerpt-source)
  (check-equal? (apply string-append
                       (map scribble-derived-token-text contracts-excerpt-derived-tokens))
                contracts-excerpt-source)
  (check-equal? (scribble-derived-token-text derived-whitespace-token)
                (substring fidelity-source
                           (sub1 (position-offset (scribble-derived-token-start derived-whitespace-token)))
                           (sub1 (position-offset (scribble-derived-token-end derived-whitespace-token)))))
  (check-not-false derived-command-char)
  (check-not-false derived-title)
  (check-not-false derived-item)
  (check-not-false derived-racket-command)
  (check-not-false streaming-first-token)
  (check-not-false derived-text)
  (check-not-false derived-body-delimiter)
  (check-not-false derived-optional-delimiter)
  (check-not-false derived-racket-escape-token)
  (check-equal? (scribble-derived-token-text streaming-first-token)
                "@"))
