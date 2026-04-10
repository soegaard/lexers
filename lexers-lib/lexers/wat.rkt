#lang racket/base

;;;
;;; WAT Lexer
;;;
;;
;; Public entry points for the WebAssembly text-format lexer.

;; make-wat-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based WAT lexer.
;; make-wat-derived-lexer : -> (input-port? -> (or/c wat-derived-token? 'eof))
;;   Construct a port-based WAT lexer that returns derived WAT token values.
;; wat-derived-token?     : any/c -> boolean?
;;   Recognize a derived WAT token value returned by the derived-token API.
;; wat-derived-token-tags : wat-derived-token? -> (listof symbol?)
;;   Extract the WAT-specific classification tags for one derived token.
;; wat-derived-token-has-tag? : wat-derived-token? symbol? -> boolean?
;;   Determine whether a derived WAT token has a given classification tag.
;; wat-derived-token-text : wat-derived-token? -> string?
;;   Extract the source text corresponding to one derived WAT token.
;; wat-derived-token-start : wat-derived-token? -> position?
;;   Extract the starting source position for one derived WAT token.
;; wat-derived-token-end  : wat-derived-token? -> position?
;;   Extract the ending source position for one derived WAT token.
;; wat-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire WAT string using the WAT lexer.
;; wat-string->derived-tokens : string? -> (listof wat-derived-token?)
;;   Tokenize an entire WAT string into derived WAT token values.
;; wat-profiles           : immutable-hash?
;;   Profile defaults for the public WAT lexer.

(provide make-wat-lexer
         make-wat-derived-lexer
         wat-derived-token?
         wat-derived-token-tags
         wat-derived-token-has-tag?
         wat-derived-token-text
         wat-derived-token-start
         wat-derived-token-end
         wat-string->tokens
         wat-string->derived-tokens
         wat-profiles)

(require parser-tools/lex
         "private/config.rkt"
         (rename-in "private/wat-derived.rkt"
                    [wat-derived-token? private-wat-derived-token?]
                    [wat-derived-token-tags private-wat-derived-token-tags]
                    [wat-derived-token-has-tag? private-wat-derived-token-has-tag?]
                    [wat-derived-token-text private-wat-derived-token-text]
                    [wat-derived-token-start private-wat-derived-token-start]
                    [wat-derived-token-end private-wat-derived-token-end]
                    [make-wat-derived-reader private-make-wat-derived-reader])
         "private/parser-tools-compat.rkt"
         "private/wat-tokenize.rkt"
         "token.rkt")

(define wat-profiles wat-profile-defaults)

;; wat-derived-token? : any/c -> boolean?
;;   Recognize a derived WAT token value returned by the derived-token API.
(define (wat-derived-token? v)
  (private-wat-derived-token? v))

;; wat-derived-token-tags : wat-derived-token? -> (listof symbol?)
;;   Extract the WAT-specific classification tags for one derived token.
(define (wat-derived-token-tags token)
  (private-wat-derived-token-tags token))

;; wat-derived-token-has-tag? : wat-derived-token? symbol? -> boolean?
;;   Determine whether a derived WAT token has a given classification tag.
(define (wat-derived-token-has-tag? token tag)
  (private-wat-derived-token-has-tag? token tag))

;; wat-derived-token-text : wat-derived-token? -> string?
;;   Extract the source text corresponding to one derived WAT token.
(define (wat-derived-token-text token)
  (private-wat-derived-token-text token))

;; wat-derived-token-start : wat-derived-token? -> position?
;;   Extract the starting source position for one derived WAT token.
(define (wat-derived-token-start token)
  (private-wat-derived-token-start token))

;; wat-derived-token-end : wat-derived-token? -> position?
;;   Extract the ending source position for one derived WAT token.
(define (wat-derived-token-end token)
  (private-wat-derived-token-end token))

;; make-wat-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based WAT lexer.
(define (make-wat-lexer #:profile          [profile 'coloring]
                        #:trivia           [trivia 'profile-default]
                        #:source-positions [source-positions 'profile-default])
  (define config
    (make-wat-config #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions))
  (make-wat-token-reader config))

;; make-wat-derived-lexer : -> (input-port? -> (or/c wat-derived-token? 'eof))
;;   Construct a port-based WAT lexer that returns derived token values.
(define (make-wat-derived-lexer)
  (private-make-wat-derived-reader))

;; wat-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire WAT string using the projected token API.
(define (wat-string->tokens source
                            #:profile          [profile 'coloring]
                            #:trivia           [trivia 'profile-default]
                            #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-wat-lexer #:profile          profile
                    #:trivia           trivia
                    #:source-positions source-positions))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (cond
      [(eof-token? token) (reverse (cons token tokens))]
      [else               (loop (cons token tokens))])))

;; wat-string->derived-tokens : string? -> (listof wat-derived-token?)
;;   Tokenize an entire WAT string into derived WAT token values.
(define (wat-string->derived-tokens source)
  (define lexer (make-wat-derived-lexer))
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
           racket/string)

  ;; token-source-slice : string? (or/c symbol? token? position-token?) -> string?
  ;;   Extract the exact source slice covered by one projected token.
  (define (token-source-slice source token)
    (define start (lexer-token-start token))
    (define end   (lexer-token-end token))
    (substring source
               (sub1 (position-offset start))
               (sub1 (position-offset end))))

  ;; contiguous-derived-stream? : (listof wat-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (wat-derived-token-end left))
         (position-offset (wat-derived-token-start right)))))

  (define basic-source
    "(module (func (result i32) (i32.const 42)))")
  (define basic-tokens
    (wat-string->tokens basic-source
                        #:profile 'compiler
                        #:source-positions #f))
  (define folded-source
    "(module\n  (func $answer (result i32)\n    i32.const 42))\n")
  (define folded-derived-tokens
    (wat-string->derived-tokens folded-source))
  (define comment-source
    ";; line comment\n(module (; outer (; inner ;) ;) (func))")
  (define comment-tokens
    (wat-string->tokens comment-source
                        #:profile 'coloring
                        #:source-positions #f))
  (define strings-source
    "(module (data (i32.const 0) \"hi\\n\"))")
  (define strings-derived-tokens
    (wat-string->derived-tokens strings-source))
  (define numeric-source
    "(module (func (result f64) f64.const nan:0x1 i32.const 0x2A i32.const 1_000))")
  (define numeric-derived-tokens
    (wat-string->derived-tokens numeric-source))
  (define id-source
    "(func $x (param $y i32) local.get $y i32.add)"
    )
  (define id-derived-tokens
    (wat-string->derived-tokens id-source))
  (define malformed-string-tokens
    (wat-string->tokens "\"unterminated"
                        #:profile 'coloring
                        #:source-positions #f))
  (define fidelity-source
    ";; line comment\n(module (; outer (; inner ;) ;)\n  (func $x (result i32)\n    i32.const 42))\n")
  (define fidelity-projected-tokens
    (wat-string->tokens fidelity-source
                        #:profile 'coloring
                        #:source-positions #t))
  (define fidelity-derived-tokens
    (wat-string->derived-tokens fidelity-source))

  (define folded-form-token
    (findf (lambda (token)
             (and (wat-derived-token-has-tag? token 'wat-form)
                  (string=? (wat-derived-token-text token) "module")))
           folded-derived-tokens))
  (define folded-type-token
    (findf (lambda (token)
             (and (wat-derived-token-has-tag? token 'wat-type)
                  (string=? (wat-derived-token-text token) "i32")))
           folded-derived-tokens))
  (define folded-instruction-token
    (findf (lambda (token)
             (and (wat-derived-token-has-tag? token 'wat-instruction)
                  (string=? (wat-derived-token-text token) "i32.const")))
           folded-derived-tokens))
  (define nested-comment-token
    (findf (lambda (token)
             (and (wat-derived-token-has-tag? token 'comment)
                  (string-prefix? (wat-derived-token-text token) "(; outer")))
           (wat-string->derived-tokens comment-source)))
  (define string-token
    (findf (lambda (token)
             (wat-derived-token-has-tag? token 'wat-string-literal))
           strings-derived-tokens))
  (define nan-token
    (findf (lambda (token)
             (and (wat-derived-token-has-tag? token 'wat-numeric-literal)
                  (string=? (wat-derived-token-text token) "nan:0x1")))
           numeric-derived-tokens))
  (define hex-token
    (findf (lambda (token)
             (and (wat-derived-token-has-tag? token 'wat-numeric-literal)
                  (string=? (wat-derived-token-text token) "0x2A")))
           numeric-derived-tokens))
  (define underscored-token
    (findf (lambda (token)
             (and (wat-derived-token-has-tag? token 'wat-numeric-literal)
                  (string=? (wat-derived-token-text token) "1_000")))
           numeric-derived-tokens))
  (define id-token
    (findf (lambda (token)
             (and (wat-derived-token-has-tag? token 'wat-identifier)
                  (string=? (wat-derived-token-text token) "$x")))
           id-derived-tokens))
  (define local-get-token
    (findf (lambda (token)
             (and (wat-derived-token-has-tag? token 'wat-instruction)
                  (string=? (wat-derived-token-text token) "local.get")))
           id-derived-tokens))
  (define add-token
    (findf (lambda (token)
             (and (wat-derived-token-has-tag? token 'wat-instruction)
                  (string=? (wat-derived-token-text token) "i32.add")))
           id-derived-tokens))

  (check-equal? (map stream-token-name basic-tokens)
                '(delimiter keyword delimiter keyword delimiter keyword keyword
                  delimiter delimiter keyword literal delimiter delimiter delimiter eof))
  (check-not-false (member 'comment (map stream-token-name comment-tokens)))
  (check-not-false (member 'whitespace (map stream-token-name comment-tokens)))
  (check-not-false folded-form-token)
  (check-not-false folded-type-token)
  (check-not-false folded-instruction-token)
  (check-not-false nested-comment-token)
  (check-not-false string-token)
  (check-not-false nan-token)
  (check-not-false hex-token)
  (check-not-false underscored-token)
  (check-not-false id-token)
  (check-not-false local-get-token)
  (check-not-false add-token)
  (check-equal? (map stream-token-name malformed-string-tokens)
                '(unknown eof))
  (check-exn exn:fail:read?
             (lambda ()
               (wat-string->tokens "\"unterminated"
                                   #:profile 'compiler
                                   #:source-positions #f)))
  (check-true (stream-token-has-positions? (car (wat-string->tokens basic-source))))
  (check-equal? (map stream-token-name (wat-string->tokens basic-source #:source-positions #f))
                '(delimiter keyword whitespace delimiter keyword whitespace delimiter
                  keyword whitespace keyword delimiter whitespace delimiter keyword
                  whitespace literal delimiter delimiter delimiter eof))
  (check-equal? (apply string-append
                       (for/list ([token (in-list fidelity-projected-tokens)]
                                  #:unless (lexer-token-eof? token))
                         (lexer-token-value token)))
                fidelity-source)
  (check-equal? (apply string-append (map wat-derived-token-text fidelity-derived-tokens))
                fidelity-source)
  (check-true (contiguous-derived-stream? fidelity-derived-tokens)))
