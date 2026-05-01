#lang racket/base

;;;
;;; Mathematica Lexer
;;;
;;
;; Public entry points for the Mathematica / Wolfram Language lexer.

;; make-mathematica-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Mathematica lexer.
;; make-mathematica-derived-lexer : -> (input-port? -> (or/c mathematica-derived-token? 'eof))
;;   Construct a port-based Mathematica lexer that returns derived token values.
;; mathematica-derived-token?     : any/c -> boolean?
;;   Recognize a derived Mathematica token value returned by the derived-token API.
;; mathematica-derived-token-tags : mathematica-derived-token? -> (listof symbol?)
;;   Extract the Mathematica-specific classification tags for one derived token.
;; mathematica-derived-token-has-tag? : mathematica-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; mathematica-derived-token-text : mathematica-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; mathematica-derived-token-start : mathematica-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; mathematica-derived-token-end  : mathematica-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; mathematica-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Mathematica string using the Mathematica lexer.
;; mathematica-string->derived-tokens : string? -> (listof mathematica-derived-token?)
;;   Tokenize an entire Mathematica string into derived Mathematica token values.
;; mathematica-profiles           : immutable-hash?
;;   Profile defaults for the public Mathematica lexer.

(provide make-mathematica-lexer
         make-mathematica-derived-lexer
         mathematica-derived-token?
         mathematica-derived-token-tags
         mathematica-derived-token-has-tag?
         mathematica-derived-token-text
         mathematica-derived-token-start
         mathematica-derived-token-end
         mathematica-string->tokens
         mathematica-string->derived-tokens
         mathematica-profiles)

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         (rename-in "private/mathematica-derived.rkt"
                    [mathematica-derived-token? private-mathematica-derived-token?]
                    [mathematica-derived-token-tags private-mathematica-derived-token-tags]
                    [mathematica-derived-token-has-tag? private-mathematica-derived-token-has-tag?]
                    [mathematica-derived-token-text private-mathematica-derived-token-text]
                    [mathematica-derived-token-start private-mathematica-derived-token-start]
                    [mathematica-derived-token-end private-mathematica-derived-token-end]
                    [make-mathematica-derived-reader private-make-mathematica-derived-reader])
         "private/mathematica-tokenize.rkt"
         "token.rkt")

(define mathematica-profiles
  mathematica-profile-defaults)

(define (mathematica-derived-token? v)
  (private-mathematica-derived-token? v))

(define (mathematica-derived-token-tags token)
  (private-mathematica-derived-token-tags token))

(define (mathematica-derived-token-has-tag? token tag)
  (private-mathematica-derived-token-has-tag? token tag))

(define (mathematica-derived-token-text token)
  (private-mathematica-derived-token-text token))

(define (mathematica-derived-token-start token)
  (private-mathematica-derived-token-start token))

(define (mathematica-derived-token-end token)
  (private-mathematica-derived-token-end token))

(define (make-mathematica-lexer #:profile          [profile 'coloring]
                                #:trivia           [trivia 'profile-default]
                                #:source-positions [source-positions 'profile-default])
  (define config
    (make-mathematica-config #:profile          profile
                             #:trivia           trivia
                             #:source-positions source-positions))
  (make-mathematica-token-reader config))

(define (make-mathematica-derived-lexer)
  (private-make-mathematica-derived-reader))

(define (mathematica-string->tokens source
                                    #:profile          [profile 'coloring]
                                    #:trivia           [trivia 'profile-default]
                                    #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-mathematica-lexer #:profile          profile
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

(define (mathematica-string->derived-tokens source)
  (define lexer
    (make-mathematica-derived-lexer))
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

  ;; contiguous-derived-stream? : (listof mathematica-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (mathematica-derived-token-end left))
         (position-offset (mathematica-derived-token-start right)))))

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
    "BeginPackage[\"Demo`\"]\nfoo[x_] := Module[{a = 16^^FF}, x /. y_ :> #name &]\n(* outer (* inner *) done *)\nassoc = <|\"a\" -> 1|>;\npart = expr[[1]];\n\\[Alpha]\n")
  (define sample-derived
    (mathematica-string->derived-tokens sample-source))
  (define sample-tokens
    (mathematica-string->tokens sample-source
                                #:profile 'coloring
                                #:source-positions #f))
  (define compiler-tokens
    (mathematica-string->tokens sample-source
                                #:profile 'compiler
                                #:source-positions #f))
  (define malformed-comment-source
    "(* unterminated")
  (define malformed-comment-derived
    (mathematica-string->derived-tokens malformed-comment-source))
  (define escape-source
    "\\[Alpha] \\:03B1 \\.7A \\|01F600\n")
  (define escape-derived
    (mathematica-string->derived-tokens escape-source))
  (define malformed-long-name-derived
    (mathematica-string->derived-tokens "\\[Alpha \n"))
  (define operator-source
    "f @* g /* h ~~ x |-> y\n")
  (define operator-derived
    (mathematica-string->derived-tokens operator-source))
  (define number-source
    "42 3.5 16^^FF 1.2`20 *^ isn't one token\n")
  (define number-derived
    (mathematica-string->derived-tokens number-source))
  (define shebang-source
    "#!/usr/bin/env wolframscript\nBeginPackage[\"Demo`\"]\n")
  (define shebang-derived
    (mathematica-string->derived-tokens shebang-source))
  (define first-streaming-token
    (first-token-before-rest? make-mathematica-derived-lexer
                              "BeginPackage["
                              "\"x\"]"))
  (define symbol-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-symbol))
           sample-derived))
  (define string-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-string-literal))
           sample-derived))
  (define number-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-number))
           sample-derived))
  (define pattern-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-pattern))
           sample-derived))
  (define slot-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-slot))
           sample-derived))
  (define association-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-association-delimiter))
           sample-derived))
  (define part-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-part-delimiter))
           sample-derived))
  (define long-name-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-long-name))
           sample-derived))
  (define package-form-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-package-form))
           sample-derived))
  (define scoping-form-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-scoping-form))
           sample-derived))
  (define assignment-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-assignment-operator))
           sample-derived))
  (define rewrite-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-rewrite-operator))
           sample-derived))
  (define pattern-condition-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-pattern-condition-operator))
           (mathematica-string->derived-tokens "f[x_] /; Positive[x] := x\n")))
  (define shebang-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-shebang-comment))
           shebang-derived))
  (define named-character-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-named-character))
           escape-derived))
  (define character-escape-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-character-escape))
           escape-derived))
  (define composition-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-composition-operator))
           operator-derived))
  (define string-pattern-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-string-pattern-operator))
           operator-derived))
  (define function-arrow-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-function-arrow-operator))
           operator-derived))
  (define integer-number-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-integer-number))
           number-derived))
  (define real-number-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-real-number))
           number-derived))
  (define base-number-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-base-number))
           number-derived))
  (define precision-number-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-precision-number))
           (mathematica-string->derived-tokens "1.2`20\n")))
  (define exponent-number-token
    (findf (lambda (token)
             (mathematica-derived-token-has-tag? token 'mathematica-exponent-number))
           (mathematica-string->derived-tokens "1.2*^3\n")))

  (check-equal? (take (map lexer-token-name sample-tokens) 10)
                '(identifier delimiter literal delimiter whitespace
                             identifier delimiter identifier operator delimiter))
  (check-equal? (last (map lexer-token-name compiler-tokens))
                'eof)
  (check-not-false symbol-token)
  (check-not-false string-token)
  (check-not-false number-token)
  (check-not-false pattern-token)
  (check-not-false slot-token)
  (check-not-false association-token)
  (check-not-false part-token)
  (check-not-false long-name-token)
  (check-not-false package-form-token)
  (check-not-false scoping-form-token)
  (check-not-false assignment-token)
  (check-not-false rewrite-token)
  (check-not-false pattern-condition-token)
  (check-not-false shebang-token)
  (check-not-false named-character-token)
  (check-not-false character-escape-token)
  (check-not-false composition-token)
  (check-not-false string-pattern-token)
  (check-not-false function-arrow-token)
  (check-not-false integer-number-token)
  (check-not-false real-number-token)
  (check-not-false base-number-token)
  (check-not-false precision-number-token)
  (check-not-false exponent-number-token)
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map mathematica-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map mathematica-derived-token-text shebang-derived))
                shebang-source)
  (check-equal? (apply string-append (map mathematica-derived-token-text escape-derived))
                escape-source)
  (check-not-false (mathematica-derived-token-has-tag? (car malformed-comment-derived)
                                                       'malformed-token))
  (check-not-false (ormap (lambda (token)
                            (mathematica-derived-token-has-tag? token 'malformed-token))
                          malformed-long-name-derived))
  (check-not-false first-streaming-token))
