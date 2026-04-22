#lang racket/base

;;;
;;; TeX Lexer
;;;
;;
;; Public entry points for the TeX lexer.

;; make-tex-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based TeX lexer.
;; make-tex-derived-lexer : -> (input-port? -> (or/c tex-derived-token? 'eof))
;;   Construct a port-based TeX lexer that returns derived token values.
;; tex-derived-token?     : any/c -> boolean?
;;   Recognize a derived TeX token value returned by the derived-token API.
;; tex-derived-token-tags : tex-derived-token? -> (listof symbol?)
;;   Extract the TeX-specific classification tags for one derived token.
;; tex-derived-token-has-tag? : tex-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; tex-derived-token-text : tex-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; tex-derived-token-start : tex-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; tex-derived-token-end  : tex-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; tex-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire TeX string using the TeX lexer.
;; tex-string->derived-tokens : string? -> (listof tex-derived-token?)
;;   Tokenize an entire TeX string into derived TeX token values.
;; tex-profiles           : immutable-hash?
;;   Profile defaults for the public TeX lexer.

(provide make-tex-lexer
         make-tex-derived-lexer
         tex-derived-token?
         tex-derived-token-tags
         tex-derived-token-has-tag?
         tex-derived-token-text
         tex-derived-token-start
         tex-derived-token-end
         tex-string->tokens
         tex-string->derived-tokens
         tex-profiles)

(require parser-tools/lex
         racket/list
         "private/config.rkt"
         (rename-in "private/tex-derived.rkt"
                    [tex-derived-token? private-tex-derived-token?]
                    [tex-derived-token-tags private-tex-derived-token-tags]
                    [tex-derived-token-has-tag? private-tex-derived-token-has-tag?]
                    [tex-derived-token-text private-tex-derived-token-text]
                    [tex-derived-token-start private-tex-derived-token-start]
                    [tex-derived-token-end private-tex-derived-token-end]
                    [make-tex-derived-reader private-make-tex-derived-reader])
         "private/tex-tokenize.rkt"
         "token.rkt")

(define tex-profiles
  tex-profile-defaults)

(define (tex-derived-token? v)
  (private-tex-derived-token? v))

(define (tex-derived-token-tags token)
  (private-tex-derived-token-tags token))

(define (tex-derived-token-has-tag? token tag)
  (private-tex-derived-token-has-tag? token tag))

(define (tex-derived-token-text token)
  (private-tex-derived-token-text token))

(define (tex-derived-token-start token)
  (private-tex-derived-token-start token))

(define (tex-derived-token-end token)
  (private-tex-derived-token-end token))

(define (make-tex-lexer #:profile          [profile 'coloring]
                        #:trivia           [trivia 'profile-default]
                        #:source-positions [source-positions 'profile-default])
  (define config
    (make-tex-config #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions))
  (make-tex-token-reader config 'tex))

(define (make-tex-derived-lexer)
  (private-make-tex-derived-reader 'tex))

(define (tex-string->tokens source
                            #:profile          [profile 'coloring]
                            #:trivia           [trivia 'profile-default]
                            #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-tex-lexer #:profile          profile
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

(define (tex-string->derived-tokens source)
  (define lexer
    (make-tex-derived-lexer))
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
      (= (position-offset (tex-derived-token-end left))
         (position-offset (tex-derived-token-start right)))))

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
    "\\section{Hi}\nText with \\% and $x+y$.\n% comment\n")
  (define sample-derived
    (tex-string->derived-tokens sample-source))
  (define sample-tokens
    (tex-string->tokens sample-source
                        #:profile 'coloring
                        #:source-positions #f))
  (define compiler-tokens
    (tex-string->tokens sample-source
                        #:profile 'compiler
                        #:source-positions #f))
  (define crlf-source
    "\\textbf{Hi}\r\n\r\n$$x$$\r\n")
  (define crlf-derived
    (tex-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-tex-derived-lexer
                              "\\section{"
                              "Hi}\n"))
  (define text-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-text))
           sample-derived))
  (define control-word-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-control-word))
           sample-derived))
  (define control-symbol-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-control-symbol))
           sample-derived))
  (define math-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-math-shift))
           sample-derived))
  (define parameter-tokens
    (map tex-derived-token-text
         (tex-string->derived-tokens "#1 ##")))
  (define special-derived
    (tex-string->derived-tokens "$$x$$ \\(z\\) & _ ^ ~ \\[y\\] \\, \\; \\! \\/ \\ \n"))
  (define display-math-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-display-math-shift))
           special-derived))
  (define inline-math-token
    (findf (lambda (token)
             (and (tex-derived-token-has-tag? token 'tex-inline-math-shift)
                  (string=? (tex-derived-token-text token) "\\(")))
           special-derived))
  (define alignment-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-alignment-tab))
           special-derived))
  (define subscript-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-subscript-mark))
           special-derived))
  (define superscript-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-superscript-mark))
           special-derived))
  (define unbreakable-space-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-unbreakable-space))
           special-derived))
  (define spacing-command-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-spacing-command))
           special-derived))
  (define italic-correction-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-italic-correction))
           special-derived))
  (define control-space-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-control-space))
           special-derived))
  (define accent-derived
    (tex-string->derived-tokens "\\'e \\~n \\\"o\n"))
  (define accent-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-accent-command))
           accent-derived))
  (define delimiter-derived
    (tex-string->derived-tokens "{x}[y]\n"))
  (define open-group-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-open-group-delimiter))
           delimiter-derived))
  (define close-group-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-close-group-delimiter))
           delimiter-derived))
  (define open-optional-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-open-optional-delimiter))
           delimiter-derived))
  (define close-optional-token
    (findf (lambda (token)
             (tex-derived-token-has-tag? token 'tex-close-optional-delimiter))
           delimiter-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 5)
                '(identifier delimiter literal delimiter whitespace))
  (check-equal? (last (map lexer-token-name compiler-tokens))
                'eof)
  (check-not-false text-token)
  (check-not-false control-word-token)
  (check-not-false control-symbol-token)
  (check-not-false math-token)
  (check-not-false display-math-token)
  (check-not-false inline-math-token)
  (check-not-false alignment-token)
  (check-not-false subscript-token)
  (check-not-false superscript-token)
  (check-not-false unbreakable-space-token)
  (check-not-false spacing-command-token)
  (check-not-false italic-correction-token)
  (check-not-false control-space-token)
  (check-not-false accent-token)
  (check-not-false open-group-token)
  (check-not-false close-group-token)
  (check-not-false open-optional-token)
  (check-not-false close-optional-token)
  (check-equal? (tex-derived-token-text control-word-token)
                "\\section")
  (check-equal? (tex-derived-token-text text-token)
                "Hi")
  (check-equal? (tex-derived-token-text display-math-token)
                "$$")
  (check-equal? (tex-derived-token-text inline-math-token)
                "\\(")
  (check-equal? (tex-derived-token-text spacing-command-token)
                "\\,")
  (check-equal? (tex-derived-token-text italic-correction-token)
                "\\/")
  (check-equal? (tex-derived-token-text control-space-token)
                "\\ ")
  (check-equal? (tex-derived-token-text accent-token)
                "\\'")
  (check-equal? (tex-derived-token-text open-group-token)
                "{")
  (check-equal? (tex-derived-token-text close-group-token)
                "}")
  (check-equal? (tex-derived-token-text open-optional-token)
                "[")
  (check-equal? (tex-derived-token-text close-optional-token)
                "]")
  (check-equal? parameter-tokens
                '("#1" " " "##"))
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? (apply string-append (map tex-derived-token-text sample-derived))
                sample-source)
  (check-equal? (apply string-append (map tex-derived-token-text crlf-derived))
                crlf-source)
  (check-not-false first-streaming-token))
