#lang racket/base

;;;
;;; LaTeX Lexer
;;;
;;
;; Public entry points for the LaTeX lexer.

;; make-latex-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based LaTeX lexer.
;; make-latex-derived-lexer : -> (input-port? -> (or/c latex-derived-token? 'eof))
;;   Construct a port-based LaTeX lexer that returns derived token values.
;; latex-derived-token?     : any/c -> boolean?
;;   Recognize a derived LaTeX token value returned by the derived-token API.
;; latex-derived-token-tags : latex-derived-token? -> (listof symbol?)
;;   Extract the LaTeX-specific classification tags for one derived token.
;; latex-derived-token-has-tag? : latex-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; latex-derived-token-text : latex-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; latex-derived-token-start : latex-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; latex-derived-token-end  : latex-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; latex-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire LaTeX string using the LaTeX lexer.
;; latex-string->derived-tokens : string? -> (listof latex-derived-token?)
;;   Tokenize an entire LaTeX string into derived LaTeX token values.
;; latex-profiles           : immutable-hash?
;;   Profile defaults for the public LaTeX lexer.

(provide make-latex-lexer
         make-latex-derived-lexer
         latex-derived-token?
         latex-derived-token-tags
         latex-derived-token-has-tag?
         latex-derived-token-text
         latex-derived-token-start
         latex-derived-token-end
         latex-string->tokens
         latex-string->derived-tokens
         latex-profiles)

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

(define latex-profiles
  tex-profile-defaults)

(define (latex-derived-token? v)
  (private-tex-derived-token? v))

(define (latex-derived-token-tags token)
  (private-tex-derived-token-tags token))

(define (latex-derived-token-has-tag? token tag)
  (private-tex-derived-token-has-tag? token tag))

(define (latex-derived-token-text token)
  (private-tex-derived-token-text token))

(define (latex-derived-token-start token)
  (private-tex-derived-token-start token))

(define (latex-derived-token-end token)
  (private-tex-derived-token-end token))

(define (make-latex-lexer #:profile          [profile 'coloring]
                          #:trivia           [trivia 'profile-default]
                          #:source-positions [source-positions 'profile-default])
  (define config
    (make-tex-config #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions))
  (make-tex-token-reader config 'latex))

(define (make-latex-derived-lexer)
  (private-make-tex-derived-reader 'latex))

(define (latex-string->tokens source
                              #:profile          [profile 'coloring]
                              #:trivia           [trivia 'profile-default]
                              #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-latex-lexer #:profile          profile
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

(define (latex-string->derived-tokens source)
  (define lexer
    (make-latex-derived-lexer))
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

  (define sample-source
    "\\section{Hi}\n\\begin{itemize}\n\\item One\n\\end{itemize}\n")
  (define sample-derived
    (latex-string->derived-tokens sample-source))
  (define sample-tokens
    (latex-string->tokens sample-source
                          #:profile 'coloring
                          #:source-positions #f))
  (define command-token
    (findf (lambda (token)
             (latex-derived-token-has-tag? token 'latex-command))
           sample-derived))
  (define env-token
    (findf (lambda (token)
             (latex-derived-token-has-tag? token 'latex-environment-command))
           sample-derived))

  (check-not-false command-token)
  (check-not-false env-token)
  (check-equal? (latex-derived-token-text command-token)
                "\\section")
  (check-equal? (take (map lexer-token-name sample-tokens) 4)
                '(keyword delimiter literal delimiter))
  (check-equal? (apply string-append (map latex-derived-token-text sample-derived))
                sample-source))
