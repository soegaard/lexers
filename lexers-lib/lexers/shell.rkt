#lang racket/base

;;;
;;; Shell Lexer
;;;
;;
;; Public entry points for the shell-script lexer.

;; make-shell-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based shell lexer.
;; make-shell-derived-lexer : keyword-arguments -> (input-port? -> (or/c shell-derived-token? 'eof))
;;   Construct a port-based shell lexer that returns derived token values.
;; shell-derived-token?     : any/c -> boolean?
;;   Recognize a derived shell token value returned by the derived-token API.
;; shell-derived-token-tags : shell-derived-token? -> (listof symbol?)
;;   Extract the shell-specific classification tags for one derived token.
;; shell-derived-token-has-tag? : shell-derived-token? symbol? -> boolean?
;;   Determine whether a derived shell token has a given classification tag.
;; shell-derived-token-text : shell-derived-token? -> string?
;;   Extract the source text corresponding to one derived shell token.
;; shell-derived-token-start : shell-derived-token? -> position?
;;   Extract the starting source position for one derived shell token.
;; shell-derived-token-end  : shell-derived-token? -> position?
;;   Extract the ending source position for one derived shell token.
;; shell-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire shell source string using the projected token API.
;; shell-string->derived-tokens : string? keyword-arguments -> (listof shell-derived-token?)
;;   Tokenize an entire shell source string into derived token values.
;; shell-profiles           : immutable-hash?
;;   Profile defaults for the public shell lexer.

(provide make-shell-lexer
         make-shell-derived-lexer
         shell-derived-token?
         shell-derived-token-tags
         shell-derived-token-has-tag?
         shell-derived-token-text
         shell-derived-token-start
         shell-derived-token-end
         shell-string->tokens
         shell-string->derived-tokens
         shell-profiles)

(require parser-tools/lex
         "private/config.rkt"
         (rename-in "private/shell-derived.rkt"
                    [shell-derived-token? private-shell-derived-token?]
                    [shell-derived-token-tags private-shell-derived-token-tags]
                    [shell-derived-token-has-tag? private-shell-derived-token-has-tag?]
                    [shell-derived-token-text private-shell-derived-token-text]
                    [shell-derived-token-start private-shell-derived-token-start]
                    [shell-derived-token-end private-shell-derived-token-end]
                    [make-shell-derived-reader private-make-shell-derived-reader])
         "private/parser-tools-compat.rkt"
         "private/shell-tokenize.rkt"
         "token.rkt")

(define shell-profiles shell-profile-defaults)

;; shell-derived-token? : any/c -> boolean?
;;   Recognize a derived shell token value returned by the derived-token API.
(define (shell-derived-token? v)
  (private-shell-derived-token? v))

;; shell-derived-token-tags : shell-derived-token? -> (listof symbol?)
;;   Extract the shell-specific classification tags for one derived token.
(define (shell-derived-token-tags token)
  (private-shell-derived-token-tags token))

;; shell-derived-token-has-tag? : shell-derived-token? symbol? -> boolean?
;;   Determine whether a derived shell token has a given classification tag.
(define (shell-derived-token-has-tag? token tag)
  (private-shell-derived-token-has-tag? token tag))

;; shell-derived-token-text : shell-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
(define (shell-derived-token-text token)
  (private-shell-derived-token-text token))

;; shell-derived-token-start : shell-derived-token? -> position?
;;   Extract the starting source position for one derived token.
(define (shell-derived-token-start token)
  (private-shell-derived-token-start token))

;; shell-derived-token-end : shell-derived-token? -> position?
;;   Extract the ending source position for one derived token.
(define (shell-derived-token-end token)
  (private-shell-derived-token-end token))

;; make-shell-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based shell lexer.
(define (make-shell-lexer #:profile          [profile 'coloring]
                          #:trivia           [trivia 'profile-default]
                          #:source-positions [source-positions 'profile-default]
                          #:shell            [shell 'bash])
  (define config
    (make-shell-config #:profile          profile
                       #:trivia           trivia
                       #:source-positions source-positions
                       #:shell            shell))
  (make-shell-token-reader config))

;; make-shell-derived-lexer : keyword-arguments -> (input-port? -> (or/c shell-derived-token? 'eof))
;;   Construct a port-based shell lexer that returns derived token values.
(define (make-shell-derived-lexer #:shell [shell 'bash])
  (private-make-shell-derived-reader shell))

;; shell-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire shell source string using the projected token API.
(define (shell-string->tokens source
                              #:profile          [profile 'coloring]
                              #:trivia           [trivia 'profile-default]
                              #:source-positions [source-positions 'profile-default]
                              #:shell            [shell 'bash])
  (define lexer
    (make-shell-lexer #:profile          profile
                      #:trivia           trivia
                      #:source-positions source-positions
                      #:shell            shell))
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

;; shell-string->derived-tokens : string? keyword-arguments -> (listof shell-derived-token?)
;;   Tokenize an entire shell source string into derived token values.
(define (shell-string->derived-tokens source
                                      #:shell [shell 'bash])
  (define lexer
    (make-shell-derived-lexer #:shell shell))
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
           racket/list
           racket/string)

  ;; token-source-slice : string? token-like? -> string?
  ;;   Extract the exact source slice covered by one projected token.
  (define (token-source-slice source token)
    (define start
      (lexer-token-start token))
    (define end
      (lexer-token-end token))
    (substring source
               (sub1 (position-offset start))
               (sub1 (position-offset end))))

  ;; contiguous-derived-stream? : (listof shell-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (shell-derived-token-end left))
         (position-offset (shell-derived-token-start right)))))

  ;; reconstruct-derived-source : (listof shell-derived-token?) -> string?
  ;;   Reconstruct the original source by concatenating derived token text.
  (define (reconstruct-derived-source tokens)
    (apply string-append
           (map shell-derived-token-text tokens)))

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
    (thread (lambda ()
              (channel-put result-channel (lexer in))))
    (define token
      (sync/timeout 1 result-channel))
    (write-string rest-chunk out)
    (close-output-port out)
    token)

  (define bash-source
    "export PATH\necho $PATH\nprintf \"%s\\n\" $(pwd)\n# done\n")
  (define bash-tokens
    (shell-string->tokens bash-source
                          #:profile 'coloring
                          #:source-positions #f))
  (define bash-derived
    (shell-string->derived-tokens bash-source))
  (define zsh-source
    "autoload -Uz compinit\ncompinit\n")
  (define zsh-derived
    (shell-string->derived-tokens zsh-source
                                  #:shell 'zsh))
  (define powershell-source
    "$name = \"world\"\nWrite-Host $name\n")
  (define powershell-derived
    (shell-string->derived-tokens powershell-source
                                  #:shell 'powershell))
  (define ansi-source
    "printf $'line\\n'\n")
  (define ansi-derived
    (shell-string->derived-tokens ansi-source
                                  #:shell 'bash))
  (define malformed-tokens
    (shell-string->tokens "\"unterminated"
                          #:profile 'coloring
                          #:source-positions #f))
  (define fidelity-source
    "echo hi\r\n# note\r\nprintf '%s\\n' \"$name\"\r\n")
  (define fidelity-projected
    (shell-string->tokens fidelity-source
                          #:profile 'coloring
                          #:source-positions #t))
  (define fidelity-derived
    (shell-string->derived-tokens fidelity-source))

  (define export-token
    (findf (lambda (token)
             (and (shell-derived-token-has-tag? token 'shell-builtin)
                  (string=? (shell-derived-token-text token) "export")))
           bash-derived))
  (define variable-token
    (findf (lambda (token)
             (and (shell-derived-token-has-tag? token 'shell-variable)
                  (string=? (shell-derived-token-text token) "$PATH")))
           bash-derived))
  (define substitution-token
    (findf (lambda (token)
             (shell-derived-token-has-tag? token 'shell-command-substitution))
           bash-derived))
  (define comment-token
    (findf (lambda (token)
             (shell-derived-token-has-tag? token 'shell-comment))
           bash-derived))
  (define zsh-token
    (findf (lambda (token)
             (and (shell-derived-token-has-tag? token 'shell-builtin)
                  (string=? (shell-derived-token-text token) "autoload")))
           zsh-derived))
  (define powershell-keyword-token
    (findf (lambda (token)
             (and (shell-derived-token-has-tag? token 'shell-keyword)
                  (string=? (shell-derived-token-text token) "Write-Host")))
           powershell-derived))
  (define ansi-string-token
    (findf (lambda (token)
             (shell-derived-token-has-tag? token 'shell-ansi-string-literal))
           ansi-derived))

  (check-equal? (take (map lexer-token-name bash-tokens) 6)
                '(keyword whitespace identifier whitespace keyword whitespace))

  (check-not-false export-token)
  (check-not-false variable-token)
  (check-not-false substitution-token)
  (check-not-false comment-token)
  (check-not-false zsh-token)
  (check-not-false powershell-keyword-token)
  (check-not-false ansi-string-token)

  (check-equal? (map lexer-token-name malformed-tokens)
                '(unknown eof))

  (check-true (contiguous-derived-stream? bash-derived))
  (check-equal? (reconstruct-derived-source fidelity-derived)
                fidelity-source)
  (check-equal? (reconstruct-derived-source ansi-derived)
                ansi-source)
  (check-equal? (apply string-append
                       (map (lambda (token)
                              (token-source-slice fidelity-source token))
                            (filter lexer-token-has-positions?
                                    fidelity-projected)))
                fidelity-source)

  (define first-token
    (first-token-before-rest?
     (lambda ()
       (make-shell-derived-lexer #:shell 'bash))
     "echo "
     " hi\n"))
  (check-not-false first-token)
  (check-true  (shell-derived-token? first-token))
  (check-equal? (shell-derived-token-text first-token) "echo"))
