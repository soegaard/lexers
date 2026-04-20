#lang racket/base

;;;
;;; Shell Corpus Checker
;;;
;;
;; Development utility for running the shell lexer across a copied shell-script
;; corpus in /tmp with per-file timeouts and round-trip checks.

(require racket/file
         racket/list
         racket/match
         racket/path
         racket/string
         lexers/shell
         lexers/token)

;; corpus-root : path-string?
;;   Copied shell corpus root.
(define corpus-root
  "/tmp/lexers-shell-corpus")

;; summary-path : path-string?
;;   Output summary written after the corpus run.
(define summary-path
  "/tmp/lexers-shell-corpus/summary.txt")

;; timeout-seconds : real?
;;   Hard time limit for lexing one file.
(define timeout-seconds
  2.0)

;; detect-shell : path? -> symbol?
;;   Choose a shell dialect from the copied file path.
(define (detect-shell p)
  (define lower
    (string-downcase (path->string p)))
  (cond
    [(or (string-suffix? lower ".ps1")
         (string-suffix? lower ".psm1")
         (string-suffix? lower ".psd1"))
     'powershell]
    [(or (string-suffix? lower ".zsh")
         (regexp-match? #px"(^|/)zsh$" lower))
     'zsh]
    [else
     'bash]))

;; corpus-files : -> (listof path?)
;;   Collect copied shell files from the corpus directory.
(define (corpus-files)
  (sort
   (for/list ([p (in-directory corpus-root)]
              #:when (file-exists? p)
              #:unless (string=? (path->string p) summary-path))
     p)
   string<?
   #:key path->string))

;; relative-corpus-path : path? -> string?
;;   Render one file path relative to the copied corpus root.
(define (relative-corpus-path p)
  (path->string (find-relative-path corpus-root p)))

;; lex-file : path? symbol? -> (list/c symbol? any/c)
;;   Lex one file with a timeout and return status plus data.
(define (lex-file p shell)
  (define result-channel
    (make-channel))
  (define worker
    (thread
     (lambda ()
       (channel-put
        result-channel
        (with-handlers ([exn:fail? (lambda (exn) (list 'error (exn-message exn)))])
          (define source
            (file->string p))
          (define projected
            (shell-string->tokens source
                                  #:profile 'coloring
                                  #:source-positions #f
                                  #:shell shell))
          (define derived
            (shell-string->derived-tokens source
                                          #:shell shell))
          (define reconstructed
            (apply string-append
                   (map shell-derived-token-text derived)))
          (define projected-text
            (apply string-append
                   (for/list ([token (in-list projected)]
                              #:unless (lexer-token-eof? token))
                     (lexer-token-value token))))
          (list 'ok
                (hash 'projected-count (length projected)
                      'derived-count (length derived)
                      'derived-roundtrip? (string=? reconstructed source)
                      'projected-roundtrip? (string=? projected-text source))))))))
  (define result
    (sync/timeout timeout-seconds result-channel))
  (cond
    [result
     result]
    [else
     (kill-thread worker)
     (list 'timeout timeout-seconds)]))

;; write-summary! : (listof hash?) -> void?
;;   Write a human-readable corpus run summary.
(define (write-summary! results)
  (call-with-output-file summary-path
    (lambda (out)
      (define ok-count
        (count (lambda (r) (eq? (hash-ref r 'status) 'ok)) results))
      (define error-count
        (count (lambda (r) (eq? (hash-ref r 'status) 'error)) results))
      (define timeout-count
        (count (lambda (r) (eq? (hash-ref r 'status) 'timeout)) results))
      (define derived-failures
        (filter (lambda (r)
                  (and (eq? (hash-ref r 'status) 'ok)
                       (not (hash-ref r 'derived-roundtrip?))))
                results))
      (define projected-failures
        (filter (lambda (r)
                  (and (eq? (hash-ref r 'status) 'ok)
                       (not (hash-ref r 'projected-roundtrip?))))
                results))
      (fprintf out "corpus-root: ~a\n" corpus-root)
      (fprintf out "timeout-seconds: ~a\n" timeout-seconds)
      (fprintf out "files: ~a\n" (length results))
      (fprintf out "ok: ~a\n" ok-count)
      (fprintf out "errors: ~a\n" error-count)
      (fprintf out "timeouts: ~a\n" timeout-count)
      (fprintf out "derived-roundtrip-failures: ~a\n" (length derived-failures))
      (fprintf out "projected-roundtrip-failures: ~a\n\n"
               (length projected-failures))
      (for ([r (in-list results)])
        (fprintf out "~a\t~a\t~a"
                 (hash-ref r 'status)
                 (hash-ref r 'shell)
                 (hash-ref r 'path))
        (cond
          [(hash-has-key? r 'message)
           (fprintf out "\t~a" (hash-ref r 'message))]
          [(hash-has-key? r 'seconds)
           (fprintf out "\t~a" (hash-ref r 'seconds))]
          [else
           (fprintf out "\tderived=~a/projected=~a\t~a\t~a"
                    (hash-ref r 'derived-count)
                    (hash-ref r 'projected-count)
                    (hash-ref r 'derived-roundtrip?)
                    (hash-ref r 'projected-roundtrip?))])
        (newline out)))
    #:exists 'replace))

;; main : -> void?
;;   Run the shell lexer across the copied corpus.
(define (main)
  (cond
    [(directory-exists? corpus-root)
     (define files
       (corpus-files))
     (define results
       (for/list ([p (in-list files)])
         (define shell
           (detect-shell p))
         (match (lex-file p shell)
           [(list 'ok stats)
            (hash 'path (relative-corpus-path p)
                  'shell shell
                  'status 'ok
                  'projected-count (hash-ref stats 'projected-count)
                  'derived-count (hash-ref stats 'derived-count)
                  'derived-roundtrip? (hash-ref stats 'derived-roundtrip?)
                  'projected-roundtrip? (hash-ref stats 'projected-roundtrip?))]
           [(list 'error msg)
            (hash 'path (relative-corpus-path p)
                  'shell shell
                  'status 'error
                  'message msg)]
           [(list 'timeout seconds)
            (hash 'path (relative-corpus-path p)
                  'shell shell
                  'status 'timeout
                  'seconds seconds)])))
     (write-summary! results)
     (printf "Wrote summary to ~a\n" summary-path)]
    [else
     (printf "Skipping shell corpus check; missing corpus directory ~a\n"
             corpus-root)]))

(main)
