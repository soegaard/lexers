#lang racket/base

;;;
;;; Scheme Corpus Checker
;;;

;; Check exact source round-tripping through Scheme lexer APIs per dialect.

(require parser-tools/lex
         racket/file
         racket/list
         racket/path
         racket/string
         rackunit
         "../lexers-lib/lexers/scheme.rkt"
         "../lexers-lib/lexers/token.rkt")

;; Local bucketed corpus root.
(define corpus-root "/tmp/lexers-scheme-corpus")

;; corpus-files : path-string? -> (listof path-string?)
;;   Collect source files below one corpus bucket.
(define (corpus-files root)
  (sort (for/list ([path (in-directory root)]
                   #:when (file-exists? path))
          (path->string path))
        string<?))

;; source-round-trips? : string? symbol? -> boolean?
;;   Check source reconstruction through both public Scheme lexer APIs.
(define (source-round-trips? source dialect)
  (define derived (scheme-string->derived-tokens source #:dialect dialect))
  (define projected
    (scheme-string->tokens source #:dialect dialect #:source-positions #f))
  (and (string=? source (apply string-append (map scheme-derived-token-text derived)))
       (string=? source
                 (apply string-append
                        (for/list ([token (in-list (drop-right projected 1))])
                          (lexer-token-value token))))))

;; main : -> void?
;;   Check all locally available Scheme corpus buckets, or skip when absent.
(define (main)
  (unless (directory-exists? corpus-root)
    (displayln "Skipping Scheme corpus check: local corpus is unavailable.")
    (exit 0))
  (define failures
    (append*
     (for/list ([dialect (in-list scheme-dialects)])
       (define bucket (build-path corpus-root (symbol->string dialect)))
       (cond [(not (directory-exists? bucket)) '()]
             [else
              (for/list ([path (in-list (corpus-files bucket))]
                         #:unless (source-round-trips? (file->string path) dialect))
                (cons dialect path))]))))
  (call-with-output-file (build-path corpus-root "summary.txt")
    (lambda (out)
      (fprintf out "roundtrip-failures: ~a\n" (length failures))
      (for ([failure (in-list failures)])
        (fprintf out "~a: ~a\n" (car failure) (cdr failure))))
    #:exists 'truncate/replace)
  (check-equal? failures '()))

(module+ main (main))
(module+ test (main))
