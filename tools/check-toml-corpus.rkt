#lang racket/base

;;;
;;; TOML Corpus Checker
;;;

;; Check exact source round-tripping through both public TOML lexer APIs.

(require parser-tools/lex
         racket/file
         racket/list
         racket/path
         racket/string
         rackunit
         "../lexers-lib/lexers/toml.rkt")

;; corpus-root : path-string?
;;   Root directory for the local TOML corpus.
(define corpus-root "/tmp/lexers-toml-corpus")

;; skip-if-missing! : -> void?
;;   Skip cleanly when the local corpus is not installed.
(define (skip-if-missing!)
  (unless (directory-exists? corpus-root)
    (displayln "Skipping TOML corpus check: local corpus is unavailable.")
    (exit 0)))

;; corpus-files : -> (listof path-string?)
;;   Enumerate copied TOML corpus files in deterministic order.
(define (corpus-files)
  (sort (for/list ([path (in-directory corpus-root)]
                   #:when (and (file-exists? path)
                               (not (equal? (path->string (file-name-from-path path))
                                            "summary.txt"))))
          (path->string path))
        string<?))

;; projected-token-text : token-like? -> string?
;;   Extract projected source text, omitting the eof marker.
(define (projected-token-text token)
  (cond
    [(symbol? token) ""]
    [else            (token-value token)]))

;; source-roundtrips? : string? -> boolean?
;;   Check exact reconstruction through the TOML public APIs.
(define (source-roundtrips? source)
  (define derived (toml-string->derived-tokens source))
  (define projected
    (toml-string->tokens source #:source-positions #f))
  (and (string=? source (apply string-append (map toml-derived-token-text derived)))
       (string=? source (apply string-append
                               (map projected-token-text (drop-right projected 1))))))

;; main : -> void?
;;   Run corpus round-trip checks and write their summary.
(define (main)
  (skip-if-missing!)
  (define files (corpus-files))
  (define failed
    (for/list ([path (in-list files)]
               #:unless (source-roundtrips? (file->string path)))
      path))
  (define summary-path (build-path corpus-root "summary.txt"))
  (call-with-output-file summary-path
    (lambda (out)
      (fprintf out "corpus-root: ~a\nfiles: ~a\nroundtrip-failures: ~a\n"
               corpus-root (length files) (length failed))
      (for ([path (in-list failed)]) (fprintf out "~a\n" path)))
    #:exists 'truncate/replace)
  (displayln (format "Wrote summary to ~a" summary-path))
  (check-equal? failed '()))

(module+ main (main))
(module+ test (main))
