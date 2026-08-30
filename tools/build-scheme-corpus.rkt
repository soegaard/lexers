#lang racket/base

;;;
;;; Scheme Corpus Builder
;;;

;; Assemble local corpora for Scheme reports and the supported implementations.

(require racket/path
         racket/string
         "private/corpus-builder.rkt")

;; Local corpus output and report paths.
(define corpus-root "/tmp/lexers-scheme-corpus")
(define summary-path "/tmp/lexers-scheme-corpus-build-summary.txt")

;; Known local source trees for the supported Scheme readers.
(define chez-root "/Users/soegaard/Dropbox/GitHub/racket-old6/racket/src/ChezScheme")
(define guile-root "/Users/soegaard/Dropbox/GitHub/guile")
(define chicken-root "/Users/soegaard/Dropbox/GitHub/crunch/chicken-6.0.0pre1")
(define benchmarks-root "/Users/soegaard/Dropbox/GitHub/r7rs-benchmarks/src")

;; scheme-source-path? : path? -> boolean?
;;   Determine whether a path has a common Scheme source extension.
(define (scheme-source-path? path)
  (define extension (path-get-extension path))
  (and (bytes? extension)
       (member (string-downcase (bytes->string/utf-8 extension))
               '(".scm" ".ss" ".sls" ".sld"))))

;; named-scheme-source-path? : string? path? -> boolean?
;;   Determine whether a path is a Scheme source whose name contains text.
(define (named-scheme-source-path? text path)
  (and (scheme-source-path? path)
       (string-contains? (string-downcase (path->string (file-name-from-path path))) text)))

;; Corpus buckets retain the dialect used to test each source collection.
(define scheme-buckets
  (list (list "r5rs" (list benchmarks-root) scheme-source-path?)
        (list "r6rs" (list chez-root) (lambda (path) (equal? (path-get-extension path) #".sls")))
        (list "r7rs" (list benchmarks-root) scheme-source-path?)
        (list "chez" (list chez-root) scheme-source-path?)
        (list "guile" (list guile-root) scheme-source-path?)
        (list "chicken" (list chicken-root) scheme-source-path?)
        (list "gambit" (list benchmarks-root)
              (lambda (path) (named-scheme-source-path? "gambit" path)))))

;; main : -> void?
;;   Build the available local Scheme corpus buckets.
(define (main)
  (build-bucketed-corpus #:name "Scheme"
                          #:corpus-root corpus-root
                          #:summary-path summary-path
                          #:buckets scheme-buckets))

(module+ main (main))
