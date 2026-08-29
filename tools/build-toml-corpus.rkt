#lang racket/base

;;;
;;; TOML Corpus Builder
;;;

;; Build a local TOML corpus in /tmp from nearby project manifests and the
;; CPython tomllib conformance fixtures.

(require racket/path
         racket/string
         "private/corpus-builder.rkt")

;; corpus-root : path-string?
;;   Output directory for the local TOML corpus.
(define corpus-root "/tmp/lexers-toml-corpus")

;; summary-path : path-string?
;;   Output file recording the corpus sources.
(define summary-path "/tmp/lexers-toml-corpus-build-summary.txt")

;; source-roots : (listof path-string?)
;;   Local repositories containing real TOML files and conformance fixtures.
(define source-roots
  '("/Users/soegaard/Dropbox/GitHub/cpython/Lib/test/test_tomllib/data"
    "/Users/soegaard/Dropbox/GitHub/build-python/cpython/Lib/test/test_tomllib/data"
    "/Users/soegaard/Dropbox/GitHub/Woxi-main"
    "/Users/soegaard/Dropbox/GitHub/assimp"
    "/Users/soegaard/Dropbox/GitHub/manim"))

;; toml-path? : path? -> boolean?
;;   Determine whether a path has a TOML extension.
(define (toml-path? path)
  (define extension (path-get-extension path))
  (and (bytes? extension)
       (string-ci=? (bytes->string/utf-8 extension) ".toml")))

;; main : -> void?
;;   Build the TOML corpus.
(define (main)
  (build-corpus #:name          "TOML"
                #:corpus-root   corpus-root
                #:summary-path  summary-path
                #:source-roots  source-roots
                #:include-path? toml-path?))

(module+ main (main))
