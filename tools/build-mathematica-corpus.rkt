#lang racket/base

;;;
;;; Mathematica Corpus Builder
;;;
;;
;; Build a local Wolfram Language / Mathematica corpus in /tmp from known
;; local source trees, deduplicated by file contents.

(require racket/path
         racket/string
         "private/corpus-builder.rkt")

;; corpus-root : path-string?
;;   Output directory for the copied Mathematica corpus.
(define corpus-root
  "/tmp/lexers-mathematica-corpus")

;; summary-path : path-string?
;;   Summary file written after the corpus is assembled.
(define summary-path
  "/tmp/lexers-mathematica-corpus-build-summary.txt")

;; source-roots : (listof path-string?)
;;   Local source trees searched for Mathematica source files.
(define source-roots
  '("/Users/soegaard/Dropbox/GitHub/codeparser"
    "/Users/soegaard/Dropbox/GitHub/linguist/samples/Mathematica"
    "/Users/soegaard/Dropbox/GitHub/wolfram-js-frontend"
    "/Users/soegaard/Dropbox/GitHub/WolframLanguageForJupyter"
    "/Users/soegaard/Dropbox/GitHub/mathics-core"))

;; mathematica-extensions : (listof string?)
;;   File extensions treated as Mathematica source.
(define mathematica-extensions
  '(".wl" ".wls" ".m"))

;; mathematica-source-path? : path? -> boolean?
;;   Determine whether a path belongs in the Mathematica corpus.
(define (mathematica-source-path? p)
  (define ext
    (path-get-extension p))
  (define path-string
    (path->string p))
  (and (bytes? ext)
       (member (string-downcase (bytes->string/utf-8 ext))
               mathematica-extensions)
       (not (regexp-match? #px"/\\.iconized/" path-string))
       (not (regexp-match? #px"/(?:dump_points_all|fields_dump(?:_[A-Za-z0-9]+)?)\\.wl$"
                           path-string))))

;; main : -> void?
;;   Build the Mathematica corpus and report the result.
(define (main)
  (build-corpus #:name          "Mathematica"
                #:corpus-root   corpus-root
                #:summary-path  summary-path
                #:source-roots  source-roots
                #:include-path? mathematica-source-path?))

(module+ main
  (main))
