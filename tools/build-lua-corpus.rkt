#lang racket/base

;;;
;;; Lua Corpus Builder
;;;

;; Build a local Lua corpus from the MAME source tree.

(require racket/path racket/string "private/corpus-builder.rkt")

(define corpus-root "/tmp/lexers-lua-corpus")
(define summary-path "/tmp/lexers-lua-corpus-build-summary.txt")
(define source-roots '("/Users/soegaard/Dropbox/GitHub/mame"
                       "/Users/soegaard/Dropbox/GitHub/Provenance-2017"))

;; lua-path? : path? -> boolean?
;;   Determine whether a path has the Lua extension.
(define (lua-path? path)
  (define extension (path-get-extension path))
  (and (bytes? extension) (string-ci=? (bytes->string/utf-8 extension) ".lua")))

(define (main)
  (build-corpus #:name "Lua" #:corpus-root corpus-root #:summary-path summary-path
                #:source-roots source-roots #:include-path? lua-path?))
(module+ main (main))
