#lang racket/base

;;;
;;; Ruby Corpus Builder
;;;
;;
;; Build a local Ruby corpus in /tmp from known local source trees,
;; deduplicated by file contents.

(require racket/path
         racket/string
         "private/corpus-builder.rkt")

;; corpus-root : path-string?
;;   Output directory for the copied Ruby corpus.
(define corpus-root
  "/tmp/lexers-ruby-corpus")

;; summary-path : path-string?
;;   Summary file written after the corpus is assembled.
(define summary-path
  "/tmp/lexers-ruby-corpus-build-summary.txt")

;; source-roots : (listof path-string?)
;;   Local source trees searched for Ruby source files.
(define source-roots
  '("/Users/soegaard/Dropbox/GitHub/linguist"
    "/Users/soegaard/Dropbox/GitHub/watchman/ruby/ruby-watchman"
    "/Users/soegaard/Dropbox/GitHub/emacs/test/lisp/progmodes/ruby-mode-resources"
    "/Users/soegaard/Dropbox/GitHub/games-others/Spoony"))

;; ruby-special-file-names : (listof string?)
;;   Extensionless file names commonly used for Ruby source.
(define ruby-special-file-names
  '("Gemfile" "Rakefile" "Guardfile" "Appraisals"))

;; ruby-source-path? : path? -> boolean?
;;   Determine whether a path belongs in the Ruby corpus.
(define (ruby-source-path? p)
  (define ext
    (path-get-extension p))
  (define file-name
    (path->string (file-name-from-path p)))
  (define path-string
    (path->string p))
  (and (not (regexp-match? #px"/(?:vendor|node_modules|tmp|log|coverage|compiled)/"
                           path-string))
       (or (and (bytes? ext)
                (string=? (string-downcase (bytes->string/utf-8 ext))
                          ".rb"))
           (member file-name ruby-special-file-names)
           (string-suffix? file-name ".gemspec"))))

;; main : -> void?
;;   Build the Ruby corpus and report the result.
(define (main)
  (build-corpus #:name          "Ruby"
                #:corpus-root   corpus-root
                #:summary-path  summary-path
                #:source-roots  source-roots
                #:include-path? ruby-source-path?))

(module+ main
  (main))
