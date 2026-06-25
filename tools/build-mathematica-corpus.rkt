#lang racket/base

;;;
;;; Mathematica Corpus Builder
;;;
;;
;; Build a local Wolfram Language / Mathematica corpus in /tmp from known
;; local source trees, deduplicated by file contents.

(require racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/string)

;; corpus-root : path-string?
;;   Output directory for the copied Mathematica corpus.
(define corpus-root
  "/tmp/lexers-mathematica-corpus")

;; summary-path : path-string?
;;   Summary file written after the corpus is assembled.
(define summary-path
  "/tmp/lexers-mathematica-corpus/build-summary.txt")

;; source-roots : (listof path-string?)
;;   Local source trees searched for Mathematica source files.
(define source-roots
  '("/Users/soegaard/Dropbox/GitHub/codeparser"
    "/Users/soegaard/Dropbox/GitHub/linguist/samples/Mathematica"))

;; mathematica-extensions : (listof string?)
;;   File extensions treated as Mathematica source.
(define mathematica-extensions
  '(".wl" ".wls" ".m"))

;; mathematica-source-path? : path? -> boolean?
;;   Determine whether a path has a Mathematica source extension.
(define (mathematica-source-path? p)
  (define ext
    (path-get-extension p))
  (cond
    [(bytes? ext)
     (member (string-downcase (bytes->string/utf-8 ext))
             mathematica-extensions)]
    [else
     #f]))

;; available-source-roots : -> (listof path-string?)
;;   Keep only source roots that exist on this machine.
(define (available-source-roots)
  (filter directory-exists?
          source-roots))

;; collect-source-files : -> (listof path-string?)
;;   Collect Mathematica source files from the configured local roots.
(define (collect-source-files)
  (sort
   (append*
    (for/list ([root (in-list (available-source-roots))])
      (for/list ([p (in-directory root)]
                 #:when (and (file-exists? p)
                             (mathematica-source-path? p)))
        (path->string p))))
   string<?))

;; sanitize-file-name : path-string? -> string?
;;   Render a corpus-safe file name from one source path.
(define (sanitize-file-name p)
  (regexp-replace* #px"[^A-Za-z0-9._+-]+"
                   (path->string (file-name-from-path p))
                   "-"))

;; unique-source-files : (listof path-string?) -> (listof path-string?)
;;   Deduplicate source files by full byte content.
(define (unique-source-files files)
  (define seen
    (make-hash))
  (for/fold ([unique '()])
            ([p (in-list files)])
    (define content
      (file->bytes p))
    (cond
      [(hash-has-key? seen content)
       unique]
      [else
       (hash-set! seen content #t)
       (cons p unique)])))

;; write-corpus! : (listof path-string?) -> void?
;;   Copy unique source files into the local corpus directory.
(define (write-corpus! files)
  (when (directory-exists? corpus-root)
    (delete-directory/files corpus-root))
  (make-directory* corpus-root)
  (for ([p     (in-list files)]
        [index (in-naturals 1)])
    (define target-name
      (~a (~r index #:min-width 4 #:pad-string "0")
          "-"
          (sanitize-file-name p)))
    (copy-file p
               (build-path corpus-root target-name))))

;; write-summary! : (listof path-string?) (listof path-string?) -> void?
;;   Write a build summary describing the assembled corpus.
(define (write-summary! all-files unique-files)
  (call-with-output-file summary-path
    (lambda (out)
      (fprintf out "corpus-root: ~a\n" corpus-root)
      (fprintf out "source-roots:\n")
      (for ([root (in-list (available-source-roots))])
        (fprintf out "  ~a\n" root))
      (fprintf out "source-files: ~a\n" (length all-files))
      (fprintf out "unique-files: ~a\n\n" (length unique-files))
      (for ([p (in-list unique-files)])
        (fprintf out "~a\n" p)))
    #:exists 'truncate/replace))

;; main : -> void?
;;   Build the Mathematica corpus and report the result.
(define (main)
  (define roots
    (available-source-roots))
  (cond
    [(empty? roots)
     (displayln "No configured Mathematica source roots are available.")
     (exit 0)]
    [else
     (define all-files
       (collect-source-files))
     (define unique-files
       (reverse (unique-source-files all-files)))
     (write-corpus! unique-files)
     (write-summary! all-files unique-files)
     (printf "Built Mathematica corpus in ~a\n" corpus-root)
     (printf "Source files: ~a\n" (length all-files))
     (printf "Unique files: ~a\n" (length unique-files))
     (printf "Wrote build summary to ~a\n" summary-path)]))

(module+ main
  (main))
