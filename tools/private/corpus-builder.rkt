#lang racket/base

;;;
;;; Corpus Builder Helper
;;;
;;
;; Shared helper for assembling local /tmp corpora from configured source
;; trees, deduplicated by file contents.

;; build-corpus : keyword-arguments -> void?
;;   Build one local corpus from the configured roots and predicates.
;; build-bucketed-corpus : keyword-arguments -> void?
;;   Build one local corpus root with multiple named sub-corpora.

(provide build-corpus
         build-bucketed-corpus)

(require racket/file
         racket/format
         racket/list
         racket/path
         racket/string)

;; available-source-roots : (listof path-string?) -> (listof path-string?)
;;   Keep only source roots that exist on this machine.
(define (available-source-roots source-roots)
  (filter directory-exists?
          source-roots))

;; collect-source-files : (listof path-string?) (path? -> boolean?) -> (listof path-string?)
;;   Collect matching source files from the configured local roots.
(define (collect-source-files source-roots include-path?)
  (sort
   (append*
    (for/list ([root (in-list (available-source-roots source-roots))])
      (for/list ([p (in-directory root)]
                 #:when (and (file-exists? p)
                             (include-path? p)))
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

;; write-corpus! : path-string? (listof path-string?) -> void?
;;   Copy unique source files into one local corpus directory.
(define (write-corpus! corpus-root files)
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

;; write-summary! : path-string? path-string? (listof path-string?) (listof path-string?) -> void?
;;   Write a build summary describing the assembled corpus.
(define (write-summary! corpus-root summary-path source-roots all-files unique-files)
  (call-with-output-file summary-path
    (lambda (out)
      (fprintf out "corpus-root: ~a\n" corpus-root)
      (fprintf out "source-roots:\n")
      (for ([root (in-list (available-source-roots source-roots))])
        (fprintf out "  ~a\n" root))
      (fprintf out "source-files: ~a\n" (length all-files))
      (fprintf out "unique-files: ~a\n\n" (length unique-files))
      (for ([p (in-list unique-files)])
        (fprintf out "~a\n" p)))
    #:exists 'truncate/replace))

;; build-corpus : keyword-arguments -> void?
;;   Build one local corpus from the configured roots and predicates.
(define (build-corpus #:name          name
                      #:corpus-root   corpus-root
                      #:summary-path  summary-path
                      #:source-roots  source-roots
                      #:include-path? include-path?)
  (define roots
    (available-source-roots source-roots))
  (cond
    [(empty? roots)
     (printf "No configured ~a source roots are available.\n" name)
     (exit 0)]
    [else
     (define all-files
       (collect-source-files source-roots include-path?))
     (define unique-files
       (reverse (unique-source-files all-files)))
     (write-corpus! corpus-root unique-files)
     (write-summary! corpus-root
                     summary-path
                     source-roots
                     all-files
        unique-files)
     (printf "Built ~a corpus in ~a\n" name corpus-root)
     (printf "Source files: ~a\n" (length all-files))
     (printf "Unique files: ~a\n" (length unique-files))
     (printf "Wrote build summary to ~a\n" summary-path)]))

;; -----------------------------------------------------------------------------
;; Bucketed corpus support

;; bucket-name : list? -> string?
;;   Extract the display name for one bucket configuration.
(define (bucket-name bucket)
  (list-ref bucket 0))

;; bucket-source-roots : list? -> (listof path-string?)
;;   Extract the source roots for one bucket configuration.
(define (bucket-source-roots bucket)
  (list-ref bucket 1))

;; bucket-include-path? : list? -> (path? -> boolean?)
;;   Extract the inclusion predicate for one bucket configuration.
(define (bucket-include-path? bucket)
  (list-ref bucket 2))

;; build-bucket! : path-string? list? -> list?
;;   Build one named bucket and report its source and unique files.
(define (build-bucket! corpus-root bucket)
  (define name
    (bucket-name bucket))
  (define source-roots
    (bucket-source-roots bucket))
  (define include-path?
    (bucket-include-path? bucket))
  (define all-files
    (collect-source-files source-roots include-path?))
  (define unique-files
    (reverse (unique-source-files all-files)))
  (write-corpus! (build-path corpus-root name)
                 unique-files)
  (list name source-roots all-files unique-files))

;; write-bucketed-summary! : path-string? path-string? (listof list?) -> void?
;;   Write a build summary for a bucketed corpus.
(define (write-bucketed-summary! corpus-root summary-path bucket-results)
  (call-with-output-file summary-path
    (lambda (out)
      (fprintf out "corpus-root: ~a\n" corpus-root)
      (fprintf out "buckets: ~a\n\n" (length bucket-results))
      (for ([result (in-list bucket-results)])
        (define name
          (list-ref result 0))
        (define source-roots
          (list-ref result 1))
        (define all-files
          (list-ref result 2))
        (define unique-files
          (list-ref result 3))
        (fprintf out "[~a]\n" name)
        (fprintf out "source-roots:\n")
        (for ([root (in-list (available-source-roots source-roots))])
          (fprintf out "  ~a\n" root))
        (fprintf out "source-files: ~a\n" (length all-files))
        (fprintf out "unique-files: ~a\n" (length unique-files))
        (for ([p (in-list unique-files)])
          (fprintf out "  ~a\n" p))
        (newline out)))
    #:exists 'truncate/replace))

;; build-bucketed-corpus : keyword-arguments -> void?
;;   Build one local corpus root with multiple named sub-corpora.
(define (build-bucketed-corpus #:name         name
                               #:corpus-root  corpus-root
                               #:summary-path summary-path
                               #:buckets      buckets)
  (define available-buckets
    (filter (lambda (bucket)
              (not (empty? (available-source-roots (bucket-source-roots bucket)))))
            buckets))
  (cond
    [(empty? available-buckets)
     (printf "No configured ~a source roots are available.\n" name)
     (exit 0)]
    [else
     (when (directory-exists? corpus-root)
       (delete-directory/files corpus-root))
     (make-directory* corpus-root)
     (define bucket-results
       (for/list ([bucket (in-list available-buckets)])
         (build-bucket! corpus-root bucket)))
     (write-bucketed-summary! corpus-root
                             summary-path
                             bucket-results)
     (printf "Built ~a corpus in ~a\n" name corpus-root)
     (for ([result (in-list bucket-results)])
       (define name
         (list-ref result 0))
       (define all-files
         (list-ref result 2))
       (define unique-files
         (list-ref result 3))
       (printf "  ~a: source-files=~a unique-files=~a\n"
               name
               (length all-files)
               (length unique-files)))
     (printf "Wrote build summary to ~a\n" summary-path)]))
