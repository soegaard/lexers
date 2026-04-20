#lang racket/base

;;;
;;; Delimited Corpus Checker
;;;
;;
;; Development utility for running the CSV and TSV lexers across a copied
;; delimited-text corpus in /tmp with per-file timeouts and round-trip checks.

(require racket/file
         racket/list
         racket/match
         racket/path
         lexers/csv
         lexers/tsv
         lexers/token)

;; corpus-root : path-string?
;;   Copied delimited-text corpus root.
(define corpus-root
  "/tmp/lexers-delimited-corpus")

;; summary-path : path-string?
;;   Output summary written after the corpus run.
(define summary-path
  "/tmp/lexers-delimited-corpus/summary.txt")

;; timeout-seconds : real?
;;   Hard time limit for lexing one file.
(define timeout-seconds
  2.0)

;; corpus-files : -> (listof path?)
;;   Collect copied CSV and TSV files from the corpus directory.
(define (corpus-files)
  (sort
   (for/list ([p (in-directory corpus-root)]
              #:when (file-exists? p)
              #:when (member (path-get-extension p)
                             '(#".csv" #".tsv"))
              #:unless (equal? (simplify-path p)
                               (simplify-path summary-path)))
     p)
   string<?
   #:key path->string))

;; relative-corpus-path : path? -> string?
;;   Render one file path relative to the copied corpus root.
(define (relative-corpus-path p)
  (path->string (find-relative-path corpus-root p)))

;; lexer-kind : path? -> symbol?
;;   Select the lexer family for one file.
(define (lexer-kind p)
  (case (path-get-extension p)
    [(#".csv") 'csv]
    [(#".tsv") 'tsv]
    [else
     (error 'lexer-kind "unsupported delimited corpus file: ~a" p)]))

;; lex-file : path? -> (list/c symbol? any/c)
;;   Lex one file with a timeout and return status plus data.
(define (lex-file p)
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
          (case (lexer-kind p)
            [(csv)
             (define projected
               (csv-string->tokens source
                                   #:profile 'coloring
                                   #:source-positions #f))
             (define derived
               (csv-string->derived-tokens source))
             (define derived-text
               (apply string-append
                      (map csv-derived-token-text derived)))
             (define projected-text
               (apply string-append
                      (for/list ([token (in-list projected)]
                                 #:unless (lexer-token-eof? token))
                        (lexer-token-value token))))
             (list 'ok
                   (hash 'kind 'csv
                         'projected-count (length projected)
                         'derived-count (length derived)
                         'derived-roundtrip? (string=? derived-text source)
                         'projected-roundtrip? (string=? projected-text source)))]
            [(tsv)
             (define projected
               (tsv-string->tokens source
                                   #:profile 'coloring
                                   #:source-positions #f))
             (define derived
               (tsv-string->derived-tokens source))
             (define derived-text
               (apply string-append
                      (map tsv-derived-token-text derived)))
             (define projected-text
               (apply string-append
                      (for/list ([token (in-list projected)]
                                 #:unless (lexer-token-eof? token))
                        (lexer-token-value token))))
             (list 'ok
                   (hash 'kind 'tsv
                         'projected-count (length projected)
                         'derived-count (length derived)
                         'derived-roundtrip? (string=? derived-text source)
                         'projected-roundtrip? (string=? projected-text source)))]))))))
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
        (fprintf out "~a\t~a"
                 (hash-ref r 'status)
                 (hash-ref r 'path))
        (cond
          [(hash-has-key? r 'message)
           (fprintf out "\t~a" (hash-ref r 'message))]
          [(hash-has-key? r 'seconds)
           (fprintf out "\t~a" (hash-ref r 'seconds))]
          [else
           (fprintf out "\t~a\tderived=~a/projected=~a\t~a\t~a"
                    (hash-ref r 'kind)
                    (hash-ref r 'derived-count)
                    (hash-ref r 'projected-count)
                    (hash-ref r 'derived-roundtrip?)
                    (hash-ref r 'projected-roundtrip?))])
        (newline out)))
    #:exists 'replace))

;; main : -> void?
;;   Run the CSV and TSV lexers across the copied corpus.
(define (main)
  (define files
    (corpus-files))
  (define results
    (for/list ([p (in-list files)])
      (match (lex-file p)
        [(list 'ok stats)
         (hash 'path (relative-corpus-path p)
               'status 'ok
               'kind (hash-ref stats 'kind)
               'projected-count (hash-ref stats 'projected-count)
               'derived-count (hash-ref stats 'derived-count)
               'derived-roundtrip? (hash-ref stats 'derived-roundtrip?)
               'projected-roundtrip? (hash-ref stats 'projected-roundtrip?))]
        [(list 'error msg)
         (hash 'path (relative-corpus-path p)
               'status 'error
               'message msg)]
        [(list 'timeout seconds)
         (hash 'path (relative-corpus-path p)
               'status 'timeout
               'seconds seconds)])))
  (write-summary! results)
  (printf "Wrote summary to ~a\n" summary-path))

(main)
