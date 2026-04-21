#lang racket/base

;;;
;;; Pascal Corpus Checker
;;;
;;
;; Round-trip and timeout checks for a local Pascal corpus.

(require parser-tools/lex
         racket/file
         racket/list
         racket/match
         racket/path
         racket/string
         rackunit
         "../lexers-lib/lexers/pascal.rkt")

(define corpus-root
  "/tmp/lexers-pascal-corpus")

(define timeout-seconds
  2.0)

;; skip-if-missing! : -> void?
;;   Skip the tool test cleanly when the local corpus directory is unavailable.
(define (skip-if-missing!)
  (unless (directory-exists? corpus-root)
    (displayln "Skipping Pascal corpus check: local corpus is unavailable.")
    (exit 0)))

;; corpus-files : -> (listof path-string?)
;;   Enumerate corpus files in sorted order.
(define (corpus-files)
  (sort (for/list ([p (in-directory corpus-root)]
                   #:when (and (file-exists? p)
                               (not (equal? (path->string (file-name-from-path p))
                                            "summary.txt"))))
          (path->string p))
        string<?))

;; with-timeout : real? (-> any) -> (or/c 'timeout any)
;;   Run thunk with a timeout in seconds.
(define (with-timeout timeout thunk)
  (define result-channel
    (make-channel))
  (thread
   (lambda ()
     (channel-put result-channel (thunk))))
  (define result
    (sync/timeout timeout result-channel))
  (cond
    [result result]
    [else   'timeout]))

;; token-text* : any/c -> string?
;;   Extract token text from projected tokens.
(define (token-text* token)
  (cond
    [(symbol? token) ""]
    [else
     (token-value token)]))

;; summarize-roundtrip : string? -> (list boolean? boolean? exact-nonnegative-integer? exact-nonnegative-integer?)
;;   Check source round-trips through both public Pascal APIs.
(define (summarize-roundtrip source)
  (define derived
    (pascal-string->derived-tokens source))
  (define projected
    (pascal-string->tokens source #:profile 'coloring #:source-positions #f))
  (list (string=? source (apply string-append (map pascal-derived-token-text derived)))
        (string=? source (apply string-append (map token-text* (drop-right projected 1))))
        (length derived)
        (length projected)))

;; format-result : string? list? -> string?
;;   Render one summary line.
(define (format-result path result)
  (match-define (list derived-ok?
                      projected-ok?
                      derived-count
                      projected-count)
    result)
  (format "ok\t~a\tderived=~a/projected=~a\t~a\t~a"
          (file-name-from-path path)
          derived-count
          projected-count
          derived-ok?
          projected-ok?))

;; main : -> void?
;;   Run the corpus checker and write a summary file.
(define (main)
  (skip-if-missing!)
  (define files
    (corpus-files))
  (define oks
    0)
  (define errors
    0)
  (define timeouts
    0)
  (define derived-failures
    0)
  (define projected-failures
    0)
  (define lines
    '())
  (for ([path (in-list files)])
    (define source
      (file->string path))
    (define outcome
      (with-timeout timeout-seconds
        (lambda ()
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (list 'error (exn-message e)))])
            (summarize-roundtrip source)))))
    (cond
      [(eq? outcome 'timeout)
       (set! timeouts (add1 timeouts))
       (set! lines
             (cons (format "timeout\t~a" (file-name-from-path path))
                   lines))]
      [(and (pair? outcome)
            (eq? (car outcome) 'error))
       (set! errors (add1 errors))
       (set! lines
             (cons (format "error\t~a\t~a"
                           (file-name-from-path path)
                           (cadr outcome))
                   lines))]
      [else
       (match-define (list derived-ok?
                           projected-ok?
                           _ _)
         outcome)
       (unless derived-ok?
         (set! derived-failures (add1 derived-failures)))
       (unless projected-ok?
         (set! projected-failures (add1 projected-failures)))
       (when (and derived-ok? projected-ok?)
         (set! oks (add1 oks)))
       (set! lines
             (cons (format-result path outcome) lines))]))
  (define summary
    (string-append
     (format "corpus-root: ~a\n" corpus-root)
     (format "timeout-seconds: ~a\n" timeout-seconds)
     (format "files: ~a\n" (length files))
     (format "ok: ~a\n" oks)
     (format "errors: ~a\n" errors)
     (format "timeouts: ~a\n" timeouts)
     (format "derived-roundtrip-failures: ~a\n" derived-failures)
     (format "projected-roundtrip-failures: ~a\n\n" projected-failures)
     (string-join (reverse lines) "\n")
     "\n"))
  (define summary-path
    (build-path corpus-root "summary.txt"))
  (call-with-output-file summary-path
    (lambda (out)
      (display summary out))
    #:exists 'truncate/replace)
  (displayln (format "Wrote summary to ~a" summary-path))
  (check-equal? errors 0)
  (check-equal? timeouts 0)
  (check-equal? derived-failures 0)
  (check-equal? projected-failures 0))

(module+ main
  (main))

(module+ test
  (main))
