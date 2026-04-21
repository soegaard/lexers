#lang racket/base

;;;
;;; TeX Corpus Checker
;;;
;;
;; Round-trip and timeout checks for a local TeX and LaTeX corpus.

(require racket/file
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         parser-tools/lex
         rackunit
         "../lexers-lib/lexers/latex.rkt"
         "../lexers-lib/lexers/tex.rkt")

(define corpus-root
  "/tmp/lexers-tex-corpus")

(define timeout-seconds
  2.0)

;; skip-if-missing! : -> void?
;;   Skip the tool test cleanly when the local corpus directory is unavailable.
(define (skip-if-missing!)
  (unless (directory-exists? corpus-root)
    (displayln "Skipping TeX corpus check: local corpus is unavailable.")
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

;; summarize-roundtrip : string? -> (list boolean? boolean? boolean? boolean? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?)
;;   Check source round-trips through both public wrappers.
(define (summarize-roundtrip source)
  (define tex-derived
    (tex-string->derived-tokens source))
  (define tex-projected
    (tex-string->tokens source #:profile 'coloring #:source-positions #f))
  (define latex-derived
    (latex-string->derived-tokens source))
  (define latex-projected
    (latex-string->tokens source #:profile 'coloring #:source-positions #f))
  (list (string=? source (apply string-append (map tex-derived-token-text tex-derived)))
        (string=? source (apply string-append (map token-text* (drop-right tex-projected 1))))
        (string=? source (apply string-append (map latex-derived-token-text latex-derived)))
        (string=? source (apply string-append (map token-text* (drop-right latex-projected 1))))
        (length tex-derived)
        (length tex-projected)
        (length latex-derived)
        (length latex-projected)))

;; format-result : string? list? -> string?
;;   Render one summary line.
(define (format-result path result)
  (match-define (list tex-derived-ok?
                      tex-projected-ok?
                      latex-derived-ok?
                      latex-projected-ok?
                      tex-derived-count
                      tex-projected-count
                      latex-derived-count
                      latex-projected-count)
    result)
  (format "ok\t~a\ttex=~a/~a\tlatex=~a/~a\t~a\t~a\t~a\t~a"
          (file-name-from-path path)
          tex-derived-count
          tex-projected-count
          latex-derived-count
          latex-projected-count
          tex-derived-ok?
          tex-projected-ok?
          latex-derived-ok?
          latex-projected-ok?))

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
  (define tex-derived-failures
    0)
  (define tex-projected-failures
    0)
  (define latex-derived-failures
    0)
  (define latex-projected-failures
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
       (match-define (list tex-derived-ok?
                           tex-projected-ok?
                           latex-derived-ok?
                           latex-projected-ok?
                           _ _ _ _)
         outcome)
       (unless tex-derived-ok?
         (set! tex-derived-failures (add1 tex-derived-failures)))
       (unless tex-projected-ok?
         (set! tex-projected-failures (add1 tex-projected-failures)))
       (unless latex-derived-ok?
         (set! latex-derived-failures (add1 latex-derived-failures)))
       (unless latex-projected-ok?
         (set! latex-projected-failures (add1 latex-projected-failures)))
       (when (and tex-derived-ok?
                  tex-projected-ok?
                  latex-derived-ok?
                  latex-projected-ok?)
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
     (format "tex-derived-roundtrip-failures: ~a\n" tex-derived-failures)
     (format "tex-projected-roundtrip-failures: ~a\n" tex-projected-failures)
     (format "latex-derived-roundtrip-failures: ~a\n" latex-derived-failures)
     (format "latex-projected-roundtrip-failures: ~a\n\n" latex-projected-failures)
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
  (check-equal? tex-derived-failures 0)
  (check-equal? tex-projected-failures 0)
  (check-equal? latex-derived-failures 0)
  (check-equal? latex-projected-failures 0))

(module+ main
  (main))

(module+ test
  (main))
