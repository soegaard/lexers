#lang racket/base

;;;
;;; SQL Corpus Checker
;;;
;;
;; Round-trip and timeout checks for a local SQL corpus with dialect buckets.

(require parser-tools/lex
         racket/file
         racket/list
         racket/match
         racket/path
         racket/string
         rackunit
         "../lexers-lib/lexers/sql.rkt")

;; corpus-root : path-string?
;;   Root directory containing the local SQL corpus buckets.
(define corpus-root
  "/tmp/lexers-sql-corpus")

;; base-timeout-seconds : real?
;;   Minimum timeout used for each corpus file.
(define base-timeout-seconds
  5.0)

;; max-timeout-seconds : real?
;;   Upper bound for one corpus-file timeout.
(define max-timeout-seconds
  180.0)

;; skip-if-missing! : -> void?
;;   Skip the tool test cleanly when the local corpus directory is unavailable.
(define (skip-if-missing!)
  (unless (directory-exists? corpus-root)
    (displayln "Skipping SQL corpus check: local corpus is unavailable.")
    (exit 0)))

;; corpus-files : -> (listof path-string?)
;;   Enumerate corpus files in sorted order.
(define (corpus-files)
  (sort (for/list ([p (in-directory corpus-root)]
                   #:when (let ([name (path->string (file-name-from-path p))])
                            (and (file-exists? p)
                                 (not (equal? name "summary.txt"))
                                 (not (regexp-match? #px"^\\." name)))))
          (path->string p))
        string<?))

;; path->dialect : path-string? -> symbol?
;;   Infer the SQL dialect from the first corpus path segment.
(define (path->dialect p)
  (define simplified
    (simplify-path p))
  (define parts
    (explode-path simplified))
  (define root-parts
    (explode-path (simplify-path corpus-root)))
  (define relative-parts
    (drop parts (length root-parts)))
  (define bucket
    (path->string (car relative-parts)))
  (cond
    [(string=? bucket "sqlite")   'sqlite]
    [(string=? bucket "postgres") 'postgres]
    [(string=? bucket "mysql")    'mysql]
    [else                         'generic]))

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

;; timeout-for-path : path-string? -> real?
;;   Choose a practical timeout based on corpus file size.
(define (timeout-for-path path)
  (define bytes
    (file-size path))
  (define extra-seconds
    (/ bytes 500000.0))
  (min max-timeout-seconds
       (+ base-timeout-seconds extra-seconds)))

;; token-text* : any/c -> string?
;;   Extract token text from projected tokens.
(define (token-text* token)
  (cond
    [(symbol? token) ""]
    [else
     (token-value token)]))

;; summarize-roundtrip : symbol? string? -> list?
;;   Check source round-trips through both public SQL APIs.
(define (summarize-roundtrip dialect source)
  (define derived
    (sql-string->derived-tokens source
                                #:dialect dialect))
  (define projected
    (sql-string->tokens source
                        #:dialect          dialect
                        #:profile          'coloring
                        #:source-positions #f))
  (list (string=? source (apply string-append (map sql-derived-token-text derived)))
        (string=? source (apply string-append (map token-text* (drop-right projected 1))))
        (length derived)
        (length projected)))

;; format-result : string? symbol? list? -> string?
;;   Render one summary line.
(define (format-result path dialect result)
  (match-define (list derived-ok?
                      projected-ok?
                      derived-count
                      projected-count)
    result)
  (format "ok\t~a\t~a\tderived=~a/projected=~a\t~a\t~a"
          dialect
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
    (define dialect
      (path->dialect path))
    (define timeout-seconds
      (timeout-for-path path))
    (define source
      (file->string path))
    (define outcome
      (with-timeout timeout-seconds
        (lambda ()
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (list 'error (exn-message e)))])
            (summarize-roundtrip dialect source)))))
    (cond
      [(eq? outcome 'timeout)
       (set! timeouts (add1 timeouts))
       (set! lines
             (cons (format "timeout\t~a\t~a"
                           dialect
                           (file-name-from-path path))
                   lines))]
      [(and (pair? outcome)
            (eq? (car outcome) 'error))
       (set! errors (add1 errors))
       (set! lines
             (cons (format "error\t~a\t~a\t~a"
                           dialect
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
             (cons (format-result path dialect outcome) lines))]))
  (define summary
    (string-append
     (format "corpus-root: ~a\n" corpus-root)
     (format "base-timeout-seconds: ~a\n" base-timeout-seconds)
     (format "max-timeout-seconds: ~a\n" max-timeout-seconds)
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
