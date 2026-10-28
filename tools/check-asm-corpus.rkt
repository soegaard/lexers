#lang racket/base

;;;
;;; Assembly Corpus Checker
;;;

;; Verify byte-for-byte reconstruction for optional local assembly corpus
;; sources under the profile that their corpus directory selects.

;; main : -> void?
;;   Run the optional local assembly corpus checker.

(require racket/file
         racket/list
         racket/match
         racket/path
         racket/string
         rackunit
         "../lexers-lib/lexers/asm.rkt")

;; Corpus configuration.
(define corpus-root
  "/tmp/asm-corpus")

(define development-root
  (build-path corpus-root "development"))

(define timeout-seconds
  15.0)

;; skip-if-missing! : -> void?
;;   Skip cleanly when the optional local assembly corpus is unavailable.
(define (skip-if-missing!)
  (unless (directory-exists? development-root)
    (displayln "Skipping assembly corpus check: local corpus is unavailable.")
    (exit 0)))

;; source-file? : path-string? -> boolean?
;;   Keep only assembly source and include files from the selected corpus trees.
(define (source-file? path)
  (member (path-get-extension path)
          '(#".S" #".asm" #".i" #".inc" #".s")))

;; path-components : path-string? -> (listof string?)
;;   Split a normalized relative corpus path into portable string components.
(define (path-components path)
  (map path->string
       (explode-path (simplify-path path))))

;; source-profile : path-string? -> (or/c (list/c symbol? (or/c symbol? #f)) #f)
;;   Select the explicit lexer profile encoded by a development corpus path.
(define (source-profile path)
  (match (path-components (find-relative-path development-root path))
    [(list "ca65" _ ...)           (list 'ca65 #f)]
    [(list "nasm" _ ...)           (list 'nasm #f)]
    [(list "vasm" _ ...)           (list 'vasm #f)]
    [(list "gas" "m68k" _ ...)    (list 'gas 'm68k)]
    [(list "gas" "x86" _ ...)     (list 'gas 'x86)]
    [(list "gas" "arm" _ ...)     (list 'gas 'arm)]
    [(list "gas" "aarch64" _ ...) (list 'gas 'aarch64)]
    [_                              #f]))

;; corpus-files : -> (listof path-string?)
;;   Enumerate selected local corpus files in deterministic order.
(define (corpus-files)
  (sort
   (for/list ([path (in-directory development-root)]
              #:when (and (file-exists? path)
                          (source-file? path)
                          (source-profile path)))
     (path->string path))
   string<?))

;; with-timeout : real? (-> any/c) -> (or/c 'timeout any/c)
;;   Run a bounded corpus operation without leaving an unbounded test behind.
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

;; read-derived : bytes? symbol? (or/c symbol? #f) -> (listof asm-derived-token?)
;;   Tokenize bytes under their explicit corpus profile.
(define (read-derived source dialect architecture)
  (define lexer
    (make-asm-derived-lexer #:dialect      dialect
                            #:architecture architecture))
  (define in
    (open-input-bytes source))
  (let loop ([tokens '()])
    (define token
      (lexer in))
    (cond
      [(eq? token 'eof)
       (reverse tokens)]
      [else
       (loop (cons token tokens))])))

;; check-source : path-string? -> (list boolean? exact-nonnegative-integer?)
;;   Check one corpus source's byte-exact derived-token reconstruction.
(define (check-source path)
  (match-define (list dialect architecture)
    (source-profile path))
  (define source
    (file->bytes path))
  (define tokens
    (read-derived source dialect architecture))
  (list (bytes=? source
                 (apply bytes-append
                        (map asm-derived-token-raw-bytes tokens)))
        (length tokens)))

;; main : -> void?
;;   Check each selected source and report aggregate failures or timeouts.
(define (main)
  (skip-if-missing!)
  (define files
    (corpus-files))
  (define failures
    '())
  (define timeouts
    '())
  (for ([path (in-list files)])
    (define outcome
      (with-timeout timeout-seconds
        (lambda ()
          (with-handlers ([exn:fail?
                           (lambda (exception)
                             (list 'error (exn-message exception)))])
            (check-source path)))))
    (cond
      [(eq? outcome 'timeout)
       (set! timeouts (cons path timeouts))]
      [(and (pair? outcome)
            (eq? (car outcome) 'error))
       (set! failures (cons (format "~a: ~a" path (cadr outcome)) failures))]
      [(not (car outcome))
       (set! failures (cons (format "~a: bytes did not round-trip" path)
                            failures))]))
  (displayln (format "Checked ~a assembly corpus files." (length files)))
  (check-equal? (reverse timeouts) '())
  (check-equal? (reverse failures) '()))

(module+ main
  (main))

(module+ test
  (main))
