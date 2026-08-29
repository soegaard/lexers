#lang racket/base

;;;
;;; Lua Corpus Checker
;;;

;; Check exact source round-tripping through both public Lua lexer APIs.

(require parser-tools/lex racket/file racket/list racket/path racket/string rackunit
         "../lexers-lib/lexers/lua.rkt"
         "../lexers-lib/lexers/token.rkt")

(define corpus-root "/tmp/lexers-lua-corpus")
(define (main)
  (unless (directory-exists? corpus-root)
    (displayln "Skipping Lua corpus check: local corpus is unavailable.")
    (exit 0))
  (define files
    (sort (for/list ([path (in-directory corpus-root)]
                     #:when (and (file-exists? path)
                                 (not (equal? (path->string (file-name-from-path path)) "summary.txt"))) )
            (path->string path)) string<?))
  (define failures
    (for/list ([path (in-list files)]
               #:unless (let* ([source (file->string path)]
                                [derived (lua-string->derived-tokens source)]
                                [projected (lua-string->tokens source #:source-positions #f)])
                          (and (string=? source (apply string-append (map lua-derived-token-text derived)))
                               (string=? source (apply string-append
                                                       (for/list ([token (in-list (drop-right projected 1))])
                                                         (lexer-token-value token)))))))
      path))
  (call-with-output-file (build-path corpus-root "summary.txt")
    (lambda (out) (fprintf out "files: ~a\nroundtrip-failures: ~a\n" (length files) (length failures)))
    #:exists 'truncate/replace)
  (check-equal? failures '()))
(module+ main (main))
(module+ test (main))
