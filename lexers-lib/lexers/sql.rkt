#lang racket/base

;;;
;;; SQL Lexer
;;;
;;
;; Public entry points for the SQL lexer with generic, SQLite, PostgreSQL, and
;; MySQL dialect support.

;; make-sql-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based SQL lexer.
;; make-sql-derived-lexer : keyword-arguments -> (input-port? -> (or/c sql-derived-token? 'eof))
;;   Construct a port-based SQL lexer that returns derived token values.
;; sql-derived-token?     : any/c -> boolean?
;;   Recognize a derived SQL token value returned by the derived-token API.
;; sql-derived-token-tags : sql-derived-token? -> (listof symbol?)
;;   Extract the SQL-specific classification tags for one derived token.
;; sql-derived-token-has-tag? : sql-derived-token? symbol? -> boolean?
;;   Determine whether a derived SQL token has a given classification tag.
;; sql-derived-token-text : sql-derived-token? -> string?
;;   Extract the source text corresponding to one derived SQL token.
;; sql-derived-token-start : sql-derived-token? -> position?
;;   Extract the starting source position for one derived SQL token.
;; sql-derived-token-end  : sql-derived-token? -> position?
;;   Extract the ending source position for one derived SQL token.
;; sql-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire SQL string using the projected token API.
;; sql-string->derived-tokens : string? keyword-arguments -> (listof sql-derived-token?)
;;   Tokenize an entire SQL string into derived SQL token values.
;; sql-profiles           : immutable-hash?
;;   Profile defaults for the public SQL lexer.

(provide make-sql-lexer
         make-sql-derived-lexer
         sql-derived-token?
         sql-derived-token-tags
         sql-derived-token-has-tag?
         sql-derived-token-text
         sql-derived-token-start
         sql-derived-token-end
         sql-string->tokens
         sql-string->derived-tokens
         sql-profiles)

(require parser-tools/lex
         "private/config.rkt"
         (rename-in "private/sql-derived.rkt"
                    [sql-derived-token? private-sql-derived-token?]
                    [sql-derived-token-tags private-sql-derived-token-tags]
                    [sql-derived-token-has-tag? private-sql-derived-token-has-tag?]
                    [sql-derived-token-text private-sql-derived-token-text]
                    [sql-derived-token-start private-sql-derived-token-start]
                    [sql-derived-token-end private-sql-derived-token-end]
                    [make-sql-derived-reader private-make-sql-derived-reader])
         "private/sql-tokenize.rkt"
         "token.rkt")

(define sql-profiles
  sql-profile-defaults)

;; sql-derived-token? : any/c -> boolean?
;;   Recognize a derived SQL token value returned by the derived-token API.
(define (sql-derived-token? v)
  (private-sql-derived-token? v))

;; sql-derived-token-tags : sql-derived-token? -> (listof symbol?)
;;   Extract the SQL-specific classification tags for one derived token.
(define (sql-derived-token-tags token)
  (private-sql-derived-token-tags token))

;; sql-derived-token-has-tag? : sql-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (sql-derived-token-has-tag? token tag)
  (private-sql-derived-token-has-tag? token tag))

;; sql-derived-token-text : sql-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
(define (sql-derived-token-text token)
  (private-sql-derived-token-text token))

;; sql-derived-token-start : sql-derived-token? -> position?
;;   Extract the starting source position for one derived token.
(define (sql-derived-token-start token)
  (private-sql-derived-token-start token))

;; sql-derived-token-end : sql-derived-token? -> position?
;;   Extract the ending source position for one derived token.
(define (sql-derived-token-end token)
  (private-sql-derived-token-end token))

;; make-sql-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based SQL lexer.
(define (make-sql-lexer #:profile          [profile 'coloring]
                        #:trivia           [trivia 'profile-default]
                        #:source-positions [source-positions 'profile-default]
                        #:dialect          [dialect 'generic])
  (define config
    (make-sql-config #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions
                     #:dialect          dialect))
  (make-sql-token-reader config))

;; make-sql-derived-lexer : keyword-arguments -> (input-port? -> (or/c sql-derived-token? 'eof))
;;   Construct a port-based SQL lexer that returns derived token values.
(define (make-sql-derived-lexer #:dialect [dialect 'generic])
  (private-make-sql-derived-reader dialect))

;; sql-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire SQL string using the projected token API.
(define (sql-string->tokens source
                            #:profile          [profile 'coloring]
                            #:trivia           [trivia 'profile-default]
                            #:source-positions [source-positions 'profile-default]
                            #:dialect          [dialect 'generic])
  (define lexer
    (make-sql-lexer #:profile          profile
                    #:trivia           trivia
                    #:source-positions source-positions
                    #:dialect          dialect))
  (define in
    (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token
      (lexer in))
    (cond
      [(lexer-token-eof? token)
       (reverse (cons token tokens))]
      [else
       (loop (cons token tokens))])))

;; sql-string->derived-tokens : string? keyword-arguments -> (listof sql-derived-token?)
;;   Tokenize an entire SQL string into derived SQL token values.
(define (sql-string->derived-tokens source
                                    #:dialect [dialect 'generic])
  (define lexer
    (make-sql-derived-lexer #:dialect dialect))
  (define in
    (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token
      (lexer in))
    (cond
      [(eq? token 'eof)
       (reverse tokens)]
      [else
       (loop (cons token tokens))])))

(module+ test
  (require rackunit
           racket/list)

  ;; contiguous-derived-stream? : (listof sql-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (sql-derived-token-end left))
         (position-offset (sql-derived-token-start right)))))

  ;; tokens->text : (listof token-like?) -> string?
  ;;   Reconstruct source text from projected tokens, excluding eof.
  (define (tokens->text tokens)
    (apply string-append
           (for/list ([token (in-list (drop-right tokens 1))])
             (lexer-token-value token))))

  (define generic-source
    "SELECT id, name FROM people WHERE id = 10;\n")
  (define sqlite-source
    "SELECT x'ABCD', [group], `name` FROM \"items\" WHERE id = ?1;\r\n")
  (define postgres-source
    "SELECT $1, $$hello$$, E'line\\n', \"user\" FROM accounts WHERE note ILIKE '%ok%';\n")
  (define mysql-source
    "# comment\nSELECT _utf8'hej', `name`, @user, @@global.time_zone FROM users;\n")

  (define generic-derived
    (sql-string->derived-tokens generic-source))
  (define sqlite-derived
    (sql-string->derived-tokens sqlite-source
                                #:dialect 'sqlite))
  (define postgres-derived
    (sql-string->derived-tokens postgres-source
                                #:dialect 'postgres))
  (define mysql-derived
    (sql-string->derived-tokens mysql-source
                                #:dialect 'mysql))
  (define sqlite-tokens
    (sql-string->tokens sqlite-source
                        #:dialect          'sqlite
                        #:profile          'coloring
                        #:source-positions #f))
  (define postgres-tokens
    (sql-string->tokens postgres-source
                        #:dialect          'postgres
                        #:profile          'coloring
                        #:source-positions #f))
  (define mysql-tokens
    (sql-string->tokens mysql-source
                        #:dialect          'mysql
                        #:profile          'coloring
                        #:source-positions #f))
  (define malformed-derived
    (sql-string->derived-tokens "SELECT 'unterminated"
                                #:dialect 'generic))

  (check-true (contiguous-derived-stream? generic-derived))
  (check-true (contiguous-derived-stream? sqlite-derived))
  (check-true (contiguous-derived-stream? postgres-derived))
  (check-true (contiguous-derived-stream? mysql-derived))
  (check-equal? (apply string-append (map sql-derived-token-text generic-derived))
                generic-source)
  (check-equal? (apply string-append (map sql-derived-token-text sqlite-derived))
                sqlite-source)
  (check-equal? (apply string-append (map sql-derived-token-text postgres-derived))
                postgres-source)
  (check-equal? (apply string-append (map sql-derived-token-text mysql-derived))
                mysql-source)
  (check-equal? (tokens->text sqlite-tokens)
                sqlite-source)
  (check-equal? (tokens->text postgres-tokens)
                postgres-source)
  (check-equal? (tokens->text mysql-tokens)
                mysql-source)
  (check-not-false (ormap (lambda (token)
                            (sql-derived-token-has-tag? token 'sql-dollar-string))
                          postgres-derived))
  (check-not-false (ormap (lambda (token)
                            (sql-derived-token-has-tag? token 'sql-quoted-identifier))
                          sqlite-derived))
  (check-not-false (ormap (lambda (token)
                            (sql-derived-token-has-tag? token 'sql-parameter))
                          mysql-derived))
  (check-not-false (sql-derived-token-has-tag? (last malformed-derived)
                                               'malformed-token)))
