#lang racket/base

;;;
;;; SQL Corpus Builder
;;;
;;
;; Build a local SQL corpus in /tmp with separate buckets for generic SQL,
;; SQLite, PostgreSQL, and MySQL sources.

(require racket/path
         racket/string
         "private/corpus-builder.rkt")

;; corpus-root : path-string?
;;   Output directory for the copied SQL corpus buckets.
(define corpus-root
  "/tmp/lexers-sql-corpus")

;; summary-path : path-string?
;;   Summary file written after the corpus is assembled.
(define summary-path
  "/tmp/lexers-sql-corpus-build-summary.txt")

;; core-source-roots : (listof path-string?)
;;   Local generic SQL sample roots.
(define core-source-roots
  '("/Users/soegaard/Dropbox/GitHub/linguist/samples/SQL"
    "/Users/soegaard/Dropbox/GitHub/linguist/samples/PLpgSQL"
    "/Users/soegaard/Dropbox/GitHub/linguist/samples/PLSQL"
    "/Users/soegaard/Dropbox/GitHub/linguist/samples/SQLPL"))

;; sqlite-source-roots : (listof path-string?)
;;   Official SQLite source roots that contain `.sql` files.
(define sqlite-source-roots
  '("/tmp/sql-grammar-sources/sqlite"))

;; postgres-source-roots : (listof path-string?)
;;   Official PostgreSQL test roots that contain `.sql` files.
(define postgres-source-roots
  '("/tmp/sql-grammar-sources/postgres/src/test/regress/sql"))

;; mysql-source-roots : (listof path-string?)
;;   Official MySQL source roots that contain `.sql` files.
(define mysql-source-roots
  '("/tmp/sql-grammar-sources/mysql-server"))

;; sql-extension? : path? -> boolean?
;;   Determine whether a path has a plain `.sql` extension.
(define (sql-extension? p)
  (define ext
    (path-get-extension p))
  (and (bytes? ext)
       (string-ci=? (bytes->string/utf-8 ext) ".sql")))

;; generic-sql-path? : path? -> boolean?
;;   Determine whether a path belongs in the generic SQL bucket.
(define (generic-sql-path? p)
  (sql-extension? p))

;; sqlite-sql-path? : path? -> boolean?
;;   Determine whether a path belongs in the SQLite bucket.
(define (sqlite-sql-path? p)
  (define path-string
    (path->string p))
  (and (sql-extension? p)
       (not (regexp-match? #px"/ext/" path-string))))

;; postgres-sql-path? : path? -> boolean?
;;   Determine whether a path belongs in the PostgreSQL bucket.
(define (postgres-sql-path? p)
  (sql-extension? p))

;; mysql-sql-path? : path? -> boolean?
;;   Determine whether a path belongs in the MySQL bucket.
(define (mysql-sql-path? p)
  (define path-string
    (path->string p))
  (and (sql-extension? p)
       (not (regexp-match? #px"/extra/" path-string))
       (not (regexp-match? #px"/router/tests/component/data/" path-string))))

;; buckets : (listof list?)
;;   Bucket configurations for the shared builder helper.
(define buckets
  (list (list "core"     core-source-roots     generic-sql-path?)
        (list "sqlite"   sqlite-source-roots   sqlite-sql-path?)
        (list "postgres" postgres-source-roots postgres-sql-path?)
        (list "mysql"    mysql-source-roots    mysql-sql-path?)))

;; main : -> void?
;;   Build the SQL corpus and report the result.
(define (main)
  (build-bucketed-corpus #:name         "SQL"
                         #:corpus-root  corpus-root
                         #:summary-path summary-path
                         #:buckets      buckets))

(module+ main
  (main))
