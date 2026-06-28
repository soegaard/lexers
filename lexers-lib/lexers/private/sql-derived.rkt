#lang racket/base

;;;
;;; SQL Derived Tokens
;;;
;;
;; Streaming SQL tokenization with dialect-aware reusable classifications.

;; sql-derived-token?         : any/c -> boolean?
;;   Recognize a derived SQL token.
;; sql-derived-token-text     : sql-derived-token? -> string?
;;   Extract the source text for one derived token.
;; sql-derived-token-start    : sql-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; sql-derived-token-end      : sql-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; sql-derived-token-tags     : sql-derived-token? -> (listof symbol?)
;;   Extract reusable SQL classification tags.
;; sql-derived-token-has-tag? : sql-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-sql-derived-reader    : [symbol?] -> (input-port? -> (or/c sql-derived-token? 'eof))
;;   Construct a stateful SQL derived-token reader.

(provide sql-derived-token?
         sql-derived-token-text
         sql-derived-token-start
         sql-derived-token-end
         sql-derived-token-tags
         sql-derived-token-has-tag?
         make-sql-derived-reader)

(require parser-tools/lex
         racket/list
         racket/string
         "parser-tools-compat.rkt")

;; A SQL token plus reusable tags.
(struct sql-derived-token (kind text start end tags) #:transparent)

;; sql-derived-token-has-tag? : sql-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (sql-derived-token-has-tag? token tag)
  (member tag (sql-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Dialect and keyword tables

;; supported-dialects : (listof symbol?)
;;   Dialect names accepted by the public SQL lexer.
(define supported-dialects
  '(generic sqlite postgres mysql))

;; common-keywords : (listof string?)
;;   Shared SQL keywords highlighted across dialects.
(define common-keywords
  '("ADD" "ALL" "ALTER" "AND" "ANY" "AS" "ASC" "BEGIN" "BETWEEN" "BY"
    "CASE" "CHECK" "COLUMN" "COMMIT" "CONSTRAINT" "CREATE" "CROSS"
    "CURRENT" "DATABASE" "DEFAULT" "DELETE" "DESC" "DISTINCT" "DO" "DROP"
    "ELSE" "END" "EXCEPT" "EXISTS" "FALSE" "FETCH" "FOR" "FOREIGN" "FROM"
    "FULL" "GRANT" "GROUP" "HAVING" "IN" "INDEX" "INNER" "INSERT"
    "INTERSECT" "INTO" "IS" "JOIN" "KEY" "LEFT" "LIKE" "LIMIT" "NOT"
    "NULL" "OF" "OFFSET" "ON" "OR" "ORDER" "OUTER" "PRIMARY"
    "PROCEDURE" "REFERENCES" "RETURNING" "RIGHT" "ROLLBACK" "ROW"
    "ROWS" "SAVEPOINT" "SELECT" "SET" "TABLE" "THEN" "TO" "TRUE"
    "UNION" "UNIQUE" "UPDATE" "USING" "VALUES" "VIEW" "WHEN" "WHERE"
    "WITH"))

;; sqlite-keywords : (listof string?)
;;   SQLite-specific keyword additions.
(define sqlite-keywords
  '("ABORT" "ANALYZE" "ATTACH" "AUTOINCREMENT" "CONFLICT" "DETACH"
    "EXCLUSIVE" "FAIL" "GLOB" "IGNORE" "IMMEDIATE" "INDEXED" "INITIALLY"
    "INSTEAD" "MATCH" "PLAN" "PRAGMA" "QUERY" "RAISE" "REGEXP" "REINDEX"
    "RENAME" "REPLACE" "RESTRICT" "TEMP" "TEMPORARY" "VACUUM" "WITHOUT"))

;; postgres-keywords : (listof string?)
;;   PostgreSQL-specific keyword additions.
(define postgres-keywords
  '("ANALYSE" "ANALYZE" "ARRAY" "COPY" "ILIKE" "LANGUAGE" "LATERAL"
    "NOTNULL" "OVER" "OWNER" "PLACING" "SERIAL" "SIMILAR" "TYPE"
    "UNLOGGED" "VARIADIC"))

;; mysql-keywords : (listof string?)
;;   MySQL-specific keyword additions.
(define mysql-keywords
  '("AUTO_INCREMENT" "DELIMITER" "DUAL" "ENGINE" "IF" "LOCK" "REPLACE"
    "SHOW" "STRAIGHT_JOIN" "UNLOCK" "XOR"))

;; dialect-keywords : symbol? -> (listof string?)
;;   Select the extra keyword set for one SQL dialect.
(define (dialect-keywords dialect)
  (case dialect
    [(sqlite)   sqlite-keywords]
    [(postgres) postgres-keywords]
    [(mysql)    mysql-keywords]
    [else       '()]))

;; -----------------------------------------------------------------------------
;; Port helpers

;; write-one! : input-port? output-port? -> void?
;;   Consume one character and append it to the output accumulator.
(define (write-one! in out)
  (write-char (read-char in) out))

;; peek-next : input-port? [exact-nonnegative-integer?] -> (or/c char? eof-object?)
;;   Peek ahead in the input stream.
(define (peek-next in [skip 0])
  (peek-char in skip))

;; peek-string : input-port? exact-nonnegative-integer? -> string?
;;   Peek ahead at the next length characters from the input stream.
(define (peek-string in len)
  (define chars
    (for/list ([i (in-range len)])
      (peek-next in i)))
  (cond
    [(for/or ([ch (in-list chars)])
       (eof-object? ch))
     ""]
    [else
     (list->string chars)]))

;; current-stream-position : input-port? -> position?
;;   Read the current parser-tools-compatible source position from a port.
(define (current-stream-position in)
  (let-values ([(line col offset) (port-next-location in)])
    (define safe-line
      (cond
        [(exact-positive-integer? line)   line]
        [else                             1]))
    (define safe-col
      (cond
        [(exact-nonnegative-integer? col) col]
        [else                             0]))
    (define safe-offset
      (cond
        [(exact-positive-integer? offset) offset]
        [else                             1]))
    (make-stream-position safe-offset safe-line safe-col)))

;; read-into-string : input-port? exact-nonnegative-integer? -> string?
;;   Consume exactly len characters and return them as one string.
(define (read-into-string in len)
  (define out
    (open-output-string))
  (for ([i (in-range len)])
    (write-one! in out))
  (get-output-string out))

;; take-while! : input-port? output-port? (char? -> boolean?) -> void?
;;   Consume consecutive characters while predicate holds.
(define (take-while! in out predicate)
  (let loop ()
    (define ch
      (peek-next in))
    (when (and (char? ch)
               (predicate ch))
      (write-one! in out)
      (loop))))

;; emit-token : position? string? (listof symbol?) -> sql-derived-token?
;;   Construct one derived token from its start position, text, and tags.
(define (emit-token start-pos text tags)
  (define end-pos
    (make-stream-position (+ (position-offset start-pos) (string-length text))
                          (position-line start-pos)
                          (+ (position-col start-pos) (string-length text))))
  (define kind
    (cond
      [(member 'comment tags)         'comment]
      [(member 'whitespace tags)      'whitespace]
      [(member 'keyword tags)         'keyword]
      [(member 'literal tags)         'literal]
      [(member 'operator tags)        'operator]
      [(member 'delimiter tags)       'delimiter]
      [(member 'malformed-token tags) 'malformed]
      [else                           'identifier]))
  (sql-derived-token kind
                     text
                     start-pos
                     end-pos
                     (remove-duplicates tags)))

;; emit-token/current-end : position? string? position? (listof symbol?) -> sql-derived-token?
;;   Construct one derived token when the exact end position is already known.
(define (emit-token/current-end start-pos text end-pos tags)
  (define kind
    (cond
      [(member 'comment tags)         'comment]
      [(member 'whitespace tags)      'whitespace]
      [(member 'keyword tags)         'keyword]
      [(member 'literal tags)         'literal]
      [(member 'operator tags)        'operator]
      [(member 'delimiter tags)       'delimiter]
      [(member 'malformed-token tags) 'malformed]
      [else                           'identifier]))
  (sql-derived-token kind
                     text
                     start-pos
                     end-pos
                     (remove-duplicates tags)))

;; -----------------------------------------------------------------------------
;; Character classes

;; whitespace-char? : char? -> boolean?
;;   Recognize SQL whitespace.
(define (whitespace-char? ch)
  (char-whitespace? ch))

;; identifier-start-char? : char? -> boolean?
;;   Recognize an unquoted SQL identifier start.
(define (identifier-start-char? ch)
  (or (char-alphabetic? ch)
      (char=? ch #\_)))

;; identifier-char? : char? -> boolean?
;;   Recognize an unquoted SQL identifier continuation.
(define (identifier-char? ch)
  (or (identifier-start-char? ch)
      (char-numeric? ch)
      (char=? ch #\$)))

;; dollar-tag-char? : char? -> boolean?
;;   Recognize a PostgreSQL dollar-quote tag character.
(define (dollar-tag-char? ch)
  (or (identifier-char? ch)
      (char=? ch #\space)))

;; operator-char? : char? -> boolean?
;;   Recognize SQL operator characters.
(define (operator-char? ch)
  (member ch
          '(#\+ #\- #\* #\/ #\% #\= #\< #\> #\! #\~ #\^ #\| #\& #\? #\: #\@ #\#)))

;; delimiter-char? : char? -> boolean?
;;   Recognize SQL delimiter characters.
(define (delimiter-char? ch)
  (member ch
          '(#\( #\) #\[ #\] #\, #\; #\.)))

;; number-start-char? : char? (or/c char? eof-object?) -> boolean?
;;   Determine whether the next token begins with a number literal.
(define (number-start-char? ch next-ch)
  (or (char-numeric? ch)
      (and (char=? ch #\.)
           (char? next-ch)
           (char-numeric? next-ch))))

;; keyword-text? : symbol? string? -> boolean?
;;   Determine whether text is a keyword in the selected dialect.
(define (keyword-text? dialect text)
  (define up
    (string-upcase text))
  (or (member up common-keywords)
      (member up (dialect-keywords dialect))))

;; prefix-string-literal? : symbol? string? -> boolean?
;;   Determine whether prefix immediately followed by ' starts one literal.
(define (prefix-string-literal? dialect prefix)
  (define up
    (string-upcase prefix))
  (or (member up '("N" "E" "B" "X"))
      (string=? up "U&")
      (and (eq? dialect 'sqlite)
           (member up '("X")))
      (and (eq? dialect 'mysql)
           (or (regexp-match? #px"^_[A-Z0-9]+$" up)
               (string=? up "N")))))

;; maybe-extend-prefix-with-ampersand! : input-port? output-port? -> void?
;;   Extend a just-read identifier prefix with & for forms such as U&'...'.
(define (maybe-extend-prefix-with-ampersand! in out)
  (define prefix
    (get-output-string out))
  (when (and (string-ci=? prefix "U")
             (char? (peek-next in))
             (char=? (peek-next in) #\&)
             (char? (peek-next in 1))
             (char=? (peek-next in 1) #\'))
    (write-one! in out)))

;; dialect-allows-hash-comment? : symbol? -> boolean?
;;   Determine whether # comments should be recognized.
(define (dialect-allows-hash-comment? dialect)
  (eq? dialect 'mysql))

;; dialect-allows-backtick-identifier? : symbol? -> boolean?
;;   Determine whether backtick-quoted identifiers should be recognized.
(define (dialect-allows-backtick-identifier? dialect)
  (member dialect '(sqlite mysql)))

;; dialect-allows-bracket-identifier? : symbol? -> boolean?
;;   Determine whether [name] quoted identifiers should be recognized.
(define (dialect-allows-bracket-identifier? dialect)
  (eq? dialect 'sqlite))

;; dialect-allows-dollar-string? : symbol? -> boolean?
;;   Determine whether PostgreSQL dollar-quoted strings should be recognized.
(define (dialect-allows-dollar-string? dialect)
  (eq? dialect 'postgres))

;; dialect-allows-nested-block-comment? : symbol? -> boolean?
;;   Determine whether nested block comments should be recognized.
(define (dialect-allows-nested-block-comment? dialect)
  (eq? dialect 'postgres))

;; -----------------------------------------------------------------------------
;; Readers

;; read-whitespace-token : input-port? position? -> sql-derived-token?
;;   Read one contiguous SQL whitespace token.
(define (read-whitespace-token in start-pos)
  (define out
    (open-output-string))
  (take-while! in out whitespace-char?)
  (emit-token/current-end start-pos
                          (get-output-string out)
                          (current-stream-position in)
                          '(whitespace sql-whitespace)))

;; read-line-comment-token : input-port? position? exact-nonnegative-integer? symbol? -> sql-derived-token?
;;   Read one SQL line comment after consuming the opener.
(define (read-line-comment-token in start-pos opener-len tag)
  (define out
    (open-output-string))
  (display (read-into-string in opener-len) out)
  (let loop ()
    (define ch
      (peek-next in))
    (when (and (char? ch)
               (not (char=? ch #\newline))
               (not (char=? ch #\return)))
      (write-one! in out)
      (loop)))
  (emit-token/current-end start-pos
                          (get-output-string out)
                          (current-stream-position in)
                          (list 'comment 'sql-comment tag)))

;; read-block-comment-token : input-port? position? symbol? -> sql-derived-token?
;;   Read one SQL block comment, optionally allowing nesting.
(define (read-block-comment-token in start-pos dialect)
  (define nested?
    (dialect-allows-nested-block-comment? dialect))
  (define out
    (open-output-string))
  (display (read-into-string in 2) out)
  (let loop ([depth 1])
    (define here
      (peek-string in 2))
    (cond
      [(string=? here "")
       (emit-token/current-end start-pos
                               (get-output-string out)
                               (current-stream-position in)
                               '(comment sql-comment sql-block-comment malformed-token))]
      [(and nested?
            (string=? here "/*"))
       (display (read-into-string in 2) out)
       (loop (add1 depth))]
      [(string=? here "*/")
       (display (read-into-string in 2) out)
       (define next-depth
         (sub1 depth))
       (cond
         [(zero? next-depth)
          (emit-token/current-end start-pos
                                  (get-output-string out)
                                  (current-stream-position in)
                                  '(comment sql-comment sql-block-comment))]
         [else
          (loop next-depth)])]
      [else
       (write-one! in out)
       (loop depth)])))

;; read-single-quoted-literal-token : input-port? position? string? (listof symbol?) -> sql-derived-token?
;;   Read one single-quoted SQL literal, including any already-consumed prefix.
(define (read-single-quoted-literal-token in start-pos prefix tags)
  (define out
    (open-output-string))
  (display prefix out)
  (when (and (positive? (string-length prefix))
             (not (char=? (peek-next in) #\')))
    (error 'read-single-quoted-literal-token
           "expected quote after prefix"))
  (when (char=? (peek-next in) #\')
    (write-one! in out))
  (let loop ()
    (define ch
      (peek-next in))
    (cond
      [(eof-object? ch)
       (emit-token/current-end start-pos
                               (get-output-string out)
                               (current-stream-position in)
                               (append tags '(malformed-token)))]
      [else
       (write-one! in out)
       (cond
         [(char=? ch #\')
          (define next-ch
            (peek-next in))
          (cond
            [(and (char? next-ch)
                  (char=? next-ch #\'))
             (write-one! in out)
             (loop)]
            [else
             (emit-token/current-end start-pos
                                     (get-output-string out)
                                     (current-stream-position in)
                                     tags)])]
         [else
          (loop)])])))

;; read-quoted-identifier-token : input-port? position? char? char? (listof symbol?) -> sql-derived-token?
;;   Read one quoted identifier with an optional doubled closer escape.
(define (read-quoted-identifier-token in start-pos opener closer tags)
  (define out
    (open-output-string))
  (unless (char=? (peek-next in) opener)
    (error 'read-quoted-identifier-token
           "expected opener"))
  (write-one! in out)
  (let loop ()
    (define ch
      (peek-next in))
    (cond
      [(eof-object? ch)
       (emit-token/current-end start-pos
                               (get-output-string out)
                               (current-stream-position in)
                               (append tags '(malformed-token)))]
      [else
       (write-one! in out)
       (cond
         [(char=? ch closer)
          (define next-ch
            (peek-next in))
          (cond
            [(and (char? next-ch)
                  (char=? next-ch closer)
                  (not (char=? opener #\[)))
             (write-one! in out)
             (loop)]
            [else
             (emit-token/current-end start-pos
                                     (get-output-string out)
                                     (current-stream-position in)
                                     tags)])]
         [else
          (loop)])])))

;; dollar-quote-opener : input-port? -> (or/c string? #f)
;;   Detect a PostgreSQL dollar-quote opener at the current position.
(define (dollar-quote-opener in)
  (define first
    (peek-next in))
  (cond
    [(or (eof-object? first)
         (not (char=? first #\$)))
     #f]
    [else
     (let loop ([i 1])
       (define ch
         (peek-next in i))
       (cond
         [(eof-object? ch) #f]
         [(char=? ch #\$)
          (peek-string in (add1 i))]
         [(or (identifier-char? ch)
              (char=? ch #\space))
          (loop (add1 i))]
         [else
          #f]))]))

;; read-dollar-quoted-token : input-port? position? string? -> sql-derived-token?
;;   Read one PostgreSQL dollar-quoted string literal.
(define (read-dollar-quoted-token in start-pos opener)
  (define out
    (open-output-string))
  (display (read-into-string in (string-length opener)) out)
  (let loop ()
    (define here
      (peek-string in (string-length opener)))
    (cond
      [(string=? here "")
       (emit-token/current-end start-pos
                               (get-output-string out)
                               (current-stream-position in)
                               '(literal sql-string-literal sql-dollar-string malformed-token))]
      [(string=? here opener)
       (display (read-into-string in (string-length opener)) out)
       (emit-token/current-end start-pos
                               (get-output-string out)
                               (current-stream-position in)
                               '(literal sql-string-literal sql-dollar-string))]
      [else
       (write-one! in out)
       (loop)])))

;; read-number-token : input-port? position? -> sql-derived-token?
;;   Read one practical SQL numeric literal.
(define (read-number-token in start-pos)
  (define out
    (open-output-string))
  (define first
    (peek-next in))
  (when (char=? first #\.)
    (write-one! in out))
  (take-while! in out char-numeric?)
  (when (and (char=? (peek-next in) #\.)
             (char? (peek-next in 1))
             (char-numeric? (peek-next in 1)))
    (write-one! in out)
    (take-while! in out char-numeric?))
  (cond
    [(and (char? (peek-next in))
          (member (char-upcase (peek-next in)) '(#\E)))
     (define exponent-out
       (open-output-string))
     (display (read-into-string in 1) exponent-out)
     (when (and (char? (peek-next in))
                (member (peek-next in) '(#\+ #\-)))
       (write-one! in exponent-out))
     (cond
       [(and (char? (peek-next in))
             (char-numeric? (peek-next in)))
        (take-while! in exponent-out char-numeric?)
        (display (get-output-string exponent-out) out)
        (emit-token/current-end start-pos
                                (get-output-string out)
                                (current-stream-position in)
                                '(literal sql-numeric-literal))]
       [else
        (display (get-output-string exponent-out) out)
        (emit-token/current-end start-pos
                                (get-output-string out)
                                (current-stream-position in)
                                '(literal sql-numeric-literal malformed-token))])]
    [else
     (emit-token/current-end start-pos
                             (get-output-string out)
                             (current-stream-position in)
                             '(literal sql-numeric-literal))]))

;; read-parameter-token : input-port? position? -> sql-derived-token?
;;   Read one parameter or variable reference token.
(define (read-parameter-token in start-pos)
  (define out
    (open-output-string))
  (write-one! in out)
  (cond
    [(char=? (string-ref (get-output-string out) 0) #\?)
     (take-while! in out char-numeric?)]
    [else
     (take-while! in out
                  (lambda (ch)
                    (or (identifier-char? ch)
                        (char-numeric? ch)
                        (member ch '(#\. #\$ #\@)))) )])
  (emit-token/current-end start-pos
                          (get-output-string out)
                          (current-stream-position in)
                          '(identifier sql-parameter)))

;; read-identifier-or-prefixed-literal-token : input-port? position? symbol? -> sql-derived-token?
;;   Read one identifier-like token, folding prefix string literals when needed.
(define (read-identifier-or-prefixed-literal-token in start-pos dialect)
  (define prefix-out
    (open-output-string))
  (write-one! in prefix-out)
  (take-while! in prefix-out identifier-char?)
  (maybe-extend-prefix-with-ampersand! in prefix-out)
  (define prefix
    (get-output-string prefix-out))
  (cond
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\')
          (prefix-string-literal? dialect prefix))
     (read-single-quoted-literal-token in
                                       start-pos
                                       prefix
                                       '(literal sql-string-literal))]
    [else
     (define tags
       (cond
         [(keyword-text? dialect prefix)
          '(keyword sql-keyword)]
         [else
          '(identifier sql-identifier)]))
     (emit-token/current-end start-pos
                             prefix
                             (current-stream-position in)
                             tags)]))

;; read-operator-token : input-port? position? -> sql-derived-token?
;;   Read one maximal SQL operator token.
(define (read-operator-token in start-pos)
  (define out
    (open-output-string))
  (take-while! in out operator-char?)
  (emit-token/current-end start-pos
                          (get-output-string out)
                          (current-stream-position in)
                          '(operator sql-operator)))

;; read-delimiter-token : input-port? position? -> sql-derived-token?
;;   Read one SQL delimiter token.
(define (read-delimiter-token in start-pos)
  (emit-token/current-end start-pos
                          (read-into-string in 1)
                          (current-stream-position in)
                          '(delimiter sql-delimiter)))

;; read-unknown-token : input-port? position? -> sql-derived-token?
;;   Read one recoverable malformed token.
(define (read-unknown-token in start-pos)
  (emit-token/current-end start-pos
                          (read-into-string in 1)
                          (current-stream-position in)
                          '(sql-unknown malformed-token)))

;; read-next-token : input-port? symbol? -> (or/c sql-derived-token? 'eof)
;;   Read the next derived SQL token from the stream.
(define (read-next-token in dialect)
  (port-count-lines! in)
  (define start-pos
    (current-stream-position in))
  (define ch
    (peek-next in))
  (cond
    [(eof-object? ch)
     'eof]
    [(whitespace-char? ch)
     (read-whitespace-token in start-pos)]
    [(and (char=? ch #\-)
          (string=? (peek-string in 2) "--"))
     (read-line-comment-token in start-pos 2 'sql-line-comment)]
    [(and (dialect-allows-hash-comment? dialect)
          (char=? ch #\#))
     (read-line-comment-token in start-pos 1 'sql-line-comment)]
    [(and (char=? ch #\/)
          (string=? (peek-string in 2) "/*"))
     (read-block-comment-token in start-pos dialect)]
    [(char=? ch #\')
     (read-single-quoted-literal-token in
                                       start-pos
                                       ""
                                       '(literal sql-string-literal))]
    [(and (dialect-allows-dollar-string? dialect)
          (dollar-quote-opener in))
     (read-dollar-quoted-token in
                               start-pos
                               (dollar-quote-opener in))]
    [(char=? ch #\")
     (read-quoted-identifier-token in
                                   start-pos
                                   #\"
                                   #\"
                                   '(identifier sql-identifier sql-quoted-identifier))]
    [(and (dialect-allows-backtick-identifier? dialect)
          (char=? ch #\`))
     (read-quoted-identifier-token in
                                   start-pos
                                   #\`
                                   #\`
                                   '(identifier sql-identifier sql-quoted-identifier))]
    [(and (dialect-allows-bracket-identifier? dialect)
          (char=? ch #\[))
     (read-quoted-identifier-token in
                                   start-pos
                                   #\[
                                   #\]
                                   '(identifier sql-identifier sql-quoted-identifier))]
    [(and (member ch '(#\? #\: #\@))
          #t)
     (read-parameter-token in start-pos)]
    [(and (char=? ch #\$)
          (char? (peek-next in 1))
          (char-numeric? (peek-next in 1)))
     (read-parameter-token in start-pos)]
    [(number-start-char? ch (peek-next in 1))
     (read-number-token in start-pos)]
    [(identifier-start-char? ch)
     (read-identifier-or-prefixed-literal-token in start-pos dialect)]
    [(operator-char? ch)
     (read-operator-token in start-pos)]
    [(delimiter-char? ch)
     (read-delimiter-token in start-pos)]
    [else
     (read-unknown-token in start-pos)]))

;; make-sql-derived-reader : [symbol?] -> (input-port? -> (or/c sql-derived-token? 'eof))
;;   Construct a stateful SQL derived-token reader.
(define (make-sql-derived-reader [dialect 'generic])
  (unless (member dialect supported-dialects)
    (raise-arguments-error 'make-sql-derived-reader
                           "unsupported SQL dialect"
                           "dialect" dialect))
  (lambda (in)
    (read-next-token in dialect)))
