#lang racket/base

;;;
;;; Swift Derived Tokens
;;;
;;
;; Stateful Swift tokenization and reusable Swift-specific classifications.

;; swift-derived-token?         : any/c -> boolean?
;;   Recognize a derived Swift token.
;; swift-derived-token-text     : swift-derived-token? -> string?
;;   Extract the source text for one derived token.
;; swift-derived-token-start    : swift-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; swift-derived-token-end      : swift-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; swift-derived-token-tags     : swift-derived-token? -> (listof symbol?)
;;   Extract reusable Swift classification tags.
;; swift-derived-token-has-tag? : swift-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-swift-derived-reader    : -> (input-port? -> (or/c swift-derived-token? 'eof))
;;   Construct a stateful Swift derived-token reader.

(provide swift-derived-token?
         swift-derived-token-text
         swift-derived-token-start
         swift-derived-token-end
         swift-derived-token-tags
         swift-derived-token-has-tag?
         make-swift-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         racket/string
         "parser-tools-compat.rkt")

;; A Swift token plus reusable tags.
(struct swift-derived-token (kind text start end tags) #:transparent)

;; swift-derived-token-has-tag? : swift-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (swift-derived-token-has-tag? token tag)
  (member tag (swift-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Swift classification tables

;; Reserved Swift keywords.
(define swift-keywords
  (list->set
   '("associatedtype" "actor" "any" "as" "async" "await"
     "break" "case" "catch" "class" "continue" "convenience"
     "default" "defer" "deinit" "didSet" "do" "dynamic"
     "else" "enum" "extension"
     "fallthrough" "false" "fileprivate" "final" "for" "func"
     "get" "guard"
     "if" "import" "in" "indirect" "infix" "init" "inout" "internal" "is"
     "lazy" "let"
     "macro" "mutating"
     "nil" "nonisolated"
     "open" "operator" "optional" "override"
     "package" "postfix" "precedencegroup" "prefix" "private" "protocol" "public"
     "repeat" "required" "rethrows" "return"
     "self" "Self" "set" "some" "static" "struct" "subscript" "super" "switch"
     "throw" "throws" "true" "try" "Type"
     "var"
     "weak" "where" "while" "willSet")))

;; Single-character delimiters in Swift source.
(define swift-delimiters
  (list->set
   '("(" ")" "[" "]" "{" "}" "," ";" ":" ".")))

;; Operator characters recognized by the first Swift lexer slice.
(define swift-operator-chars
  (list->set
   '(#\/ #\= #\- #\+ #\! #\* #\% #\< #\> #\& #\| #\^ #\~ #\?)))

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

;; read-while! : input-port? output-port? (char? -> boolean?) -> void?
;;   Consume characters while pred? holds.
(define (read-while! in out pred?)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(and (char? next) (pred? next))
       (write-one! in out)
       (loop)]
      [else
       (void)])))

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

;; make-token-from-text : position? position? string? (listof symbol?) -> swift-derived-token?
;;   Construct one derived token from explicit positions, text, and tags.
(define (make-token-from-text start-pos end-pos text tags)
  (define kind
    (cond
      [(member 'comment tags)         'comment]
      [(member 'whitespace tags)      'whitespace]
      [(member 'malformed-token tags) 'malformed]
      [(member 'keyword tags)         'keyword]
      [(member 'literal tags)         'literal]
      [(member 'operator tags)        'operator]
      [(member 'delimiter tags)       'delimiter]
      [else                           'identifier]))
  (swift-derived-token kind
                       text
                       start-pos
                       end-pos
                       (remove-duplicates tags)))

;; -----------------------------------------------------------------------------
;; Character classes

;; newline-start? : (or/c char? eof-object?) -> boolean?
;;   Determine whether a character begins a physical newline.
(define (newline-start? ch)
  (and (char? ch)
       (or (char=? ch #\newline)
           (char=? ch #\return))))

;; swift-inline-whitespace? : char? -> boolean?
;;   Recognize Swift whitespace other than newlines.
(define (swift-inline-whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\page)))

;; swift-ident-start? : char? -> boolean?
;;   Recognize an identifier-start character for the first Swift slice.
(define (swift-ident-start? ch)
  (or (char-alphabetic? ch)
      (char=? ch #\_)
      (char=? ch #\$)))

;; swift-ident-char? : char? -> boolean?
;;   Recognize an identifier continuation character.
(define (swift-ident-char? ch)
  (or (swift-ident-start? ch)
      (char-numeric? ch)))

;; operator-char? : char? -> boolean?
;;   Recognize one operator character.
(define (operator-char? ch)
  (set-member? swift-operator-chars ch))

;; number-tail-char? : char? -> boolean?
;;   Recognize a broad tail character for one Swift number token candidate.
(define (number-tail-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (char=? ch #\_)
      (char=? ch #\.)
      (char=? ch #\+)
      (char=? ch #\-)))

;; -----------------------------------------------------------------------------
;; Small scanners

;; read-newline! : input-port? output-port? -> void?
;;   Consume one physical newline sequence, preserving CRLF.
(define (read-newline! in out)
  (define next
    (peek-next in))
  (cond
    [(char=? next #\return)
     (write-one! in out)
     (when (and (char? (peek-next in))
                (char=? (peek-next in) #\newline))
       (write-one! in out))]
    [else
     (write-one! in out)]))

;; read-line-comment! : input-port? output-port? -> void?
;;   Consume one Swift line comment without its terminating newline.
(define (read-line-comment! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       (void)]
      [(newline-start? next)
       (void)]
      [else
       (write-one! in out)
       (loop)])))

;; read-block-comment! : input-port? output-port? -> boolean?
;;   Consume one possibly nested Swift block comment and report whether it closed.
(define (read-block-comment! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ([depth 1])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(and (char=? next #\/)
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\*))
       (write-one! in out)
       (write-one! in out)
       (loop (add1 depth))]
      [(and (char=? next #\*)
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\/))
       (write-one! in out)
       (write-one! in out)
       (cond
         [(= depth 1)
          #t]
         [else
          (loop (sub1 depth))])]
      [else
       (write-one! in out)
       (loop depth)])))

;; read-name! : input-port? output-port? -> void?
;;   Consume one Swift identifier-like token.
(define (read-name! in out)
  (read-while! in out swift-ident-char?))

;; read-backticked-identifier! : input-port? output-port? -> boolean?
;;   Consume one backticked identifier and report whether it terminated.
(define (read-backticked-identifier! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [else
       (write-one! in out)
       (cond
         [(char=? next #\`)
          #t]
         [else
          (loop)])])))

;; read-pound-directive! : input-port? output-port? -> void?
;;   Consume one #directive-style token.
(define (read-pound-directive! in out)
  (write-one! in out)
  (read-while! in out swift-ident-char?))

;; read-attribute! : input-port? output-port? -> void?
;;   Consume one @attribute-style token.
(define (read-attribute! in out)
  (write-one! in out)
  (read-while! in out swift-ident-char?))

;; read-number! : input-port? output-port? -> void?
;;   Consume one Swift number token candidate.
(define (read-number! in out)
  (read-while! in out number-tail-char?))

;; read-operator! : input-port? output-port? -> void?
;;   Consume one operator token candidate.
(define (read-operator! in out)
  (read-while! in out operator-char?))

;; swift-raw-string-opener : input-port? -> (or/c (cons/c exact-positive-integer? boolean?) #f)
;;   Determine whether the current position starts one raw Swift string.
(define (swift-raw-string-opener in)
  (let loop ([hashes 0])
    (define next
      (peek-next in hashes))
    (cond
      [(and (char? next)
            (char=? next #\#))
       (loop (add1 hashes))]
      [(zero? hashes)
       #f]
      [(and (char? next)
            (char=? next #\")
            (char? (peek-next in (add1 hashes)))
            (char? (peek-next in (+ hashes 2)))
            (char=? (peek-next in (add1 hashes)) #\")
            (char=? (peek-next in (+ hashes 2)) #\"))
       (cons hashes #t)]
      [(and (char? next)
            (char=? next #\"))
       (cons hashes #f)]
      [else
       #f])))

;; read-swift-raw-string! : input-port? output-port? exact-positive-integer? boolean? -> boolean?
;;   Consume one raw Swift string literal and report whether it terminated.
(define (read-swift-raw-string! in out hash-count triple?)
  (for ([i (in-range hash-count)])
    (write-one! in out))
  (for ([i (in-range (if triple? 3 1))])
    (write-one! in out))
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [triple?
       (cond
         [(and (char=? next #\")
               (char? (peek-next in 1))
               (char? (peek-next in 2))
               (char=? (peek-next in 1) #\")
               (char=? (peek-next in 2) #\")
               (for/and ([i (in-range hash-count)])
                 (and (char? (peek-next in (+ 3 i)))
                      (char=? (peek-next in (+ 3 i)) #\#))))
          (for ([i (in-range (+ 3 hash-count))])
            (write-one! in out))
          #t]
         [else
          (write-one! in out)
          (loop)])]
      [else
       (cond
         [(newline-start? next)
          #f]
         [(and (char=? next #\")
               (for/and ([i (in-range hash-count)])
                 (and (char? (peek-next in (add1 i)))
                      (char=? (peek-next in (add1 i)) #\#))))
          (write-one! in out)
          (for ([i (in-range hash-count)])
            (write-one! in out))
          #t]
         [else
          (write-one! in out)
          (loop)])])))

;; read-swift-string! : input-port? output-port? -> boolean?
;;   Consume one Swift string literal and report whether it terminated.
(define (read-swift-string! in out)
  (define triple?
    (and (char=? (peek-next in) #\")
         (char? (peek-next in 1))
         (char? (peek-next in 2))
         (char=? (peek-next in 1) #\")
         (char=? (peek-next in 2) #\")))
  (cond
    [triple?
     (for ([i (in-range 3)])
       (write-one! in out))
     (let loop ()
       (define next
         (peek-next in))
       (cond
         [(eof-object? next)
          #f]
         [(and (char=? next #\")
               (char? (peek-next in 1))
               (char? (peek-next in 2))
               (char=? (peek-next in 1) #\")
               (char=? (peek-next in 2) #\"))
          (for ([i (in-range 3)])
            (write-one! in out))
          #t]
         [(char=? next #\\)
          (write-one! in out)
          (when (char? (peek-next in))
            (write-one! in out))
          (loop)]
         [else
          (write-one! in out)
          (loop)]))]
    [else
     (write-one! in out)
     (let loop ()
       (define next
         (peek-next in))
       (cond
         [(eof-object? next)
          #f]
         [(newline-start? next)
          #f]
         [(char=? next #\\)
          (write-one! in out)
          (cond
            [(char? (peek-next in))
             (write-one! in out)
             (loop)]
            [else
             #f])]
         [(char=? next #\")
          (write-one! in out)
          #t]
         [else
          (write-one! in out)
          (loop)]))]))

;; -----------------------------------------------------------------------------
;; Main reader

;; make-swift-derived-reader : -> (input-port? -> (or/c swift-derived-token? 'eof))
;;   Construct a stateful Swift derived-token reader.
(define (make-swift-derived-reader)
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-swift-derived-reader "input-port?" in))
    (port-count-lines! in)
    (define start-pos
      (current-stream-position in))
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       'eof]
      [(newline-start? next)
       (define out
         (open-output-string))
       (read-newline! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(whitespace swift-whitespace))]
      [(swift-inline-whitespace? next)
       (define out
         (open-output-string))
       (read-while! in out swift-inline-whitespace?)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(whitespace swift-whitespace))]
      [(and (char=? next #\/)
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\/))
       (define out
         (open-output-string))
       (read-line-comment! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(comment swift-comment))]
      [(and (char=? next #\/)
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\*))
       (define out
         (open-output-string))
       (define terminated?
         (read-block-comment! in out))
       (define text
         (get-output-string out))
       (cond
         [terminated?
          (make-token-from-text start-pos
                                (current-stream-position in)
                                text
                                '(comment swift-comment))]
         [else
          (make-token-from-text start-pos
                                (current-stream-position in)
                                text
                                '(malformed-token swift-error))])]
      [(char=? next #\`)
       (define out
         (open-output-string))
       (define terminated?
         (read-backticked-identifier! in out))
       (define text
         (get-output-string out))
       (cond
         [terminated?
          (make-token-from-text start-pos
                                (current-stream-position in)
                                text
                                '(identifier swift-identifier))]
         [else
          (make-token-from-text start-pos
                                (current-stream-position in)
                                text
                                '(malformed-token swift-error))])]
      [(char=? next #\#)
       (define raw-opener
         (swift-raw-string-opener in))
       (cond
         [raw-opener
          (define out
            (open-output-string))
          (define terminated?
            (read-swift-raw-string! in out (car raw-opener) (cdr raw-opener)))
          (define text
            (get-output-string out))
          (cond
            [terminated?
             (make-token-from-text start-pos
                                   (current-stream-position in)
                                   text
                                   '(literal swift-string-literal swift-raw-string-literal))]
            [else
             (make-token-from-text start-pos
                                   (current-stream-position in)
                                   text
                                   '(malformed-token swift-error))])]
         [else
          (define out
            (open-output-string))
          (read-pound-directive! in out)
          (make-token-from-text start-pos
                                (current-stream-position in)
                                (get-output-string out)
                                '(keyword swift-keyword swift-pound-directive))])]
      [(char=? next #\@)
       (define out
         (open-output-string))
       (read-attribute! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(keyword swift-keyword swift-attribute))]
      [(char=? next #\")
       (define out
         (open-output-string))
       (define terminated?
         (read-swift-string! in out))
       (define text
         (get-output-string out))
       (cond
         [terminated?
          (make-token-from-text start-pos
                                (current-stream-position in)
                                text
                                '(literal swift-string-literal))]
         [else
          (make-token-from-text start-pos
                                (current-stream-position in)
                                text
                                '(malformed-token swift-error))])]
      [(char-numeric? next)
       (define out
         (open-output-string))
       (read-number! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(literal swift-numeric-literal))]
      [(swift-ident-start? next)
       (define out
         (open-output-string))
       (read-name! in out)
       (define text
         (get-output-string out))
       (define tags
         (cond
           [(set-member? swift-keywords text)
            '(keyword swift-keyword)]
           [else
            '(identifier swift-identifier)]))
       (make-token-from-text start-pos
                             (current-stream-position in)
                             text
                             tags)]
      [(and (char=? next #\.)
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\.))
       (define out
         (open-output-string))
       (write-one! in out)
       (write-one! in out)
       (when (and (char? (peek-next in))
                  (or (char=? (peek-next in) #\<)
                      (char=? (peek-next in) #\.)))
         (write-one! in out))
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(operator swift-operator))]
      [(set-member? swift-delimiters (string next))
       (define out
         (open-output-string))
       (write-one! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(delimiter swift-delimiter))]
      [(operator-char? next)
       (define out
         (open-output-string))
       (read-operator! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(operator swift-operator))]
      [else
       (define out
         (open-output-string))
       (write-one! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(malformed-token swift-error))])))
