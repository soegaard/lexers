#lang racket/base

;;;
;;; Rust Derived Tokens
;;;
;;
;; Stateful Rust tokenization and reusable Rust-specific classifications.

;; rust-derived-token?         : any/c -> boolean?
;;   Recognize a derived Rust token.
;; rust-derived-token-text     : rust-derived-token? -> string?
;;   Extract the source text for one derived token.
;; rust-derived-token-start    : rust-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; rust-derived-token-end      : rust-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; rust-derived-token-tags     : rust-derived-token? -> (listof symbol?)
;;   Extract reusable Rust classification tags.
;; rust-derived-token-has-tag? : rust-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-rust-derived-reader    : -> (input-port? -> (or/c rust-derived-token? 'eof))
;;   Construct a stateful Rust-derived-token reader.

(provide rust-derived-token?
         rust-derived-token-text
         rust-derived-token-start
         rust-derived-token-end
         rust-derived-token-tags
         rust-derived-token-has-tag?
         make-rust-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         racket/string
         "parser-tools-compat.rkt")

;; A Rust token plus reusable tags.
(struct rust-derived-token (kind text start end tags) #:transparent)

;; rust-derived-token-has-tag? : rust-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (rust-derived-token-has-tag? token tag)
  (member tag (rust-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Classification tables

;; Rust keywords in the first lexer slice.
(define rust-keywords
  (list->set
   '("as" "async" "await" "break" "const" "continue" "crate" "dyn" "else"
     "enum" "extern" "false" "fn" "for" "if" "impl" "in" "let" "loop"
     "match" "mod" "move" "mut" "pub" "ref" "return" "self" "Self"
     "static" "struct" "super" "trait" "true" "type" "unsafe" "use"
     "where" "while" "abstract" "become" "box" "do" "final" "macro"
     "override" "priv" "typeof" "unsized" "virtual" "yield" "try")))

;; Rust delimiters.
(define rust-delimiters
  (list->set
   '("(" ")" "[" "]" "{" "}")))

;; Rust punctuation/operators in longest-match order.
(define rust-punctuators
  '(">>=" "<<=" "..." "..=" "::" "->" "=>" "==" "!=" "<=" ">=" "&&" "||"
    "+=" "-=" "*=" "/=" "%=" "^=" "&=" "|=" "<<" ">>" ".."
    "@" "#" "$" "?" "~" ":" ";" "," "." "=" "!" "<" ">" "-" "&" "|" "+"
    "*" "/" "%" "^" "(" ")" "[" "]" "{" "}"))

;; -----------------------------------------------------------------------------
;; Port helpers

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

;; peek-next : input-port? [exact-nonnegative-integer?] -> (or/c char? eof-object?)
;;   Peek ahead in the input stream.
(define (peek-next in [skip 0])
  (peek-char in skip))

;; write-one! : input-port? output-port? -> void?
;;   Consume one character and append it to the output accumulator.
(define (write-one! in out)
  (write-char (read-char in) out))

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

;; make-token-from-text : position? position? string? (listof symbol?) -> rust-derived-token?
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
  (rust-derived-token kind
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

;; rust-inline-whitespace? : char? -> boolean?
;;   Recognize Rust whitespace other than newlines.
(define (rust-inline-whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\page)))

;; rust-whitespace? : (or/c char? eof-object?) -> boolean?
;;   Recognize Rust whitespace.
(define (rust-whitespace? ch)
  (and (char? ch)
       (or (rust-inline-whitespace? ch)
           (newline-start? ch))))

;; ident-start? : char? -> boolean?
;;   Recognize an ASCII identifier-start character for the first Rust slice.
(define (ident-start? ch)
  (or (char-alphabetic? ch)
      (char=? ch #\_)))

;; ident-char? : char? -> boolean?
;;   Recognize an ASCII identifier continuation character.
(define (ident-char? ch)
  (or (ident-start? ch)
      (char-numeric? ch)))

;; numeric-tail-char? : char? -> boolean?
;;   Recognize a broad tail character for a Rust numeric literal.
(define (numeric-tail-char? ch)
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

;; read-whitespace! : input-port? output-port? -> void?
;;   Consume one run of Rust whitespace.
(define (read-whitespace! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       (void)]
      [(newline-start? next)
       (read-newline! in out)
       (loop)]
      [(rust-inline-whitespace? next)
       (write-one! in out)
       (loop)]
      [else
       (void)])))

;; read-line-comment! : input-port? output-port? -> void?
;;   Consume one Rust line or doc comment without its terminating newline.
(define (read-line-comment! in out)
  (write-one! in out)
  (write-one! in out)
  (when (and (char? (peek-next in))
             (char=? (peek-next in) #\/))
    (write-one! in out))
  (when (and (char? (peek-next in))
             (char=? (peek-next in) #\!))
    (write-one! in out))
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(or (eof-object? next)
           (newline-start? next))
       (void)]
      [else
       (write-one! in out)
       (loop)])))

;; read-block-comment! : input-port? output-port? -> boolean?
;;   Consume one nested block or doc comment and report whether it terminated.
(define (read-block-comment! in out)
  (write-one! in out)
  (write-one! in out)
  (when (and (char? (peek-next in))
             (member (peek-next in) '(#\* #\!)))
    (write-one! in out))
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

;; read-identifier! : input-port? output-port? -> void?
;;   Consume one identifier-like token.
(define (read-identifier! in out)
  (read-while! in out ident-char?))

;; read-string-literal! : input-port? output-port? -> boolean?
;;   Consume one quoted string literal and report whether it terminated.
(define (read-string-literal! in out)
  (write-one! in out)
  (let loop ([escaped? #f])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [escaped?
       (write-one! in out)
       (loop #f)]
      [(char=? next #\\)
       (write-one! in out)
       (loop #t)]
      [(char=? next #\")
       (write-one! in out)
       #t]
      [(newline-start? next)
       #f]
      [else
       (write-one! in out)
       (loop #f)])))

;; read-char-literal! : input-port? output-port? -> boolean?
;;   Consume one quoted char literal and report whether it terminated.
(define (read-char-literal! in out)
  (write-one! in out)
  (let loop ([escaped? #f])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [escaped?
       (write-one! in out)
       (loop #f)]
      [(char=? next #\\)
       (write-one! in out)
       (loop #t)]
      [(char=? next #\')
       (write-one! in out)
       #t]
      [(newline-start? next)
       #f]
      [else
       (write-one! in out)
       (loop #f)])))

;; raw-string-prefix-length : input-port? string? -> (or/c exact-nonnegative-integer? #f)
;;   Determine whether prefix is followed by a valid raw-string opener.
(define (raw-string-prefix-length in prefix)
  (define prefix-len
    (string-length prefix))
  (let loop ([i prefix-len])
    (define next
      (peek-next in i))
    (cond
      [(and (char? next) (char=? next #\#))
       (loop (add1 i))]
      [(and (char? next) (char=? next #\"))
       (add1 i)]
      [else
       #f])))

;; read-raw-string-literal! : input-port? output-port? string? -> boolean?
;;   Consume one raw string-like literal with the given prefix and report success.
(define (read-raw-string-literal! in out prefix)
  (define opener-length
    (raw-string-prefix-length in prefix))
  (cond
    [(not opener-length)
     #f]
    [else
     (for ([i (in-range opener-length)])
       (write-one! in out))
     (define hash-count
       (- opener-length (string-length prefix) 1))
     (let loop ()
       (define next
         (peek-next in))
       (cond
         [(eof-object? next)
          #f]
         [(char=? next #\")
          (define matches?
            (for/and ([i (in-range hash-count)])
              (and (char? (peek-next in (add1 i)))
                   (char=? (peek-next in (add1 i)) #\#))))
          (cond
            [matches?
             (write-one! in out)
             (for ([i (in-range hash-count)])
               (write-one! in out))
             #t]
            [else
             (write-one! in out)
             (loop)])]
         [else
          (write-one! in out)
          (loop)]))]))

;; read-number! : input-port? output-port? -> void?
;;   Consume one broad Rust numeric literal candidate.
(define (read-number! in out)
  (read-while! in out numeric-tail-char?))

;; read-lifetime! : input-port? output-port? -> void?
;;   Consume one lifetime token.
(define (read-lifetime! in out)
  (write-one! in out)
  (cond
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\r)
          (char? (peek-next in 1))
          (char=? (peek-next in 1) #\#))
     (write-one! in out)
     (write-one! in out)
     (read-while! in out ident-char?)]
    [else
     (read-while! in out ident-char?)]))

;; lifetime-token? : input-port? -> boolean?
;;   Determine whether the next source form is a Rust lifetime token.
(define (lifetime-token? in)
  (and (char? (peek-next in 1))
       (or (ident-start? (peek-next in 1))
           (and (char=? (peek-next in 1) #\r)
                (char? (peek-next in 2))
                (char=? (peek-next in 2) #\#)
                (char? (peek-next in 3))
                (ident-start? (peek-next in 3))))
       (let loop ([i 1])
         (define next
           (peek-next in i))
         (cond
           [(eof-object? next)
            #t]
           [(char=? next #\')
            #f]
           [(ident-char? next)
            (loop (add1 i))]
           [(and (= i 1)
                 (char=? next #\r)
                 (char? (peek-next in (+ i 1)))
                 (char=? (peek-next in (+ i 1)) #\#))
            (loop (+ i 2))]
           [else
            #t]))))

;; consume-punctuator! : input-port? output-port? -> (values string? (listof symbol?))
;;   Consume one Rust punctuator using longest-match order.
(define (consume-punctuator! in out)
  (define matched
    (for/or ([punct (in-list rust-punctuators)])
      (define plen
        (string-length punct))
      (and (for/and ([i (in-range plen)])
             (and (char? (peek-next in i))
                  (char=? (peek-next in i) (string-ref punct i))))
           punct)))
  (cond
    [matched
     (for ([i (in-range (string-length matched))])
       (write-one! in out))
     (values matched
             (cond
               [(set-member? rust-delimiters matched)
                '(delimiter rust-delimiter)]
               [else
                '(operator rust-punctuation)]))]
    [else
     (write-one! in out)
     (define text
       (get-output-string out))
     (values text '(operator rust-punctuation))]))

;; -----------------------------------------------------------------------------
;; Reader

;; make-rust-derived-reader : -> (input-port? -> (or/c rust-derived-token? 'eof))
;;   Construct a stateful Rust-derived-token reader.
(define (make-rust-derived-reader)
  (lambda (in)
    (define next
      (peek-next in))
    (if (eof-object? next)
        'eof
        (let ([start (current-stream-position in)]
              [out   (open-output-string)])
          (cond
            [(rust-whitespace? next)
             (read-whitespace! in out)
             (make-token-from-text start
                                   (current-stream-position in)
                                   (get-output-string out)
                                   '(whitespace rust-whitespace))]
            [(and (char=? next #\/)
                  (char? (peek-next in 1))
                  (char=? (peek-next in 1) #\/))
             (read-line-comment! in out)
             (make-token-from-text start
                                   (current-stream-position in)
                                   (get-output-string out)
                                   (append '(comment rust-comment)
                                           (cond
                                             [(or (string-prefix? (get-output-string out) "///")
                                                  (string-prefix? (get-output-string out) "//!"))
                                              '(rust-doc-comment)]
                                             [else
                                              '()])))]
            [(and (char=? next #\/)
                  (char? (peek-next in 1))
                  (char=? (peek-next in 1) #\*))
             (define terminated?
               (read-block-comment! in out))
             (make-token-from-text start
                                   (current-stream-position in)
                                   (get-output-string out)
                                   (append
                                    (if terminated?
                                        '(comment rust-comment)
                                        '(malformed-token rust-comment))
                                    (cond
                                      [(or (string-prefix? (get-output-string out) "/**")
                                           (string-prefix? (get-output-string out) "/*!"))
                                       '(rust-doc-comment)]
                                      [else
                                       '()])))]
            [(and (char=? next #\b)
                  (char? (peek-next in 1))
                  (char=? (peek-next in 1) #\'))
             (write-one! in out)
             (define terminated?
               (read-char-literal! in out))
             (make-token-from-text start
                                   (current-stream-position in)
                                   (get-output-string out)
                                   (if terminated?
                                       '(literal rust-byte-literal)
                                       '(malformed-token rust-byte-literal)))]
            [(and (char=? next #\b)
                  (char? (peek-next in 1))
                  (char=? (peek-next in 1) #\"))
             (write-one! in out)
             (define terminated?
               (read-string-literal! in out))
             (make-token-from-text start
                                   (current-stream-position in)
                                   (get-output-string out)
                                   (if terminated?
                                       '(literal rust-byte-string-literal)
                                       '(malformed-token rust-byte-string-literal)))]
            [(and (char=? next #\b)
                  (char? (peek-next in 1))
                  (char=? (peek-next in 1) #\r))
             (define terminated?
               (read-raw-string-literal! in out "br"))
             (cond
               [terminated?
                (make-token-from-text start
                                      (current-stream-position in)
                                      (get-output-string out)
                                      '(literal rust-raw-string-literal
                                                rust-byte-string-literal))]
               [else
                (write-one! in out)
                (make-token-from-text start
                                      (current-stream-position in)
                                      (get-output-string out)
                                      '(identifier rust-identifier))])]
            [(char=? next #\")
             (define terminated?
               (read-string-literal! in out))
             (make-token-from-text start
                                   (current-stream-position in)
                                   (get-output-string out)
                                   (if terminated?
                                       '(literal rust-string-literal)
                                       '(malformed-token rust-string-literal)))]
            [(char=? next #\r)
             (cond
               [(read-raw-string-literal! in out "r")
                (make-token-from-text start
                                      (current-stream-position in)
                                      (get-output-string out)
                                      '(literal rust-raw-string-literal))]
               [(and (char? (peek-next in 1))
                     (char=? (peek-next in 1) #\#)
                     (char? (peek-next in 2))
                     (ident-start? (peek-next in 2)))
                (write-one! in out)
                (write-one! in out)
                (read-identifier! in out)
                (make-token-from-text start
                                      (current-stream-position in)
                                      (get-output-string out)
                                      '(identifier rust-identifier rust-raw-identifier))]
               [else
                (read-identifier! in out)
                (make-token-from-text start
                                      (current-stream-position in)
                                      (get-output-string out)
                                      (if (set-member? rust-keywords (get-output-string out))
                                          '(keyword rust-keyword)
                                          '(identifier rust-identifier)))])]
            [(and (char=? next #\c)
                  (char? (peek-next in 1))
                  (char=? (peek-next in 1) #\"))
             (write-one! in out)
             (define terminated?
               (read-string-literal! in out))
             (make-token-from-text start
                                   (current-stream-position in)
                                   (get-output-string out)
                                   (if terminated?
                                       '(literal rust-c-string-literal)
                                       '(malformed-token rust-c-string-literal)))]
            [(and (char=? next #\c)
                  (char? (peek-next in 1))
                  (char=? (peek-next in 1) #\r))
             (define terminated?
               (read-raw-string-literal! in out "cr"))
             (cond
               [terminated?
                (make-token-from-text start
                                      (current-stream-position in)
                                      (get-output-string out)
                                      '(literal rust-raw-string-literal
                                                rust-c-string-literal))]
               [else
                (read-identifier! in out)
                (make-token-from-text start
                                      (current-stream-position in)
                                      (get-output-string out)
                                      (if (set-member? rust-keywords (get-output-string out))
                                          '(keyword rust-keyword)
                                          '(identifier rust-identifier)))])]
            [(char=? next #\')
             (cond
               [(lifetime-token? in)
                (read-lifetime! in out)
                (make-token-from-text start
                                      (current-stream-position in)
                                      (get-output-string out)
                                      '(identifier rust-lifetime))]
               [else
                (define terminated?
                  (read-char-literal! in out))
                (make-token-from-text start
                                      (current-stream-position in)
                                      (get-output-string out)
                                      (if terminated?
                                          '(literal rust-char-literal)
                                          '(malformed-token rust-char-literal)))])]
            [(char-numeric? next)
             (read-number! in out)
             (make-token-from-text start
                                   (current-stream-position in)
                                   (get-output-string out)
                                   '(literal rust-numeric-literal))]
            [(ident-start? next)
             (read-identifier! in out)
             (make-token-from-text start
                                   (current-stream-position in)
                                   (get-output-string out)
                                   (if (set-member? rust-keywords (get-output-string out))
                                       '(keyword rust-keyword)
                                       '(identifier rust-identifier)))]
            [else
             (define-values (text tags)
               (consume-punctuator! in out))
             (make-token-from-text start
                                   (current-stream-position in)
                                   text
                                   tags)])))))
