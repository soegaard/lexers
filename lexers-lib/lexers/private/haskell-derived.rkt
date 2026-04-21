#lang racket/base

;;;
;;; Haskell Derived Tokens
;;;
;;
;; Stateful Haskell tokenization and reusable Haskell-specific classifications.

;; haskell-derived-token?         : any/c -> boolean?
;;   Recognize a derived Haskell token.
;; haskell-derived-token-text     : haskell-derived-token? -> string?
;;   Extract the source text for one derived token.
;; haskell-derived-token-start    : haskell-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; haskell-derived-token-end      : haskell-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; haskell-derived-token-tags     : haskell-derived-token? -> (listof symbol?)
;;   Extract reusable Haskell classification tags.
;; haskell-derived-token-has-tag? : haskell-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-haskell-derived-reader    : -> (input-port? -> (or/c haskell-derived-token? 'eof))
;;   Construct a stateful Haskell derived-token reader.

(provide haskell-derived-token?
         haskell-derived-token-text
         haskell-derived-token-start
         haskell-derived-token-end
         haskell-derived-token-tags
         haskell-derived-token-has-tag?
         make-haskell-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         racket/string
         "parser-tools-compat.rkt")

;; A Haskell token plus reusable tags.
(struct haskell-derived-token (kind text start end tags) #:transparent)

;; haskell-derived-token-has-tag? : haskell-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (haskell-derived-token-has-tag? token tag)
  (member tag (haskell-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Classification tables

;; Haskell 2010 reserved identifiers plus a few common modern GHC additions.
(define haskell-keywords
  (list->set
   '("case" "class" "data" "default" "deriving" "do" "else" "if"
     "import" "in" "infix" "infixl" "infixr" "instance" "let"
     "module" "newtype" "of" "then" "type" "where" "_"
     "foreign" "family" "role" "pattern" "stock" "via"
     "qualified" "hiding" "safe" "unsafe" "mdo" "proc" "rec")))

;; Reserved Haskell operators.
(define reserved-operators
  (list->set
   '(".." ":" "::" "=" "\\" "|" "<-" "->" "@" "~" "=>")))

;; Special delimiter characters.
(define haskell-delimiters
  (list->set
   '("(" ")" "," ";" "[" "]" "`" "{" "}")))

;; Symbol characters in the ASCII core lexer slice.
(define symbol-chars
  (list->set
   '(#\! #\# #\$ #\% #\& #\* #\+ #\. #\/ #\< #\= #\> #\? #\@
     #\\ #\^ #\| #\- #\~ #\:)))

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

;; make-token-from-text : position? position? string? (listof symbol?) -> haskell-derived-token?
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
  (haskell-derived-token kind
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

;; haskell-inline-whitespace? : char? -> boolean?
;;   Recognize Haskell whitespace other than newlines.
(define (haskell-inline-whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\page)
      (char=? ch #\vtab)))

;; small-start? : char? -> boolean?
;;   Recognize the start of a variable identifier.
(define (small-start? ch)
  (or (char-lower-case? ch)
      (char=? ch #\_)))

;; large-start? : char? -> boolean?
;;   Recognize the start of a constructor identifier.
(define (large-start? ch)
  (char-upper-case? ch))

;; identifier-char? : char? -> boolean?
;;   Recognize identifier continuation characters for the ASCII core slice.
(define (identifier-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (char=? ch #\_)
      (char=? ch #\')))

;; symbol-char? : char? -> boolean?
;;   Recognize one symbolic operator character.
(define (symbol-char? ch)
  (set-member? symbol-chars ch))

;; hex-digit? : char? -> boolean?
;;   Recognize one hexadecimal digit.
(define (hex-digit? ch)
  (or (char-numeric? ch)
      (member (char-downcase ch)
              '(#\a #\b #\c #\d #\e #\f))))

;; oct-digit? : char? -> boolean?
;;   Recognize one octal digit.
(define (oct-digit? ch)
  (and (char-numeric? ch)
       (char<=? ch #\7)))

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
;;   Consume one run of Haskell whitespace.
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
      [(haskell-inline-whitespace? next)
       (write-one! in out)
       (loop)]
      [else
       (void)])))

;; starts-line-comment? : input-port? -> boolean?
;;   Determine whether the current position begins a Haskell line comment.
(define (starts-line-comment? in)
  (and (char? (peek-next in))
       (char=? (peek-next in) #\-)
       (char? (peek-next in 1))
       (char=? (peek-next in 1) #\-)
       (let loop ([i 2])
         (define next
           (peek-next in i))
         (cond
           [(and (char? next) (char=? next #\-))
            (loop (add1 i))]
           [(or (eof-object? next)
                (newline-start? next))
            #t]
           [else
            (not (symbol-char? next))]))))

;; read-line-comment! : input-port? output-port? -> void?
;;   Consume one Haskell line comment without its terminating newline.
(define (read-line-comment! in out)
  (write-one! in out)
  (write-one! in out)
  (read-while! in out (lambda (ch) (char=? ch #\-)))
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

;; matches-ahead? : input-port? string? -> boolean?
;;   Determine whether text appears at the current input position.
(define (matches-ahead? in text)
  (for/and ([i (in-range (string-length text))])
    (and (char? (peek-next in i))
         (char=? (peek-next in i)
                 (string-ref text i)))))

;; read-nested-comment! : input-port? output-port? -> boolean?
;;   Consume one nested Haskell comment and report whether it terminated.
(define (read-nested-comment! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ([depth 1])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(matches-ahead? in "{-")
       (write-one! in out)
       (write-one! in out)
       (loop (add1 depth))]
      [(matches-ahead? in "-}")
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

;; read-pragma! : input-port? output-port? -> boolean?
;;   Consume one pragma comment and report whether it terminated.
(define (read-pragma! in out)
  (write-one! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(matches-ahead? in "#-}")
       (write-one! in out)
       (write-one! in out)
       (write-one! in out)
       #t]
      [else
       (write-one! in out)
       (loop)])))

;; read-identifier! : input-port? output-port? -> void?
;;   Consume one Haskell identifier.
(define (read-identifier! in out)
  (read-while! in out identifier-char?))

;; read-symbol-operator! : input-port? output-port? -> void?
;;   Consume one symbolic operator token.
(define (read-symbol-operator! in out)
  (read-while! in out (lambda (ch)
                        (or (symbol-char? ch)
                            (char=? ch #\:)))))

;; read-char-literal! : input-port? output-port? -> boolean?
;;   Consume one Haskell character literal and report whether it terminated.
(define (read-char-literal! in out)
  (write-one! in out)
  (let loop ([escaped? #f])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(newline-start? next)
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
      [else
       (write-one! in out)
       (loop #f)])))

;; read-string-literal! : input-port? output-port? -> boolean?
;;   Consume one Haskell string literal and report whether it terminated.
(define (read-string-literal! in out)
  (write-one! in out)
  (let loop ([escaped? #f])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(newline-start? next)
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
      [else
       (write-one! in out)
       (loop #f)])))

;; read-number! : input-port? output-port? -> void?
;;   Consume one Haskell numeric literal candidate.
(define (read-number! in out)
  (cond
    [(and (char=? (peek-next in) #\0)
          (char? (peek-next in 1))
          (member (peek-next in 1) '(#\o #\O)))
     (write-one! in out)
     (write-one! in out)
     (read-while! in out oct-digit?)]
    [(and (char=? (peek-next in) #\0)
          (char? (peek-next in 1))
          (member (peek-next in 1) '(#\x #\X)))
     (write-one! in out)
     (write-one! in out)
     (read-while! in out hex-digit?)]
    [else
     (read-while! in out char-numeric?)
     (when (and (char? (peek-next in))
                (char=? (peek-next in) #\.)
                (char? (peek-next in 1))
                (char-numeric? (peek-next in 1)))
       (write-one! in out)
       (read-while! in out char-numeric?))
     (when (and (char? (peek-next in))
                (member (peek-next in) '(#\e #\E)))
       (write-one! in out)
       (when (and (char? (peek-next in))
                  (member (peek-next in) '(#\+ #\-)))
         (write-one! in out))
       (read-while! in out char-numeric?))]))

;; consume-special! : input-port? output-port? -> (values string? (listof symbol?))
;;   Consume one reserved operator, operator symbol, or special delimiter.
(define (consume-special! in out)
  (cond
    [(or (char=? (peek-next in) #\()
         (char=? (peek-next in) #\))
         (char=? (peek-next in) #\,)
         (char=? (peek-next in) #\;)
         (char=? (peek-next in) #\[)
         (char=? (peek-next in) #\])
         (char=? (peek-next in) #\`)
         (char=? (peek-next in) #\{)
         (char=? (peek-next in) #\}))
     (write-one! in out)
     (values (get-output-string out)
             '(delimiter haskell-delimiter))]
    [else
     (read-symbol-operator! in out)
     (define text
       (get-output-string out))
     (values text
             (append '(operator)
                     (cond
                       [(string=? text "")
                        '(haskell-variable-operator malformed-token)]
                       [(set-member? reserved-operators text)
                        '(haskell-variable-operator)]
                       [(char=? (string-ref text 0) #\:)
                        '(haskell-constructor-operator)]
                       [else
                        '(haskell-variable-operator)])))]))

;; -----------------------------------------------------------------------------
;; Reader

;; make-haskell-derived-reader : -> (input-port? -> (or/c haskell-derived-token? 'eof))
;;   Construct a stateful Haskell derived-token reader.
(define (make-haskell-derived-reader)
  (lambda (in)
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       'eof]
      [else
       (define start
         (current-stream-position in))
       (define out
         (open-output-string))
       (cond
         [(or (newline-start? next)
              (haskell-inline-whitespace? next))
          (read-whitespace! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(whitespace haskell-whitespace))]
         [(starts-line-comment? in)
          (read-line-comment! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(comment haskell-comment haskell-line-comment))]
         [(matches-ahead? in "{-#")
          (define terminated?
            (read-pragma! in out))
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                (if terminated?
                                    '(comment haskell-comment haskell-pragma)
                                    '(comment haskell-comment haskell-pragma malformed-token)))]
         [(matches-ahead? in "{-")
          (define terminated?
            (read-nested-comment! in out))
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                (if terminated?
                                    '(comment haskell-comment haskell-nested-comment)
                                    '(comment haskell-comment haskell-nested-comment malformed-token)))]
         [(char=? next #\')
          (define terminated?
            (read-char-literal! in out))
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                (if terminated?
                                    '(literal haskell-char-literal)
                                    '(literal haskell-char-literal malformed-token)))]
         [(char=? next #\")
          (define terminated?
            (read-string-literal! in out))
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                (if terminated?
                                    '(literal haskell-string-literal)
                                    '(literal haskell-string-literal malformed-token)))]
         [(char-numeric? next)
          (read-number! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(literal haskell-numeric-literal))]
         [(small-start? next)
          (read-identifier! in out)
          (define text
            (get-output-string out))
          (make-token-from-text start
                                (current-stream-position in)
                                text
                                (if (set-member? haskell-keywords text)
                                    '(keyword haskell-keyword)
                                    '(identifier haskell-variable-identifier)))]
         [(large-start? next)
          (read-identifier! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(identifier haskell-constructor-identifier))]
         [else
          (define-values (text tags)
            (consume-special! in out))
          (make-token-from-text start
                                (current-stream-position in)
                                text
                                tags)])])))
