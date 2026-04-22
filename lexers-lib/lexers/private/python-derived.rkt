#lang racket/base

;;;
;;; Python Derived Tokens
;;;
;;
;; Stateful Python tokenization and reusable Python-specific classifications.

;; python-derived-token?         : any/c -> boolean?
;;   Recognize a derived Python token.
;; python-derived-token-text     : python-derived-token? -> string?
;;   Extract the source text for one derived Python token.
;; python-derived-token-start    : python-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; python-derived-token-end      : python-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; python-derived-token-tags     : python-derived-token? -> (listof symbol?)
;;   Extract reusable Python classification tags.
;; python-derived-token-has-tag? : python-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-python-derived-reader    : -> (input-port? -> (or/c python-derived-token? 'eof))
;;   Construct a stateful Python derived-token reader.

(provide python-derived-token?
         python-derived-token-text
         python-derived-token-start
         python-derived-token-end
         python-derived-token-tags
         python-derived-token-has-tag?
         make-python-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         racket/string
         "parser-tools-compat.rkt")

;; A Python token plus reusable tags.
(struct python-derived-token (kind text start end tags) #:transparent)

;; python-derived-token-has-tag? : python-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (python-derived-token-has-tag? token tag)
  (member tag (python-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Python classification tables

;; Reserved Python keywords.
(define python-keywords
  (list->set
   '("False" "None" "True"
     "and" "as" "assert" "async" "await"
     "break"
     "class" "continue"
     "def" "del"
     "elif" "else" "except"
     "finally" "for" "from"
     "global"
     "if" "import" "in" "is"
     "lambda"
     "nonlocal" "not"
     "or"
     "pass"
     "raise" "return"
     "try"
     "while" "with"
     "yield")))

;; Parser-level soft keywords, still useful for syntax-coloring heuristics.
(define python-soft-keywords
  (list->set
   '("_" "case" "match" "type")))

;; Symbolic delimiters in Python source.
(define python-delimiters
  (list->set
   '("(" ")" "[" "]" "{" "}" "," ":" ";" "." "..." "@" "->")))

;; Symbolic operators in Python source.
(define python-operators
  (list->set
   '("+" "-" "*" "**" "/" "//" "%" "@"
     "<<" ">>" "&" "|" "^" "~"
     ":="
     "<" ">" "<=" ">=" "==" "!=" "!"
     "="
     "+=" "-=" "*=" "**=" "/=" "//=" "%="
     "@=" "&=" "|=" "^=" "<<=" ">>=")))

;; Operator and delimiter spellings in longest-match order.
(define python-symbol-tokens
  '("**=" "//=" "<<=" ">>="
    "..." "==" "!=" "<=" ">=" "->" ":="
    "+=" "-=" "*=" "/=" "%=" "@=" "&=" "|=" "^="
    "**" "//" "<<" ">>"
    "(" ")" "[" "]" "{" "}" "," ":" ";" "." "@"
    "+" "-" "*" "/" "%" "&" "|" "^" "~" "<" ">" "=" "!"))

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

;; make-token-from-text : position? position? string? (listof symbol?) -> python-derived-token?
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
  (python-derived-token kind
                        text
                        start-pos
                        end-pos
                        (remove-duplicates tags)))

;; make-zero-width-token : position? (listof symbol?) -> python-derived-token?
;;   Construct one zero-width synthetic indentation token.
(define (make-zero-width-token pos tags)
  (make-token-from-text pos pos "" tags))

;; queue-pending! : (listof python-derived-token?) (listof python-derived-token?) -> (listof python-derived-token?)
;;   Append queued synthetic or lookahead-derived tokens.
(define (queue-pending! pending additions)
  (append pending additions))

;; -----------------------------------------------------------------------------
;; Character classes

;; indentation-char? : char? -> boolean?
;;   Recognize leading indentation characters.
(define (indentation-char? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\page)))

;; python-inline-whitespace? : char? -> boolean?
;;   Recognize Python whitespace other than newlines.
(define (python-inline-whitespace? ch)
  (indentation-char? ch))

;; newline-start? : (or/c char? eof-object?) -> boolean?
;;   Determine whether a character begins a physical newline.
(define (newline-start? ch)
  (or (char=? ch #\newline)
      (char=? ch #\return)))

;; python-ident-start? : char? -> boolean?
;;   Recognize an identifier-start character.
(define (python-ident-start? ch)
  (or (char-alphabetic? ch)
      (char=? ch #\_)))

;; python-ident-char? : char? -> boolean?
;;   Recognize an identifier continuation character.
(define (python-ident-char? ch)
  (or (python-ident-start? ch)
      (char-numeric? ch)))

;; string-prefix-char? : char? -> boolean?
;;   Recognize a valid Python string-prefix character.
(define (string-prefix-char? ch)
  (member (char-downcase ch)
          '(#\r #\b #\f #\u #\t)))

;; Valid Python string prefixes, case-insensitive.
(define python-string-prefixes
  (list->set
   '("" "r" "u" "b" "br" "rb" "f" "fr" "rf" "t" "tr" "rt")))

;; number-tail-char? : char? -> boolean?
;;   Recognize a broad tail character for numeric literals.
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
;;   Consume one Python line comment without its terminating newline.
(define (read-line-comment! in out)
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

;; read-name! : input-port? output-port? -> void?
;;   Consume one NAME-like token.
(define (read-name! in out)
  (read-while! in out python-ident-char?))

;; string-prefix-length : input-port? -> (or/c exact-nonnegative-integer? #f)
;;   Determine whether a string prefix is followed by a quote.
(define (string-prefix-length in)
  (cond
    [(member (peek-next in) (list #\" #\'))
     0]
    [else
     (for/or ([len '(2 1)])
       (define quote
         (peek-next in len))
       (define prefix
         (let loop ([i     0]
                    [chars null])
           (cond
             [(= i len)
              chars]
             [else
              (define next
                (peek-next in i))
              (and (char? next)
                   (string-prefix-char? next)
                   (loop (add1 i)
                         (cons (char-downcase next) chars)))])))
        (and (list? prefix)
            (member quote (list #\" #\'))
            (set-member? python-string-prefixes
                         (list->string (reverse prefix)))
            len))]))

;; prefix-raw? : string? -> boolean?
;;   Determine whether a consumed string prefix is raw.
(define (prefix-raw? prefix)
  (for/or ([ch (in-string prefix)])
    (char=? (char-downcase ch) #\r)))

;; prefix-bytes? : string? -> boolean?
;;   Determine whether a consumed string prefix is bytes-oriented.
(define (prefix-bytes? prefix)
  (for/or ([ch (in-string prefix)])
    (char=? (char-downcase ch) #\b)))

;; prefix-formatted? : string? -> boolean?
;;   Determine whether a consumed string prefix is formatted-string-oriented.
(define (prefix-formatted? prefix)
  (for/or ([ch (in-string prefix)])
    (char=? (char-downcase ch) #\f)))

;; prefix-template? : string? -> boolean?
;;   Determine whether a consumed string prefix is template-string-oriented.
(define (prefix-template? prefix)
  (for/or ([ch (in-string prefix)])
    (char=? (char-downcase ch) #\t)))

;; read-python-string! : input-port? output-port? -> boolean?
;;   Consume one Python string literal and report whether it terminated.
(define (read-python-string! in out)
  (define prefix-len
    (string-prefix-length in))
  (for ([i (in-range prefix-len)])
    (write-one! in out))
  (define prefix
    (get-output-string out))
  (define raw?
    (prefix-raw? prefix))
  (define quote
    (peek-next in))
  (define triple?
    (and (char? quote)
         (char? (peek-next in 1))
         (char? (peek-next in 2))
         (char=? quote (peek-next in 1))
         (char=? quote (peek-next in 2))))
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
         [(and (not raw?) (char=? next #\\))
          (write-one! in out)
          (when (char? (peek-next in))
            (write-one! in out))
          (loop)]
         [(and (char=? next quote)
               (char? (peek-next in 1))
               (char? (peek-next in 2))
               (char=? (peek-next in 1) quote)
               (char=? (peek-next in 2) quote))
          (for ([i (in-range 3)])
            (write-one! in out))
          #t]
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
         [(and (not raw?) (char=? next #\\))
          (write-one! in out)
          (cond
            [(char? (peek-next in))
             (write-one! in out)
             (loop)]
            [else
             #f])]
         [(char=? next quote)
          (write-one! in out)
          #t]
         [else
          (write-one! in out)
          (loop)]))]))

;; read-python-number! : input-port? output-port? -> string?
;;   Consume one Python numeric literal candidate.
(define (read-python-number! in out)
  (define first
    (peek-next in))
  (cond
    [(char=? first #\.)
     (write-one! in out)
     (read-while! in out (lambda (ch)
                           (or (char-numeric? ch)
                               (char=? ch #\_))))
     (when (and (char? (peek-next in))
                (member (peek-next in) '(#\e #\E)))
       (write-one! in out)
       (when (and (char? (peek-next in))
                  (member (peek-next in) '(#\+ #\-)))
         (write-one! in out))
       (read-while! in out (lambda (ch)
                             (or (char-numeric? ch)
                                 (char=? ch #\_)))))
     (when (and (char? (peek-next in))
                (member (peek-next in) '(#\j #\J)))
       (write-one! in out))]
    [else
     (write-one! in out)
     (cond
       [(and (char=? first #\0)
             (char? (peek-next in))
             (member (peek-next in) '(#\x #\X #\o #\O #\b #\B)))
        (write-one! in out)
        (read-while! in out (lambda (ch)
                              (or (char-alphabetic? ch)
                                  (char-numeric? ch)
                                  (char=? ch #\_))))
        (when (and (char? (peek-next in))
                   (member (peek-next in) '(#\j #\J)))
          (write-one! in out))]
       [else
        (read-while! in out (lambda (ch)
                              (or (char-numeric? ch)
                                  (char=? ch #\_))))
        (when (and (char? (peek-next in))
                   (char=? (peek-next in) #\.))
          (write-one! in out)
          (read-while! in out (lambda (ch)
                                (or (char-numeric? ch)
                                    (char=? ch #\_)))))
        (when (and (char? (peek-next in))
                   (member (peek-next in) '(#\e #\E)))
          (write-one! in out)
          (when (and (char? (peek-next in))
                     (member (peek-next in) '(#\+ #\-)))
            (write-one! in out))
          (read-while! in out (lambda (ch)
                                (or (char-numeric? ch)
                                    (char=? ch #\_)))))
        (when (and (char? (peek-next in))
                   (member (peek-next in) '(#\j #\J)))
          (write-one! in out))])])
  (get-output-string out))

;; valid-python-number? : string? -> boolean?
;;   Check whether a numeric candidate is valid enough for the first lexer slice.
(define (valid-python-number? text)
  (regexp-match?
   #px"^(?:0[bB][01](?:_?[01])*|0[oO][0-7](?:_?[0-7])*|0[xX][0-9A-Fa-f](?:_?[0-9A-Fa-f])*|(?:[0-9](?:_?[0-9])*)(?:\\.(?:[0-9](?:_?[0-9])*)?)?(?:[eE][+-]?[0-9](?:_?[0-9])*)?[jJ]?|\\.[0-9](?:_?[0-9])*(?:[eE][+-]?[0-9](?:_?[0-9])*)?[jJ]?)$"
   text))

;; symbol-token-text : input-port? -> (or/c string? #f)
;;   Find the longest operator or delimiter token starting at the current port position.
(define (symbol-token-text in)
  (for/or ([candidate (in-list python-symbol-tokens)])
    (define len
      (string-length candidate))
    (define matched?
      (for/and ([i (in-range len)])
        (define next
          (peek-next in i))
        (and (char? next)
             (char=? next (string-ref candidate i)))))
    (and matched?
         candidate)))

;; read-symbol! : input-port? output-port? string? -> void?
;;   Consume one operator or delimiter token with known text.
(define (read-symbol! in out text)
  (for ([i (in-range (string-length text))])
    (write-one! in out)))

;; indentation-width : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Compute the indentation column for one leading-whitespace slice.
(define (indentation-width text start-col)
  (for/fold ([col start-col])
            ([ch (in-string text)])
    (cond
      [(char=? ch #\space) (add1 col)]
      [(char=? ch #\tab)   (+ col (- 8 (modulo col 8)))]
      [(char=? ch #\page)  0]
      [else                col])))

;; dedent-to! : (listof exact-nonnegative-integer?) exact-nonnegative-integer? position? -> (values (listof exact-nonnegative-integer?) (listof python-derived-token?))
;;   Pop indentation levels until target-width or produce a malformed token.
(define (dedent-to! stack target-width pos)
  (let loop ([current stack]
             [tokens  '()])
    (cond
      [(null? (cdr current))
       (cond
         [(= (car current) target-width)
          (values current (reverse tokens))]
         [else
          (values (cons target-width current)
                  (reverse
                   (cons (make-zero-width-token pos
                                                '(delimiter malformed-token python-error))
                         tokens)))])]
      [(<= target-width (cadr current))
       (define next-stack
         (cdr current))
       (define next-tokens
         (cons (make-zero-width-token pos
                                      '(delimiter python-dedent))
               tokens))
       (cond
         [(= target-width (car next-stack))
          (values next-stack (reverse next-tokens))]
         [else
          (loop next-stack next-tokens)])]
      [else
       (values current (reverse tokens))])))

;; -----------------------------------------------------------------------------
;; Public reader

;; make-python-derived-reader : -> (input-port? -> (or/c python-derived-token? 'eof))
;;   Construct a stateful Python derived-token reader.
(define (make-python-derived-reader)
  (define indent-stack
    '(0))
  (define pending
    '())
  (define paren-depth
    0)
  (define line-start?
    #t)
  (define continued-line?
    #f)

  ;; pop-pending! : -> (or/c python-derived-token? #f)
  ;;   Pop one queued token if available.
  (define (pop-pending!)
    (cond
      [(pair? pending)
       (define token
         (car pending))
       (set! pending
             (cdr pending))
       token]
      [else
       #f]))

  ;; queue-token! : python-derived-token? -> void?
  ;;   Enqueue one token after existing pending tokens.
  (define (queue-token! token)
    (set! pending
          (queue-pending! pending (list token))))

  ;; queue-tokens! : (listof python-derived-token?) -> void?
  ;;   Enqueue many tokens after existing pending tokens.
  (define (queue-tokens! tokens)
    (set! pending
          (queue-pending! pending tokens)))

  ;; process-line-start! : input-port? -> void?
  ;;   Handle indentation-sensitive line starts when not in implicit continuation.
  (define (process-line-start! in)
    (when (and line-start?
               (not continued-line?)
               (zero? paren-depth))
      (define start-pos
        (current-stream-position in))
      (define out
        (open-output-string))
      (read-while! in out indentation-char?)
      (define indent-text
        (get-output-string out))
      (define indent-end-pos
        (current-stream-position in))
      (define next
        (peek-next in))
      (cond
        [(eof-object? next)
         (when (positive? (string-length indent-text))
           (queue-token!
            (make-token-from-text start-pos
                                  indent-end-pos
                                  indent-text
                                  '(whitespace python-whitespace python-indentation))))
         (set! line-start?
               #f)]
        [(newline-start? next)
         (when (positive? (string-length indent-text))
           (queue-token!
            (make-token-from-text start-pos
                                  indent-end-pos
                                  indent-text
                                  '(whitespace python-whitespace python-indentation))))
         (define newline-start-pos
           (current-stream-position in))
         (define newline-out
           (open-output-string))
         (read-newline! in newline-out)
         (queue-token!
          (make-token-from-text newline-start-pos
                                (current-stream-position in)
                                (get-output-string newline-out)
                                '(whitespace python-whitespace python-nl)))
         (set! line-start?
               #t)]
        [(char=? next #\#)
         (when (positive? (string-length indent-text))
           (queue-token!
            (make-token-from-text start-pos
                                  indent-end-pos
                                  indent-text
                                  '(whitespace python-whitespace python-indentation))))
         (define comment-start-pos
           (current-stream-position in))
         (define comment-out
           (open-output-string))
         (read-line-comment! in comment-out)
         (queue-token!
          (make-token-from-text comment-start-pos
                                (current-stream-position in)
                                (get-output-string comment-out)
                                '(comment python-comment)))
         (when (newline-start? (peek-next in))
           (define newline-start-pos
             (current-stream-position in))
           (define newline-out
             (open-output-string))
           (read-newline! in newline-out)
           (queue-token!
            (make-token-from-text newline-start-pos
                                  (current-stream-position in)
                                  (get-output-string newline-out)
                                  '(whitespace python-whitespace python-nl))))
         (set! line-start?
               #t)]
        [else
         (when (positive? (string-length indent-text))
           (queue-token!
            (make-token-from-text start-pos
                                  indent-end-pos
                                  indent-text
                                  '(whitespace python-whitespace python-indentation))))
         (define indent-col
           (indentation-width indent-text 0))
         (define current-indent
           (car indent-stack))
         (cond
           [(> indent-col current-indent)
            (set! indent-stack
                  (cons indent-col indent-stack))
            (queue-token!
             (make-zero-width-token indent-end-pos
                                    '(delimiter python-indent)))]
           [(< indent-col current-indent)
            (define-values (next-stack dedent-tokens)
              (dedent-to! indent-stack indent-col indent-end-pos))
            (set! indent-stack
                  next-stack)
            (queue-tokens! dedent-tokens)]
           [else
            (void)])
         (set! line-start?
               #f)])))

  ;; next-derived-token : input-port? -> (or/c python-derived-token? 'eof)
  ;;   Read the next derived token from one input port.
  (define (next-derived-token in)
    (define queued
      (pop-pending!))
    (cond
      [queued
       queued]
      [else
       (process-line-start! in)
       (define maybe-queued
         (pop-pending!))
       (cond
         [maybe-queued
          maybe-queued]
         [else
          (define start-pos
            (current-stream-position in))
          (define next
            (peek-next in))
          (cond
            [(eof-object? next)
             (cond
               [(pair? (cdr indent-stack))
                (set! indent-stack
                      (cdr indent-stack))
                (make-zero-width-token start-pos
                                       '(delimiter python-dedent))]
               [else
                'eof])]
            [(python-inline-whitespace? next)
             (define out
               (open-output-string))
             (read-while! in out python-inline-whitespace?)
             (make-token-from-text start-pos
                                   (current-stream-position in)
                                   (get-output-string out)
                                   '(whitespace python-whitespace))]
            [(newline-start? next)
             (define out
               (open-output-string))
             (read-newline! in out)
             (define tags
               (cond
                 [(or (positive? paren-depth) continued-line? line-start?)
                  '(whitespace python-whitespace python-nl)]
                 [else
                  '(whitespace python-whitespace python-newline)]))
             (when continued-line?
               (set! continued-line?
                     #f))
             (set! line-start?
                   #t)
             (make-token-from-text start-pos
                                   (current-stream-position in)
                                   (get-output-string out)
                                   tags)]
            [(and (char=? next #\\)
                  (char? (peek-next in 1))
                  (newline-start? (peek-next in 1)))
             (define out
               (open-output-string))
             (write-one! in out)
             (read-newline! in out)
             (set! line-start?
                   #t)
             (set! continued-line?
                   #t)
             (make-token-from-text start-pos
                                   (current-stream-position in)
                                   (get-output-string out)
                                   '(whitespace python-whitespace python-line-join))]
            [(char=? next #\#)
             (define out
               (open-output-string))
             (read-line-comment! in out)
             (make-token-from-text start-pos
                                   (current-stream-position in)
                                   (get-output-string out)
                                   '(comment python-comment))]
            [(or (member next '(#\" #\'))
                 (string-prefix-length in))
             (define out
               (open-output-string))
             (define terminated?
               (read-python-string! in out))
             (define text
               (get-output-string out))
             (define prefix-len
               (or (string-prefix-length (open-input-string text))
                   0))
             (define prefix
               (substring text 0 prefix-len))
             (define tags
               (append
                '(literal)
                (cond
                  [(prefix-bytes? prefix)
                   '(python-bytes-literal)]
                  [(prefix-formatted? prefix)
                   '(python-string-literal python-f-string-literal)]
                  [(prefix-template? prefix)
                   '(python-string-literal python-t-string-literal)]
                  [else
                   '(python-string-literal)])
                (if (prefix-raw? prefix)
                    '(python-raw-string-literal)
                    '())
                (if terminated?
                    '()
                    '(malformed-token python-error))))
             (make-token-from-text start-pos
                                   (current-stream-position in)
                                   text
                                   tags)]
            [(python-ident-start? next)
             (define out
               (open-output-string))
             (read-name! in out)
             (define text
               (get-output-string out))
             (define tags
               (cond
                 [(set-member? python-keywords text)
                  '(keyword python-keyword)]
                 [(set-member? python-soft-keywords text)
                  '(keyword python-soft-keyword)]
                 [else
                  '(identifier python-identifier)]))
             (set! continued-line?
                   #f)
             (make-token-from-text start-pos
                                   (current-stream-position in)
                                   text
                                   tags)]
            [(or (char-numeric? next)
                 (and (char=? next #\.)
                      (char? (peek-next in 1))
                      (char-numeric? (peek-next in 1))))
             (define out
               (open-output-string))
             (define text
               (read-python-number! in out))
             (make-token-from-text start-pos
                                   (current-stream-position in)
                                   text
                                   (if (valid-python-number? text)
                                       '(literal python-numeric-literal)
                                       '(literal malformed-token python-error python-numeric-literal)))]
            [else
             (define symbol-text
               (symbol-token-text in))
             (cond
               [symbol-text
                (define out
                  (open-output-string))
                (read-symbol! in out symbol-text)
                (cond
                  [(member symbol-text '("(" "[" "{"))
                   (set! paren-depth
                         (add1 paren-depth))]
                  [(member symbol-text '(")" "]" "}"))
                   (set! paren-depth
                         (max 0 (sub1 paren-depth)))]
                  [else
                   (void)])
                (make-token-from-text start-pos
                                      (current-stream-position in)
                                      symbol-text
                                      (cond
                                        [(set-member? python-delimiters symbol-text)
                                         '(delimiter python-delimiter)]
                                        [else
                                         '(operator python-operator)]))]
               [else
                (define out
                  (open-output-string))
                (write-one! in out)
                (make-token-from-text start-pos
                                      (current-stream-position in)
                                      (get-output-string out)
                                      '(malformed-token python-error))])])])]))

  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-python-derived-reader "input-port?" in))
    (port-count-lines! in)
    (next-derived-token in)))
