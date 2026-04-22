#lang racket/base

;;;
;;; Pascal Derived Tokens
;;;
;;
;; Stateful Pascal tokenization and reusable Pascal-specific classifications.

;; pascal-derived-token?         : any/c -> boolean?
;;   Recognize a derived Pascal token.
;; pascal-derived-token-text     : pascal-derived-token? -> string?
;;   Extract the source text for one derived token.
;; pascal-derived-token-start    : pascal-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; pascal-derived-token-end      : pascal-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; pascal-derived-token-tags     : pascal-derived-token? -> (listof symbol?)
;;   Extract reusable Pascal classification tags.
;; pascal-derived-token-has-tag? : pascal-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-pascal-derived-reader    : -> (input-port? -> (or/c pascal-derived-token? 'eof))
;;   Construct a stateful Pascal derived-token reader.

(provide pascal-derived-token?
         pascal-derived-token-text
         pascal-derived-token-start
         pascal-derived-token-end
         pascal-derived-token-tags
         pascal-derived-token-has-tag?
         make-pascal-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         racket/string
         "parser-tools-compat.rkt")

;; A Pascal token plus reusable tags.
(struct pascal-derived-token (kind text start end tags) #:transparent)

;; pascal-derived-token-has-tag? : pascal-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (pascal-derived-token-has-tag? token tag)
  (member tag (pascal-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Classification tables

;; Pascal and Object Pascal reserved words for the first lexer slice.
(define pascal-keywords
  (list->set
   '("absolute" "abstract" "and" "array" "asm" "begin" "case" "class"
     "const" "constructor" "destructor" "dispinterface" "div" "do" "downto"
     "else" "end" "except" "exports" "file" "finalization" "finally" "for"
     "function" "goto" "if" "implementation" "in" "inherited"
     "initialization" "inline" "interface" "is" "label" "library" "mod"
     "nil" "not" "object" "of" "on" "operator" "or" "packed" "procedure"
     "program" "property" "raise" "record" "repeat" "resourcestring"
     "set" "shl" "shr" "string" "then" "threadvar" "to" "try" "type"
     "unit" "until" "uses" "var" "while" "with" "xor"
     "deprecated" "experimental" "platform" "unimplemented"
     "private" "protected" "public" "published" "virtual" "override")))

;; Pascal delimiters.
(define pascal-delimiters
  (list->set
   '("(" ")" "[" "]" "{" "}" "," ";" "." ":")))

;; Pascal punctuation/operators in longest-match order.
(define pascal-punctuators
  '("(."
    ".)"
    ":="
    "<="
    ">="
    "<>"
    "><"
    "<<"
    ">>"
    "**"
    "+="
    "-="
    "*="
    "/="
    "//"
    "@"
    "^"
    "="
    "<"
    ">"
    "+"
    "-"
    "*"
    "/"
    "("
    ")"
    "["
    "]"
    "{"
    "}"
    ","
    ";"
    "."
    ":"))

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

;; make-token-from-text : position? position? string? (listof symbol?) -> pascal-derived-token?
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
  (pascal-derived-token kind
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

;; pascal-inline-whitespace? : char? -> boolean?
;;   Recognize Pascal whitespace other than newlines.
(define (pascal-inline-whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\page)))

;; identifier-start? : char? -> boolean?
;;   Recognize the start of a Pascal identifier.
(define (identifier-start? ch)
  (or (char-alphabetic? ch)
      (char=? ch #\_)))

;; identifier-char? : char? -> boolean?
;;   Recognize the continuation of a Pascal identifier.
(define (identifier-char? ch)
  (or (identifier-start? ch)
      (char-numeric? ch)))

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

;; bin-digit? : char? -> boolean?
;;   Recognize one binary digit.
(define (bin-digit? ch)
  (or (char=? ch #\0)
      (char=? ch #\1)))

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
;;   Consume one run of Pascal whitespace.
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
      [(pascal-inline-whitespace? next)
       (write-one! in out)
       (loop)]
      [else
       (void)])))

;; read-line-comment! : input-port? output-port? -> void?
;;   Consume one Pascal line comment without its terminating newline.
(define (read-line-comment! in out)
  (write-one! in out)
  (write-one! in out)
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

;; read-nested-comment! : input-port? output-port? string? string? -> boolean?
;;   Consume one nested comment delimited by opener and closer.
(define (read-nested-comment! in out opener closer)
  (for ([ch (in-string opener)])
    (write-one! in out))
  (let loop ([depth 1])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(matches-ahead? in opener)
       (for ([i (in-range (string-length opener))])
         (write-one! in out))
       (loop (add1 depth))]
      [(matches-ahead? in closer)
       (for ([i (in-range (string-length closer))])
         (write-one! in out))
       (cond
         [(= depth 1)
          #t]
         [else
          (loop (sub1 depth))])]
      [(and (char=? next #\/)
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\/))
         (read-line-comment! in out)
         (loop depth)]
      [else
       (write-one! in out)
       (loop depth)])))

;; read-identifier! : input-port? output-port? -> void?
;;   Consume one Pascal identifier.
(define (read-identifier! in out)
  (read-while! in out identifier-char?))

;; read-string-literal! : input-port? output-port? -> boolean?
;;   Consume one Pascal single-quoted string and report whether it terminated.
(define (read-string-literal! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(newline-start? next)
       #f]
      [(char=? next #\')
       (write-one! in out)
       (cond
         [(and (char? (peek-next in))
               (char=? (peek-next in) #\'))
          (write-one! in out)
          (loop)]
         [else
          #t])]
      [else
       (write-one! in out)
       (loop)])))

;; read-control-string! : input-port? output-port? -> boolean?
;;   Consume one #<digits> control-string fragment.
(define (read-control-string! in out)
  (write-one! in out)
  (cond
    [(and (char? (peek-next in))
          (char-numeric? (peek-next in)))
     (read-while! in out char-numeric?)
     #t]
    [else
     #f]))

;; pascal-compiler-directive-comment? : string? -> boolean?
;;   Determine whether one Pascal comment is a compiler directive.
(define (pascal-compiler-directive-comment? text)
  (define len
    (string-length text))
  (and (>= len 3)
       (cond
         [(and (char=? (string-ref text 0) #\{)
               (char=? (string-ref text 1) #\$))
          #t]
         [(and (>= len 4)
               (char=? (string-ref text 0) #\()
               (char=? (string-ref text 1) #\*)
               (char=? (string-ref text 2) #\$))
          #t]
         [else
          #f])))

;; read-decimal-or-real! : input-port? output-port? -> void?
;;   Consume one decimal or real literal.
(define (read-decimal-or-real! in out)
  (read-while! in out char-numeric?)
  (when (and (char? (peek-next in))
             (char=? (peek-next in) #\.)
             (char? (peek-next in 1))
             (char-numeric? (peek-next in 1)))
    (write-one! in out)
    (read-while! in out char-numeric?))
  (when (and (char? (peek-next in))
             (member (char-downcase (peek-next in))
                     '(#\e)))
    (write-one! in out)
    (when (and (char? (peek-next in))
               (member (peek-next in) '(#\+ #\-)))
      (write-one! in out))
    (read-while! in out char-numeric?)))

;; valid-pascal-decimal-or-real? : string? -> boolean?
;;   Determine whether one decimal or real literal has a valid exponent form.
(define (valid-pascal-decimal-or-real? text)
  (regexp-match?
   #px"^(?:[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?)$"
   text))

;; valid-pascal-prefixed-number? : string? char? (char? -> boolean?) -> boolean?
;;   Determine whether one prefixed Pascal number contains at least one valid digit.
(define (valid-pascal-prefixed-number? text prefix-char digit?)
  (and (positive? (string-length text))
       (char=? (string-ref text 0) prefix-char)
       (> (string-length text) 1)
       (for/and ([ch (in-string (substring text 1))])
         (digit? ch))))

;; consume-punctuator! : input-port? output-port? -> (values string? (listof symbol?))
;;   Consume one Pascal punctuator using longest-match order.
(define (consume-punctuator! in out)
  (define matched
    (for/or ([punct (in-list pascal-punctuators)])
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
               [(or (set-member? pascal-delimiters matched)
                    (member matched '("(." ".)")))
                '(delimiter pascal-delimiter)]
               [else
                '(operator pascal-operator)]))]
    [else
     (write-one! in out)
     (values (get-output-string out)
             '(operator pascal-operator malformed-token))]))

;; -----------------------------------------------------------------------------
;; Reader

;; make-pascal-derived-reader : -> (input-port? -> (or/c pascal-derived-token? 'eof))
;;   Construct a stateful Pascal derived-token reader.
(define (make-pascal-derived-reader)
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
              (pascal-inline-whitespace? next))
          (read-whitespace! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(whitespace pascal-whitespace))]
         [(and (char=? next #\/)
               (char? (peek-next in 1))
               (char=? (peek-next in 1) #\/))
          (read-line-comment! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(comment pascal-comment))]
         [(char=? next #\{)
          (define terminated?
            (read-nested-comment! in out "{" "}"))
          (define text
            (get-output-string out))
          (make-token-from-text start
                                (current-stream-position in)
                                text
                                (append
                                 '(comment pascal-comment)
                                 (if (pascal-compiler-directive-comment? text)
                                     '(pascal-compiler-directive)
                                     '())
                                 (if terminated?
                                     '()
                                     '(malformed-token))))]
         [(and (char=? next #\()
               (char? (peek-next in 1))
               (char=? (peek-next in 1) #\*))
          (define terminated?
            (read-nested-comment! in out "(*" "*)"))
          (define text
            (get-output-string out))
          (make-token-from-text start
                                (current-stream-position in)
                                text
                                (append
                                 '(comment pascal-comment)
                                 (if (pascal-compiler-directive-comment? text)
                                     '(pascal-compiler-directive)
                                     '())
                                 (if terminated?
                                     '()
                                     '(malformed-token))))]
         [(char=? next #\')
          (define terminated?
            (read-string-literal! in out))
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                (if terminated?
                                    '(literal pascal-string-literal)
                                    '(literal pascal-string-literal malformed-token)))]
         [(char=? next #\#)
          (define ok?
            (read-control-string! in out))
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                (if ok?
                                    '(literal pascal-control-string)
                                    '(literal pascal-control-string malformed-token)))]
         [(char=? next #\$)
          (write-one! in out)
          (read-while! in out hex-digit?)
          (define text
            (get-output-string out))
          (make-token-from-text start
                                (current-stream-position in)
                                text
                                (if (valid-pascal-prefixed-number? text #\$ hex-digit?)
                                    '(literal pascal-numeric-literal)
                                    '(literal pascal-numeric-literal malformed-token)))]
         [(char=? next #\%)
          (write-one! in out)
          (read-while! in out bin-digit?)
          (define text
            (get-output-string out))
          (make-token-from-text start
                                (current-stream-position in)
                                text
                                (if (valid-pascal-prefixed-number? text #\% bin-digit?)
                                    '(literal pascal-numeric-literal)
                                    '(literal pascal-numeric-literal malformed-token)))]
         [(char=? next #\&)
          (write-one! in out)
          (cond
            [(and (char? (peek-next in))
                  (identifier-start? (peek-next in)))
             (read-identifier! in out)
             (make-token-from-text start
                                   (current-stream-position in)
                                   (get-output-string out)
                                   '(identifier pascal-identifier pascal-escaped-identifier))]
            [else
             (read-while! in out oct-digit?)
             (define text
               (get-output-string out))
             (make-token-from-text start
                                   (current-stream-position in)
                                   text
                                   (if (valid-pascal-prefixed-number? text #\& oct-digit?)
                                       '(literal pascal-numeric-literal)
                                       '(literal pascal-numeric-literal malformed-token)))])]
         [(char-numeric? next)
          (read-decimal-or-real! in out)
          (define text
            (get-output-string out))
          (make-token-from-text start
                                (current-stream-position in)
                                text
                                (if (valid-pascal-decimal-or-real? text)
                                    '(literal pascal-numeric-literal)
                                    '(literal pascal-numeric-literal malformed-token)))]
         [(identifier-start? next)
          (read-identifier! in out)
          (define text
            (get-output-string out))
          (define lower-text
            (string-downcase text))
          (make-token-from-text start
                                (current-stream-position in)
                                text
                                (if (set-member? pascal-keywords lower-text)
                                    '(keyword pascal-keyword)
                                    '(identifier pascal-identifier)))]
         [else
          (define-values (text tags)
            (consume-punctuator! in out))
          (make-token-from-text start
                                (current-stream-position in)
                                text
                                tags)])])))
