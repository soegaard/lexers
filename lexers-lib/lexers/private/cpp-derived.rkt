#lang racket/base

;;;
;;; C++ Derived Tokens
;;;
;;
;; Stateful C++ tokenization and reusable C++-specific classifications.

;; cpp-derived-token?         : any/c -> boolean?
;;   Recognize a derived C++ token.
;; cpp-derived-token-text     : cpp-derived-token? -> string?
;;   Extract the source text for one derived token.
;; cpp-derived-token-start    : cpp-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; cpp-derived-token-end      : cpp-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; cpp-derived-token-tags     : cpp-derived-token? -> (listof symbol?)
;;   Extract reusable C++ classification tags.
;; cpp-derived-token-has-tag? : cpp-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-cpp-derived-reader    : -> (input-port? -> (or/c cpp-derived-token? 'eof))
;;   Construct a stateful C++ derived-token reader.

(provide cpp-derived-token?
         cpp-derived-token-text
         cpp-derived-token-start
         cpp-derived-token-end
         cpp-derived-token-tags
         cpp-derived-token-has-tag?
         make-cpp-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         racket/string
         "parser-tools-compat.rkt")

;; A C++ token plus reusable tags.
(struct cpp-derived-token (kind text start end tags) #:transparent)

;; cpp-derived-token-has-tag? : cpp-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (cpp-derived-token-has-tag? token tag)
  (member tag (cpp-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; C++ classification tables

;; Reserved C++ keywords.
(define cpp-keywords
  (list->set
   '("alignas" "alignof" "asm" "auto" "bool" "break" "case" "catch"
     "char" "char8_t" "char16_t" "char32_t" "class" "compl" "concept"
     "const" "consteval" "constexpr" "const_cast" "continue" "co_await"
     "co_return" "co_yield" "decltype" "default" "delete" "do" "double"
     "dynamic_cast" "else" "enum" "explicit" "export" "extern" "false"
     "float" "for" "friend" "goto" "if" "inline" "int" "long" "mutable"
     "namespace" "new" "noexcept" "nullptr" "operator" "private"
     "protected" "public" "register" "reinterpret_cast" "requires"
     "return" "short" "signed" "sizeof" "static" "static_assert"
     "static_cast" "struct" "switch" "template" "this" "thread_local"
     "throw" "true" "try" "typedef" "typeid" "typename" "union"
     "unsigned" "using" "virtual" "void" "volatile" "wchar_t" "while")))

;; Identifier-like operator spellings.
(define cpp-operator-words
  (list->set
   '("and" "and_eq" "bitand" "bitor" "not" "not_eq"
     "or" "or_eq" "xor" "xor_eq")))

;; C++ delimiters.
(define cpp-delimiters
  (list->set
   '("#" "(" ")" "[" "]" "{" "}" "," ";" ":" "::" "?")))

;; C++ operators and punctuators in longest-match order.
(define cpp-symbol-tokens
  '("%:%:" "<=>" ">>=" "<<=" "..."
    "##" "->*" "->" ".*" "++" "--" "<<"
    ">>" "<=" ">=" "==" "!=" "&&" "||"
    "+=" "-=" "*=" "/=" "%=" "^=" "&="
    "|=" "::" "<:" ":>" "<%" "%>" "%:"
    "." "&" "*" "+" "-" "~" "!" "/" "%"
    "^" "|" "=" "<" ">" "#" "(" ")" "["
    "]" "{" "}" "," ";" ":" "?"))

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

;; make-token-from-text : position? position? string? (listof symbol?) -> cpp-derived-token?
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
  (cpp-derived-token kind
                     text
                     start-pos
                     end-pos
                     (remove-duplicates tags)))

;; -----------------------------------------------------------------------------
;; Character classes

;; inline-whitespace-char? : char? -> boolean?
;;   Recognize inline C++ whitespace other than newlines.
(define (inline-whitespace-char? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\page)))

;; newline-start? : (or/c char? eof-object?) -> boolean?
;;   Determine whether a character begins a physical newline.
(define (newline-start? ch)
  (or (char=? ch #\newline)
      (char=? ch #\return)))

;; identifier-start-char? : char? -> boolean?
;;   Recognize an identifier-start character.
(define (identifier-start-char? ch)
  (or (char-alphabetic? ch)
      (char=? ch #\_)))

;; identifier-char? : char? -> boolean?
;;   Recognize an identifier continuation character.
(define (identifier-char? ch)
  (or (identifier-start-char? ch)
      (char-numeric? ch)))

;; pp-number-char? : char? -> boolean?
;;   Recognize a broad preprocessing-number continuation character.
(define (pp-number-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (char=? ch #\_)
      (char=? ch #\.)
      (char=? ch #\+)
      (char=? ch #\-)
      (char=? ch #\')))

;; delimiter-token? : string? -> boolean?
;;   Determine whether a token spelling is delimiter-like.
(define (delimiter-token? text)
  (set-member? cpp-delimiters text))

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
;;   Consume one // comment without its terminating newline.
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
;;   Consume one /* */ comment and report whether it terminated.
(define (read-block-comment! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(char=? next #\*)
       (write-one! in out)
       (define maybe-close
         (peek-next in))
       (cond
         [(and (char? maybe-close)
               (char=? maybe-close #\/))
          (write-one! in out)
          #t]
         [else
          (loop)])]
      [else
       (write-one! in out)
       (loop)])))

;; read-identifier! : input-port? output-port? -> void?
;;   Consume one C++ identifier candidate.
(define (read-identifier! in out)
  (read-while! in out identifier-char?))

;; read-identifier-suffix! : input-port? output-port? -> void?
;;   Consume one identifier-like literal suffix when present.
(define (read-identifier-suffix! in out)
  (when (and (char? (peek-next in))
             (identifier-start-char? (peek-next in)))
    (read-while! in out identifier-char?)))

;; string-prefix-length : input-port? -> exact-nonnegative-integer?
;;   Determine the length of the current non-raw string-literal prefix.
(define (string-prefix-length in)
  (define first  (peek-next in))
  (define second (peek-next in 1))
  (define third  (peek-next in 2))
  (cond
    [(and (char? first)
          (char=? first #\u)
          (char? second)
          (char=? second #\8)
          (char? third)
          (char=? third #\"))
     2]
    [(and (char? first)
          (member first '(#\u #\U #\L))
          (char? second)
          (char=? second #\"))
     1]
    [(and (char? first)
          (char=? first #\"))
     0]
    [else
     0]))

;; raw-string-prefix-length : input-port? -> (or/c exact-nonnegative-integer? #f)
;;   Determine the length of the current raw string prefix, including R.
(define (raw-string-prefix-length in)
  (define first  (peek-next in))
  (define second (peek-next in 1))
  (define third  (peek-next in 2))
  (define fourth (peek-next in 3))
  (cond
    [(and (char? first)
          (char=? first #\u)
          (char? second)
          (char=? second #\8)
          (char? third)
          (char=? third #\R)
          (char? fourth)
          (char=? fourth #\"))
     3]
    [(and (char? first)
          (member first '(#\u #\U #\L))
          (char? second)
          (char=? second #\R)
          (char? third)
          (char=? third #\"))
     2]
    [(and (char? first)
          (char=? first #\R)
          (char? second)
          (char=? second #\"))
     1]
    [else
     #f]))

;; char-prefix-length : input-port? -> exact-nonnegative-integer?
;;   Determine the length of the current character-literal prefix.
(define (char-prefix-length in)
  (define first
    (peek-next in))
  (define second
    (peek-next in 1))
  (cond
    [(and (char? first)
          (member first '(#\u #\U #\L))
          (char? second)
          (char=? second #\'))
     1]
    [(and (char? first)
          (char=? first #\'))
     0]
    [else
     0]))

;; read-delimited-literal! : input-port? output-port? exact-nonnegative-integer? char? -> boolean?
;;   Consume one string-like literal with an optional prefix.
(define (read-delimited-literal! in out prefix-length delimiter)
  (for ([i (in-range prefix-length)])
    (write-one! in out))
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [else
       (define ch
         next)
       (cond
         [(char=? ch delimiter)
          (write-one! in out)
          #t]
         [(newline-start? ch)
          #f]
         [(char=? ch #\\)
          (write-one! in out)
          (define escaped
            (peek-next in))
          (cond
            [(eof-object? escaped)
             #f]
            [(newline-start? escaped)
             (read-newline! in out)
             (loop)]
            [else
             (write-one! in out)
             (loop)])]
         [else
          (write-one! in out)
          (loop)])])))

;; read-raw-string-literal! : input-port? output-port? exact-nonnegative-integer? -> boolean?
;;   Consume one raw string literal with an optional prefix.
(define (read-raw-string-literal! in out prefix-length)
  (for ([i (in-range prefix-length)])
    (write-one! in out))
  (write-one! in out)
  (define delimiter-out
    (open-output-string))
  (let read-delimiter ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(newline-start? next)
       #f]
      [(char=? next #\()
       (write-one! in out)
       (define delimiter-text
         (get-output-string delimiter-out))
       (let read-body ()
         (define body-next
           (peek-next in))
         (cond
           [(eof-object? body-next)
            #f]
           [(char=? body-next #\))
            (define maybe-close?
              (for/and ([i (in-range (string-length delimiter-text))])
                (define ch
                  (peek-next in (add1 i)))
                (and (char? ch)
                     (char=? ch (string-ref delimiter-text i)))))
            (define closing-quote
              (peek-next in (add1 (string-length delimiter-text))))
            (cond
              [(and maybe-close?
                    (char? closing-quote)
                    (char=? closing-quote #\"))
               (write-one! in out)
               (for ([i (in-range (string-length delimiter-text))])
                 (write-one! in out))
               (write-one! in out)
               #t]
              [else
               (write-one! in out)
               (read-body)])]
           [else
            (write-one! in out)
            (read-body)]))]
      [else
       (define ch
         next)
       (when (or (char-whitespace? ch)
                 (char=? ch #\\)
                 (char=? ch #\))
                 (char=? ch #\())
         (error 'read-raw-string-literal!
                "invalid raw string delimiter character: ~a"
                ch))
       (write-one! in out)
       (write-char ch delimiter-out)
       (read-delimiter)])))

;; read-angle-header! : input-port? output-port? -> boolean?
;;   Consume one <...> include header name.
(define (read-angle-header! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(newline-start? next)
       #f]
      [else
       (define ch
         next)
       (write-one! in out)
       (cond
         [(char=? ch #\>)
          #t]
         [else
          (loop)])])))

;; read-pp-number! : input-port? output-port? -> void?
;;   Consume one preprocessing-number candidate.
(define (read-pp-number! in out)
  (write-one! in out)
  (read-while! in out pp-number-char?))

;; symbol-token-at : input-port? -> (or/c string? #f)
;;   Find the longest symbolic C++ token at the current port position.
(define (symbol-token-at in)
  (for/or ([text (in-list cpp-symbol-tokens)])
    (define len
      (string-length text))
    (define maybe
      (for/and ([i (in-range len)])
        (define ch
          (peek-next in i))
        (and (char? ch)
             (char=? ch (string-ref text i)))))
    (and maybe text)))

;; -----------------------------------------------------------------------------
;; Reader

;; make-cpp-derived-reader : -> (input-port? -> (or/c cpp-derived-token? 'eof))
;;   Construct a stateful C++ derived-token reader.
(define (make-cpp-derived-reader)
  (define line-prefix-only?
    #t)
  (define in-preprocessor?
    #f)
  (define expect-directive-name?
    #f)
  (define expect-include-target?
    #f)
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-cpp-derived-reader "input-port?" in))
    (port-count-lines! in)
    (define start-pos
      (current-stream-position in))
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       'eof]
      [(inline-whitespace-char? next)
       (define out
         (open-output-string))
       (read-while! in out inline-whitespace-char?)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(whitespace cpp-whitespace))]
      [(newline-start? next)
       (define out
         (open-output-string))
       (read-newline! in out)
       (set! line-prefix-only? #t)
       (set! in-preprocessor? #f)
       (set! expect-directive-name? #f)
       (set! expect-include-target? #f)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(whitespace cpp-whitespace))]
      [(and (char=? next #\\)
            (newline-start? (peek-next in 1)))
       (define out
         (open-output-string))
       (write-one! in out)
       (read-newline! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(whitespace cpp-whitespace cpp-line-splice))]
      [(and (char=? next #\/)
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\/))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (read-line-comment! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(comment cpp-comment))]
      [(and (char=? next #\/)
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\*))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (define terminated?
         (read-block-comment! in out))
       (define text
         (get-output-string out))
       (when (regexp-match? #px"\r\n|\r|\n" text)
         (set! in-preprocessor? #f)
         (set! expect-directive-name? #f)
         (set! expect-include-target? #f))
       (make-token-from-text start-pos
                             (current-stream-position in)
                             text
                             (append '(comment cpp-comment)
                                     (if terminated?
                                         '()
                                         '(cpp-error malformed-token))))]
      [(and expect-include-target?
            (char=? next #\<))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (set! expect-include-target? #f)
       (define terminated?
         (read-angle-header! in out))
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             (append '(literal cpp-header-name)
                                     (if terminated?
                                         '()
                                         '(cpp-error malformed-token))))]
      [(raw-string-prefix-length in)
       => (lambda (prefix-length)
            (define out
              (open-output-string))
            (set! line-prefix-only? #f)
            (define terminated?
              (with-handlers ([exn:fail? (lambda (_) #f)])
                (read-raw-string-literal! in out prefix-length)))
            (set! expect-include-target? #f)
            (when terminated?
              (read-identifier-suffix! in out))
            (make-token-from-text start-pos
                                  (current-stream-position in)
                                  (get-output-string out)
                                  (append '(literal cpp-string-literal)
                                          (if terminated?
                                              '()
                                              '(cpp-error malformed-token)))))]
      [(or (positive? (string-prefix-length in))
           (char=? next #\"))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (define header-name?
         expect-include-target?)
       (define prefix-length
         (string-prefix-length in))
       (define terminated?
         (read-delimited-literal! in out prefix-length #\"))
       (set! expect-include-target? #f)
       (when (and terminated? (not header-name?))
         (read-identifier-suffix! in out))
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             (append (if header-name?
                                         '(literal cpp-header-name)
                                         '(literal cpp-string-literal))
                                     (if terminated?
                                         '()
                                         '(cpp-error malformed-token))))]
      [(or (positive? (char-prefix-length in))
           (char=? next #\'))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (set! expect-include-target? #f)
       (define terminated?
         (read-delimited-literal! in out (char-prefix-length in) #\'))
       (when terminated?
         (read-identifier-suffix! in out))
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             (append '(literal cpp-char-literal)
                                     (if terminated?
                                         '()
                                         '(cpp-error malformed-token))))]
      [(identifier-start-char? next)
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (read-identifier! in out)
       (define text
         (get-output-string out))
       (define directive-name?
         expect-directive-name?)
       (define include-name?
         (and directive-name?
              (string=? text "include")))
       (set! expect-directive-name? #f)
       (when expect-include-target?
         (set! expect-include-target? #f))
       (when include-name?
         (set! expect-include-target? #t))
       (make-token-from-text
        start-pos
        (current-stream-position in)
        text
        (cond
          [directive-name?
           '(keyword cpp-keyword cpp-preprocessor-directive)]
          [(set-member? cpp-operator-words text)
           '(operator cpp-operator)]
          [(or (string=? text "true")
               (string=? text "false")
               (string=? text "nullptr"))
           '(literal cpp-literal cpp-keyword)]
          [(set-member? cpp-keywords text)
           '(keyword cpp-keyword)]
          [else
           '(identifier cpp-identifier)]))]
      [(or (char-numeric? next)
           (and (char=? next #\.)
                (char? (peek-next in 1))
                (char-numeric? (peek-next in 1))))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (set! expect-include-target? #f)
       (read-pp-number! in out)
       (read-identifier-suffix! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(literal cpp-numeric-literal))]
      [else
       (define symbol-text
         (symbol-token-at in))
       (cond
         [symbol-text
          (define out
            (open-output-string))
          (for ([i (in-range (string-length symbol-text))])
            (write-one! in out))
          (define token-text
            (get-output-string out))
          (define pp-marker?
            (and line-prefix-only?
                 (not in-preprocessor?)
                 (string=? token-text "#")))
          (set! line-prefix-only? #f)
          (cond
            [pp-marker?
             (set! in-preprocessor? #t)
             (set! expect-directive-name? #t)
             (set! expect-include-target? #f)]
            [else
             (when expect-include-target?
               (set! expect-include-target? #f))])
          (make-token-from-text
           start-pos
           (current-stream-position in)
           token-text
           (cond
             [(delimiter-token? token-text)
              '(delimiter cpp-delimiter)]
             [else
              '(operator cpp-operator)]))]
         [else
          (define out
            (open-output-string))
          (write-one! in out)
          (set! line-prefix-only? #f)
          (set! expect-include-target? #f)
          (make-token-from-text start-pos
                                (current-stream-position in)
                                (get-output-string out)
                                '(cpp-error malformed-token))])])))
