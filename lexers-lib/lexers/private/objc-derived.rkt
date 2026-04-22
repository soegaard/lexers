#lang racket/base

;;;
;;; Objective-C Derived Tokens
;;;
;;
;; Stateful Objective-C tokenization and reusable Objective-C-specific
;; classifications.

;; objc-derived-token?         : any/c -> boolean?
;;   Recognize a derived Objective-C token.
;; objc-derived-token-text     : objc-derived-token? -> string?
;;   Extract the source text for one derived token.
;; objc-derived-token-start    : objc-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; objc-derived-token-end      : objc-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; objc-derived-token-tags     : objc-derived-token? -> (listof symbol?)
;;   Extract reusable Objective-C classification tags.
;; objc-derived-token-has-tag? : objc-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-objc-derived-reader    : -> (input-port? -> (or/c objc-derived-token? 'eof))
;;   Construct a stateful Objective-C derived-token reader.

(provide objc-derived-token?
         objc-derived-token-text
         objc-derived-token-start
         objc-derived-token-end
         objc-derived-token-tags
         objc-derived-token-has-tag?
         make-objc-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         racket/string
         "parser-tools-compat.rkt")

;; An Objective-C token plus reusable tags.
(struct objc-derived-token (kind text start end tags) #:transparent)

;; objc-derived-token-has-tag? : objc-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (objc-derived-token-has-tag? token tag)
  (member tag (objc-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Objective-C classification tables

;; Reserved C / Objective-C keywords.
(define objc-keywords
  (list->set
   '("auto" "break" "case" "char" "const" "continue" "default" "do"
     "double" "else" "enum" "extern" "float" "for" "goto" "if"
     "inline" "int" "long" "register" "restrict" "return" "short"
     "signed" "sizeof" "static" "struct" "switch" "typedef" "union"
     "unsigned" "void" "volatile" "while" "BOOL" "Class" "id" "IMP"
     "nil" "Nil" "SEL" "self" "super")))

;; Objective-C @-keywords and directives.
(define objc-at-keywords
  (list->set
   '("autoreleasepool" "catch" "class" "compatibility_alias" "defs"
     "dynamic" "encode" "end" "finally" "implementation" "import"
     "interface" "optional" "package" "private" "property" "protected"
     "protocol" "public" "required" "selector" "synchronized" "synthesize"
     "throw" "try" "YES" "NO")))

;; Objective-C delimiters.
(define objc-delimiters
  (list->set
   '("#" "(" ")" "[" "]" "{" "}" "," ";" ":" "?" "@[" "@{" "@(")))

;; Objective-C operators and punctuators in longest-match order.
(define objc-symbol-tokens
  '(">>=" "<<=" "..."
    "##" "->" "++" "--" "<<"
    ">>" "<=" ">=" "==" "!="
    "&&" "||" "*=" "/=" "%="
    "+=" "-=" "&=" "^=" "|="
    "@[" "@{" "@("
    "." "&" "*" "+" "-" "~"
    "!" "/" "%" "<" ">" "^"
    "|" "=" "#" "(" ")" "["
    "]" "{" "}" "," ";" ":"
    "?"))

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

;; make-token-from-text : position? position? string? (listof symbol?) -> objc-derived-token?
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
  (objc-derived-token kind
                      text
                      start-pos
                      end-pos
                      (remove-duplicates tags)))

;; -----------------------------------------------------------------------------
;; Character classes

;; inline-whitespace-char? : char? -> boolean?
;;   Recognize inline Objective-C whitespace other than newlines.
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
      (char=? ch #\-)))

;; delimiter-token? : string? -> boolean?
;;   Determine whether a token spelling is delimiter-like.
(define (delimiter-token? text)
  (set-member? objc-delimiters text))

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
;;   Consume one Objective-C identifier candidate.
(define (read-identifier! in out)
  (read-while! in out identifier-char?))

;; string-prefix-length : input-port? -> exact-nonnegative-integer?
;;   Determine the length of the current string-literal prefix.
(define (string-prefix-length in)
  (define first
    (peek-next in))
  (cond
    [(and (char? first)
          (char=? first #\"))
     0]
    [else
     0]))

;; char-prefix-length : input-port? -> exact-nonnegative-integer?
;;   Determine the length of the current character-literal prefix.
(define (char-prefix-length in)
  (define first
    (peek-next in))
  (cond
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

;; -----------------------------------------------------------------------------
;; Literal validation

;; oct-digit? : char? -> boolean?
;;   Determine whether one character is an octal digit.
(define (oct-digit? ch)
  (and (char? ch)
       (char<=? #\0 ch #\7)))

;; hex-digit? : char? -> boolean?
;;   Determine whether one character is a hexadecimal digit.
(define (hex-digit? ch)
  (and (char? ch)
       (or (char-numeric? ch)
           (char<=? #\a (char-downcase ch) #\f))))

;; valid-objc-simple-escape? : char? -> boolean?
;;   Determine whether one character completes a simple Objective-C escape.
(define (valid-objc-simple-escape? ch)
  (member ch (list #\' #\" #\? #\\ #\a #\b #\f #\n #\r #\t #\v)))

;; quoted-literal-content : string? exact-nonnegative-integer? -> string?
;;   Extract one quoted literal body by trimming prefix and delimiters.
(define (quoted-literal-content text prefix-length)
  (substring text
             (+ prefix-length 1)
             (sub1 (string-length text))))

;; valid-unicode-escape-end : string? exact-nonnegative-integer? -> (or/c exact-nonnegative-integer? #f)
;;   Return the index after one valid universal-character escape, if present.
(define (valid-unicode-escape-end content start)
  (define len
    (string-length content))
  (cond
    [(or (>= (add1 start) len)
         (not (member (string-ref content start) (list #\u #\U))))
     #f]
    [else
     (define digits
       (if (char=? (string-ref content start) #\u) 4 8))
     (define end
       (+ start 1 digits))
     (and (<= end len)
          (for/and ([i (in-range (add1 start) end)])
            (hex-digit? (string-ref content i)))
          end)]))

;; valid-objc-escape-end : string? exact-nonnegative-integer? -> (or/c exact-nonnegative-integer? #f)
;;   Return the index after one valid non-raw Objective-C escape, if present.
(define (valid-objc-escape-end content start)
  (define len
    (string-length content))
  (cond
    [(>= (add1 start) len)
     #f]
    [else
     (define next
       (string-ref content (add1 start)))
     (cond
       [(valid-objc-simple-escape? next)
        (+ start 2)]
       [(char=? next #\x)
        (let loop ([i (+ start 2)]
                   [saw-hex? #f])
          (cond
            [(>= i len)
             (and saw-hex? i)]
            [(hex-digit? (string-ref content i))
             (loop (add1 i) #t)]
            [else
             (and saw-hex? i)]))]
       [(oct-digit? next)
        (let loop ([i (+ start 1)]
                   [count 0])
          (cond
            [(and (< i len)
                  (< count 3)
                  (oct-digit? (string-ref content i)))
             (loop (add1 i) (add1 count))]
            [else
             (and (positive? count) i)]))]
       [(member next (list #\u #\U))
        (valid-unicode-escape-end content (add1 start))]
       [else
        #f])]))

;; valid-objc-literal-content? : string? boolean? -> boolean?
;;   Determine whether one ordinary Objective-C string/char literal body is valid.
(define (valid-objc-literal-content? content char?)
  (define len
    (string-length content))
  (let loop ([i 0]
             [units 0])
    (cond
      [(= i len)
       (or (not char?)
           (= units 1))]
      [else
       (define ch
         (string-ref content i))
       (cond
         [(char=? ch #\\)
          (define end
            (valid-objc-escape-end content i))
          (and end
               (loop end (add1 units)))]
         [(newline-start? ch)
          #f]
         [else
          (loop (add1 i) (add1 units))])])))

;; valid-objc-delimited-literal? : string? exact-nonnegative-integer? boolean? -> boolean?
;;   Determine whether one non-raw string/char literal has valid content.
(define (valid-objc-delimited-literal? text prefix-length char?)
  (valid-objc-literal-content? (quoted-literal-content text prefix-length) char?))

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
;;   Find the longest symbolic Objective-C token at the current port position.
(define (symbol-token-at in)
  (for/or ([text (in-list objc-symbol-tokens)])
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

;; make-objc-derived-reader : -> (input-port? -> (or/c objc-derived-token? 'eof))
;;   Construct a stateful Objective-C derived-token reader.
(define (make-objc-derived-reader)
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
      (raise-argument-error 'make-objc-derived-reader "input-port?" in))
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
                             '(whitespace objc-whitespace))]
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
                             '(whitespace objc-whitespace))]
      [(and (char=? next #\\)
            (newline-start? (peek-next in 1)))
       (define out
         (open-output-string))
       (write-one! in out)
       (read-newline! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(whitespace objc-whitespace objc-line-splice))]
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
                             '(comment objc-comment))]
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
                             (append '(comment objc-comment)
                                     (if terminated?
                                         '()
                                         '(objc-error malformed-token))))]
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
                             (append '(literal objc-header-name)
                                     (if terminated?
                                         '()
                                         '(objc-error malformed-token))))]
      [(and (char=? next #\@)
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\"))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (set! expect-include-target? #f)
       (write-one! in out)
       (define terminated?
         (read-delimited-literal! in out 0 #\"))
       (define text
         (get-output-string out))
       (make-token-from-text start-pos
                             (current-stream-position in)
                             text
                             (append '(literal objc-string-literal)
                                     (if (and terminated?
                                              (valid-objc-delimited-literal? text 1 #f))
                                         '()
                                         '(objc-error malformed-token))))]
      [(and (char=? next #\@)
            (char? (peek-next in 1))
            (or (char=? (peek-next in 1) #\[)
                (char=? (peek-next in 1) #\{)
                (char=? (peek-next in 1) #\()))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (set! expect-include-target? #f)
       (write-one! in out)
       (write-one! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(delimiter objc-delimiter objc-literal-introducer))]
      [(and (char=? next #\@)
            (char? (peek-next in 1))
            (identifier-start-char? (peek-next in 1)))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (set! expect-include-target? #f)
       (write-one! in out)
       (read-identifier! in out)
       (define text
         (get-output-string out))
       (define at-name
         (substring text 1))
       (make-token-from-text
        start-pos
        (current-stream-position in)
        text
        (cond
          [(set-member? objc-at-keywords at-name)
           '(keyword objc-keyword objc-at-keyword)]
          [else
           '(identifier objc-identifier)]))]
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
       (define text
         (get-output-string out))
       (set! expect-include-target? #f)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             text
                             (append (if header-name?
                                         '(literal objc-header-name)
                                         '(literal objc-string-literal))
                                     (if (and terminated?
                                              (or header-name?
                                                  (valid-objc-delimited-literal? text prefix-length #f)))
                                         '()
                                         '(objc-error malformed-token))))]
      [(or (positive? (char-prefix-length in))
           (char=? next #\'))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (set! expect-include-target? #f)
       (define prefix-length
         (char-prefix-length in))
       (define terminated?
         (read-delimited-literal! in out prefix-length #\'))
       (define text
         (get-output-string out))
       (make-token-from-text start-pos
                             (current-stream-position in)
                             text
                             (append '(literal objc-char-literal)
                                     (if (and terminated?
                                              (valid-objc-delimited-literal? text prefix-length #t))
                                         '()
                                         '(objc-error malformed-token))))]
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
              (or (string=? text "include")
                  (string=? text "import"))))
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
           '(keyword objc-keyword objc-preprocessor-directive)]
          [(set-member? objc-keywords text)
           '(keyword objc-keyword)]
          [else
           '(identifier objc-identifier)]))]
      [(or (char-numeric? next)
           (and (char=? next #\.)
                (char? (peek-next in 1))
                (char-numeric? (peek-next in 1))))
       (define out
         (open-output-string))
       (set! line-prefix-only? #f)
       (set! expect-include-target? #f)
       (read-pp-number! in out)
       (make-token-from-text start-pos
                             (current-stream-position in)
                             (get-output-string out)
                             '(literal objc-numeric-literal))]
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
              '(delimiter objc-delimiter)]
             [else
              '(operator objc-operator)]))]
         [else
          (define out
            (open-output-string))
          (write-one! in out)
          (set! line-prefix-only? #f)
          (set! expect-include-target? #f)
          (make-token-from-text start-pos
                                (current-stream-position in)
                                (get-output-string out)
                                '(objc-error malformed-token))])])))
