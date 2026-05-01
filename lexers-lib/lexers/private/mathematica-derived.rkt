#lang racket/base

;;;
;;; Mathematica Derived Tokens
;;;
;;
;; Stateful Wolfram Language / Mathematica tokenization and reusable
;; classifications for a practical linear-syntax subset.

;; mathematica-derived-token?         : any/c -> boolean?
;;   Recognize a derived Mathematica token.
;; mathematica-derived-token-text     : mathematica-derived-token? -> string?
;;   Extract the source text for one derived token.
;; mathematica-derived-token-start    : mathematica-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; mathematica-derived-token-end      : mathematica-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; mathematica-derived-token-tags     : mathematica-derived-token? -> (listof symbol?)
;;   Extract reusable Mathematica classification tags.
;; mathematica-derived-token-has-tag? : mathematica-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-mathematica-derived-reader    : -> (input-port? -> (or/c mathematica-derived-token? 'eof))
;;   Construct a stateful Mathematica derived-token reader.

(provide mathematica-derived-token?
         mathematica-derived-token-text
         mathematica-derived-token-start
         mathematica-derived-token-end
         mathematica-derived-token-tags
         mathematica-derived-token-has-tag?
         make-mathematica-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         "parser-tools-compat.rkt")

;; A Mathematica token plus reusable tags.
(struct mathematica-derived-token (kind text start end tags) #:transparent)

;; mathematica-derived-token-has-tag? : mathematica-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (mathematica-derived-token-has-tag? token tag)
  (member tag (mathematica-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Classification tables

;; Mathematica delimiter sequences in longest-match order.
(define mathematica-delimiters
  '("[[" "]]" "<|" "|>"
    "(" ")" "[" "]" "{" "}" "," ";"))

;; Mathematica operator sequences in longest-match order.
(define mathematica-operators
  '("///." "//." "//" "/." "/@" "/:" "/*" "/;" ":>" ":=" "->" "|->" "<>" "==="
    "=!=" "==" "!=" "<=" ">=" "&&" "||" "@@@" "@@" "@*" ">>" "<<" "..." ".."
    "::" ";;" "++" "--" "~~>" "~~" "=." "=!" "^=" "+=" "-=" "*=" "/=" "~" "@"
    "^" "+" "-" "*" "/" "=" ":" "." "?" "!" "<" ">" "|" "&"))

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

;; write-count! : input-port? output-port? exact-nonnegative-integer? -> void?
;;   Consume a fixed number of characters and append them to the output accumulator.
(define (write-count! in out count)
  (for ([i (in-range count)])
    (write-one! in out)))

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

;; peek-string=? : input-port? string? -> boolean?
;;   Determine whether the upcoming input matches text.
(define (peek-string=? in text)
  (for/and ([ch (in-string text)]
            [i  (in-naturals)])
    (define next
      (peek-next in i))
    (and (char? next)
         (char=? next ch))))

;; make-token-from-text : position? position? string? (listof symbol?) -> mathematica-derived-token?
;;   Construct one derived token from explicit positions, text, and tags.
(define (make-token-from-text start-pos end-pos text tags)
  (define kind
    (cond
      [(member 'comment tags)         'comment]
      [(member 'whitespace tags)      'whitespace]
      [(member 'malformed-token tags) 'malformed]
      [(member 'literal tags)         'literal]
      [(member 'operator tags)        'operator]
      [(member 'delimiter tags)       'delimiter]
      [else                           'identifier]))
  (mathematica-derived-token kind
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

;; mathematica-inline-whitespace? : char? -> boolean?
;;   Recognize Mathematica whitespace other than newlines.
(define (mathematica-inline-whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\page)
      (char=? ch #\vtab)))

;; symbol-start? : char? -> boolean?
;;   Recognize a symbol-start character for the first Mathematica slice.
(define (symbol-start? ch)
  (or (char-alphabetic? ch)
      (char=? ch #\$)))

;; symbol-char? : char? -> boolean?
;;   Recognize an ordinary symbol continuation character.
(define (symbol-char? ch)
  (or (symbol-start? ch)
      (char-numeric? ch)))

;; decimal-digit? : char? -> boolean?
;;   Recognize one decimal digit.
(define (decimal-digit? ch)
  (char-numeric? ch))

;; radix-digit? : char? -> boolean?
;;   Recognize one alphanumeric digit accepted in base forms.
(define (radix-digit? ch)
  (or (char-numeric? ch)
      (char-alphabetic? ch)))

;; hex-digit? : char? -> boolean?
;;   Recognize one hexadecimal digit.
(define (hex-digit? ch)
  (or (char-numeric? ch)
      (member ch
              '(#\a #\b #\c #\d #\e #\f
                #\A #\B #\C #\D #\E #\F))))

;; valid-long-name-char? : char? -> boolean?
;;   Recognize a valid character in one \[Name] long-name body.
(define (valid-long-name-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (char=? ch #\$)))

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
;;   Consume one run of Mathematica whitespace.
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
      [(mathematica-inline-whitespace? next)
       (write-one! in out)
       (loop)]
      [else
       (void)])))

;; read-comment! : input-port? output-port? -> boolean?
;;   Consume one nested Mathematica comment and report whether it terminated.
(define (read-comment! in out)
  (write-count! in out 2)
  (let loop ([depth 1])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(peek-string=? in "(*")
       (write-count! in out 2)
       (loop (add1 depth))]
      [(peek-string=? in "*)")
       (write-count! in out 2)
       (define new-depth
         (sub1 depth))
       (cond
         [(zero? new-depth) #t]
         [else              (loop new-depth)])]
      [else
       (write-one! in out)
       (loop depth)])))

;; read-line-comment! : input-port? output-port? -> void?
;;   Consume one line-style comment without its terminating newline.
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

;; read-string-literal! : input-port? output-port? -> boolean?
;;   Consume one Mathematica string literal and report whether it terminated.
(define (read-string-literal! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(char=? next #\\)
       (write-one! in out)
       (define escaped
         (peek-next in))
       (cond
         [(eof-object? escaped)
          #f]
         [else
          (write-one! in out)
          (loop)])]
      [(char=? next #\")
       (write-one! in out)
       #t]
      [else
       (write-one! in out)
       (loop)])))

;; read-long-name! : input-port? output-port? -> boolean?
;;   Consume one \[Name] long-name form and report whether it terminated cleanly.
(define (read-long-name! in out)
  (write-count! in out 2)
  (let loop ([saw-body? #f])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [(char=? next #\])
       (write-one! in out)
       saw-body?]
      [(valid-long-name-char? next)
       (write-one! in out)
       (loop #t)]
      [else
       #f])))

;; peek-character-escape-start? : input-port? -> boolean?
;;   Determine whether the upcoming input begins a simple Wolfram character escape.
(define (peek-character-escape-start? in)
  (or (peek-string=? in "\\.")
      (peek-string=? in "\\:")
      (peek-string=? in "\\|")))

;; read-fixed-hex-escape! : input-port? output-port? exact-positive-integer? -> boolean?
;;   Consume one fixed-width hexadecimal escape and report whether it was complete.
(define (read-fixed-hex-escape! in out digit-count)
  (write-count! in out 2)
  (let loop ([remaining digit-count])
    (cond
      [(zero? remaining)
       #t]
      [else
       (define next
         (peek-next in))
       (cond
         [(and (char? next) (hex-digit? next))
          (write-one! in out)
          (loop (sub1 remaining))]
         [else
          #f])])))

;; read-character-escape! : input-port? output-port? -> boolean?
;;   Consume one simple Wolfram hexadecimal character escape.
(define (read-character-escape! in out)
  (cond
    [(peek-string=? in "\\.")
     (read-fixed-hex-escape! in out 2)]
    [(peek-string=? in "\\:")
     (read-fixed-hex-escape! in out 4)]
    [(peek-string=? in "\\|")
     (read-fixed-hex-escape! in out 6)]
    [else
     #f]))

;; read-symbol! : input-port? output-port? -> void?
;;   Consume one Mathematica symbol-like token, including contexts and long names.
(define (read-symbol! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       (void)]
      [(and (char? next) (symbol-char? next))
       (write-one! in out)
       (loop)]
      [(and (char? next) (char=? next #\`))
       (write-one! in out)
       (loop)]
      [(peek-string=? in "\\[")
       (read-long-name! in out)
       (loop)]
      [(peek-character-escape-start? in)
       (read-character-escape! in out)
       (loop)]
      [else
       (void)])))

;; read-pattern! : input-port? output-port? -> void?
;;   Consume one Mathematica pattern marker token.
(define (read-pattern! in out)
  (write-one! in out)
  (when (and (char? (peek-next in))
             (char=? (peek-next in) #\_))
    (write-one! in out)
    (when (and (char? (peek-next in))
               (char=? (peek-next in) #\_))
      (write-one! in out)))
  (when (and (char? (peek-next in))
             (char=? (peek-next in) #\.))
    (write-one! in out)))

;; read-slot! : input-port? output-port? -> void?
;;   Consume one Mathematica slot token.
(define (read-slot! in out)
  (write-one! in out)
  (when (and (char? (peek-next in))
             (char=? (peek-next in) #\#))
    (write-one! in out))
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       (void)]
      [(and (char? next)
            (or (symbol-char? next)
                (char-numeric? next)))
       (write-one! in out)
       (loop)]
      [else
       (void)])))

;; read-digit-sequence! : input-port? output-port? (char? -> boolean?) -> void?
;;   Consume one digit sequence.
(define (read-digit-sequence! in out digit?)
  (read-while! in out digit?))

;; read-optional-signed-decimal! : input-port? output-port? -> void?
;;   Consume an optional signed decimal fragment.
(define (read-optional-signed-decimal! in out)
  (when (and (char? (peek-next in))
             (or (char=? (peek-next in) #\+)
                 (char=? (peek-next in) #\-)))
    (write-one! in out))
  (read-digit-sequence! in out decimal-digit?))

;; read-number-precision! : input-port? output-port? -> void?
;;   Consume Mathematica precision / accuracy suffixes using backquotes.
(define (read-number-precision! in out)
  (cond
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\`))
     (write-one! in out)
     (cond
       [(and (char? (peek-next in))
             (char=? (peek-next in) #\`))
        (write-one! in out)
        (read-optional-signed-decimal! in out)]
       [else
        (read-optional-signed-decimal! in out)
        (when (and (char? (peek-next in))
                   (char=? (peek-next in) #\`))
          (write-one! in out)
          (read-optional-signed-decimal! in out))])]
    [else
     (void)]))

;; read-number-exponent! : input-port? output-port? -> void?
;;   Consume one Mathematica exponent fragment.
(define (read-number-exponent! in out)
  (cond
    [(peek-string=? in "*^")
     (write-count! in out 2)
     (read-optional-signed-decimal! in out)]
    [(and (char? (peek-next in))
          (member (peek-next in) '(#\e #\E)))
     (write-one! in out)
     (read-optional-signed-decimal! in out)]
    [else
     (void)]))

;; read-number! : input-port? output-port? -> void?
;;   Consume one practical Mathematica numeric literal.
(define (read-number! in out)
  (cond
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\.))
     (write-one! in out)
     (read-digit-sequence! in out decimal-digit?)
     (read-number-precision! in out)
     (read-number-exponent! in out)]
    [else
     (read-digit-sequence! in out decimal-digit?)
     (cond
       [(peek-string=? in "^^")
        (write-count! in out 2)
        (read-digit-sequence! in out radix-digit?)
        (when (and (char? (peek-next in))
                   (char=? (peek-next in) #\.))
          (write-one! in out)
          (read-digit-sequence! in out radix-digit?))
        (read-number-precision! in out)
        (read-number-exponent! in out)]
       [else
        (when (and (char? (peek-next in))
                   (char=? (peek-next in) #\.)
                   (char? (peek-next in 1))
                   (decimal-digit? (peek-next in 1)))
          (write-one! in out)
          (read-digit-sequence! in out decimal-digit?))
        (read-number-precision! in out)
        (read-number-exponent! in out)])]))

;; longest-matching-string : input-port? (listof string?) -> (or/c string? #f)
;;   Find the longest candidate that matches the upcoming input.
(define (longest-matching-string in candidates)
  (for/first ([candidate (in-list candidates)]
              #:when (peek-string=? in candidate))
    candidate))

;; -----------------------------------------------------------------------------
;; Tag helpers

;; delimiter-tags : string? -> (listof symbol?)
;;   Compute reusable tags for one Mathematica delimiter token.
(define (delimiter-tags text)
  (append
   '(delimiter mathematica-delimiter)
   (cond
     [(member text '("(" ")" "[" "]" "{" "}"))
      '(mathematica-group-delimiter)]
     [(member text '("[[" "]]"))
      '(mathematica-part-delimiter)]
     [(member text '("<|" "|>"))
      '(mathematica-association-delimiter)]
     [else
      '()])))

;; classify-symbol-by-text : string? -> (listof symbol?)
;;   Add reusable tags for common Mathematica symbol roles.
(define (classify-symbol-by-text text)
  (append
   (cond
     [(member text '("BeginPackage" "Begin" "EndPackage" "End"))
      '(mathematica-package-form)]
     [else
      '()])
   (cond
     [(member text '("Module" "Block" "With" "Function"))
      '(mathematica-scoping-form)]
     [else
      '()])
   (cond
     [(member text '("Set" "SetDelayed" "TagSet" "TagSetDelayed"))
     '(mathematica-definition-form)]
     [else
      '()])))

;; long-name-tags : string? -> (listof symbol?)
;;   Compute reusable tags for a Wolfram named-character form.
(define (long-name-tags text)
  (append
   '(identifier mathematica-long-name mathematica-named-character)
   (cond
     [(regexp-match? #px"\\\\\\[[A-Za-z$][A-Za-z0-9$]*\\]$" text)
      '()]
     [else
      '(mathematica-error malformed-token)])))

;; character-escape-tags : string? -> (listof symbol?)
;;   Compute reusable tags for a hexadecimal character escape.
(define (character-escape-tags text)
  (append
   '(identifier mathematica-character-escape mathematica-hex-escape)
   (cond
     [(or (regexp-match? #px"^\\\\\\.[0-9A-Fa-f]{2}$" text)
          (regexp-match? #px"^\\\\:[0-9A-Fa-f]{4}$" text)
          (regexp-match? #px"^\\\\\\|[0-9A-Fa-f]{6}$" text))
      '()]
     [else
      '(mathematica-error malformed-token)])))

;; operator-tags : string? -> (listof symbol?)
;;   Compute reusable tags for one Mathematica operator-like token.
(define (operator-tags text)
  (append
   '(operator mathematica-operator)
   (cond
     [(regexp-match? #px"^_{1,3}[.]?$" text)
      '(mathematica-pattern)]
     [(regexp-match? #px"^##?[A-Za-z0-9$]*$" text)
     '(mathematica-slot)]
     [(member text '("=" ":=" "=."))
      '(mathematica-assignment-operator)]
     [(member text '("->" ":>" "/." "//."))
      '(mathematica-rewrite-operator)]
     [(member text '("@*" "/*"))
      '(mathematica-composition-operator)]
     [(member text '("~~" "~~>"))
      '(mathematica-string-pattern-operator)]
     [(string=? text "|->")
      '(mathematica-function-arrow-operator)]
     [(string=? text "/;")
      '(mathematica-pattern-condition-operator)]
     [else
      '()])))

;; symbol-tags : string? -> (listof symbol?)
;;   Compute reusable tags for one Mathematica symbol token.
(define (symbol-tags text)
  (append
   '(identifier mathematica-symbol)
   (classify-symbol-by-text text)
   (cond
     [(regexp-match? #px"`" text)
     '(mathematica-context)]
     [else
      '()])))

;; number-tags : string? -> (listof symbol?)
;;   Compute reusable tags for one Mathematica numeric literal.
(define (number-tags text)
  (append
   '(literal mathematica-number)
   (cond
     [(regexp-match? #px"\\^\\^" text)
      '(mathematica-base-number)]
     [else
      '()])
   (cond
     [(regexp-match? #px"`" text)
      '(mathematica-precision-number)]
     [else
      '()])
   (cond
     [(regexp-match? #px"(\\*\\^|[eE][+-]?[0-9])" text)
      '(mathematica-exponent-number)]
     [else
      '()])
   (cond
     [(and (not (regexp-match? #px"[.`]" text))
           (not (regexp-match? #px"(\\*\\^|[eE][+-]?[0-9])" text)))
      '(mathematica-integer-number)]
     [else
      '(mathematica-real-number)])))

;; -----------------------------------------------------------------------------
;; Reader

;; make-mathematica-derived-reader : -> (input-port? -> (or/c mathematica-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived Mathematica tokens.
(define (make-mathematica-derived-reader)
  (define at-beginning?
    #t)
  (lambda (in)
    (define start-pos
      (current-stream-position in))
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       'eof]
      [else
       (define out
         (open-output-string))
       (define tags
         (cond
           [(and at-beginning?
                 (peek-string=? in "#!"))
            (read-line-comment! in out)
            '(comment mathematica-comment mathematica-shebang-comment)]
           [(or (newline-start? next)
                (and (char? next)
                     (mathematica-inline-whitespace? next)))
            (read-whitespace! in out)
            '(whitespace mathematica-whitespace)]
           [(peek-string=? in "(*")
            (define terminated?
              (read-comment! in out))
            (cond
              [terminated?
               '(comment mathematica-comment)]
              [else
               '(comment mathematica-comment mathematica-error malformed-token)])]
           [(and (char? next) (char=? next #\"))
            (define terminated?
              (read-string-literal! in out))
            (cond
              [terminated?
               '(literal mathematica-string-literal)]
              [else
               '(literal mathematica-string-literal mathematica-error malformed-token)])]
           [(peek-string=? in "\\[")
           (define terminated?
              (read-long-name! in out))
            (define text
              (get-output-string out))
            (cond
              [terminated?
               (long-name-tags text)]
              [else
               '(identifier mathematica-long-name mathematica-named-character
                            mathematica-error malformed-token)])]
           [(peek-character-escape-start? in)
            (define complete?
              (read-character-escape! in out))
            (define text
              (get-output-string out))
            (cond
              [complete?
               (character-escape-tags text)]
              [else
               '(identifier mathematica-character-escape mathematica-hex-escape
                            mathematica-error malformed-token)])]
           [(and (char? next) (symbol-start? next))
            (read-symbol! in out)
            (symbol-tags (get-output-string out))]
           [(and (char? next)
                 (char=? next #\_))
            (read-pattern! in out)
            (operator-tags (get-output-string out))]
           [(and (char? next)
                 (char=? next #\#))
            (read-slot! in out)
            (operator-tags (get-output-string out))]
           [(or (and (char? next)
                     (decimal-digit? next))
                (and (char? next)
                     (char=? next #\.)
                     (char? (peek-next in 1))
                     (decimal-digit? (peek-next in 1))))
            (read-number! in out)
            (number-tags (get-output-string out))]
           [else
            (define delimiter
              (longest-matching-string in mathematica-delimiters))
            (cond
              [delimiter
               (write-count! in out (string-length delimiter))
               (delimiter-tags delimiter)]
              [else
               (define operator
                 (longest-matching-string in mathematica-operators))
               (cond
                 [operator
                  (write-count! in out (string-length operator))
                  (operator-tags operator)]
                 [else
                 (write-one! in out)
                  '(mathematica-error malformed-token)])])]))
       (define end-pos
         (current-stream-position in))
       (set! at-beginning? #f)
       (make-token-from-text start-pos
                             end-pos
                             (get-output-string out)
                             tags)])))
