#lang racket/base

;;;
;;; Makefile Derived Tokens
;;;
;;
;; Stateful Makefile tokenization and reusable Makefile-specific
;; classifications.

;; makefile-derived-token?         : any/c -> boolean?
;;   Recognize a derived Makefile token.
;; makefile-derived-token-text     : makefile-derived-token? -> string?
;;   Extract the source text for one derived token.
;; makefile-derived-token-start    : makefile-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; makefile-derived-token-end      : makefile-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; makefile-derived-token-tags     : makefile-derived-token? -> (listof symbol?)
;;   Extract reusable Makefile classification tags.
;; makefile-derived-token-has-tag? : makefile-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-makefile-derived-reader    : -> (input-port? -> (or/c makefile-derived-token? 'eof))
;;   Construct a stateful Makefile derived-token reader.

(provide makefile-derived-token?
         makefile-derived-token-text
         makefile-derived-token-start
         makefile-derived-token-end
         makefile-derived-token-tags
         makefile-derived-token-has-tag?
         make-makefile-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         racket/string
         "../shell.rkt"
         "parser-tools-compat.rkt")

;; A Makefile token plus reusable tags.
(struct makefile-derived-token (kind text start end tags) #:transparent)

;; makefile-derived-token-has-tag? : makefile-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (makefile-derived-token-has-tag? token tag)
  (member tag (makefile-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Makefile classification tables

;; GNU make directive keywords recognized by the first lexer slice.
(define makefile-directives
  (list->set
   '("include" "-include" "sinclude"
     "define" "endef" "undefine"
     "ifdef" "ifndef" "ifeq" "ifneq" "else" "endif"
     "override" "export" "unexport" "private" "vpath")))

;; Assignment operators in longest-match order.
(define makefile-assignment-operators
  '(":::=" "::=" ":=" "+=" "?=" "!=" "="))

;; Delimiters in longest-match order.
(define makefile-delimiters
  '("::" ":" ";" "|" "(" ")" "{" "}" ","))

;; -----------------------------------------------------------------------------
;; Port and position helpers

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

;; advance-position : position? string? -> position?
;;   Advance one parser-tools-compatible position across text.
(define (advance-position start text)
  (let loop ([index 0]
             [line (position-line start)]
             [col  (position-col start)]
             [off  (position-offset start)])
    (cond
      [(>= index (string-length text))
       (make-stream-position off line col)]
      [else
       (define ch
         (string-ref text index))
       (cond
         [(char=? ch #\return)
          (cond
            [(and (< (add1 index) (string-length text))
                  (char=? (string-ref text (add1 index)) #\newline))
             (loop (+ index 2)
                   (add1 line)
                   0
                   (+ off 2))]
            [else
             (loop (add1 index)
                   (add1 line)
                   0
                   (add1 off))])]
         [(char=? ch #\newline)
          (loop (add1 index)
                (add1 line)
                0
                (add1 off))]
         [else
          (loop (add1 index)
                line
                (add1 col)
                (add1 off))])])))

;; make-token-from-text : position? string? (listof symbol?) -> makefile-derived-token?
;;   Construct one derived token from a start position, text, and tags.
(define (make-token-from-text start text tags)
  (define end
    (advance-position start text))
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
  (makefile-derived-token kind
                          text
                          start
                          end
                          (remove-duplicates tags)))

;; read-physical-line : input-port? -> (or/c string? eof-object?)
;;   Read one physical line while preserving newline spelling.
(define (read-physical-line in)
  (define first
    (peek-char in))
  (cond
    [(eof-object? first)
     eof]
    [else
     (define out
       (open-output-string))
     (let loop ()
       (define ch
         (read-char in))
       (cond
         [(eof-object? ch)
          (get-output-string out)]
         [else
          (write-char ch out)
          (cond
            [(char=? ch #\return)
             (when (and (char? (peek-char in))
                        (char=? (peek-char in) #\newline))
               (write-char (read-char in) out))
             (get-output-string out)]
            [(char=? ch #\newline)
             (get-output-string out)]
            [else
             (loop)])]))]))

;; -----------------------------------------------------------------------------
;; Local scanning helpers

;; string-prefix-at? : string? exact-nonnegative-integer? string? -> boolean?
;;   Determine whether source starts with prefix at index.
(define (string-prefix-at? source index prefix)
  (define end
    (+ index (string-length prefix)))
  (and (<= end (string-length source))
       (string=? (substring source index end) prefix)))

;; inline-whitespace-char? : char? -> boolean?
;;   Recognize inline Makefile whitespace.
(define (inline-whitespace-char? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\page)))

;; newline-char? : char? -> boolean?
;;   Recognize a physical newline starter.
(define (newline-char? ch)
  (or (char=? ch #\newline)
      (char=? ch #\return)))

;; word-char? : char? -> boolean?
;;   Recognize one identifier-like Makefile character.
(define (word-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (member ch
              '(#\_ #\- #\. #\/ #\% #\+ #\@ #\< #\^ #\? #\*))))

;; escaped-comment-char? : string? exact-nonnegative-integer? -> boolean?
;;   Determine whether a # at index is escaped by an odd backslash run.
(define (escaped-comment-char? source index)
  (let loop ([i (sub1 index)]
             [count 0])
    (cond
      [(negative? i)
       (odd? count)]
      [(char=? (string-ref source i) #\\)
       (loop (sub1 i) (add1 count))]
      [else
       (odd? count)])))

;; take-while-end : string? exact-nonnegative-integer? (char? -> boolean?) -> exact-nonnegative-integer?
;;   Find the end index of the longest matching character run.
(define (take-while-end source index pred?)
  (let loop ([i index])
    (cond
      [(>= i (string-length source))
       i]
      [(pred? (string-ref source i))
       (loop (add1 i))]
      [else
       i])))

;; first-nonspace-index : string? -> exact-nonnegative-integer?
;;   Find the index of the first non-inline-whitespace character.
(define (first-nonspace-index source)
  (let loop ([i 0])
    (cond
      [(>= i (string-length source))
       i]
      [(inline-whitespace-char? (string-ref source i))
       (loop (add1 i))]
      [else
       i])))

;; strip-line-ending : string? -> string?
;;   Remove one trailing physical newline sequence from a line.
(define (strip-line-ending line)
  (define len
    (string-length line))
  (cond
    [(and (>= len 2)
          (char=? (string-ref line (- len 2)) #\return)
          (char=? (string-ref line (- len 1)) #\newline))
     (substring line 0 (- len 2))]
    [(and (>= len 1)
          (or (char=? (string-ref line (sub1 len)) #\newline)
              (char=? (string-ref line (sub1 len)) #\return)))
     (substring line 0 (sub1 len))]
    [else
     line]))

;; directive-word-and-end : string? -> (or/c (cons/c string? exact-nonnegative-integer?) #f)
;;   Extract a leading directive word when one is present.
(define (directive-word-and-end line)
  (define start
    (first-nonspace-index line))
  (define end
    (take-while-end line start
                    (lambda (ch)
                      (and (not (inline-whitespace-char? ch))
                           (not (newline-char? ch))))))
  (define word
    (substring line start end))
  (cond
    [(set-member? makefile-directives word)
     (cons word end)]
    [else
     #f]))

;; find-assignment-operator : string? -> (or/c (cons/c exact-nonnegative-integer? string?) #f)
;;   Find the first assignment operator in a line.
(define (find-assignment-operator line)
  (define line-body
    (strip-line-ending line))
  (let loop ([i 0])
    (cond
      [(>= i (string-length line-body))
       #f]
      [else
         (define ch
           (string-ref line-body i))
       (cond
         [(and (char=? ch #\#)
               (not (escaped-comment-char? line-body i)))
          #f]
         [else
          (define match
            (for/or ([operator (in-list makefile-assignment-operators)])
              (and (string-prefix-at? line-body i operator)
                   (cons i operator))))
          (cond
            [match
             match]
            [else
             (loop (add1 i))])])])))

;; find-rule-colon : string? -> (or/c exact-nonnegative-integer? #f)
;;   Find the first rule delimiter colon in a line.
(define (find-rule-colon line)
  (define line-body
    (strip-line-ending line))
  (let loop ([i 0])
    (cond
      [(>= i (string-length line-body))
       #f]
      [else
         (define ch
           (string-ref line-body i))
       (cond
         [(and (char=? ch #\#)
               (not (escaped-comment-char? line-body i)))
          #f]
         [(and (char=? ch #\:)
               (not (and (> i 0)
                         (member (string-ref line-body (sub1 i))
                                 '(#\= #\:)))))
          i]
         [else
          (loop (add1 i))])])))

;; variable-reference-length : string? exact-nonnegative-integer? -> exact-positive-integer?
;;   Determine the source length of one variable reference spelling.
(define (variable-reference-length source index)
  (define len
    (string-length source))
  (cond
    [(>= (add1 index) len)
     1]
    [else
     (define next
       (string-ref source (add1 index)))
     (case next
       [(#\$)
        2]
       [(#\()
        (let loop ([i (+ index 2)] [depth 1])
          (cond
            [(>= i len)
             (- len index)]
            [else
             (define ch
               (string-ref source i))
             (cond
               [(char=? ch #\()
                (loop (add1 i) (add1 depth))]
               [(char=? ch #\))
                (cond
                  [(= depth 1)
                   (+ (- i index) 1)]
                  [else
                   (loop (add1 i) (sub1 depth))])]
               [else
                (loop (add1 i) depth)])]))]
       [(#\{)
        (let loop ([i (+ index 2)] [depth 1])
          (cond
            [(>= i len)
             (- len index)]
            [else
             (define ch
               (string-ref source i))
             (cond
               [(char=? ch #\{)
                (loop (add1 i) (add1 depth))]
               [(char=? ch #\})
                (cond
                  [(= depth 1)
                   (+ (- i index) 1)]
                  [else
                   (loop (add1 i) (sub1 depth))])]
               [else
                (loop (add1 i) depth)])]))]
       [else
        2])]))

;; tokenize-line : string? position? -> (listof makefile-derived-token?)
;;   Tokenize one physical Makefile line.
(define (tokenize-line line start-pos)
  (define length
    (string-length line))
  (define tokens
    '())
  (define index
    0)
  (define current-pos
    start-pos)
  (define assignment
    (find-assignment-operator line))
  (define assignment-index
    (and assignment (car assignment)))
  (define assignment-operator
    (and assignment (cdr assignment)))
  (define rule-colon-index
    (and (not assignment-index)
         (find-rule-colon line)))
  (define directive
    (directive-word-and-end line))
  (define directive-word
    (and directive (car directive)))
  (define recipe-line?
    (and (> length 0)
         (char=? (string-ref line 0) #\tab)))

  ;; emit! : string? (listof symbol?) -> void?
  ;;   Append one derived token and advance the local cursor.
  (define (emit! text tags)
    (define token
      (make-token-from-text current-pos text tags))
    (set! tokens (cons token tokens))
    (set! index (+ index (string-length text)))
    (set! current-pos (makefile-derived-token-end token)))

  ;; emit-range! : exact-nonnegative-integer? exact-nonnegative-integer? (listof symbol?) -> void?
  ;;   Emit a token from one source slice.
  (define (emit-range! start end tags)
    (emit! (substring line start end) tags))

  ;; shell-tags->makefile-tags : (listof symbol?) -> (listof symbol?)
  ;;   Add Makefile recipe tags to delegated shell tags.
  (define (shell-tags->makefile-tags tags)
    (append tags '(makefile-recipe embedded-shell)))

  ;; emit-shell-chunk! : string? -> void?
  ;;   Delegate one non-Make expansion chunk to the shell lexer.
  (define (emit-shell-chunk! text)
    (for ([token (in-list (shell-string->derived-tokens text #:shell 'bash))])
      (emit! (shell-derived-token-text token)
             (shell-tags->makefile-tags
              (shell-derived-token-tags token)))))

  ;; emit-recipe-body! : exact-nonnegative-integer? exact-nonnegative-integer? -> void?
  ;;   Tokenize one recipe-body slice using shell delegation plus Make expansions.
  (define (emit-recipe-body! start end)
    (let loop ([cursor start] [chunk-start start])
      (cond
        [(>= cursor end)
         (when (< chunk-start end)
           (emit-shell-chunk! (substring line chunk-start end)))]
        [(char=? (string-ref line cursor) #\$)
         (when (< chunk-start cursor)
           (emit-shell-chunk! (substring line chunk-start cursor)))
         (define ref-length
           (variable-reference-length line cursor))
         (define ref-end
           (min end (+ cursor ref-length)))
         (emit-range! cursor
                      ref-end
                      '(identifier makefile-variable-reference makefile-recipe))
         (loop ref-end ref-end)]
        [else
         (loop (add1 cursor) chunk-start)])))

  ;; next-special-index : exact-nonnegative-integer? -> exact-nonnegative-integer?
  ;;   Find the next structural character worth splitting on.
  (define (next-special-index start)
    (let loop ([i start])
      (cond
        [(>= i length)
         i]
        [else
         (define ch
           (string-ref line i))
         (cond
           [(or (inline-whitespace-char? ch)
                (newline-char? ch)
                (char=? ch #\#)
                (char=? ch #\$)
                (char=? ch #\:)
                (char=? ch #\;)
                (char=? ch #\|)
                (char=? ch #\()
                (char=? ch #\))
                (char=? ch #\{)
                (char=? ch #\})
                (char=? ch #\,)
                (char=? ch #\=)
                (char=? ch #\!)
                (char=? ch #\?)
                (char=? ch #\+))
            i]
           [else
            (loop (add1 i))])])))

  ;; emit-generic-chunk! : exact-nonnegative-integer? exact-nonnegative-integer? -> void?
  ;;   Emit one non-special chunk using the current line context.
  (define (emit-generic-chunk! start end)
    (define text
      (substring line start end))
    (cond
      [(and directive-word
            (= start (first-nonspace-index line))
            (string=? text directive-word))
       (emit! text '(keyword makefile-directive))]
      [(and rule-colon-index
            (< start rule-colon-index)
            (string-prefix? text "."))
       (emit! text '(identifier makefile-rule-target makefile-special-target))]
      [(and assignment-index
            (< start assignment-index))
       (emit! text '(identifier makefile-variable))]
      [(and rule-colon-index
            (< start rule-colon-index))
       (emit! text '(identifier makefile-rule-target))]
      [(and (> end start)
            (word-char? (string-ref text 0)))
       (emit! text '(identifier))]
      [else
       (emit! text '(literal))]))

  (let loop ()
    (cond
      [(>= index length)
       (reverse tokens)]
      [else
       (define ch
         (string-ref line index))
       (cond
         [recipe-line?
          (cond
            [(= index 0)
             (emit! "\t" '(whitespace makefile-recipe-prefix makefile-recipe))
             (loop)]
            [else
             (emit-recipe-body! index length)
             (reverse tokens)])]
         [(or (inline-whitespace-char? ch)
              (newline-char? ch))
          (define end
            (take-while-end line index
                            (lambda (c)
                              (or (inline-whitespace-char? c)
                                  (newline-char? c)))))
          (emit-range! index end '(whitespace))
          (loop)]
         [(and (not recipe-line?)
               (char=? ch #\#)
               (not (escaped-comment-char? line index)))
          (emit-range! index length '(comment))
          (loop)]
         [(char=? ch #\$)
          (define ref-length
            (variable-reference-length line index))
          (emit-range! index
                       (min length (+ index ref-length))
                       '(identifier makefile-variable-reference))
          (loop)]
         [(and assignment-index
               (= index assignment-index))
          (emit! assignment-operator '(operator makefile-assignment-operator))
          (loop)]
         [(and rule-colon-index
               (= index rule-colon-index))
          (emit! ":" '(delimiter))
          (loop)]
         [else
          (define delimiter-match
            (for/or ([delimiter (in-list makefile-delimiters)])
              (and (string-prefix-at? line index delimiter)
                   delimiter)))
         (cond
            [delimiter-match
             (emit! delimiter-match '(delimiter))
             (loop)]
            [else
             (define end
               (next-special-index index))
             (cond
               [(> end index)
                (emit-generic-chunk! index end)]
               [recipe-line?
                (emit! (string ch) '(literal))]
               [(member ch '(#\! #\? #\+ #\=))
                (emit! (string ch) '(operator))]
               [else
                (emit! (string ch) '(literal))])
             (loop)])])])))

;; make-makefile-derived-reader : -> (input-port? -> (or/c makefile-derived-token? 'eof))
;;   Construct a stateful Makefile derived-token reader.
(define (make-makefile-derived-reader)
  (define pending
    '())
  (lambda (in)
    (let loop ()
      (cond
        [(pair? pending)
         (define next
           (car pending))
         (set! pending (cdr pending))
         next]
        [else
         (define start-pos
           (current-stream-position in))
         (define line
           (read-physical-line in))
         (cond
           [(eof-object? line)
            'eof]
           [else
            (set! pending (tokenize-line line start-pos))
            (loop)])]))))
