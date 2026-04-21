#lang racket/base

;;;
;;; TeX Derived Tokens
;;;
;;
;; Stateful TeX tokenization and reusable TeX-specific classifications.

;; tex-derived-token?         : any/c -> boolean?
;;   Recognize a derived TeX token.
;; tex-derived-token-text     : tex-derived-token? -> string?
;;   Extract the source text for one derived token.
;; tex-derived-token-start    : tex-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; tex-derived-token-end      : tex-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; tex-derived-token-tags     : tex-derived-token? -> (listof symbol?)
;;   Extract reusable TeX classification tags.
;; tex-derived-token-has-tag? : tex-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-tex-derived-reader    : [symbol?] -> (input-port? -> (or/c tex-derived-token? 'eof))
;;   Construct a stateful TeX-derived-token reader.

(provide tex-derived-token?
         tex-derived-token-text
         tex-derived-token-start
         tex-derived-token-end
         tex-derived-token-tags
         tex-derived-token-has-tag?
         make-tex-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         racket/string
         "parser-tools-compat.rkt")

;; A TeX token plus reusable tags.
(struct tex-derived-token (kind text start end tags) #:transparent)

;; tex-derived-token-has-tag? : tex-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (tex-derived-token-has-tag? token tag)
  (member tag (tex-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Classification tables

;; Common LaTeX control words highlighted specially by the thin LaTeX wrapper.
(define latex-command-words
  (list->set
   '("begin" "end" "chapter" "section" "subsection" "subsubsection"
     "paragraph" "subparagraph" "part" "title" "author" "date" "maketitle"
     "tableofcontents" "textbf" "textit" "emph" "mathrm" "mathbf" "mathit"
     "frac" "sqrt" "left" "right" "item" "itemize" "enumerate" "description"
     "documentclass" "usepackage" "include" "input" "caption" "label" "ref"
     "pageref" "cite" "footnote" "url" "verb" "includegraphics")))

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

;; make-token-from-text : position? position? string? (listof symbol?) -> tex-derived-token?
;;   Construct one derived token from explicit positions, text, and tags.
(define (make-token-from-text start-pos end-pos text tags)
  (define kind
    (cond
      [(member 'comment tags)         'comment]
      [(member 'whitespace tags)      'whitespace]
      [(member 'malformed-token tags) 'malformed]
      [(member 'keyword tags)         'keyword]
      [(member 'literal tags)         'literal]
      [(member 'delimiter tags)       'delimiter]
      [else                           'identifier]))
  (tex-derived-token kind
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

;; inline-whitespace? : char? -> boolean?
;;   Recognize TeX whitespace other than newline characters.
(define (inline-whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\page)))

;; tex-whitespace? : (or/c char? eof-object?) -> boolean?
;;   Recognize TeX whitespace in the first lexer slice.
(define (tex-whitespace? ch)
  (and (char? ch)
       (or (inline-whitespace? ch)
           (newline-start? ch))))

;; letter? : char? -> boolean?
;;   Recognize a letter for control-word scanning.
(define (letter? ch)
  (char-alphabetic? ch))

;; tex-special-char? : (or/c char? eof-object?) -> boolean?
;;   Recognize characters that terminate plain-text runs.
(define (tex-special-char? ch)
  (and (char? ch)
       (or (tex-whitespace? ch)
           (member ch '(#\\ #\% #\{ #\} #\[ #\] #\$ #\# #\& #\_ #\^ #\~)))))

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
;;   Consume a run of TeX whitespace, preserving physical newlines.
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
      [(inline-whitespace? next)
       (write-one! in out)
       (loop)]
      [else
       (void)])))

;; read-comment! : input-port? output-port? -> void?
;;   Consume one TeX line comment without its terminating newline.
(define (read-comment! in out)
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

;; read-control-word! : input-port? output-port? -> void?
;;   Consume a control word after the leading backslash.
(define (read-control-word! in out)
  (write-one! in out)
  (read-while! in out letter?))

;; read-control-symbol! : input-port? output-port? -> boolean?
;;   Consume a control symbol after the leading backslash and report success.
(define (read-control-symbol! in out)
  (write-one! in out)
  (define next
    (peek-next in))
  (cond
    [(eof-object? next)
     #f]
    [else
     (write-one! in out)
     #t]))

;; read-parameter! : input-port? output-port? -> void?
;;   Consume a parameter marker such as #1 or ##.
(define (read-parameter! in out)
  (write-one! in out)
  (define next
    (peek-next in))
  (when (and (char? next)
             (or (char-numeric? next)
                 (char=? next #\#)))
    (write-one! in out)))

;; read-text! : input-port? output-port? -> void?
;;   Consume one plain-text run until a special character begins.
(define (read-text! in out)
  (read-while! in out
               (lambda (ch)
                 (not (tex-special-char? ch)))))

;; -----------------------------------------------------------------------------
;; Reader

;; control-word-tags : string? symbol? -> (listof symbol?)
;;   Choose derived tags for one control word in the selected mode.
(define (control-word-tags text mode)
  (define name
    (substring text 1))
  (cond
    [(and (eq? mode 'latex)
          (set-member? latex-command-words name))
     (append '(identifier tex-control-word latex-command keyword)
             (cond
               [(or (string=? name "begin")
                    (string=? name "end"))
                '(latex-environment-command)]
               [else
                '()]))]
    [else
     '(identifier tex-control-word)]))

;; control-symbol-tags : string? -> (listof symbol?)
;;   Choose derived tags for one control symbol.
(define (control-symbol-tags text)
  (cond
    [(or (string=? text "\\(")
         (string=? text "\\)")
         (string=? text "\\[")
         (string=? text "\\]"))
     '(delimiter tex-control-symbol tex-math-shift)]
    [else
     '(identifier tex-control-symbol)]))

;; read-control-token : position? input-port? output-port? symbol? -> tex-derived-token?
;;   Read one control word or control symbol token.
(define (read-control-token start in out mode)
  (define next-next
    (peek-next in 1))
  (cond
    [(letter? next-next)
     (read-control-word! in out)
     (make-token-from-text start
                           (current-stream-position in)
                           (get-output-string out)
                           (control-word-tags (get-output-string out) mode))]
    [else
     (define terminated?
       (read-control-symbol! in out))
     (make-token-from-text start
                           (current-stream-position in)
                           (get-output-string out)
                           (if terminated?
                               (control-symbol-tags (get-output-string out))
                               '(malformed-token tex-control-symbol)))]))

;; make-tex-derived-reader : [symbol?] -> (input-port? -> (or/c tex-derived-token? 'eof))
;;   Construct a stateful TeX-derived-token reader.
(define (make-tex-derived-reader [mode 'tex])
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
         [(tex-whitespace? next)
          (read-whitespace! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(whitespace tex-whitespace))]
         [(char=? next #\%)
          (read-comment! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(comment tex-comment))]
         [(char=? next #\\)
          (read-control-token start in out mode)]
         [(char=? next #\{)
          (write-one! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(delimiter tex-group-delimiter))]
         [(char=? next #\})
          (write-one! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(delimiter tex-group-delimiter))]
         [(char=? next #\[)
          (write-one! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(delimiter tex-optional-delimiter))]
         [(char=? next #\])
          (write-one! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(delimiter tex-optional-delimiter))]
         [(char=? next #\$)
          (write-one! in out)
          (when (and (char? (peek-next in))
                     (char=? (peek-next in) #\$))
            (write-one! in out))
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(delimiter tex-math-shift))]
         [(char=? next #\#)
          (read-parameter! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(identifier tex-parameter))]
         [(member next '(#\& #\_ #\^ #\~))
          (write-one! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(delimiter tex-special-character))]
         [else
          (read-text! in out)
          (make-token-from-text start
                                (current-stream-position in)
                                (get-output-string out)
                                '(literal tex-text))])])))
