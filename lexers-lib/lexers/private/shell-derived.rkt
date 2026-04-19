#lang racket/base

;;;
;;; Shell Derived Tokens
;;;
;;
;; Stateful shell tokenization and reusable shell-specific classifications for
;; Bash, Zsh, and PowerShell.

;; shell-derived-token?         : any/c -> boolean?
;;   Recognize a derived shell token.
;; shell-derived-token-text     : shell-derived-token? -> string?
;;   Extract the source text for one derived shell token.
;; shell-derived-token-start    : shell-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; shell-derived-token-end      : shell-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; shell-derived-token-tags     : shell-derived-token? -> (listof symbol?)
;;   Extract reusable shell classification tags.
;; shell-derived-token-has-tag? : shell-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-shell-derived-reader    : symbol? -> (input-port? -> (or/c shell-derived-token? 'eof))
;;   Construct a stateful shell derived-token reader for one shell dialect.

(provide shell-derived-token?
         shell-derived-token-text
         shell-derived-token-start
         shell-derived-token-end
         shell-derived-token-tags
         shell-derived-token-has-tag?
         make-shell-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         racket/string
         "parser-tools-compat.rkt")

;; A shell token plus reusable tags.
(struct shell-derived-token (kind text start end tags) #:transparent)

;; -----------------------------------------------------------------------------
;; Shell classification tables

;; Shared Bourne-style shell keywords.
(define shell-keywords/common
  (list->set
   '("if" "then" "elif" "else" "fi"
     "for" "while" "until" "do" "done"
     "case" "in" "esac" "select"
     "function" "time" "coproc")))

;; Shared Bourne-style builtins.
(define shell-builtins/common
  (list->set
   '("cd" "echo" "printf" "read"
     "export" "unset" "readonly"
     "alias" "unalias"
     "set" "shift" "test" "source"
     "." "eval" "exec" "exit" "return"
     "local" "typeset")))

;; Zsh-specific builtins.
(define shell-builtins/zsh
  (list->set
   '("autoload" "setopt" "unsetopt" "emulate" "zmodload")))

;; PowerShell keywords.
(define shell-keywords/powershell
  (list->set
   '("if" "elseif" "else" "switch"
     "for" "foreach" "while" "do" "until"
     "break" "continue"
     "function" "filter" "param"
     "begin" "process" "end"
     "return" "throw" "try" "catch" "finally" "trap"
     "class" "enum" "using")))

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

;; make-token-from-text : position? position? string? (listof symbol?) -> shell-derived-token?
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
  (shell-derived-token kind
                       text
                       start-pos
                       end-pos
                       (remove-duplicates tags)))

;; advance-position : position? string? -> position?
;;   Advance one source position over exact token text, preserving CRLF width.
(define (advance-position pos text)
  (let loop ([i      0]
             [offset (position-offset pos)]
             [line   (position-line pos)]
             [col    (position-col pos)])
    (cond
      [(= i (string-length text))
       (make-stream-position offset line col)]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(char=? ch #\return)
          (cond
            [(and (< (add1 i) (string-length text))
                  (char=? (string-ref text (add1 i)) #\newline))
             (loop (+ i 2) (+ offset 2) (add1 line) 0)]
            [else
             (loop (add1 i) (add1 offset) (add1 line) 0)])]
         [(char=? ch #\newline)
          (loop (add1 i) (add1 offset) (add1 line) 0)]
         [else
          (loop (add1 i) (add1 offset) line (add1 col))])])))

;; -----------------------------------------------------------------------------
;; Character classes

;; shell-ident-start? : char? -> boolean?
;;   Recognize an identifier-start character.
(define (shell-ident-start? c)
  (or (char-alphabetic? c)
      (char=? c #\_)
      (char=? c #\-)))

;; shell-ident-char? : char? -> boolean?
;;   Recognize an identifier character.
(define (shell-ident-char? c)
  (or (shell-ident-start? c)
      (char-numeric? c)
      (char=? c #\.)
      (char=? c #\:)))

;; shell-punctuation-char? : char? -> boolean?
;;   Recognize punctuation that always breaks shell words.
(define (shell-punctuation-char? c)
  (or (char=? c #\()
      (char=? c #\))
      (char=? c #\{)
      (char=? c #\})
      (char=? c #\[)
      (char=? c #\])
      (char=? c #\;)
      (char=? c #\|)
      (char=? c #\&)
      (char=? c #\<)
      (char=? c #\>)
      (char=? c #\=)))

;; shell-word-char? : char? -> boolean?
;;   Recognize a character that can continue an ordinary shell word.
(define (shell-word-char? c)
  (and (not (char-whitespace? c))
       (not (shell-punctuation-char? c))
       (not (char=? c #\"))
       (not (char=? c #\'))
       (not (char=? c #\`))
       (not (char=? c #\$))))

;; shell-comment-after-token? : string? -> boolean?
;;   Determine whether a subsequent # should start a comment.
(define (shell-comment-after-token? text)
  (cond
    [(string=? text "") #t]
    [else
     (define last
       (string-ref text (sub1 (string-length text))))
     (or (char-whitespace? last)
         (member last '(#\; #\| #\& #\( #\) #\{ #\} #\[ #\] #\< #\>)))]))

;; -----------------------------------------------------------------------------
;; Compound readers

;; read-string-literal! : input-port? output-port? char? -> boolean?
;;   Consume one quoted string literal and report whether it terminated.
(define (read-string-literal! in out quote)
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
      [else
       (define ch
         next)
       (write-one! in out)
       (cond
         [(and (char=? quote #\") (char=? ch #\\))
          (loop #t)]
         [(char=? ch quote)
          #t]
         [else
          (loop #f)])])))

;; read-shell-braced-var! : input-port? output-port? -> boolean?
;;   Consume one ${...} variable form and report whether it terminated.
(define (read-shell-braced-var! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ([depth 1])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [else
       (define ch
         next)
       (write-one! in out)
       (cond
         [(char=? ch #\\)
          (when (char? (peek-next in))
            (write-one! in out))
          (loop depth)]
         [(char=? ch #\{)
          (loop (add1 depth))]
         [(char=? ch #\})
          (define next-depth
            (sub1 depth))
          (cond
            [(zero? next-depth) #t]
            [else               (loop next-depth)])]
         [else
          (loop depth)])])))

;; read-shell-command-subst! : input-port? output-port? -> boolean?
;;   Consume one $(...) substitution and report whether it terminated.
(define (read-shell-command-subst! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ([depth 1])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [else
       (define ch
         next)
       (cond
         [(char=? ch #\\)
          (write-one! in out)
          (when (char? (peek-next in))
            (write-one! in out))
          (loop depth)]
         [(or (char=? ch #\") (char=? ch #\'))
          (read-string-literal! in out ch)
          (loop depth)]
         [(and (char=? ch #\$)
               (char? (peek-next in 1))
               (char=? (peek-next in 1) #\())
          (write-one! in out)
          (write-one! in out)
          (loop (add1 depth))]
         [else
          (write-one! in out)
          (cond
            [(char=? ch #\()
             (loop (add1 depth))]
            [(char=? ch #\))
             (define next-depth
               (sub1 depth))
             (cond
               [(zero? next-depth) #t]
               [else               (loop next-depth)])]
            [else
             (loop depth)])])])))

;; read-shell-backticks! : input-port? output-port? -> boolean?
;;   Consume one backtick command substitution and report termination.
(define (read-shell-backticks! in out)
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
      [else
       (define ch
         next)
       (write-one! in out)
       (cond
         [(char=? ch #\\)
          (loop #t)]
         [(char=? ch #\`)
          #t]
         [else
          (loop #f)])])))

;; read-line-comment! : input-port? output-port? -> void?
;;   Consume one shell line comment without its terminating newline.
(define (read-line-comment! in out)
  (write-one! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       (void)]
      [(or (char=? next #\newline)
           (char=? next #\return))
       (void)]
      [else
       (write-one! in out)
       (loop)])))

;; read-shell-word! : input-port? output-port? -> void?
;;   Consume one ordinary shell word.
(define (read-shell-word! in out)
  (read-while! in out shell-word-char?))

;; read-punctuation! : input-port? output-port? -> void?
;;   Consume one punctuation token, preferring multi-character operators.
(define (read-punctuation! in out)
  (define three
    (if (and (char? (peek-next in 0))
             (char? (peek-next in 1))
             (char? (peek-next in 2)))
        (string (peek-next in 0) (peek-next in 1) (peek-next in 2))
        #f))
  (define two
    (if (and (char? (peek-next in 0))
             (char? (peek-next in 1)))
        (string (peek-next in 0) (peek-next in 1))
        #f))
  (cond
    [(and three
          (member three '("<<-" "<<<")))
     (write-one! in out)
     (write-one! in out)
     (write-one! in out)]
    [(and two
          (member two '("&&" "||" ">>" "<<" ";;" ";&" "|&" ">&" "<&" ">|")))
     (write-one! in out)
     (write-one! in out)]
    [else
     (write-one! in out)]))

;; read-variable! : input-port? output-port? symbol? -> (listof symbol?)
;;   Consume one variable reference and return its classification tags.
(define (read-variable! in out shell)
  (write-one! in out)
  (cond
    [(and (char? (peek-next in))
          (char=? (peek-next in) #\{))
     (define terminated?
       (read-shell-braced-var! in out))
     (cond
       [terminated? '(literal shell-variable)]
       [else        '(malformed-token shell-variable)])]
    [(eq? shell 'powershell)
     (read-while! in out
                  (lambda (c)
                    (or (shell-ident-char? c)
                        (char=? c #\?))))
     '(literal shell-variable)]
    [(and (char? (peek-next in))
          (or (shell-ident-start? (peek-next in))
              (char-numeric? (peek-next in))
              (member (peek-next in)
                      '(#\* #\@ #\# #\? #\! #\- #\$ #\_))))
     (read-while! in out
                  (lambda (c)
                    (or (shell-ident-char? c)
                        (char-numeric? c)
                        (member c '(#\* #\@ #\# #\? #\! #\- #\$ #\_)))))
     '(literal shell-variable)]
    [else
     '(literal shell-variable)]))

;; -----------------------------------------------------------------------------
;; Word classification

;; shell-word-tags : symbol? string? -> (listof symbol?)
;;   Classify one shell word for the selected dialect.
(define (shell-word-tags shell text)
  (define down
    (string-downcase text))
  (cond
    [(string=? down "")
     '(identifier shell-word)]
    [(and (eq? shell 'powershell)
          (or (set-member? shell-keywords/powershell down)
              (regexp-match? #px"^[a-z][a-z0-9]*-[a-z][a-z0-9-]*$" down)))
     '(keyword shell-keyword)]
    [(set-member? shell-keywords/common down)
     '(keyword shell-keyword)]
    [(and (eq? shell 'zsh)
          (set-member? shell-builtins/zsh down))
     '(keyword shell-builtin)]
    [(set-member? shell-builtins/common down)
     '(keyword shell-builtin)]
    [(regexp-match? #px"^[a-zA-Z_][a-zA-Z0-9_]*=.*$" text)
     '(identifier shell-assignment)]
    [(regexp-match? #px"^-{1,2}[a-zA-Z0-9][a-zA-Z0-9_-]*$" text)
     '(literal shell-option)]
    [(regexp-match? #px"^[0-9]+([.][0-9]+)?$" text)
     '(literal shell-numeric-literal)]
    [else
     '(identifier shell-word)]))

;; -----------------------------------------------------------------------------
;; Public reader

;; make-shell-derived-reader : symbol? -> (input-port? -> (or/c shell-derived-token? 'eof))
;;   Construct a stateful shell derived-token reader for one shell dialect.
(define (make-shell-derived-reader shell)
  (define comment-context? #t) ; # starts comments at BOF and after separators.
  (define current-pos
    (make-stream-position 1 1 0))
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-shell-derived-reader "input-port?" in))
    (port-count-lines! in)
    (define start-pos
      current-pos)
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
           [(char-whitespace? next)
            (read-while! in out char-whitespace?)
            '(whitespace)]
           [(and (char=? next #\#) comment-context?)
            (read-line-comment! in out)
            '(comment shell-comment)]
           [(or (char=? next #\") (char=? next #\'))
            (define terminated?
              (read-string-literal! in out next))
            (cond
              [terminated? '(literal shell-string-literal)]
              [else        '(malformed-token shell-string-literal)])]
           [(char=? next #\`)
            (define terminated?
              (read-shell-backticks! in out))
            (cond
              [terminated? '(literal shell-command-substitution)]
              [else        '(malformed-token shell-command-substitution)])]
           [(and (char=? next #\$)
                 (char? (peek-next in 1))
                 (char=? (peek-next in 1) #\())
            (define terminated?
              (read-shell-command-subst! in out))
            (cond
              [terminated? '(literal shell-command-substitution)]
              [else        '(malformed-token shell-command-substitution)])]
           [(char=? next #\$)
            (read-variable! in out shell)]
           [(shell-punctuation-char? next)
            (read-punctuation! in out)
            '(delimiter shell-punctuation)]
           [else
            (read-shell-word! in out)
            (shell-word-tags shell
                             (get-output-string out))]))
       (define text
         (get-output-string out))
       (define end-pos
         (advance-position start-pos text))
       (set! comment-context?
             (or (member 'whitespace tags)
                 (member 'comment tags)
                 (and (member 'delimiter tags)
                      (shell-comment-after-token? text))))
       (set! current-pos end-pos)
       (make-token-from-text start-pos
                             end-pos
                             text
                             tags)])))

;; shell-derived-token-has-tag? : shell-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (shell-derived-token-has-tag? token tag)
  (member tag (shell-derived-token-tags token)))
