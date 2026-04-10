#lang racket/base

;;;
;;; WAT Derived Tokens
;;;
;;
;; Stateful WebAssembly text-format tokenization and reusable WAT-specific
;; classifications.

;; wat-derived-token?         : any/c -> boolean?
;;   Recognize a derived WAT token.
;; wat-derived-token-text     : wat-derived-token? -> string?
;;   Extract the source text for one derived WAT token.
;; wat-derived-token-start    : wat-derived-token? -> position?
;;   Extract the starting source position for one derived WAT token.
;; wat-derived-token-end      : wat-derived-token? -> position?
;;   Extract the ending source position for one derived WAT token.
;; wat-derived-token-tags     : wat-derived-token? -> (listof symbol?)
;;   Extract the reusable classification tags for one derived WAT token.
;; wat-derived-token-has-tag? : wat-derived-token? symbol? -> boolean?
;;   Determine whether a derived WAT token has a given classification tag.
;; make-wat-derived-reader    : -> (input-port? -> (or/c wat-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived WAT tokens.

(provide wat-derived-token?
         wat-derived-token-text
         wat-derived-token-start
         wat-derived-token-end
         wat-derived-token-tags
         wat-derived-token-has-tag?
         make-wat-derived-reader)

(require racket/list
         racket/set
         racket/string
         "parser-tools-compat.rkt")

;; A derived WAT token with reusable tags and source positions.
(struct wat-derived-token (kind text start end tags) #:transparent)

;; WAT form keywords.
(define wat-form-keywords
  (list->set
   '("module" "func" "param" "result"
     "local" "global" "memory" "table" "type"
     "import" "export" "data" "elem" "start"
     "offset" "align" "mut")))

;; WAT type keywords.
(define wat-type-keywords
  (list->set
   '("i32" "i64" "f32" "f64" "v128" "funcref" "externref")))

;; WAT instruction keywords.
(define wat-instruction-keywords
  (list->set
   '("block" "loop" "if" "then" "else" "end"
     "call" "call_indirect" "return" "drop" "select"
     "unreachable" "nop" "br" "br_if" "br_table"
     "local.get" "local.set" "local.tee"
     "global.get" "global.set")))

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

;; make-wat-token : position? position? symbol? string? (listof symbol?) -> wat-derived-token?
;;   Construct one WAT derived token from explicit positions and tags.
(define (make-wat-token start-pos end-pos kind text tags)
  (wat-derived-token kind
                     text
                     start-pos
                     end-pos
                     (remove-duplicates tags)))

;; write-one! : input-port? output-port? -> void?
;;   Consume one character from a port and append it to an output string port.
(define (write-one! in out)
  (write-char (read-char in) out))

;; peek-next : input-port? [exact-nonnegative-integer?] -> (or/c char? eof-object?)
;;   Peek ahead in a WAT source port.
(define (peek-next in [skip 0])
  (peek-char in skip))

;; wat-word-char? : char? -> boolean?
;;   Recognize a character allowed in WAT word-like tokens.
(define (wat-word-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (memv ch '(#\$ #\_ #\- #\. #\/ #\! #\? #\= #\+ #\< #\> #\: #\@))))

;; wat-number? : string? -> boolean?
;;   Recognize the numeric formats supported by the initial WAT lexer.
(define (wat-number? txt)
  (or (regexp-match? #px"^[+-]?[0-9][0-9_]*$" txt)
      (regexp-match? #px"^[+-]?0x[0-9a-fA-F][0-9a-fA-F_]*$" txt)
      (regexp-match? #px"^[+-]?[0-9][0-9_]*\\.[0-9_]+([eE][+-]?[0-9_]+)?$" txt)
      (regexp-match? #px"^[+-]?[0-9][0-9_]*[eE][+-]?[0-9_]+$" txt)
      (regexp-match? #px"^[+-]?(inf|nan)(:0x[0-9a-fA-F][0-9a-fA-F_]*)?$"
                     (string-downcase txt))))

;; wat-word-tags : string? -> (listof symbol?)
;;   Classify one WAT word-like token.
(define (wat-word-tags text)
  (cond
    [(set-member? wat-form-keywords text)
     '(keyword wat-form)]
    [(set-member? wat-type-keywords text)
     '(keyword wat-type)]
    [(set-member? wat-instruction-keywords text)
     '(keyword wat-instruction)]
    [(or (regexp-match? #px"^[if][0-9]{2}\\.[a-z][a-z0-9_]*$" text)
         (regexp-match? #px"^v[0-9]+\\.[a-z][a-z0-9_]*$" text))
     '(keyword wat-instruction)]
    [(string-prefix? text "$")
     '(identifier wat-identifier)]
    [(wat-number? text)
     '(literal wat-numeric-literal)]
    [else
     '(identifier wat-identifier)]))

;; read-while! : input-port? output-port? (char? -> boolean?) -> void?
;;   Consume characters while pred? holds and append them to out.
(define (read-while! in out pred?)
  (let loop ()
    (define next (peek-next in))
    (cond
      [(and (char? next) (pred? next))
       (write-one! in out)
       (loop)]
      [else
       (void)])))

;; read-line-comment! : input-port? output-port? -> void?
;;   Consume a WAT line comment without consuming the terminating newline.
(define (read-line-comment! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ()
    (define next (peek-next in))
    (cond
      [(eof-object? next)
       (void)]
      [(char=? next #\newline)
       (void)]
      [else
       (write-one! in out)
       (loop)])))

;; read-block-comment! : input-port? output-port? -> boolean?
;;   Consume a nested WAT block comment and report whether it terminated.
(define (read-block-comment! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ([depth 1])
    (define first  (peek-next in 0))
    (define second (peek-next in 1))
    (cond
      [(eof-object? first)
       #f]
      [(and (char? first)
            (char? second)
            (char=? first #\()
            (char=? second #\;))
       (write-one! in out)
       (write-one! in out)
       (loop (add1 depth))]
      [(and (char? first)
            (char? second)
            (char=? first #\;)
            (char=? second #\)))
       (write-one! in out)
       (write-one! in out)
       (define next-depth (sub1 depth))
       (cond
         [(zero? next-depth) #t]
         [else               (loop next-depth)])]
      [else
       (write-one! in out)
       (loop depth)])))

;; read-string-literal! : input-port? output-port? -> boolean?
;;   Consume a WAT string literal and report whether it terminated.
(define (read-string-literal! in out)
  (write-one! in out)
  (let loop ([escaped? #f])
    (define next (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [escaped?
       (write-one! in out)
       (loop #f)]
      [else
       (define ch next)
       (write-one! in out)
       (cond
         [(char=? ch #\\) (loop #t)]
         [(char=? ch #\") #t]
         [else            (loop #f)])])))

;; read-word-token! : input-port? output-port? -> void?
;;   Consume a WAT word-like token.
(define (read-word-token! in out)
  (read-while! in out wat-word-char?))

;; make-token-from-out : input-port? position? symbol? output-port? (listof symbol?) -> wat-derived-token?
;;   Construct one derived token using the current port position as its end.
(define (make-token-from-out in start-pos kind out tags)
  (make-wat-token start-pos
                  (current-stream-position in)
                  kind
                  (get-output-string out)
                  tags))

;; make-token-for-word : input-port? position? output-port? -> wat-derived-token?
;;   Construct one derived token for a completed WAT word.
(define (make-token-for-word in start-pos out)
  (define text
    (get-output-string out))
  (define tags
    (wat-word-tags text))
  (define kind
    (cond
      [(member 'keyword tags)    'keyword]
      [(member 'literal tags)    'literal]
      [(member 'identifier tags) 'identifier]
      [else                      'unknown]))
  (make-wat-token start-pos
                  (current-stream-position in)
                  kind
                  text
                  tags))

;; make-wat-derived-reader : -> (input-port? -> (or/c wat-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived WAT tokens.
(define (make-wat-derived-reader)
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-wat-derived-reader "input-port?" in))
    (port-count-lines! in)
    (define start-pos
      (current-stream-position in))
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       'eof]
      [(char-whitespace? next)
       (define out (open-output-string))
       (read-while! in out char-whitespace?)
       (make-token-from-out in start-pos 'whitespace out '(whitespace))]
      [(and (char? next)
            (char=? next #\;)
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\;))
       (define out (open-output-string))
       (read-line-comment! in out)
       (make-token-from-out in start-pos 'comment out '(comment))]
      [(and (char? next)
            (char=? next #\()
            (char? (peek-next in 1))
            (char=? (peek-next in 1) #\;))
       (define out (open-output-string))
       (define terminated?
         (read-block-comment! in out))
       (cond
         [terminated?
          (make-token-from-out in start-pos 'comment out '(comment))]
         [else
          (make-token-from-out in start-pos 'unknown out '(malformed-token))])]
      [(or (char=? next #\()
           (char=? next #\)))
       (define out (open-output-string))
       (write-one! in out)
       (make-token-from-out in start-pos 'delimiter out '(delimiter))]
      [(char=? next #\")
       (define out (open-output-string))
       (define terminated?
         (read-string-literal! in out))
       (cond
         [terminated?
          (make-token-from-out in start-pos
                               'literal
                               out
                               '(literal wat-string-literal))]
         [else
          (make-token-from-out in start-pos
                               'unknown
                               out
                               '(malformed-token))])]
      [(wat-word-char? next)
       (define out (open-output-string))
       (read-word-token! in out)
       (make-token-for-word in start-pos out)]
      [else
       (define out (open-output-string))
       (write-one! in out)
       (make-token-from-out in start-pos 'unknown out '(malformed-token))])))

;; wat-derived-token-has-tag? : wat-derived-token? symbol? -> boolean?
;;   Determine whether a derived WAT token has a given classification tag.
(define (wat-derived-token-has-tag? token tag)
  (member tag (wat-derived-token-tags token)))
