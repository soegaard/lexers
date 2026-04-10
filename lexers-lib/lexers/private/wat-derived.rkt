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

(require parser-tools/lex
         racket/list
         racket/port
         racket/set
         racket/string
         "parser-tools-compat.rkt"
         "string-compat.rkt")

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

;; line-starts : string? -> (vectorof exact-nonnegative-integer?)
;;   Compute the starting index of each line in a source string.
(define (line-starts source)
  (list->vector
   (let loop ([i 0] [starts '(0)])
     (cond
       [(>= i (string-length source))
        (reverse starts)]
       [(char=? (string-ref source i) #\newline)
        (loop (add1 i) (cons (add1 i) starts))]
       [else
        (loop (add1 i) starts)]))))

;; position-at : (vectorof exact-nonnegative-integer?) exact-nonnegative-integer? -> position?
;;   Convert a source index into a parser-tools-compatible source position.
(define (position-at starts index)
  (define len (vector-length starts))
  (let loop ([line-idx 0])
    (cond
      [(= line-idx (sub1 len))
       (define line-start (vector-ref starts line-idx))
       (make-stream-position (add1 index)
                             (add1 line-idx)
                             (- index line-start))]
      [(< index (vector-ref starts (add1 line-idx)))
       (define line-start (vector-ref starts line-idx))
       (make-stream-position (add1 index)
                             (add1 line-idx)
                             (- index line-start))]
      [else
       (loop (add1 line-idx))])))

;; make-wat-token : (vectorof exact-nonnegative-integer?) exact-nonnegative-integer? exact-nonnegative-integer? symbol? string? (listof symbol?) -> wat-derived-token?
;;   Construct one WAT derived token from explicit bounds and tags.
(define (make-wat-token starts start end kind text tags)
  (wat-derived-token kind
                     text
                     (position-at starts start)
                     (position-at starts end)
                     (remove-duplicates tags)))

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

;; read-while-index : string? exact-nonnegative-integer? (char? -> boolean?) -> exact-nonnegative-integer?
;;   Advance while pred? holds.
(define (read-while-index source start pred?)
  (define len (string-length source))
  (let loop ([i start])
    (cond
      [(>= i len) i]
      [(pred? (string-ref source i))
       (loop (add1 i))]
      [else
       i])))

;; read-wat-string : string? exact-nonnegative-integer? -> (values exact-nonnegative-integer? boolean?)
;;   Read a WAT string literal and report whether it was terminated.
(define (read-wat-string source start)
  (define len (string-length source))
  (let loop ([i (add1 start)] [escaped? #f])
    (cond
      [(>= i len) (values len #f)]
      [escaped? (loop (add1 i) #f)]
      [else
       (define ch (string-ref source i))
       (cond
         [(char=? ch #\\) (loop (add1 i) #t)]
         [(char=? ch #\") (values (add1 i) #t)]
         [else            (loop (add1 i) #f)])])))

;; read-wat-block-comment : string? exact-nonnegative-integer? -> (values exact-nonnegative-integer? boolean?)
;;   Read a nested WAT block comment and report whether it was terminated.
(define (read-wat-block-comment source start)
  (define len (string-length source))
  (let loop ([i (+ start 2)] [depth 1])
    (cond
      [(>= i len) (values len #f)]
      [(and (< (add1 i) len)
            (char=? (string-ref source i) #\()
            (char=? (string-ref source (add1 i)) #\;))
       (loop (+ i 2) (add1 depth))]
      [(and (< (add1 i) len)
            (char=? (string-ref source i) #\;)
            (char=? (string-ref source (add1 i)) #\)))
       (define next-depth (sub1 depth))
       (cond
         [(zero? next-depth) (values (+ i 2) #t)]
         [else               (loop (+ i 2) next-depth)])]
      [else
       (loop (add1 i) depth)])))

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

;; tokenize-wat-source : string? -> (listof wat-derived-token?)
;;   Tokenize an entire WAT source string into derived tokens.
(define (tokenize-wat-source source)
  (define len    (string-length source))
  (define starts (line-starts source))
  (let loop ([i 0] [acc '()])
    (cond
      [(>= i len)
       (reverse acc)]
      [else
       (define ch (string-ref source i))
       (cond
         [(char-whitespace? ch)
          (define j (read-while-index source i char-whitespace?))
          (loop j
                (cons (make-wat-token starts i j
                                      'whitespace
                                      (substring source i j)
                                      '(whitespace))
                      acc))]
         [(and (< (add1 i) len)
               (char=? ch #\;)
               (char=? (string-ref source (add1 i)) #\;))
          (define j
            (let find-line-end ([k (+ i 2)])
              (cond
                [(>= k len) k]
                [(char=? (string-ref source k) #\newline) k]
                [else (find-line-end (add1 k))])))
          (loop j
                (cons (make-wat-token starts i j
                                      'comment
                                      (substring source i j)
                                      '(comment))
                      acc))]
         [(and (< (add1 i) len)
               (char=? ch #\()
               (char=? (string-ref source (add1 i)) #\;))
          (define-values (j terminated?)
            (read-wat-block-comment source i))
          (define tags
            (cond
              [terminated? '(comment)]
              [else        '(malformed-token)]))
          (define kind
            (cond
              [terminated? 'comment]
              [else        'unknown]))
          (loop j
                (cons (make-wat-token starts i j
                                      kind
                                      (substring source i j)
                                      tags)
                      acc))]
         [(or (char=? ch #\()
              (char=? ch #\)))
          (define j (add1 i))
          (loop j
                (cons (make-wat-token starts i j
                                      'delimiter
                                      (substring source i j)
                                      '(delimiter))
                      acc))]
         [(char=? ch #\")
          (define-values (j terminated?)
            (read-wat-string source i))
          (define tags
            (cond
              [terminated? '(literal wat-string-literal)]
              [else        '(malformed-token)]))
          (define kind
            (cond
              [terminated? 'literal]
              [else        'unknown]))
          (loop j
                (cons (make-wat-token starts i j
                                      kind
                                      (substring source i j)
                                      tags)
                      acc))]
         [else
          (define j
            (read-while-index source i
                              (lambda (c)
                                (and (not (char-whitespace? c))
                                     (not (char=? c #\())
                                     (not (char=? c #\)))
                                     (not (char=? c #\;))
                                     (not (char=? c #\"))))))
          (cond
            [(= i j)
             (define k (add1 i))
             (loop k
                   (cons (make-wat-token starts i k
                                         'unknown
                                         (substring source i k)
                                         '(malformed-token))
                         acc))]
            [else
             (define text (substring source i j))
             (define tags (wat-word-tags text))
             (define kind
               (cond
                 [(member 'keyword tags)    'keyword]
                 [(member 'literal tags)    'literal]
                 [(member 'identifier tags) 'identifier]
                 [else                      'unknown]))
             (loop j
                   (cons (make-wat-token starts i j kind text tags)
                         acc))])])])))

;; make-wat-derived-reader : -> (input-port? -> (or/c wat-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived WAT tokens.
(define (make-wat-derived-reader)
  (define buffered-source #f)
  (define buffered-tokens #f)
  (define next-index      0)

  ;; initialize! : input-port? -> void?
  ;;   Buffer the remaining input and tokenize it on first use.
  (define (initialize! in)
    (when (not buffered-tokens)
      (set! buffered-source (port->string in))
      (set! buffered-tokens (list->vector (tokenize-wat-source buffered-source)))))

  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-wat-derived-reader "input-port?" in))
    (initialize! in)
    (cond
      [(>= next-index (vector-length buffered-tokens))
       'eof]
      [else
       (define token (vector-ref buffered-tokens next-index))
       (set! next-index (add1 next-index))
       token])))

;; wat-derived-token-has-tag? : wat-derived-token? symbol? -> boolean?
;;   Determine whether a derived WAT token has a given classification tag.
(define (wat-derived-token-has-tag? token tag)
  (member tag (wat-derived-token-tags token)))
