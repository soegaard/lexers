#lang racket/base

;;;
;;; Scheme Derived Tokens
;;;

;; Streaming lexical analysis for Scheme reports and selected implementations.

;; scheme-derived-token?         : any/c -> boolean?
;; scheme-derived-token-text     : scheme-derived-token? -> string?
;; scheme-derived-token-start    : scheme-derived-token? -> position?
;; scheme-derived-token-end      : scheme-derived-token? -> position?
;; scheme-derived-token-tags     : scheme-derived-token? -> (listof symbol?)
;; scheme-derived-token-has-tag? : scheme-derived-token? symbol? -> boolean?
;; make-scheme-derived-reader    : symbol? -> (input-port? -> (or/c scheme-derived-token? eof))

(provide scheme-derived-token?
         scheme-derived-token-text
         scheme-derived-token-start
         scheme-derived-token-end
         scheme-derived-token-tags
         scheme-derived-token-has-tag?
         make-scheme-derived-reader)

(require racket/list
         "parser-tools-compat.rkt")

;; A Scheme token plus reusable syntax-role tags.
(struct scheme-derived-token (kind text start end tags) #:transparent)

;; scheme-derived-token-has-tag? : scheme-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (scheme-derived-token-has-tag? token tag)
  (member tag (scheme-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Port helpers

;; peek-next : input-port? [exact-nonnegative-integer?] -> (or/c char? eof-object?)
;;   Peek ahead in an input port without consuming characters.
(define (peek-next in [skip 0]) (peek-char in skip))

;; peek-string=? : input-port? string? -> boolean?
;;   Determine whether upcoming input matches text.
(define (peek-string=? in text)
  (for/and ([ch (in-string text)] [index (in-naturals)])
    (define next (peek-next in index))
    (and (char? next) (char=? next ch))))

;; write-one! : input-port? output-port? -> void?
;;   Consume one character and append it to the output accumulator.
(define (write-one! in out) (write-char (read-char in) out))

;; take-while! : input-port? output-port? (char? -> boolean?) -> void?
;;   Consume consecutive characters accepted by predicate.
(define (take-while! in out predicate)
  (let loop ()
    (define next (peek-next in))
    (when (and (char? next) (predicate next))
      (write-one! in out)
      (loop))))

;; current-stream-position : input-port? -> position?
;;   Read a parser-tools-compatible source position from a port.
(define (current-stream-position in)
  (let-values ([(line column offset) (port-next-location in)])
    (make-stream-position (if (exact-positive-integer? offset) offset 1)
                          (if (exact-positive-integer? line) line 1)
                          (if (exact-nonnegative-integer? column) column 0))))

;; make-token : input-port? position? string? (listof symbol?) -> scheme-derived-token?
;;   Construct a token ending at the input port's current position.
(define (make-token in start text tags)
  (define kind
    (cond [(member 'comment tags) 'comment]
          [(member 'whitespace tags) 'whitespace]
          [(member 'malformed-token tags) 'malformed]
          [(member 'keyword tags) 'keyword]
          [(member 'literal tags) 'literal]
          [(member 'delimiter tags) 'delimiter]
          [else 'identifier]))
  (scheme-derived-token kind text start (current-stream-position in)
                        (remove-duplicates tags)))

;; -----------------------------------------------------------------------------
;; Reader syntax

;; Scheme datum delimiters. Brackets and braces are reader extensions in some
;; reports, but are structural delimiters in the supported implementations.
(define scheme-delimiters '(#\( #\) #\[ #\] #\{ #\}))

;; token-terminator? : char? -> boolean?
;;   Recognize a character that terminates a Scheme identifier or number.
(define (token-terminator? ch)
  (or (char-whitespace? ch)
      (member ch scheme-delimiters)
      (member ch '(#\" #\; #\' #\` #\,))))

;; scheme-number? : string? -> boolean?
;;   Recognize common exact, inexact, radix-prefixed, and complex number forms.
(define (scheme-number? text)
  (regexp-match?
   #px"^(?:(?:#[eEiIbBoOdDxX]){0,2})?[+-]?(?:(?:[0-9A-Fa-f]+(?:/[0-9A-Fa-f]+)?)|(?:[0-9A-Fa-f]*\\.[0-9A-Fa-f]+))(?:[eEsSfFdDlL][+-]?[0-9]+)?(?:[+-](?:[0-9A-Fa-f]+(?:/[0-9A-Fa-f]+)?|[0-9A-Fa-f]*\\.[0-9A-Fa-f]+)i)?$"
   text))

;; boolean-literal? : string? symbol? -> boolean?
;;   Recognize boolean spellings accepted by the selected report or reader.
(define (boolean-literal? text dialect)
  (or (member text '("#t" "#f" "#T" "#F"))
      (and (member dialect '(r7rs chez guile chicken gambit))
           (member text '("#true" "#false" "#TRUE" "#FALSE")))))

;; suffix-keyword? : string? symbol? -> boolean?
;;   Recognize implementation keyword syntax that ends with a colon.
(define (suffix-keyword? text dialect)
  (and (member dialect '(guile gambit))
       (> (string-length text) 1)
       (char=? (string-ref text (sub1 (string-length text))) #\:)))

;; read-line-comment! : input-port? output-port? -> void?
;;   Consume a semicolon comment through, but not including, its newline.
(define (read-line-comment! in out)
  (take-while! in out
               (lambda (ch) (not (or (char=? ch #\newline) (char=? ch #\return))))))

;; read-block-comment! : input-port? output-port? -> boolean?
;;   Consume a nested #| ... |# comment and report whether it closes.
(define (read-block-comment! in out)
  (write-one! in out)
  (write-one! in out)
  (let loop ([depth 1])
    (cond [(eof-object? (peek-next in)) #f]
          [(peek-string=? in "#|")
           (write-one! in out)
           (write-one! in out)
           (loop (add1 depth))]
          [(peek-string=? in "|#")
           (write-one! in out)
           (write-one! in out)
           (if (= depth 1) #t (loop (sub1 depth)))]
          [else (write-one! in out) (loop depth)])))

;; read-string! : input-port? output-port? char? -> boolean?
;;   Consume a double-quoted string and report whether it closes cleanly.
(define (read-string! in out quote)
  (write-one! in out)
  (let loop ()
    (define next (peek-next in))
    (cond [(eof-object? next) #f]
          [(or (char=? next #\newline) (char=? next #\return)) #f]
          [else
           (write-one! in out)
           (cond [(char=? next quote) #t]
                 [(char=? next #\\)
                  (cond [(eof-object? (peek-next in)) #f]
                        [else (write-one! in out) (loop)])]
                 [else (loop)])])))

;; read-bar-identifier! : input-port? output-port? -> boolean?
;;   Consume a vertical-bar escaped identifier and report whether it closes.
(define (read-bar-identifier! in out)
  (write-one! in out)
  (let loop ()
    (define next (peek-next in))
    (cond [(eof-object? next) #f]
          [else
           (write-one! in out)
           (cond [(char=? next #\|) #t]
                 [(char=? next #\\)
                  (cond [(eof-object? (peek-next in)) #f]
                        [else (write-one! in out) (loop)])]
                 [else (loop)])])))

;; read-character! : input-port? output-port? -> boolean?
;;   Consume a #\\ character literal and report whether it has a character name.
(define (read-character! in out)
  (write-one! in out)
  (write-one! in out)
  (define next (peek-next in))
  (cond [(eof-object? next) #f]
        [else
         (write-one! in out)
         (take-while! in out (lambda (ch) (not (token-terminator? ch))))
         #t]))

;; read-atom! : input-port? output-port? -> void?
;;   Consume an identifier, keyword, number, or reader-prefixed atom.
(define (read-atom! in out)
  (take-while! in out (lambda (ch) (not (token-terminator? ch)))))

;; classify-atom : string? symbol? -> (listof symbol?)
;;   Classify a completed Scheme atom according to the selected reader dialect.
(define (classify-atom text dialect)
  (cond [(boolean-literal? text dialect) '(literal scheme-boolean scheme-constant)]
        [(scheme-number? text) '(literal scheme-number)]
        [(and (>= (string-length text) 2)
              (char=? (string-ref text 0) #\#)
              (char=? (string-ref text 1) #\:))
         '(keyword scheme-keyword scheme-prefix-keyword)]
        [(suffix-keyword? text dialect) '(keyword scheme-keyword scheme-suffix-keyword)]
        [else '(identifier scheme-identifier)]))

;; make-scheme-derived-reader : symbol? -> (input-port? -> (or/c scheme-derived-token? eof))
;;   Construct a streaming Scheme reader for a report or implementation dialect.
(define (make-scheme-derived-reader dialect)
  (lambda (in)
    (define start (current-stream-position in))
    (define next (peek-next in))
    (cond
      [(eof-object? next) 'eof]
      [(char-whitespace? next)
       (define out (open-output-string))
       (take-while! in out char-whitespace?)
       (make-token in start (get-output-string out) '(whitespace scheme-whitespace))]
      [(char=? next #\;)
       (define out (open-output-string))
       (read-line-comment! in out)
       (make-token in start (get-output-string out) '(comment scheme-comment scheme-line-comment))]
      [(peek-string=? in "#|")
       (define out (open-output-string))
       (define closed? (read-block-comment! in out))
       (make-token in start (get-output-string out)
                   (append '(comment scheme-comment scheme-block-comment)
                           (if closed? '() '(malformed-token scheme-error))))]
      [(peek-string=? in "#;")
       (read-char in)
       (read-char in)
       (make-token in start "#;" '(comment scheme-comment scheme-datum-comment))]
      [(and (char=? next #\#) (char? (peek-next in 1)) (char=? (peek-next in 1) #\!))
       (define out (open-output-string))
       (read-line-comment! in out)
       (make-token in start (get-output-string out) '(comment scheme-comment scheme-reader-directive))]
      [(char=? next #\")
       (define out (open-output-string))
       (define closed? (read-string! in out next))
       (make-token in start (get-output-string out)
                   (append '(literal scheme-string)
                           (if closed? '() '(malformed-token scheme-error))))]
      [(char=? next #\')
       (read-char in)
       (make-token in start "'" '(delimiter scheme-abbreviation scheme-quote))]
      [(char=? next #\|)
       (define out (open-output-string))
       (define closed? (read-bar-identifier! in out))
       (make-token in start (get-output-string out)
                   (append '(identifier scheme-escaped-identifier)
                           (if closed? '() '(malformed-token scheme-error))))]
      [(member next scheme-delimiters)
       (read-char in)
       (make-token in start (string next) '(delimiter scheme-delimiter))]
      [(member next '(#\` #\,))
       (define text
         (cond [(and (char=? next #\,) (char? (peek-next in 1)) (char=? (peek-next in 1) #\@))
                (read-char in) (read-char in) ",@"]
               [else (read-char in) (string next)]))
       (make-token in start text '(delimiter scheme-abbreviation))]
      [(peek-string=? in "#\\")
       (define out (open-output-string))
       (define valid? (read-character! in out))
       (make-token in start (get-output-string out)
                   (append '(literal scheme-character)
                           (if valid? '() '(malformed-token scheme-error))))]
      [(or (peek-string=? in "#u8(") (peek-string=? in "#U8("))
       (define text (if (peek-string=? in "#u8(") "#u8(" "#U8("))
       (for ([ch (in-string text)]) (read-char in))
       (make-token in start text '(delimiter scheme-bytevector-open))]
      [(peek-string=? in "#(")
       (read-char in)
       (read-char in)
       (make-token in start "#(" '(delimiter scheme-vector-open))]
      [(or (peek-string=? in "#'") (peek-string=? in "#,") (peek-string=? in "#$"))
       (define text (string (read-char in) (read-char in)))
       (make-token in start text
                   (append '(delimiter scheme-reader-abbreviation)
                           (cond [(string=? text "#'") '(scheme-syntax-quote)]
                                 [(string=? text "#,") '(scheme-unsyntax)]
                                 [else '(scheme-chicken-foreign)])))]
      [else
       (define out (open-output-string))
       (read-atom! in out)
       (define text (get-output-string out))
       (cond [(zero? (string-length text))
              (write-one! in out)
              (make-token in start (get-output-string out) '(malformed-token scheme-error))]
             [else (make-token in start text (classify-atom text dialect))])])))
