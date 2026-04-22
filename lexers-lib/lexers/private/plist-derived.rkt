#lang racket/base

;;;
;;; Plist Derived Tokens
;;;
;;
;; Stateful XML property-list tokenization and reusable plist-specific
;; classifications.

;; plist-derived-token?         : any/c -> boolean?
;;   Recognize a derived plist token.
;; plist-derived-token-text     : plist-derived-token? -> string?
;;   Extract the source text for one derived token.
;; plist-derived-token-start    : plist-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; plist-derived-token-end      : plist-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; plist-derived-token-tags     : plist-derived-token? -> (listof symbol?)
;;   Extract reusable plist classification tags.
;; plist-derived-token-has-tag? : plist-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-plist-derived-reader    : -> (input-port? -> (or/c plist-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived plist tokens.

(provide plist-derived-token?
         plist-derived-token-text
         plist-derived-token-start
         plist-derived-token-end
         plist-derived-token-tags
         plist-derived-token-has-tag?
         make-plist-derived-reader)

(require parser-tools/lex
         racket/list
         racket/set
         racket/string
         "parser-tools-compat.rkt")

;; A derived plist token with reusable tags and source positions.
(struct plist-derived-token (kind text start end tags) #:transparent)

;; plist-derived-token-has-tag? : plist-derived-token? symbol? -> boolean?
;;   Determine whether a derived plist token has a given classification tag.
(define (plist-derived-token-has-tag? token tag)
  (member tag (plist-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Classification tables

;; Built-in XML plist element names.
(define plist-element-names
  (list->set
   '("plist" "dict" "array" "key" "string" "data" "date" "integer"
     "real" "true" "false")))

;; Elements whose body text is semantically meaningful.
(define plist-text-elements
  (list->set
   '("key" "string" "data" "date" "integer" "real")))

;; Characters allowed in XML-like names.
(define (plist-name-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (member ch '(#\- #\_ #\: #\.))))

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

;; read-exactly! : input-port? exact-nonnegative-integer? -> string?
;;   Consume exactly n characters from an input port.
(define (read-exactly! in n)
  (define out
    (open-output-string))
  (for ([i (in-range n)])
    (define next
      (read-char in))
    (when (char? next)
      (write-char next out)))
  (get-output-string out))

;; read-while! : input-port? (char? -> boolean?) -> string?
;;   Consume characters while the predicate holds.
(define (read-while! in pred?)
  (define out
    (open-output-string))
  (let loop ()
    (define next
      (peek-char in))
    (cond
      [(and (char? next) (pred? next))
       (write-char (read-char in) out)
       (loop)]
      [else
       (get-output-string out)])))

;; read-until! : input-port? string? -> (values string? boolean?)
;;   Consume input until sentinel is read or eof is reached.
(define (read-until! in sentinel)
  (define out
    (open-output-string))
  (define sent-len
    (string-length sentinel))
  (let loop ()
    (define next
      (read-char in))
    (cond
      [(eof-object? next)
       (values (get-output-string out) #f)]
      [else
       (write-char next out)
       (define current
         (get-output-string out))
       (cond
         [(and (>= (string-length current) sent-len)
               (string=? (substring current
                                    (- (string-length current) sent-len))
                        sentinel))
          (values current #t)]
         [else
          (loop)])])))

;; make-token-from-text : input-port? position? symbol? string? (listof symbol?) -> plist-derived-token?
;;   Construct a derived plist token with source positions.
(define (make-token-from-text in start-pos kind text tags)
  (plist-derived-token kind
                       text
                       start-pos
                       (current-stream-position in)
                       (remove-duplicates tags)))

;; -----------------------------------------------------------------------------
;; Small scanners

;; read-plist-text! : input-port? -> string?
;;   Consume text content until the next tag start.
(define (read-plist-text! in)
  (read-while! in (lambda (ch) (not (char=? ch #\<)))))

;; read-quoted-value! : input-port? -> (values string? boolean?)
;;   Consume a quoted attribute value and report whether it terminated.
(define (read-quoted-value! in)
  (define out
    (open-output-string))
  (define quote
    (read-char in))
  (write-char quote out)
  (let loop ()
    (define next
      (read-char in))
    (cond
      [(eof-object? next)
       (values (get-output-string out) #f)]
      [else
       (write-char next out)
       (cond
         [(char=? next quote)
          (values (get-output-string out) #t)]
         [else
          (loop)])])))

;; read-unquoted-value! : input-port? -> string?
;;   Consume an unquoted attribute value.
(define (read-unquoted-value! in)
  (read-while! in
               (lambda (ch)
                 (not (or (char-whitespace? ch)
                          (char=? ch #\>)
                          (char=? ch #\/))))))

;; plist-text-tags : (or/c string? #f) string? -> (listof symbol?)
;;   Choose tags for one text segment based on the current containing element.
(define (plist-text-tags current-text-element text)
  (cond
    [(regexp-match? #px"^[ \t\r\n]+$" text)
     '(whitespace)]
    [(not current-text-element)
     '(literal plist-text)]
    [(string=? current-text-element "key")
     '(literal plist-key-text)]
    [(string=? current-text-element "string")
     '(literal plist-string-text)]
    [(string=? current-text-element "data")
     '(literal plist-data-text)]
    [(string=? current-text-element "date")
     '(literal plist-date-text)]
    [(string=? current-text-element "integer")
     '(literal plist-integer-text)]
    [(string=? current-text-element "real")
     '(literal plist-real-text)]
    [else
     '(literal plist-text)]))

;; -----------------------------------------------------------------------------
;; Tag parsing

;; parse-open-or-close-tag! :
;;   input-port? (listof string?) -> (values (listof plist-derived-token?)
;;                                           (listof string?)
;;                                           (or/c string? #f))
;;   Parse one ordinary XML plist tag and update the open-element stack.
(define (parse-open-or-close-tag! in stack)
  (define tokens
    '())
  (define closing?
    #f)
  (define self-closing?
    #f)
  (define normalized-name
    "")

  ;; add-token! : position? symbol? string? (listof symbol?) -> void?
  ;;   Append one token to the tag-local token list.
  (define (add-token! start kind text tags)
    (set! tokens
          (append tokens
                  (list (make-token-from-text in start kind text tags)))))

  (define start
    (current-stream-position in))
  (define start-delimiter
    (cond
      [(and (char? (peek-char in 1))
            (char=? (peek-char in 1) #\/))
       (set! closing? #t)
       (read-exactly! in 2)]
      [else
       (read-exactly! in 1)]))
  (add-token! start 'delimiter start-delimiter '(delimiter))

  (define name-start
    (current-stream-position in))
  (define raw-name
    (read-while! in plist-name-char?))
  (set! normalized-name (string-downcase raw-name))
  (when (positive? (string-length raw-name))
    (define tag-kind
      (if (set-member? plist-element-names normalized-name)
          'keyword
          'identifier))
    (define tag-tags
      (append
       (if (set-member? plist-element-names normalized-name)
           '(keyword plist-tag-name)
           '(identifier plist-tag-name))
       (if closing?
           '(plist-closing-tag-name)
           '())))
    (add-token! name-start tag-kind raw-name tag-tags))

  (let loop ()
    (define next
      (peek-char in))
    (cond
      [(eof-object? next)
       (values tokens stack #f)]
      [(char-whitespace? next)
       (define ws-start
         (current-stream-position in))
       (define ws
         (read-while! in char-whitespace?))
       (add-token! ws-start 'whitespace ws '(whitespace))
       (loop)]
      [(and (char=? next #\/)
            (char? (peek-char in 1))
            (char=? (peek-char in 1) #\>))
       (define delim-start
         (current-stream-position in))
       (set! self-closing? #t)
       (add-token! delim-start 'delimiter (read-exactly! in 2) '(delimiter))
       (define new-stack
         stack)
       (values tokens new-stack #f)]
      [(char=? next #\>)
       (define delim-start
         (current-stream-position in))
       (add-token! delim-start 'delimiter (read-exactly! in 1) '(delimiter))
       (define new-stack
         (cond
           [closing?
            (cond
              [(and (pair? stack)
                    (string=? (car stack) normalized-name))
               (cdr stack)]
              [else
               stack])]
           [(or self-closing?
                (not (set-member? plist-text-elements normalized-name)))
            stack]
           [else
            (cons normalized-name stack)]))
       (values tokens new-stack (and (pair? new-stack) (car new-stack)))]
      [else
       (define attr-start
         (current-stream-position in))
       (define attr-name
         (read-while! in plist-name-char?))
       (cond
         [(string=? attr-name "")
          (add-token! attr-start 'malformed (read-exactly! in 1) '(malformed-token))
          (loop)]
         [else
          (add-token! attr-start 'identifier attr-name '(identifier plist-attribute-name))
          (when (char-whitespace? (peek-char in))
            (define ws-start
              (current-stream-position in))
            (define ws
              (read-while! in char-whitespace?))
            (add-token! ws-start 'whitespace ws '(whitespace)))
          (when (and (char? (peek-char in))
                     (char=? (peek-char in) #\=))
            (define eq-start
              (current-stream-position in))
            (add-token! eq-start 'operator (read-exactly! in 1) '(operator))
            (when (char-whitespace? (peek-char in))
              (define ws-start
                (current-stream-position in))
              (define ws
                (read-while! in char-whitespace?))
              (add-token! ws-start 'whitespace ws '(whitespace)))
            (define value-start
              (current-stream-position in))
            (define value-lead
              (peek-char in))
            (cond
              [(and (char? value-lead)
                    (or (char=? value-lead #\")
                        (char=? value-lead #\')))
               (define-values (value terminated?)
                 (read-quoted-value! in))
               (add-token! value-start
                           (if terminated? 'literal 'malformed)
                           value
                           (append
                            '(plist-attribute-value)
                            (if terminated?
                                '(literal)
                                '(malformed-token))))]
              [else
               (define value
                 (read-unquoted-value! in))
               (add-token! value-start
                           'malformed
                           value
                           '(malformed-token plist-attribute-value))]))
          (loop)])])))

;; -----------------------------------------------------------------------------
;; Reader

;; make-plist-derived-reader : -> (input-port? -> (or/c plist-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived plist tokens.
(define (make-plist-derived-reader)
  (define pending
    '())
  (define open-stack
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
         (define next
           (peek-char in))
         (cond
           [(eof-object? next)
            'eof]
           [(char=? next #\<)
            (cond
              [(and (char? (peek-char in 1))
                    (char=? (peek-char in 1) #\!)
                    (char? (peek-char in 2))
                    (char=? (peek-char in 2) #\-)
                    (char? (peek-char in 3))
                    (char=? (peek-char in 3) #\-))
               (define start
                 (current-stream-position in))
               (define prefix
                 (read-exactly! in 4))
               (define-values (rest terminated?) (read-until! in "-->"))
               (set! pending
                     (list (plist-derived-token (if terminated? 'comment 'malformed)
                                                (string-append prefix rest)
                                                start
                                                (current-stream-position in)
                                                (if terminated?
                                                    '(comment plist-comment)
                                                    '(malformed-token plist-comment)))))
               (loop)]
              [(and (char? (peek-char in 1))
                    (char=? (peek-char in 1) #\?))
               (define start
                 (current-stream-position in))
               (define prefix
                 (read-exactly! in 2))
               (define-values (rest terminated?) (read-until! in "?>"))
               (set! pending
                     (list (plist-derived-token (if terminated? 'keyword 'malformed)
                                                (string-append prefix rest)
                                                start
                                                (current-stream-position in)
                                                (if terminated?
                                                    '(keyword plist-processing-instruction)
                                                    '(malformed-token plist-processing-instruction)))))
               (loop)]
              [(and (char? (peek-char in 1))
                    (char=? (peek-char in 1) #\!))
               (define start
                 (current-stream-position in))
               (define prefix
                 (read-exactly! in 2))
               (define-values (rest terminated?) (read-until! in ">"))
               (set! pending
                     (list (plist-derived-token (if terminated? 'keyword 'malformed)
                                                (string-append prefix rest)
                                                start
                                                (current-stream-position in)
                                                (if terminated?
                                                    '(keyword plist-doctype)
                                                    '(malformed-token plist-doctype)))))
               (loop)]
              [else
               (define-values (tokens new-stack current-text)
                 (parse-open-or-close-tag! in open-stack))
               (set! open-stack new-stack)
               (set! pending tokens)
               (loop)])]
           [else
            (define start
              (current-stream-position in))
            (define text
              (read-plist-text! in))
            (set! pending
                  (list (plist-derived-token (if (regexp-match? #px"^[ \t\r\n]+$" text)
                                                'whitespace
                                                'literal)
                                             text
                                             start
                                             (current-stream-position in)
                                             (plist-text-tags (and (pair? open-stack)
                                                                   (car open-stack))
                                                              text))))
            (loop)])]))))
