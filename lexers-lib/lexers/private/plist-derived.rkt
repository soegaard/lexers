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

;; advance-position : position? string? -> position?
;;   Advance one parser-tools-compatible position across text.
(define (advance-position start text)
  (let loop ([index 0]
             [line  (position-line start)]
             [col   (position-col start)]
             [off   (position-offset start)])
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

;; plist-entity-char? : char? -> boolean?
;;   Recognize one XML entity-name character.
(define (plist-entity-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (member ch '(#\_ #\- #\. #\:))))

;; plist-entity-length : string? exact-nonnegative-integer? -> (or/c exact-positive-integer? #f)
;;   Determine whether source has one XML entity reference at index.
(define (plist-entity-length source index)
  (define len
    (string-length source))
  (cond
    [(or (>= index len)
         (not (char=? (string-ref source index) #\&)))
     #f]
    [else
     (let loop ([i (add1 index)])
       (cond
         [(>= i len)
          #f]
         [else
          (define ch
            (string-ref source i))
          (cond
            [(char=? ch #\;)
             (define body
               (substring source (add1 index) i))
             (cond
               [(regexp-match? #px"^[A-Za-z_:][A-Za-z0-9_.:-]*$" body)
                (+ (- i index) 1)]
               [(regexp-match? #px"^#[0-9]+$" body)
                (+ (- i index) 1)]
               [(regexp-match? #px"^#x[0-9A-Fa-f]+$" body)
                (+ (- i index) 1)]
               [else
                #f])]
            [(or (plist-entity-char? ch)
                 (and (= i (add1 index))
                      (char=? ch #\#))
                 (and (> i (+ index 2))
                      (char=? (string-ref source (add1 index)) #\#)
                      (char=? (string-ref source (+ index 2)) #\x)
                      (or (char-numeric? ch)
                          (member ch '(#\a #\b #\c #\d #\e #\f
                                       #\A #\B #\C #\D #\E #\F)))))
             (loop (add1 i))]
            [else
             #f])]))]))

;; text-entity-tags : (listof symbol?) -> (listof symbol?)
;;   Refine text tags for one XML entity reference.
(define (text-entity-tags base-tags)
  (append '(literal plist-entity)
          (filter (lambda (tag)
                    (member tag
                            '(plist-text
                              plist-key-text
                              plist-string-text
                              plist-data-text
                              plist-date-text
                              plist-integer-text
                              plist-real-text)))
                  base-tags)))

;; text->derived-tokens : input-port? position? (or/c string? #f) string? -> (listof plist-derived-token?)
;;   Split one plist text run into ordinary text and entity-reference tokens.
(define (text->derived-tokens in start-pos current-text-element text)
  (define base-tags
    (plist-text-tags current-text-element text))
  (cond
    [(or (member 'whitespace base-tags)
         (not (regexp-match? #px"&" text)))
     (list (plist-derived-token (if (member 'whitespace base-tags)
                                    'whitespace
                                    'literal)
                                text
                                start-pos
                                (current-stream-position in)
                                base-tags))]
    [else
     (define tokens
       '())
     (define cursor
       0)
     (define token-start
       start-pos)

     ;; add-chunk! : exact-nonnegative-integer? exact-nonnegative-integer? (listof symbol?) -> void?
     ;;   Add one text chunk token and advance the local position cursor.
     (define (add-chunk! start end tags)
       (define chunk
         (substring text start end))
       (define token
         (plist-derived-token 'literal
                              chunk
                              token-start
                              (advance-position token-start chunk)
                              tags))
       (set! tokens (append tokens (list token)))
       (set! token-start (plist-derived-token-end token))
       (set! cursor end))

     (let loop ()
       (cond
         [(>= cursor (string-length text))
          tokens]
         [else
          (define next-entity
            (let find ([i cursor])
              (cond
                [(>= i (string-length text))
                 #f]
                [(char=? (string-ref text i) #\&)
                 i]
                [else
                 (find (add1 i))])))
          (cond
            [(not next-entity)
             (add-chunk! cursor (string-length text) base-tags)
             tokens]
            [(> next-entity cursor)
             (add-chunk! cursor next-entity base-tags)
             (loop)]
            [else
             (define entity-length
               (plist-entity-length text cursor))
             (cond
               [entity-length
                (add-chunk! cursor
                            (+ cursor entity-length)
                            (text-entity-tags base-tags))
                (loop)]
               [else
                (add-chunk! cursor (add1 cursor) base-tags)
                (loop)])])]))]))

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
                  (text->derived-tokens in
                                        start
                                        (and (pair? open-stack)
                                             (car open-stack))
                                        text))
            (loop)])]))))
