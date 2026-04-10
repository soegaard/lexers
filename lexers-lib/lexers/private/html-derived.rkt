#lang racket/base

;;;
;;; HTML Derived Tokens
;;;
;;
;; Stateful HTML tokenization and reusable HTML-specific classifications.

;; html-derived-token?          : any/c -> boolean?
;;   Recognize a derived HTML token.
;; html-derived-token-text      : html-derived-token? -> string?
;;   Extract the source text for one derived HTML token.
;; html-derived-token-start     : html-derived-token? -> position?
;;   Extract the starting source position for one derived HTML token.
;; html-derived-token-end       : html-derived-token? -> position?
;;   Extract the ending source position for one derived HTML token.
;; html-derived-token-tags      : html-derived-token? -> (listof symbol?)
;;   Extract the reusable classification tags for one derived HTML token.
;; html-derived-token-has-tag?  : html-derived-token? symbol? -> boolean?
;;   Determine whether a derived HTML token has a given classification tag.
;; make-html-derived-reader     : -> (input-port? -> (or/c html-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived HTML tokens.

(provide html-derived-token?
         html-derived-token-text
         html-derived-token-start
         html-derived-token-end
         html-derived-token-tags
         html-derived-token-has-tag?
         make-html-derived-reader)

(require parser-tools/lex
         racket/list
         racket/string
         "../css.rkt"
         "../javascript.rkt"
         "parser-tools-compat.rkt"
         "string-compat.rkt")

;; A derived HTML token with reusable tags and source positions.
(struct html-derived-token (kind text start end tags) #:transparent)

;; html-name-char? : char? -> boolean?
;;   Recognize a simplified HTML tag or attribute name character.
(define (html-name-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (member ch '(#\- #\_ #\: #\.))))

;; current-stream-position : input-port? -> position?
;;   Read the current parser-tools-compatible source position from a port.
(define (current-stream-position in)
  (let-values ([(line col offset) (port-next-location in)])
    (define safe-line   (cond [(exact-positive-integer? line)   line]   [else 1]))
    (define safe-col    (cond [(exact-nonnegative-integer? col) col]    [else 0]))
    (define safe-offset (cond [(exact-positive-integer? offset) offset] [else 1]))
    (make-stream-position safe-offset safe-line safe-col)))

;; string-ci-prefix-at? : input-port? string? -> boolean?
;;   Determine whether the input begins with a case-insensitive prefix.
(define (string-ci-prefix-at? in prefix)
  (define n (string-length prefix))
  (let loop ([i 0])
    (cond
      [(= i n) #t]
      [else
       (define next (peek-char in i))
       (and (char? next)
            (char-ci=? next (string-ref prefix i))
            (loop (add1 i)))])))

;; read-while! : input-port? (char? -> boolean?) -> string?
;;   Consume characters while the predicate holds.
(define (read-while! in pred?)
  (define out (open-output-string))
  (let loop ()
    (define next (peek-char in))
    (cond
      [(and (char? next) (pred? next))
       (write-char (read-char in) out)
       (loop)]
      [else
       (get-output-string out)])))

;; read-exactly! : input-port? exact-nonnegative-integer? -> string?
;;   Consume exactly n characters from an input port.
(define (read-exactly! in n)
  (define out (open-output-string))
  (for ([i (in-range n)])
    (define next (read-char in))
    (when (char? next)
      (write-char next out)))
  (get-output-string out))

;; read-until! : input-port? string? -> string?
;;   Consume input until the sentinel suffix is read or eof is reached.
(define (read-until! in sentinel)
  (define out (open-output-string))
  (define sent-len (string-length sentinel))
  (let loop ()
    (define next (read-char in))
    (cond
      [(eof-object? next)
       (get-output-string out)]
      [else
       (write-char next out)
       (define current (get-output-string out))
       (if (and (>= (string-length current) sent-len)
                (substring-ci=? current
                                (- (string-length current) sent-len)
                                sentinel))
           current
           (loop))])))

;; derived-token : input-port? position? symbol? string? (listof symbol?) -> html-derived-token?
;;   Construct a derived HTML token with source positions.
(define (derived-token in start-pos kind text tags)
  (html-derived-token kind
                      text
                      start-pos
                      (current-stream-position in)
                      (remove-duplicates tags)))

;; make-derived-from-text : string? position? position? symbol? (listof symbol?) -> html-derived-token?
;;   Construct a derived token from explicit text and positions.
(define (make-derived-from-text text start-pos end-pos kind tags)
  (html-derived-token kind text start-pos end-pos (remove-duplicates tags)))

;; adjust-position : position? position? -> position?
;;   Translate a delegated token position into the surrounding HTML source positions.
(define (adjust-position base-pos local-pos)
  (make-stream-position (+ (position-offset base-pos)
                           (sub1 (position-offset local-pos)))
                        (+ (position-line base-pos)
                           (sub1 (position-line local-pos)))
                        (if (= (position-line local-pos) 1)
                            (+ (position-col base-pos)
                               (position-col local-pos))
                            (position-col local-pos))))

;; html-text-break? : char? -> boolean?
;;   Determine whether text mode should stop before the character.
(define (html-text-break? ch)
  (or (char=? ch #\<)
      (char=? ch #\&)))

;; read-html-text! : input-port? -> string?
;;   Consume text-mode content until the next tag or entity.
(define (read-html-text! in)
  (read-while! in (lambda (ch) (not (html-text-break? ch)))))

;; read-html-entity! : input-port? -> string?
;;   Consume a simple HTML entity-like sequence.
(define (read-html-entity! in)
  (define out (open-output-string))
  (write-char (read-char in) out)
  (let loop ()
    (define next (peek-char in))
    (cond
      [(eof-object? next)
       (get-output-string out)]
      [(char=? next #\;)
       (write-char (read-char in) out)
       (get-output-string out)]
      [(or (char-whitespace? next)
           (char=? next #\<)
           (char=? next #\&))
       (get-output-string out)]
      [else
       (write-char (read-char in) out)
       (loop)])))

;; read-html-quoted-attribute-value! : input-port? -> (values string? boolean?)
;;   Consume a quoted HTML attribute value and report whether it terminated cleanly.
(define (read-html-quoted-attribute-value! in)
  (define out (open-output-string))
  (define quote-char (read-char in))
  (write-char quote-char out)
  (let loop ()
    (define next (read-char in))
    (cond
      [(eof-object? next)
       (values (get-output-string out) #f)]
      [else
       (write-char next out)
       (cond
         [(char=? next quote-char)
          (values (get-output-string out) #t)]
         [(or (char=? next #\newline)
              (char=? next #\return))
          (values (get-output-string out) #f)]
         [else
          (loop)])])))

;; read-html-unquoted-attribute-value! : input-port? -> string?
;;   Consume an unquoted HTML attribute value.
(define (read-html-unquoted-attribute-value! in)
  (read-while! in
               (lambda (ch)
                 (not (or (char-whitespace? ch)
                          (char=? ch #\>)
                          (char=? ch #\/))))))

;; enqueue-html-whitespace! : input-port? (listof html-derived-token?) -> (listof html-derived-token?)
;;   Read a whitespace segment inside a tag and append a whitespace token when non-empty.
(define (enqueue-html-whitespace! in tokens)
  (define start-pos (current-stream-position in))
  (define text (read-while! in char-whitespace?))
  (cond
    [(string=? text "") tokens]
    [else
     (append tokens
             (list (make-derived-from-text text
                                           start-pos
                                           (current-stream-position in)
                                           'whitespace
                                           '(whitespace))))]))

;; parse-html-tag! : input-port? -> (values (listof html-derived-token?) string? boolean? boolean?)
;;   Parse one HTML tag and return its tokens, tag name, closing flag, and self-closing flag.
(define (parse-html-tag! in)
  (define tokens '())
  (define closing? #f)
  (define self-closing? #f)
  (define tag-name "")
  (define open-start (current-stream-position in))
  (cond
    [(string-ci-prefix-at? in "</")
     (set! closing? #t)
     (set! tokens
           (append tokens
                   (list (make-derived-from-text (read-exactly! in 2)
                                                 open-start
                                                 (current-stream-position in)
                                                 'delimiter
                                                 '(delimiter)))))]
    [else
     (set! tokens
           (append tokens
                   (list (make-derived-from-text (read-exactly! in 1)
                                                 open-start
                                                 (current-stream-position in)
                                                 'delimiter
                                                 '(delimiter)))) )])
  (define name-start (current-stream-position in))
  (define raw-name (read-while! in html-name-char?))
  (set! tag-name (string-downcase raw-name))
  (when (positive? (string-length raw-name))
    (set! tokens
          (append tokens
                  (list (make-derived-from-text raw-name
                                                name-start
                                                (current-stream-position in)
                                                'identifier
                                                (if closing?
                                                    '(identifier html-closing-tag-name)
                                                    '(identifier html-tag-name)))))))
  (let loop ()
    (define next (peek-char in))
    (cond
      [(eof-object? next)
       (values tokens tag-name closing? self-closing?)]
      [(or (char=? next #\newline)
           (char=? next #\return))
       (values tokens tag-name closing? self-closing?)]
      [(char-whitespace? next)
       (set! tokens (enqueue-html-whitespace! in tokens))
       (loop)]
      [(string-ci-prefix-at? in "/>")
       (define delim-start (current-stream-position in))
       (set! self-closing? #t)
       (set! tokens
             (append tokens
                     (list (make-derived-from-text (read-exactly! in 2)
                                                   delim-start
                                                   (current-stream-position in)
                                                   'delimiter
                                                   '(delimiter)))))
       (values tokens tag-name closing? self-closing?)]
      [(char=? next #\>)
       (define delim-start (current-stream-position in))
       (set! tokens
             (append tokens
                     (list (make-derived-from-text (read-exactly! in 1)
                                                   delim-start
                                                   (current-stream-position in)
                                                   'delimiter
                                                   '(delimiter)))))
       (values tokens tag-name closing? self-closing?)]
      [else
       (define attr-start (current-stream-position in))
       (define attr-name (read-while! in html-name-char?))
       (cond
         [(string=? attr-name "")
          (define bad-start (current-stream-position in))
          (set! tokens
                (append tokens
                        (list (make-derived-from-text (read-exactly! in 1)
                                                      bad-start
                                                      (current-stream-position in)
                                                      'malformed
                                                      '(malformed-token)))))
          (loop)]
         [else
          (set! tokens
                (append tokens
                        (list (make-derived-from-text attr-name
                                                      attr-start
                                                      (current-stream-position in)
                                                      'identifier
                                                      '(identifier html-attribute-name)))))
          (set! tokens (enqueue-html-whitespace! in tokens))
          (when (and (char? (peek-char in))
                     (char=? (peek-char in) #\=))
            (define eq-start (current-stream-position in))
            (set! tokens
                  (append tokens
                          (list (make-derived-from-text (read-exactly! in 1)
                                                        eq-start
                                                        (current-stream-position in)
                                                        'operator
                                                        '(operator)))))
            (set! tokens (enqueue-html-whitespace! in tokens))
            (define value-start (current-stream-position in))
            (define next-value (peek-char in))
            (cond
              [(and (char? next-value)
                    (or (char=? next-value #\")
                        (char=? next-value #\')))
               (define-values (value terminated?) (read-html-quoted-attribute-value! in))
               (set! tokens
                     (append tokens
                             (list (make-derived-from-text value
                                                           value-start
                                                           (current-stream-position in)
                                                           (if terminated? 'literal 'malformed)
                                                           (append
                                                            '(html-attribute-value)
                                                            (if terminated?
                                                                '(literal)
                                                                '(malformed-token)))))))
               (unless terminated?
                 (values tokens tag-name closing? self-closing?))]
              [else
               (define value (read-html-unquoted-attribute-value! in))
               (when (positive? (string-length value))
                 (set! tokens
                       (append tokens
                               (list (make-derived-from-text value
                                                             value-start
                                                             (current-stream-position in)
                                                             'literal
                                                             '(literal html-attribute-value))))))]))
          (loop)])])))

;; read-embedded-body! : input-port? string? -> string?
;;   Consume embedded CSS or JavaScript until the matching closing tag begins.
(define (read-embedded-body! in tag-name)
  (define out (open-output-string))
  (define close-mark (string-append "</" tag-name))
  (let loop ([in-single?       #f]
             [in-double?       #f]
             [in-backtick?     #f]
             [in-line-comment? #f]
             [in-block-comment? #f]
             [escaped?         #f])
    (cond
      [(and (not in-single?)
            (not in-double?)
            (not in-backtick?)
            (not in-line-comment?)
            (not in-block-comment?)
            (string-ci-prefix-at? in close-mark))
       (get-output-string out)]
      [else
       (define next (read-char in))
       (cond
         [(eof-object? next)
          (get-output-string out)]
         [else
          (write-char next out)
          (define next2 (peek-char in))
          (cond
            [in-line-comment?
             (cond
               [(or (char=? next #\newline)
                    (char=? next #\return))
                (loop in-single? in-double? in-backtick? #f in-block-comment? #f)]
               [else
                (loop in-single? in-double? in-backtick? #t in-block-comment? #f)])]
            [in-block-comment?
             (cond
               [(and (char=? next #\*)
                     (char? next2)
                     (char=? next2 #\/))
                (write-char (read-char in) out)
                (loop in-single? in-double? in-backtick? #f #f #f)]
               [else
                (loop in-single? in-double? in-backtick? #f #t #f)])]
            [escaped?
             (loop in-single? in-double? in-backtick? #f #f #f)]
            [in-single?
             (cond
               [(char=? next #\\)
                (loop #t in-double? in-backtick? #f #f #t)]
               [(char=? next #\')
                (loop #f in-double? in-backtick? #f #f #f)]
               [else
                (loop #t in-double? in-backtick? #f #f #f)])]
            [in-double?
             (cond
               [(char=? next #\\)
                (loop in-single? #t in-backtick? #f #f #t)]
               [(char=? next #\")
                (loop in-single? #f in-backtick? #f #f #f)]
               [else
                (loop in-single? #t in-backtick? #f #f #f)])]
            [in-backtick?
             (cond
               [(char=? next #\\)
                (loop in-single? in-double? #t #f #f #t)]
               [(char=? next #\`)
                (loop in-single? in-double? #f #f #f #f)]
               [else
                (loop in-single? in-double? #t #f #f #f)])]
            [else
             (cond
               [(and (char=? next #\/)
                     (char? next2)
                     (char=? next2 #\/))
                (write-char (read-char in) out)
                (loop in-single? in-double? in-backtick? #t #f #f)]
               [(and (char=? next #\/)
                     (char? next2)
                     (char=? next2 #\*))
                (write-char (read-char in) out)
                (loop in-single? in-double? in-backtick? #f #t #f)]
               [(char=? next #\')
                (loop #t in-double? in-backtick? #f #f #f)]
               [(char=? next #\")
                (loop in-single? #t in-backtick? #f #f #f)]
               [(char=? next #\`)
                (loop in-single? in-double? #t #f #f #f)]
               [else
                (loop #f #f #f #f #f #f)])])])])))

;; Embedded-language punctuation and operator tables.
(define embedded-delimiters
  '("." "," ":" ";" "(" ")" "[" "]" "{" "}" "`"))

(define embedded-operators
  '("=" "+" "-" "*" "/" "%" "!" "?" "<" ">" "<=" ">=" "==" "!=" "===" "!=="
    "&&" "||" "??" "&" "|" "^" "~" "+=" "-=" "*=" "/=" "%="))

;; whitespace-text? : string? -> boolean?
;;   Determine whether a token text consists only of whitespace.
(define (whitespace-text? text)
  (and (not (string=? text ""))
       (regexp-match? #px"^\\s+$" text)))

;; css-derived-generic-tags : css-derived-token? -> (listof symbol?)
;;   Infer shared category tags from one derived CSS token.
(define (css-derived-generic-tags token)
  (define text (css-derived-token-text token))
  (define tags (css-derived-token-tags token))
  (cond
    [(member 'comment tags) '(comment)]
    [(whitespace-text? text) '(whitespace)]
    [(or (member 'color-literal tags)
         (member 'color-function tags)
         (member 'gradient-function tags)
         (member 'length-dimension tags)
         (member 'declaration-value-token tags)
         (regexp-match? #px"^(#|\"|').*" text))
     '(literal)]
    [(or (member 'selector-token tags)
         (member 'property-name tags)
         (member 'property-name-candidate tags)
         (member 'custom-property-name tags)
         (member 'at-rule-name tags)
         (regexp-match? #px"^(--|[-_A-Za-z])[-_A-Za-z0-9:\\.-]*$" text))
     '(identifier)]
    [(member text embedded-delimiters) '(delimiter)]
    [(member text embedded-operators)  '(operator)]
    [else                              '()]))

;; javascript-derived-generic-tags : javascript-derived-token? -> (listof symbol?)
;;   Infer shared category tags from one derived JavaScript token.
(define (javascript-derived-generic-tags token)
  (define text (javascript-derived-token-text token))
  (define tags (javascript-derived-token-tags token))
  (cond
    [(member 'comment tags) '(comment)]
    [(whitespace-text? text) '(whitespace)]
    [(member 'keyword tags) '(keyword)]
    [(or (member 'identifier tags)
         (member 'declaration-name tags)
         (member 'parameter-name tags)
         (member 'property-name tags)
         (member 'private-name tags)
         (member 'object-key tags)
         (member 'method-name tags))
     '(identifier)]
    [(or (member 'string-literal tags)
         (member 'numeric-literal tags)
         (member 'regex-literal tags)
         (member 'template-literal tags)
         (member 'template-chunk tags))
     '(literal)]
    [(member text embedded-delimiters) '(delimiter)]
    [(member text embedded-operators)  '(operator)]
    [else                              '()]))

;; css-derived->html-derived : css-derived-token? position? -> html-derived-token?
;;   Rewrap one derived CSS token as an embedded HTML derived token.
(define (css-derived->html-derived token base-pos)
  (make-derived-from-text
   (css-derived-token-text token)
   (adjust-position base-pos (css-derived-token-start token))
   (adjust-position base-pos (css-derived-token-end token))
   'embedded-css
   (append '(embedded-css)
           (css-derived-generic-tags token)
           (css-derived-token-tags token))))

;; javascript-derived->html-derived : javascript-derived-token? position? -> html-derived-token?
;;   Rewrap one derived JavaScript token as an embedded HTML derived token.
(define (javascript-derived->html-derived token base-pos)
  (make-derived-from-text
   (javascript-derived-token-text token)
   (adjust-position base-pos (javascript-derived-token-start token))
   (adjust-position base-pos (javascript-derived-token-end token))
   'embedded-javascript
   (append '(embedded-javascript)
           (javascript-derived-generic-tags token)
           (javascript-derived-token-tags token))))

;; enqueue-embedded-body! : input-port? symbol? (listof html-derived-token?) -> (listof html-derived-token?)
;;   Read an embedded body and append delegated derived tokens.
(define (enqueue-embedded-body! in mode tokens)
  (define body-start (current-stream-position in))
  (define body-text
    (case mode
      [(style-body)  (read-embedded-body! in "style")]
      [else          (read-embedded-body! in "script")]))
  (append
   tokens
   (case mode
     [(style-body)
      (map (lambda (token)
             (css-derived->html-derived token body-start))
           (css-string->derived-tokens body-text))]
     [else
      (map (lambda (token)
             (javascript-derived->html-derived token body-start))
           (javascript-string->derived-tokens body-text #:jsx? #f))])))

;; make-html-derived-reader : -> (input-port? -> (or/c html-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived HTML tokens.
(define (make-html-derived-reader)
  (define pending-tokens '())
  (define mode 'text)
  (define (enqueue! tokens)
    (set! pending-tokens (append pending-tokens tokens)))
  (define (dequeue!)
    (define next-token (car pending-tokens))
    (set! pending-tokens (cdr pending-tokens))
    next-token)
  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-html-derived-reader "input-port?" in))
    (port-count-lines! in)
    (cond
      [(pair? pending-tokens)
       (dequeue!)]
      [(eq? mode 'style-body)
       (set! mode 'text)
       (enqueue! (enqueue-embedded-body! in 'style-body '()))
       (if (pair? pending-tokens) (dequeue!) 'eof)]
      [(eq? mode 'script-body)
       (set! mode 'text)
       (enqueue! (enqueue-embedded-body! in 'script-body '()))
       (if (pair? pending-tokens) (dequeue!) 'eof)]
      [else
       (define start-pos (current-stream-position in))
       (define next (peek-char in))
       (cond
         [(eof-object? next) 'eof]
         [(string-ci-prefix-at? in "<!--")
          (derived-token in start-pos 'comment (read-until! in "-->") '(comment))]
         [(and (string-ci-prefix-at? in "<!")
               (not (string-ci-prefix-at? in "<!--")))
          (derived-token in start-pos 'doctype (read-until! in ">") '(keyword html-doctype))]
         [(char=? next #\<)
          (define-values (tag-tokens tag-name closing? self-closing?)
            (parse-html-tag! in))
          (unless (or closing? self-closing?)
            (case (string-downcase tag-name)
              [("style")  (set! mode 'style-body)]
              [("script") (set! mode 'script-body)]
              [else       (void)]))
          (cond
            [(pair? tag-tokens)
             (enqueue! (cdr tag-tokens))
             (car tag-tokens)]
            [else
             (make-derived-from-text "<"
                                     start-pos
                                     (current-stream-position in)
                                     'malformed
                                     '(malformed-token))])]
         [(char=? next #\&)
          (derived-token in start-pos 'entity (read-html-entity! in) '(literal html-entity))]
         [else
          (derived-token in start-pos 'text (read-html-text! in) '(literal html-text))])])))

;; html-derived-token-has-tag? : html-derived-token? symbol? -> boolean?
;;   Determine whether a derived HTML token has a given classification tag.
(define (html-derived-token-has-tag? token tag)
  (member tag (html-derived-token-tags token)))
