#lang racket/base

;;;
;;; YAML Derived Tokens
;;;
;;
;; Stateful YAML tokenization and reusable YAML-specific classifications.

;; yaml-derived-token?         : any/c -> boolean?
;;   Recognize a derived YAML token.
;; yaml-derived-token-text     : yaml-derived-token? -> string?
;;   Extract the source text for one derived YAML token.
;; yaml-derived-token-start    : yaml-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; yaml-derived-token-end      : yaml-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; yaml-derived-token-tags     : yaml-derived-token? -> (listof symbol?)
;;   Extract reusable YAML classification tags.
;; yaml-derived-token-has-tag? : yaml-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-yaml-derived-reader    : -> (input-port? -> (or/c yaml-derived-token? 'eof))
;;   Construct a stateful YAML derived-token reader.

(provide yaml-derived-token?
         yaml-derived-token-text
         yaml-derived-token-start
         yaml-derived-token-end
         yaml-derived-token-tags
         yaml-derived-token-has-tag?
         make-yaml-derived-reader)

(require parser-tools/lex
         racket/list
         racket/string
         "parser-tools-compat.rkt")

;; A YAML token plus reusable tags.
(struct yaml-derived-token (kind text start end tags) #:transparent)

;; yaml-derived-token-has-tag? : yaml-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (yaml-derived-token-has-tag? token tag)
  (member tag (yaml-derived-token-tags token)))

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
;;   Advance one source position by the exact characters in text.
(define (advance-position pos text)
  (let loop ([i 0]
             [line (position-line pos)]
             [col  (position-col pos)]
             [off  (position-offset pos)])
    (cond
      [(= i (string-length text))
       (make-stream-position off line col)]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(char=? ch #\return)
          (define next-i
            (cond
              [(and (< (add1 i) (string-length text))
                    (char=? (string-ref text (add1 i)) #\newline))
               (+ i 2)]
              [else
               (add1 i)]))
          (loop next-i
                (add1 line)
                0
                (+ off (- next-i i)))]
         [(char=? ch #\newline)
          (loop (add1 i)
                (add1 line)
                0
                (add1 off))]
         [else
          (loop (add1 i)
                line
                (add1 col)
                (add1 off))])])))

;; make-token-from-text : position? position? string? (listof symbol?) -> yaml-derived-token?
;;   Construct one derived token from explicit positions, text, and tags.
(define (make-token-from-text start-pos end-pos text tags)
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
  (yaml-derived-token kind
                      text
                      start-pos
                      end-pos
                      (remove-duplicates tags)))

;; -----------------------------------------------------------------------------
;; Line helpers

;; read-line-fragment : input-port? -> (or/c string? eof-object?)
;;   Read one physical line including its newline, if any.
(define (read-line-fragment in)
  (define out
    (open-output-string))
  (let loop ([seen-any? #f])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       (cond
         [seen-any? (get-output-string out)]
         [else      next])]
      [(char=? next #\return)
       (write-one! in out)
       (when (and (char? (peek-next in))
                  (char=? (peek-next in) #\newline))
         (write-one! in out))
       (get-output-string out)]
      [(char=? next #\newline)
       (write-one! in out)
       (get-output-string out)]
      [else
       (write-one! in out)
       (loop #t)])))

;; split-line-ending : string? -> (values string? string?)
;;   Split one physical line into its body and newline text.
(define (split-line-ending line)
  (define len
    (string-length line))
  (cond
    [(and (>= len 2)
          (char=? (string-ref line (- len 2)) #\return)
          (char=? (string-ref line (sub1 len)) #\newline))
     (values (substring line 0 (- len 2))
             "\r\n")]
    [(and (positive? len)
          (char=? (string-ref line (sub1 len)) #\newline))
     (values (substring line 0 (sub1 len))
             "\n")]
    [(and (positive? len)
          (char=? (string-ref line (sub1 len)) #\return))
     (values (substring line 0 (sub1 len))
             "\r")]
    [else
     (values line "")]))

;; local-position : position? exact-nonnegative-integer? -> position?
;;   Compute one position within the current physical line body.
(define (local-position base-pos offset)
  (make-stream-position (+ (position-offset base-pos) offset)
                        (position-line base-pos)
                        (+ (position-col base-pos) offset)))

;; body-end-position : position? exact-nonnegative-integer? -> position?
;;   Compute the position at the end of one physical line body.
(define (body-end-position base-pos body-length)
  (local-position base-pos body-length))

;; -----------------------------------------------------------------------------
;; Classification helpers

;; whitespace-char? : char? -> boolean?
;;   Recognize YAML inline whitespace.
(define (whitespace-char? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)))

;; blank-line-body? : string? -> boolean?
;;   Determine whether the line body contains only whitespace.
(define (blank-line-body? body)
  (for/and ([ch (in-string body)])
    (whitespace-char? ch)))

;; count-leading-indent : string? -> exact-nonnegative-integer?
;;   Count leading space indentation for one line body.
(define (count-leading-indent body)
  (let loop ([i 0])
    (cond
      [(>= i (string-length body)) i]
      [(char=? (string-ref body i) #\space)
       (loop (add1 i))]
      [else
       i])))

;; plain-null? : string? -> boolean?
;;   Recognize a null-like plain scalar.
(define (plain-null? text)
  (member text '("null" "Null" "NULL" "~")))

;; plain-bool? : string? -> boolean?
;;   Recognize a boolean-like plain scalar.
(define (plain-bool? text)
  (member text '("true" "True" "TRUE" "false" "False" "FALSE")))

;; plain-number? : string? -> boolean?
;;   Recognize a practical YAML numeric scalar.
(define (plain-number? text)
  (or (regexp-match? #px"^-?(0|[1-9][0-9_]*)$" text)
      (regexp-match? #px"^-?(0|[1-9][0-9_]*)(\\.[0-9_]+)?([eE][+-]?[0-9_]+)?$"
                     text)))

;; plain-scalar-tags : string? -> (listof symbol?)
;;   Classify one plain scalar token.
(define (plain-scalar-tags text)
  (append
   '(literal yaml-plain-scalar)
   (cond
     [(plain-null? text)   '(yaml-null)]
     [(plain-bool? text)   '(yaml-boolean)]
     [(plain-number? text) '(yaml-number)]
     [else                 '()])))

;; comment-start? : string? exact-nonnegative-integer? -> boolean?
;;   Determine whether # starts a YAML comment at index i.
(define (comment-start? body i)
  (and (char=? (string-ref body i) #\#)
       (or (zero? i)
           (whitespace-char? (string-ref body (sub1 i))))))

;; indicator-separator? : string? exact-nonnegative-integer? -> boolean?
;;   Determine whether an indicator is followed by separation or line end.
(define (indicator-separator? body i)
  (define next-i
    (add1 i))
  (cond
    [(= next-i (string-length body)) #t]
    [else
     (define next-ch
       (string-ref body next-i))
     (or (whitespace-char? next-ch)
         (member next-ch '(#\[ #\] #\{ #\} #\, #\#)))]))

;; plain-stop? : string? exact-nonnegative-integer? -> boolean?
;;   Determine whether plain-scalar scanning must stop at index i.
(define (plain-stop? body i)
  (define ch
    (string-ref body i))
  (cond
    [(whitespace-char? ch) #t]
    [(member ch '(#\[ #\] #\{ #\} #\, #\" #\' #\& #\* #\! #\| #\>)) #t]
    [(comment-start? body i) #t]
    [(char=? ch #\:)
     (or (= (add1 i) (string-length body))
         (whitespace-char? (string-ref body (add1 i)))
         (member (string-ref body (add1 i))
                 '(#\[ #\] #\{ #\} #\, #\#)))]
    [else
     #f]))

;; scalar-like-token? : yaml-derived-token? -> boolean?
;;   Determine whether one token can be re-tagged as a mapping key.
(define (scalar-like-token? token)
  (or (yaml-derived-token-has-tag? token 'yaml-plain-scalar)
      (yaml-derived-token-has-tag? token 'yaml-string-literal)
      (yaml-derived-token-has-tag? token 'yaml-boolean)
      (yaml-derived-token-has-tag? token 'yaml-null)
      (yaml-derived-token-has-tag? token 'yaml-number)))

;; add-key-tag-to-recent-scalar : (listof yaml-derived-token?) -> (listof yaml-derived-token?)
;;   Add yaml-key-scalar to the most recent scalar-like token in reverse order.
(define (add-key-tag-to-recent-scalar rev-tokens)
  (let loop ([prefix '()] [rest rev-tokens])
    (cond
      [(null? rest)
       rev-tokens]
      [else
       (define token
         (car rest))
       (cond
         [(or (yaml-derived-token-has-tag? token 'whitespace)
              (yaml-derived-token-has-tag? token 'comment))
          (loop (cons token prefix) (cdr rest))]
         [(scalar-like-token? token)
          (append (reverse prefix)
                  (cons (struct-copy yaml-derived-token
                                     token
                                     [tags (remove-duplicates
                                            (cons 'yaml-key-scalar
                                                  (yaml-derived-token-tags token)))])
                        (cdr rest)))]
         [else
          rev-tokens])])))

;; -----------------------------------------------------------------------------
;; Local scanners

;; scan-name-like : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Scan one anchor, alias, tag, or directive token.
(define (scan-name-like body start)
  (let loop ([i start])
    (cond
      [(>= i (string-length body))
       i]
      [else
       (define ch
         (string-ref body i))
       (cond
         [(or (whitespace-char? ch)
              (member ch '(#\# #\[ #\] #\{ #\} #\, #\:)))
          i]
         [else
          (loop (add1 i))])])))

;; hex-digit? : char? -> boolean?
;;   Determine whether one character is a hexadecimal digit.
(define (hex-digit? ch)
  (and (char? ch)
       (or (char-numeric? ch)
           (char<=? #\a (char-downcase ch) #\f))))

;; valid-yaml-double-escape-end : string? exact-nonnegative-integer? -> (or/c exact-nonnegative-integer? #f)
;;   Return the index after one valid YAML double-quoted escape, if present.
(define (valid-yaml-double-escape-end body start)
  (define len
    (string-length body))
  (cond
    [(>= (add1 start) len)
     #f]
    [else
     (define next
       (string-ref body (add1 start)))
     (cond
       [(member next
                (list #\0 #\a #\b #\t #\newline #\v #\f #\r #\e #\space #\" #\/ #\\
                      #\N #\_ #\L #\P))
        (+ start 2)]
       [(char=? next #\x)
        (and (<= (+ start 4) len)
             (hex-digit? (string-ref body (+ start 2)))
             (hex-digit? (string-ref body (+ start 3)))
             (+ start 4))]
       [(char=? next #\u)
        (and (<= (+ start 6) len)
             (for/and ([i (in-range (+ start 2) (+ start 6))])
               (hex-digit? (string-ref body i)))
             (+ start 6))]
       [(char=? next #\U)
        (and (<= (+ start 10) len)
             (for/and ([i (in-range (+ start 2) (+ start 10))])
               (hex-digit? (string-ref body i)))
             (+ start 10))]
       [else
        #f])]))

;; scan-quoted-string : string? exact-nonnegative-integer? char? -> (values exact-nonnegative-integer? boolean? boolean?)
;;   Scan one single-line quoted scalar and report end index, termination, and validity.
(define (scan-quoted-string body start quote)
  (let loop ([i (add1 start)]
             [valid? #t])
    (cond
      [(>= i (string-length body))
       (values (string-length body) #f valid?)]
      [else
       (define ch
         (string-ref body i))
       (cond
         [(char=? ch quote)
          (cond
            [(and (char=? quote #\')
                  (< (add1 i) (string-length body))
                  (char=? (string-ref body (add1 i)) #\'))
             (loop (+ i 2) valid?)]
            [else
             (values (add1 i) #t valid?)])]
         [(and (char=? quote #\")
               (char=? ch #\\))
          (define end
            (valid-yaml-double-escape-end body i))
          (cond
            [end
             (loop end valid?)]
            [else
             (loop (min (+ i 2) (string-length body)) #f)])]
         [else
          (loop (add1 i) valid?)])])))

;; scan-plain-scalar : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Scan one plain scalar token.
(define (scan-plain-scalar body start)
  (let loop ([i start])
    (cond
      [(>= i (string-length body))
       i]
      [(plain-stop? body i)
       i]
      [else
       (loop (add1 i))])))

;; scan-block-header : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Scan one block-scalar header token.
(define (scan-block-header body start)
  (let loop ([i (add1 start)])
    (cond
      [(>= i (string-length body))
       i]
      [else
       (define ch
         (string-ref body i))
       (cond
         [(or (char-numeric? ch)
              (member ch '(#\+ #\-)))
          (loop (add1 i))]
         [else
          i])])))

;; block-indent-after-header : string? exact-nonnegative-integer? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Determine the minimum content indentation for a block scalar.
(define (block-indent-after-header body start current-indent)
  (define header-end
    (scan-block-header body start))
  (define header-text
    (substring body start header-end))
  (define digit-match
    (regexp-match #px"[1-9]" header-text))
  (cond
    [digit-match
     (+ current-indent
        (string->number (car digit-match)))]
    [else
     (add1 current-indent)]))

;; tokenize-line : string? position? (or/c exact-nonnegative-integer? #f) -> (values (listof yaml-derived-token?) (or/c exact-nonnegative-integer? #f))
;;   Tokenize one physical YAML line and update block-scalar state.
(define (tokenize-line line base-pos block-scalar-indent)
  (define-values (body newline-text)
    (split-line-ending line))
  (define body-length
    (string-length body))
  (define rev-tokens
    '())
  (define next-block-scalar-indent
    block-scalar-indent)
  (define (emit-body! start end tags)
    (define text
      (substring body start end))
    (set! rev-tokens
          (cons (make-token-from-text (local-position base-pos start)
                                      (local-position base-pos end)
                                      text
                                      tags)
                rev-tokens)))
  (define (emit-newline!)
    (unless (string=? newline-text "")
      (define start-pos
        (body-end-position base-pos body-length))
      (define end-pos
        (advance-position start-pos newline-text))
      (set! rev-tokens
            (cons (make-token-from-text start-pos
                                        end-pos
                                        newline-text
                                        '(whitespace yaml-whitespace))
                  rev-tokens))))
  (define current-indent
    (count-leading-indent body))
  (cond
    [(and block-scalar-indent
          (or (blank-line-body? body)
              (>= current-indent block-scalar-indent)))
     (unless (zero? body-length)
       (emit-body! 0 body-length '(literal yaml-block-scalar-content)))
     (emit-newline!)
     (values (reverse rev-tokens)
             next-block-scalar-indent)]
    [else
     (when block-scalar-indent
       (set! next-block-scalar-indent #f))
     (let loop ([i 0] [line-content-start? #t])
       (cond
         [(>= i body-length)
          (emit-newline!)
          (values (reverse rev-tokens)
                  next-block-scalar-indent)]
         [else
          (define ch
            (string-ref body i))
          (cond
            [(whitespace-char? ch)
             (define start i)
             (let scan ([j i])
               (cond
                 [(and (< j body-length)
                       (whitespace-char? (string-ref body j)))
                  (scan (add1 j))]
                 [else
                  (emit-body! start j '(whitespace yaml-whitespace))
                  (loop j line-content-start?)]))]
            [(comment-start? body i)
             (emit-body! i body-length '(comment yaml-comment))
             (emit-newline!)
             (values (reverse rev-tokens)
                     next-block-scalar-indent)]
            [(and line-content-start?
                  (<= (+ i 3) body-length)
                  (member (substring body i (+ i 3)) '("---" "..."))
                  (or (= (+ i 3) body-length)
                      (whitespace-char? (string-ref body (+ i 3)))
                      (char=? (string-ref body (+ i 3)) #\#)))
             (emit-body! i (+ i 3) '(delimiter yaml-document-marker))
             (loop (+ i 3) #f)]
            [(and line-content-start?
                  (char=? ch #\%))
             (define end
               (scan-name-like body i))
             (emit-body! i end '(keyword yaml-directive))
             (loop end #f)]
            [(and line-content-start?
                  (char=? ch #\-)
                  (indicator-separator? body i))
             (emit-body! i (add1 i) '(delimiter yaml-sequence-indicator))
             (loop (add1 i) #f)]
            [(and line-content-start?
                  (char=? ch #\?)
                  (indicator-separator? body i))
             (emit-body! i (add1 i) '(delimiter yaml-key-indicator))
             (loop (add1 i) #f)]
            [(member ch '(#\[ #\] #\{ #\} #\,))
             (emit-body! i (add1 i) '(delimiter yaml-flow-delimiter))
             (loop (add1 i) #f)]
            [(and (char=? ch #\:)
                  (or (= (add1 i) body-length)
                      (whitespace-char? (string-ref body (add1 i)))
                      (member (string-ref body (add1 i))
                              '(#\[ #\] #\{ #\} #\, #\#))))
             (set! rev-tokens
                   (add-key-tag-to-recent-scalar rev-tokens))
             (emit-body! i (add1 i) '(delimiter yaml-value-indicator))
             (loop (add1 i) #f)]
            [(or (char=? ch #\|)
                 (char=? ch #\>))
             (define end
               (scan-block-header body i))
             (emit-body! i end '(delimiter yaml-block-scalar-header))
             (set! next-block-scalar-indent
                   (block-indent-after-header body i current-indent))
             (loop end #f)]
            [(char=? ch #\&)
             (define end
               (scan-name-like body (add1 i)))
             (emit-body! i end '(identifier yaml-anchor))
             (loop end #f)]
            [(char=? ch #\*)
             (define end
               (scan-name-like body (add1 i)))
             (emit-body! i end '(identifier yaml-alias))
             (loop end #f)]
            [(char=? ch #\!)
             (define end
               (scan-name-like body (add1 i)))
             (emit-body! i end '(identifier yaml-tag))
             (loop end #f)]
            [(or (char=? ch #\")
                 (char=? ch #\'))
             (define-values (end terminated? valid?)
               (scan-quoted-string body i ch))
             (emit-body! i
                         end
                         (append '(literal yaml-string-literal)
                                 (if (and terminated? valid?)
                                     '()
                                     '(yaml-error malformed-token))))
             (loop end #f)]
            [else
             (define end
               (scan-plain-scalar body i))
             (cond
               [(= end i)
                (emit-body! i (add1 i) '(yaml-error malformed-token))
                (loop (add1 i) #f)]
               [else
                (emit-body! i end (plain-scalar-tags (substring body i end)))
                (loop end #f)])])]))]))

;; -----------------------------------------------------------------------------
;; Reader

;; make-yaml-derived-reader : -> (input-port? -> (or/c yaml-derived-token? 'eof))
;;   Construct a stateful YAML derived-token reader.
(define (make-yaml-derived-reader)
  (define pending
    '())
  (define block-scalar-indent
    #f)
  (letrec ([next-token
            (lambda (in)
              (unless (input-port? in)
                (raise-argument-error 'make-yaml-derived-reader "input-port?" in))
              (port-count-lines! in)
              (cond
                [(pair? pending)
                 (define token
                   (car pending))
                 (set! pending (cdr pending))
                 token]
                [else
                 (define start-pos
                   (current-stream-position in))
                 (define line
                   (read-line-fragment in))
                 (cond
                   [(eof-object? line)
                    'eof]
                   [else
                    (define-values (tokens next-indent)
                      (tokenize-line line start-pos block-scalar-indent))
                    (set! block-scalar-indent next-indent)
                    (cond
                      [(pair? tokens)
                       (define token
                         (car tokens))
                       (set! pending (cdr tokens))
                       token]
                      [else
                       (next-token in)])])]))])
    next-token))
