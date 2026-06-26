#lang racket/base

;;;
;;; Ruby Derived Tokens
;;;
;;
;; Stateful Ruby tokenization and reusable Ruby-specific classifications.

;; ruby-derived-token?         : any/c -> boolean?
;;   Recognize a derived Ruby token.
;; ruby-derived-token-text     : ruby-derived-token? -> string?
;;   Extract the source text for one derived token.
;; ruby-derived-token-start    : ruby-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; ruby-derived-token-end      : ruby-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; ruby-derived-token-tags     : ruby-derived-token? -> (listof symbol?)
;;   Extract reusable Ruby classification tags.
;; ruby-derived-token-has-tag? : ruby-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; make-ruby-derived-reader    : -> (input-port? -> (or/c ruby-derived-token? 'eof))
;;   Construct a stateful Ruby derived-token reader.

(provide ruby-derived-token?
         ruby-derived-token-text
         ruby-derived-token-start
         ruby-derived-token-end
         ruby-derived-token-tags
         ruby-derived-token-has-tag?
         make-ruby-derived-reader)

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         racket/list
         racket/match
         racket/set
         racket/string
         "parser-tools-compat.rkt")

;; A Ruby token plus reusable tags.
(struct ruby-derived-token (kind text start end tags) #:transparent)

;; One pending Ruby heredoc body to read after the following newline.
(struct ruby-heredoc-spec (delimiter allow-indent? squiggly? command? interpolated?) #:transparent)

;; ruby-derived-token-has-tag? : ruby-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (ruby-derived-token-has-tag? token tag)
  (member tag (ruby-derived-token-tags token)))

;; -----------------------------------------------------------------------------
;; Classification tables

;; Reserved Ruby keywords in the first lexer slice.
(define ruby-keywords
  (list->set
   '("BEGIN" "END" "alias" "and" "begin" "break" "case" "class" "def"
     "defined?" "do" "else" "elsif" "end" "ensure" "false" "for" "if"
     "in" "module" "next" "nil" "not" "or" "redo" "rescue" "retry"
     "return" "self" "super" "then" "true" "undef" "unless" "until"
     "when" "while" "yield" "__ENCODING__" "__FILE__" "__LINE__")))

;; Ruby delimiter spellings.
(define ruby-delimiters
  (list->set
   '("(" ")" "[" "]" "{" "}" "," ";" "." ".." "..." "::" ":")))

;; Ruby operator spellings.
(define ruby-operators
  (list->set
   '("!" "!=" "!~" "%" "%=" "&" "&&" "&&=" "&." "&=" "*" "**" "**="
     "*=" "+" "+=" "-" "-=" "/" "/=" "<" "<<" "<<=" "<=" "<=>" "="
     "==" "===" "=~" "=>" ">" ">=" ">>" ">>=" "?" "^" "^=" "|" "||"
     "||=" "|=" "~")))

;; -----------------------------------------------------------------------------
;; Lexer abbreviations

(define-lex-abbrevs
  [ruby-inline-whitespace (:or #\space #\tab #\page #\vtab)]
  [ruby-newline           (:or "\r\n" #\return #\newline)]
  [ruby-lower-alpha       (:/ #\a #\z)]
  [ruby-upper-alpha       (:/ #\A #\Z)]
  [ruby-digit             (:/ #\0 #\9)]
  [ruby-ident-start       (:or ruby-lower-alpha ruby-upper-alpha #\_)]
  [ruby-ident-char        (:or ruby-ident-start ruby-digit)]
  [ruby-ident-core        (:: ruby-ident-start (:* ruby-ident-char))]
  [ruby-ident-like        (:: ruby-ident-core (:? (:or #\? #\!)))]
  [ruby-constant-like     (:: ruby-upper-alpha (:* ruby-ident-char))]
  [ruby-instance-variable (:: #\@ ruby-ident-core)]
  [ruby-class-variable    (:: "@@" ruby-ident-core)]
  [ruby-global-variable   (:or (:: #\$ ruby-ident-core)
                               (:: #\$ (:+ ruby-digit))
                               (:: #\$ (:or #\! #\" #\& #\' #\+ #\, #\. #\/
                                            #\: #\; #\< #\= #\> #\? #\@
                                            #\\ #\` #\~ #\* #\$ #\_)))]
  [ruby-decimal-digits    (:: ruby-digit (:* (:or ruby-digit #\_)))]
  [ruby-hex-digit         (:/ #\0 #\9 #\a #\f #\A #\F)]
  [ruby-hex-digits        (:: ruby-hex-digit (:* (:or ruby-hex-digit #\_)))]
  [ruby-binary-digits     (:: (:or #\0 #\1) (:* (:or #\0 #\1 #\_)))]
  [ruby-octal-digits      (:: (:/ #\0 #\7) (:* (:or (:/ #\0 #\7) #\_)))]
  [ruby-number-suffix     (:? (:or "ri" #\r #\i))]
  [ruby-decimal-integer   (:: ruby-decimal-digits ruby-number-suffix)]
  [ruby-hex-integer       (:: #\0 (:or #\x #\X) ruby-hex-digits ruby-number-suffix)]
  [ruby-binary-integer    (:: #\0 (:or #\b #\B) ruby-binary-digits ruby-number-suffix)]
  [ruby-octal-integer     (:or (:: #\0 (:or #\o #\O) ruby-octal-digits ruby-number-suffix)
                               (:: #\0 ruby-octal-digits ruby-number-suffix))]
  [ruby-exponent-part     (:: (:or #\e #\E)
                              (:? (:or #\+ #\-))
                              ruby-decimal-digits)]
  [ruby-float             (:: (:or (:: ruby-decimal-digits #\. (:* ruby-decimal-digits))
                                   (:: ruby-decimal-digits ruby-exponent-part)
                                   (:: ruby-decimal-digits
                                       #\.
                                       (:* ruby-decimal-digits)
                                       ruby-exponent-part))
                           ruby-number-suffix)]
  [ruby-symbol-literal    (:: #\: ruby-ident-like)]
  [ruby-operator-token    (:or "<<=" ">>=" "&&=" "||=" "**=" "<=>"
                               "===" "=~" "!~" "==" "!=" "<=" ">="
                               "<<" ">>" "&&" "||" "**" "+=" "-="
                               "*=" "/=" "%=" "&=" "|=" "^=" "=>"
                               "&." "::" "..." ".."
                               #\! #\% #\& #\* #\+ #\- #\/ #\< #\=
                               #\> #\? #\^ #\| #\~)]
  [ruby-delimiter-token   (:or #\( #\) #\[ #\] #\{ #\} #\, #\; #\. #\:)]
  [ruby-any-char          any-char])

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

;; make-token-from-text : position? position? string? (listof symbol?) -> ruby-derived-token?
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
  (ruby-derived-token kind
                      text
                      start-pos
                      end-pos
                      (remove-duplicates tags)))

;; advance-position-by-text : position? string? -> position?
;;   Advance one source position by the characters in text.
(define (advance-position-by-text start-pos text)
  (let loop ([i    0]
             [line (position-line start-pos)]
             [col  (position-col start-pos)]
             [off  (position-offset start-pos)])
    (cond
      [(= i (string-length text))
       (make-stream-position off line col)]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(char=? ch #\return)
          (define next-i
            (add1 i))
          (cond
            [(and (< next-i (string-length text))
                  (char=? (string-ref text next-i) #\newline))
             (loop (add1 next-i) (add1 line) 0 (+ off 2))]
            [else
             (loop next-i (add1 line) 0 (add1 off))])]
         [(char=? ch #\newline)
          (loop (add1 i) (add1 line) 0 (add1 off))]
         [else
          (loop (add1 i) line (add1 col) (add1 off))])])))

;; split-derived-token-with-chunks : ruby-derived-token? (listof (cons/c string? (listof symbol?))) -> (listof ruby-derived-token?)
;;   Rebuild one token as multiple contiguous derived tokens from chunk text and tags.
(define (split-derived-token-with-chunks token chunks)
  (let loop ([start-pos (ruby-derived-token-start token)]
             [remaining chunks]
             [acc       '()])
    (cond
      [(empty? remaining)
       (reverse acc)]
      [else
       (define text
         (car (first remaining)))
       (define tags
         (cdr (first remaining)))
       (define end-pos
         (advance-position-by-text start-pos text))
       (define next-token
         (make-token-from-text start-pos end-pos text tags))
       (loop end-pos
             (rest remaining)
             (cons next-token acc))])))

;; keyword-token-tags : string? -> (listof symbol?)
;;   Choose reusable tags for one identifier-like token.
(define (keyword-token-tags text)
  (cond
    [(set-member? ruby-keywords text)
     '(keyword ruby-keyword)]
    [(and (positive? (string-length text))
          (char-upper-case? (string-ref text 0)))
     '(identifier ruby-constant)]
    [else
     '(identifier ruby-identifier)]))

;; operator-token-tags : string? -> (listof symbol?)
;;   Choose reusable tags for one operator-like token.
(define (operator-token-tags text)
  (cond
    [(set-member? ruby-delimiters text)
     '(delimiter ruby-delimiter)]
    [(set-member? ruby-operators text)
     '(operator ruby-operator)]
    [else
     '(delimiter ruby-delimiter)]))

;; -----------------------------------------------------------------------------
;; Small scanners

;; read-until-line-end! : input-port? output-port? -> void?
;;   Consume a comment body without consuming the terminating newline.
(define (read-until-line-end! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       (void)]
      [(or (char=? next #\return)
           (char=? next #\newline))
       (void)]
      [else
       (write-one! in out)
       (loop)])))

;; read-quoted-string! : input-port? output-port? char? -> boolean?
;;   Consume one quoted Ruby string and report whether it terminated cleanly.
(define (read-quoted-string! in out delimiter)
  (let loop ([escaped? #f])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [else
       (write-one! in out)
       (cond
         [escaped?
          (loop #f)]
         [(char=? next #\\)
          (loop #t)]
         [(char=? next delimiter)
          #t]
         [else
          (loop #f)])])))

;; read-symbol-string! : input-port? output-port? -> boolean?
;;   Consume one quoted symbol literal after the leading colon.
(define (read-symbol-string! in out)
  (define delimiter
    (peek-next in))
  (cond
    [(or (char=? delimiter #\')
         (char=? delimiter #\"))
     (write-one! in out)
     (read-quoted-string! in out delimiter)]
    [else
     #f]))

;; text-contains-interpolation? : string? -> boolean?
;;   Determine whether one Ruby literal text contains an unescaped interpolation marker.
(define (text-contains-interpolation? text)
  (define len
    (string-length text))
  (let loop ([i 0]
             [escaped? #f])
    (cond
      [(>= i len)
       #f]
      [else
       (define ch
         (string-ref text i))
       (cond
         [escaped?
          (loop (add1 i) #f)]
         [(char=? ch #\\)
          (loop (add1 i) #t)]
         [(and (char=? ch #\#)
               (< (+ i 1) len)
               (member (string-ref text (add1 i)) '(#\{ #\@ #\$)))
          #t]
         [else
          (loop (add1 i) #f)])])))

;; interpolation-close-index : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Find the end of one #{...} interpolation chunk that starts at start-index.
(define (interpolation-close-index text start-index)
  (define len
    (string-length text))
  (let loop ([i        (+ start-index 2)]
             [depth    1]
             [escaped? #f]
             [quote    #f])
    (cond
      [(>= i len)
       len]
      [else
       (define ch
         (string-ref text i))
       (cond
         [escaped?
          (loop (add1 i) depth #f quote)]
         [(and quote (char=? ch #\\))
          (loop (add1 i) depth #t quote)]
         [quote
          (cond
            [(char=? ch quote)
             (loop (add1 i) depth #f #f)]
            [else
             (loop (add1 i) depth #f quote)])]
         [(or (char=? ch #\')
              (char=? ch #\")
              (char=? ch #\`))
          (loop (add1 i) depth #f ch)]
         [(char=? ch #\{)
          (loop (add1 i) (add1 depth) #f #f)]
         [(char=? ch #\})
          (define next-depth
            (sub1 depth))
          (cond
            [(zero? next-depth) (add1 i)]
            [else               (loop (add1 i) next-depth #f #f)])]
         [else
          (loop (add1 i) depth #f #f)])])))

;; interpolation-var-close-index : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Find the end of one #@name or #$name interpolation chunk.
(define (interpolation-var-close-index text start-index)
  (define len
    (string-length text))
  (let loop ([i (+ start-index 2)])
    (cond
      [(>= i len)
       len]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(or (char-alphabetic? ch)
              (char-numeric? ch)
              (char=? ch #\_)
              (char=? ch #\@))
          (loop (add1 i))]
         [else
          i])])))

;; split-interpolated-literal-chunks : ruby-derived-token? -> (or/c #f (listof (cons/c string? (listof symbol?))))
;;   Split one interpolated literal into literal and interpolation display chunks.
(define (split-interpolated-literal-chunks token)
  (cond
    [(not (ruby-derived-token-has-tag? token 'ruby-interpolated-literal))
     #f]
    [else
     (define text
       (ruby-derived-token-text token))
     (define len
       (string-length text))
     (define base-tags
       (filter (lambda (tag)
                 (not (eq? tag 'ruby-interpolated-literal)))
               (ruby-derived-token-tags token)))
     (let loop ([i     0]
                [start 0]
                [acc   '()]
                [escaped? #f])
       (cond
         [(= i len)
          (reverse
           (cond
             [(< start len)
              (cons (cons (substring text start len) base-tags) acc)]
             [else
              acc]))]
         [else
          (define ch
            (string-ref text i))
          (cond
            [escaped?
             (loop (add1 i) start acc #f)]
            [(char=? ch #\\)
             (loop (add1 i) start acc #t)]
            [(and (char=? ch #\#)
                  (< (add1 i) len)
                  (char=? (string-ref text (add1 i)) #\{))
             (define chunk-end
               (interpolation-close-index text i))
             (define next-acc
               (cond
                 [(< start i)
                  (cons (cons (substring text start i) base-tags) acc)]
                 [else
                  acc]))
             (loop chunk-end
                   chunk-end
                   (cons (cons (substring text i chunk-end)
                               '(literal ruby-interpolation))
                         next-acc)
                   #f)]
            [(and (char=? ch #\#)
                  (< (add1 i) len)
                  (member (string-ref text (add1 i)) '(#\@ #\$)))
             (define chunk-end
               (interpolation-var-close-index text i))
             (define next-acc
               (cond
                 [(< start i)
                  (cons (cons (substring text start i) base-tags) acc)]
                 [else
                  acc]))
             (loop chunk-end
                   chunk-end
                   (cons (cons (substring text i chunk-end)
                               '(literal ruby-interpolation))
                         next-acc)
                   #f)]
            [else
             (loop (add1 i) start acc #f)])]))]))

;; percent-literal-type-char? : char? -> boolean?
;;   Recognize a percent-literal type marker.
(define (percent-literal-type-char? ch)
  (member ch '(#\Q #\q #\W #\w #\I #\i #\R #\r #\X #\x #\S #\s)))

;; numeric-char? : any/c -> boolean?
;;   Recognize one character that can continue a simple escaped character literal.
(define (numeric-char? ch)
  (and (char? ch)
       (or (char-alphabetic? ch)
           (char-numeric? ch)
           (char=? ch #\_))))

;; hex-digit-char? : any/c -> boolean?
;;   Recognize one hexadecimal digit character.
(define (hex-digit-char? ch)
  (and (char? ch)
       (or (char-numeric? ch)
           (member ch '(#\a #\b #\c #\d #\e #\f
                        #\A #\B #\C #\D #\E #\F)))))

;; paired-delimiter-close : char? -> (or/c char? #f)
;;   Return the matching close delimiter for a paired opener.
(define (paired-delimiter-close ch)
  (case ch
    [(#\() (integer->char 41)]
    [(#\[) #\]]
    [(#\{) #\}]
    [(#\<) #\>]
    [else   #f]))

;; read-percent-literal-body! : input-port? output-port? char? char? -> boolean?
;;   Consume the body of a percent literal through its closing delimiter.
(define (read-percent-literal-body! in out open-delim close-delim)
  (define nesting?
    (not (char=? open-delim close-delim)))
  (let loop ([escaped? #f]
             [depth    1])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [else
       (write-one! in out)
       (cond
         [escaped?
          (loop #f depth)]
         [(char=? next #\\)
          (loop #t depth)]
         [(and nesting? (char=? next open-delim))
          (loop #f (add1 depth))]
         [(char=? next close-delim)
          (define next-depth
            (sub1 depth))
          (cond
            [(zero? next-depth) #t]
            [else               (loop #f next-depth)])]
         [else
          (loop #f depth)])])))

;; read-regexp-body! : input-port? output-port? char? -> boolean?
;;   Consume a regexp-like body terminated by close-delim and trailing flags.
(define (read-regexp-body! in out close-delim)
  (define open-delim
    (cond
      [(char=? close-delim #\)) #\(]
      [(char=? close-delim #\]) #\[]
      [(char=? close-delim #\}) #\{]
      [(char=? close-delim #\>) #\<]
      [else                     #f]))
  (define in-char-class?
    #f)
  (define escaped?
    #f)
  (define depth
    (cond
      [open-delim 1]
      [else       0]))
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       #f]
      [else
       (write-one! in out)
       (cond
         [escaped?
          (set! escaped? #f)
          (loop)]
         [(char=? next #\\)
          (set! escaped? #t)
          (loop)]
         [(and (not in-char-class?) (char=? next #\[))
          (set! in-char-class? #t)
          (loop)]
         [(and in-char-class? (char=? next #\]))
          (set! in-char-class? #f)
          (loop)]
         [(and open-delim (not in-char-class?) (char=? next open-delim))
          (set! depth (add1 depth))
          (loop)]
         [(and (not in-char-class?) (char=? next close-delim))
          (cond
            [(zero? depth)
             (read-regexp-flags! in out)
             #t]
            [else
             (set! depth (sub1 depth))
             (cond
               [(zero? depth)
                (read-regexp-flags! in out)
                #t]
               [else
                (loop)])])]
         [else
          (loop)])])))

;; read-regexp-flags! : input-port? output-port? -> void?
;;   Consume trailing regexp option letters.
(define (read-regexp-flags! in out)
  (let loop ()
    (define next
      (peek-next in))
    (cond
      [(and (char? next) (char-alphabetic? next))
       (write-one! in out)
       (loop)]
      [else
       (void)])))

;; try-read-percent-literal : input-port? -> (or/c #f (list symbol? string? (listof symbol?)))
;;   Try to consume one Ruby percent literal from the current port position.
(define (try-read-percent-literal in)
  (define first
    (peek-next in))
  (cond
    [(not (and (char? first) (char=? first #\%)))
     #f]
    [else
     (define second
       (peek-next in 1))
     (define type-char
       (cond
         [(and (char? second) (percent-literal-type-char? second))
          second]
         [else
          #f]))
     (define delim-index
       (cond
         [type-char 2]
         [else      1]))
     (define delimiter
       (peek-next in delim-index))
     (cond
       [(or (not (char? delimiter))
            (char-alphabetic? delimiter)
            (char-numeric? delimiter)
            (char-whitespace? delimiter))
        #f]
       [else
        (define close-delim
          (or (paired-delimiter-close delimiter)
              delimiter))
        (define out
          (open-output-string))
        (write-one! in out)
        (when type-char
          (write-one! in out))
        (write-one! in out)
        (define terminated?
          (cond
            [(and type-char
                  (or (char=? type-char #\r)
                      (char=? type-char #\R)))
             (read-regexp-body! in out close-delim)]
            [else
             (read-percent-literal-body! in out delimiter close-delim)]))
        (define text
          (get-output-string out))
        (define tags
          (let ([base-tags
                 (case type-char
                   [(#\r #\R) '(literal ruby-percent-literal ruby-regexp-literal)]
                   [(#\x #\X) '(literal ruby-percent-literal ruby-command-literal)]
                   [(#\s #\S) '(literal ruby-percent-literal ruby-symbol-literal)]
                   [(#\i #\I)
                    '(literal ruby-percent-literal ruby-symbol-list-literal)]
                   [(#\w #\W)
                    '(literal ruby-percent-literal ruby-word-list-literal)]
                   [(#\Q #\q)
                    '(literal ruby-percent-literal ruby-string-literal)]
                   [else
                    '(literal ruby-percent-literal ruby-string-literal)])])
            (cond
              [(and (text-contains-interpolation? text)
                    (member type-char '(#\Q #\W #\I #\R #\X)))
               (append base-tags '(ruby-interpolated-literal))]
              [else
               base-tags])))
        (cond
          [terminated?
           (list 'literal text tags)]
          [else
           (list 'malformed text (append tags '(malformed-token ruby-error)))])])]))

;; read-regexp-literal : input-port? -> (list symbol? string? (listof symbol?))
;;   Consume one slash-delimited regexp literal from the current port position.
(define (read-regexp-literal in)
  (define out
    (open-output-string))
  (write-one! in out)
  (define terminated?
    (read-regexp-body! in out #\/))
  (define text
    (get-output-string out))
  (cond
    [terminated?
     (list 'literal text '(literal ruby-regexp-literal))]
    [else
     (list 'malformed text '(literal ruby-regexp-literal malformed-token ruby-error))]))

;; read-backtick-command-literal : input-port? -> (list symbol? string? (listof symbol?))
;;   Consume one backtick-delimited command literal.
(define (read-backtick-command-literal in)
  (define out
    (open-output-string))
  (write-one! in out)
  (define terminated?
    (read-quoted-string! in out #\`))
  (define text
    (get-output-string out))
  (cond
    [terminated?
     (define tags
       (cond
         [(text-contains-interpolation? text)
          '(literal ruby-command-literal ruby-interpolated-literal)]
         [else
          '(literal ruby-command-literal)]))
     (list 'literal text tags)]
    [else
     (list 'malformed text '(literal ruby-command-literal malformed-token ruby-error))]))

;; read-one-char-token : input-port? symbol? (listof symbol?) -> (list symbol? string? (listof symbol?))
;;   Consume one single-character token with fixed tags.
(define (read-one-char-token in kind tags)
  (define out
    (open-output-string))
  (write-one! in out)
  (list kind (get-output-string out) tags))

;; ruby-method-name-leading-char? : any/c -> boolean?
;;   Recognize one leading character for a method-style identifier name.
(define (ruby-method-name-leading-char? ch)
  (and (char? ch)
       (or (char-alphabetic? ch)
           (char=? ch #\_))))

;; ruby-method-name-char? : any/c -> boolean?
;;   Recognize one subsequent character for a method-style identifier name.
(define (ruby-method-name-char? ch)
  (and (char? ch)
       (or (ruby-method-name-leading-char? ch)
           (char-numeric? ch))))

;; operator-method-name-start? : any/c -> boolean?
;;   Recognize one leading character for an operator-style Ruby method name.
(define (operator-method-name-start? ch)
  (and (char? ch)
       (member ch '(#\! #\% #\& #\* #\+ #\- #\/ #\< #\= #\> #\^ #\| #\~ #\[))))

;; try-read-operator-method-name : input-port? -> (or/c #f (list symbol? string? (listof symbol?)))
;;   Try to consume one operator-style method name such as << or []=.
(define (try-read-operator-method-name in)
  (define first
    (peek-next in))
  (cond
    [(not (operator-method-name-start? first))
     #f]
    [else
     (define out
       (open-output-string))
     (cond
       [(char=? first #\[)
        (write-one! in out)
        (cond
          [(and (char? (peek-next in))
                (char=? (peek-next in) #\]))
           (write-one! in out)
           (when (and (char? (peek-next in))
                      (char=? (peek-next in) #\=))
             (write-one! in out))
           (list 'operator
                 (get-output-string out)
                 '(operator ruby-method-name ruby-operator-method-name))]
          [else
           #f])]
       [else
        (let loop ()
          (define next
            (peek-next in))
          (when (operator-method-name-start? next)
            (write-one! in out)
            (loop)))
        (define text
          (get-output-string out))
        (cond
          [(or (set-member? ruby-operators text)
               (member text '("==" "===" "=~" "!~" "<=>" "<<" ">>" "+" "-"
                              "*" "/" "%" "&" "|" "^" "~" "`")))
           (list 'operator
                 text
                 '(operator ruby-method-name ruby-operator-method-name))]
          [else
           #f])])]))

;; try-read-symbol-literal : input-port? -> (or/c #f (list symbol? string? (listof symbol?)))
;;   Try to consume one Ruby symbol literal, including method-like names.
(define (try-read-symbol-literal in)
  (define first
    (peek-next in))
  (define second
    (peek-next in 1))
  (cond
    [(or (not (char? first))
         (not (char=? first #\:))
         (not (char? second))
         (char-whitespace? second)
         (char=? second #\:))
     #f]
    [else
     (cond
       [(or (char=? second #\')
            (char=? second #\"))
        (define out
          (open-output-string))
        (write-one! in out)
        (define delimiter
          second)
        (write-one! in out)
        (define terminated?
          (read-quoted-string! in out delimiter))
        (define text
          (get-output-string out))
        (cond
          [terminated?
           (list 'literal text '(literal ruby-symbol-literal ruby-string-literal))]
          [else
           (list 'malformed text '(literal malformed-token ruby-symbol-literal ruby-string-literal ruby-error))])]
       [(ruby-method-name-leading-char? second)
        (define out
          (open-output-string))
        (write-one! in out)
        (write-one! in out)
        (let loop ()
          (define next
            (peek-next in))
          (when (ruby-method-name-char? next)
            (write-one! in out)
            (loop)))
        (when (and (char? (peek-next in))
                   (member (peek-next in) '(#\? #\! #\=)))
          (write-one! in out))
        (list 'literal
              (get-output-string out)
              '(literal ruby-symbol-literal ruby-method-symbol-literal))]
       [(char=? second #\@)
        (define out
          (open-output-string))
        (write-one! in out)
        (write-one! in out)
        (when (and (char? (peek-next in))
                   (char=? (peek-next in) #\@))
          (write-one! in out))
        (cond
          [(ruby-method-name-leading-char? (peek-next in))
           (write-one! in out)
           (let loop ()
             (define next
               (peek-next in))
             (when (ruby-method-name-char? next)
               (write-one! in out)
               (loop)))
           (list 'literal
                 (get-output-string out)
                 '(literal ruby-symbol-literal ruby-variable-symbol-literal))]
          [else
           #f])]
       [(char=? second #\$)
        (define out
          (open-output-string))
        (write-one! in out)
        (write-one! in out)
        (cond
          [(ruby-method-name-leading-char? (peek-next in))
           (write-one! in out)
           (let loop ()
             (define next
               (peek-next in))
             (when (ruby-method-name-char? next)
               (write-one! in out)
               (loop)))
           (list 'literal
                 (get-output-string out)
                 '(literal ruby-symbol-literal ruby-variable-symbol-literal))]
          [else
           #f])]
       [(operator-method-name-start? second)
        (define out
          (open-output-string))
        (write-one! in out)
        (define operator-raw
          (try-read-operator-method-name in))
        (cond
          [operator-raw
           (list 'literal
                 (string-append ":" (list-ref operator-raw 1))
                 '(literal ruby-symbol-literal ruby-method-symbol-literal))]
          [else
           #f])]
       [else
        #f])]))

;; try-read-identifier-method-name : input-port? -> (or/c #f (list symbol? string? (listof symbol?)))
;;   Try to consume one identifier-style method name such as foo!, foo?, or foo=.
(define (try-read-identifier-method-name in)
  (define first
    (peek-next in))
  (cond
    [(not (ruby-method-name-leading-char? first))
     #f]
    [else
     (define out
       (open-output-string))
     (write-one! in out)
     (let loop ()
       (define next
         (peek-next in))
       (when (ruby-method-name-char? next)
         (write-one! in out)
         (loop)))
     (define base-text
       (get-output-string out))
     (cond
       [(and (string=? base-text "self")
             (or (and (char? (peek-next in))
                      (char=? (peek-next in) #\.))
                 (and (char? (peek-next in))
                      (char=? (peek-next in) #\:)
                      (char? (peek-next in 1))
                      (char=? (peek-next in 1) #\:))))
        (list 'keyword
              base-text
              '(keyword ruby-keyword))]
       [else
        (when (and (char? (peek-next in))
                   (member (peek-next in) '(#\? #\! #\=)))
          (write-one! in out))
        (list 'identifier
              (get-output-string out)
              '(identifier ruby-method-name))])]))

;; identifier-token->method-reference : (list symbol? string? (listof symbol?)) -> (list symbol? string? (listof symbol?))
;;   Retag one identifier-like token as a Ruby method reference.
(define (identifier-token->method-reference raw)
  (match raw
    [(list kind text tags)
     (list kind
           text
           (remove-duplicates
            (append (filter (lambda (tag)
                              (not (memq tag '(ruby-identifier ruby-method-name))))
                            tags)
                    '(ruby-method-reference))))]))

;; retag-keyword-argument-label : ruby-derived-token? input-port? -> ruby-derived-token?
;;   Add a highlighting tag for bare keyword-argument labels such as foo:.
(define (retag-keyword-argument-label token in)
  (define next
    (peek-next in))
  (cond
    [(and (or (ruby-derived-token-has-tag? token 'ruby-identifier)
              (ruby-derived-token-has-tag? token 'ruby-constant))
          (char? next)
          (char=? next #\:))
     (define next2
       (peek-next in 1))
     (cond
       [(and (char? next2)
             (char=? next2 #\:))
        token]
       [else
        (match token
          [(ruby-derived-token kind text start end tags)
           (ruby-derived-token kind
                               text
                               start
                               end
                               (append tags '(ruby-keyword-argument-label)))])])]
    [else
     token]))

;; read-character-literal-escape! : input-port? output-port? -> void?
;;   Consume the escape portion of one Ruby character literal after ?\.
(define (read-character-literal-escape! in out)
  (define next
    (peek-next in))
  (cond
    [(not (char? next))
     (void)]
    [(member next '(#\C #\M #\c))
     (write-one! in out)
     (when (and (char? (peek-next in))
                (char=? (peek-next in) #\-))
       (write-one! in out)
       (read-character-literal-escape! in out))]
    [(char=? next #\u)
     (write-one! in out)
     (cond
       [(and (char? (peek-next in))
             (char=? (peek-next in) #\{))
        (write-one! in out)
        (let loop ()
          (define ch
            (peek-next in))
          (when (char? ch)
            (write-one! in out)
            (unless (char=? ch #\})
              (loop))))]
       [else
        (for ([i (in-range 4)])
          (when (hex-digit-char? (peek-next in))
            (write-one! in out)))])]
    [(char=? next #\x)
     (write-one! in out)
     (for ([i (in-range 2)])
       (when (hex-digit-char? (peek-next in))
         (write-one! in out)))]
    [else
     (let loop ()
       (define ch
         (peek-next in))
       (when (numeric-char? ch)
         (write-one! in out)
         (loop)))]))

;; try-read-character-literal : input-port? -> (or/c #f (list symbol? string? (listof symbol?)))
;;   Try to consume one Ruby character literal such as ?a, ?., or ?\n.
(define (try-read-character-literal in)
  (define first
    (peek-next in))
  (define second
    (peek-next in 1))
  (cond
    [(or (not (char? first))
         (not (char=? first #\?))
         (not (char? second))
         (char-whitespace? second))
     #f]
    [else
     (define out
       (open-output-string))
     (write-one! in out)
     (write-one! in out)
     (when (char=? second #\\)
       (read-character-literal-escape! in out))
     (list 'literal
           (get-output-string out)
           '(literal ruby-character-literal))]))

;; read-heredoc-delimiter! : input-port? output-port? -> (or/c ruby-heredoc-spec? #f)
;;   Consume one heredoc delimiter after <<, <<-, or <<~.
(define (read-heredoc-delimiter! in out)
  (define next
    (peek-next in))
  (cond
    [(not (char? next))
     #f]
    [(or (char=? next #\')
         (char=? next #\")
         (char=? next #\`))
     (define quote-char
       next)
     (write-one! in out)
     (define delimiter-out
       (open-output-string))
     (let loop ()
       (define ch
         (peek-next in))
       (cond
         [(eof-object? ch)
          #f]
         [else
          (write-one! in out)
          (cond
            [(char=? ch quote-char)
             (ruby-heredoc-spec (get-output-string delimiter-out)
                                #f
                                #f
                                (char=? quote-char #\`)
                                (or (char=? quote-char #\")
                                    (char=? quote-char #\`)))]
            [else
             (write-char ch delimiter-out)
             (loop)])]))]
    [(or (char-alphabetic? next)
         (char=? next #\_))
     (define delimiter-out
       (open-output-string))
     (let loop ()
       (define ch
         (peek-next in))
       (cond
         [(and (char? ch)
               (or (char-alphabetic? ch)
                   (char-numeric? ch)
                   (char=? ch #\_)))
          (write-one! in out)
          (write-char ch delimiter-out)
          (loop)]
         [else
         (ruby-heredoc-spec (get-output-string delimiter-out)
                             #f
                             #f
                             #f
                             #t)]))]
    [else
     #f]))

;; heredoc-spec->flavor-tags : ruby-heredoc-spec? -> (listof symbol?)
;;   Choose reusable heredoc flavor tags for one heredoc specification.
(define (heredoc-spec->flavor-tags spec)
  (append
   (cond
     [(ruby-heredoc-spec-command? spec)
      '(ruby-command-heredoc)]
     [(ruby-heredoc-spec-squiggly? spec)
      '(ruby-squiggly-heredoc)]
     [(ruby-heredoc-spec-allow-indent? spec)
      '(ruby-indented-heredoc)]
     [else
      '(ruby-plain-heredoc)])
   (cond
     [(ruby-heredoc-spec-interpolated? spec)
      '(ruby-interpolating-heredoc)]
     [else
      '(ruby-noninterpolating-heredoc)])))

;; try-read-heredoc-introducer : input-port? -> (or/c #f (list/c any/c ruby-heredoc-spec?))
;;   Try to consume one heredoc introducer from the current port position.
(define (try-read-heredoc-introducer in)
  (define first
    (peek-next in))
  (define second
    (peek-next in 1))
  (cond
    [(or (not (char? first))
         (not (char? second))
         (not (char=? first #\<))
         (not (char=? second #\<)))
     #f]
    [else
     (define third
       (peek-next in 2))
     (define modifier?
       (and (char? third)
            (or (char=? third #\-)
                (char=? third #\~))))
     (define delimiter-index
       (cond
         [modifier? 3]
         [else      2]))
     (define delimiter-start
       (peek-next in delimiter-index))
     (cond
       [(not (and (char? delimiter-start)
                  (or (char-alphabetic? delimiter-start)
                      (char=? delimiter-start #\_)
                      (char=? delimiter-start #\')
                      (char=? delimiter-start #\")
                      (char=? delimiter-start #\`))))
        #f]
       [else
        (define out
          (open-output-string))
        (write-one! in out)
        (write-one! in out)
        (define allow-indent?
          #f)
        (define squiggly?
          #f)
        (define third
          (peek-next in))
        (cond
          [(and (char? third) (char=? third #\-))
           (set! allow-indent? #t)
           (write-one! in out)]
          [(and (char? third) (char=? third #\~))
           (set! allow-indent? #t)
           (set! squiggly? #t)
           (write-one! in out)]
          [else
           (void)])
        (define spec
          (read-heredoc-delimiter! in out))
        (cond
          [spec
           (define flavor-tags
             (heredoc-spec->flavor-tags
              (match spec
                [(ruby-heredoc-spec delimiter _ _ command? interpolated?)
                 (ruby-heredoc-spec delimiter
                                    allow-indent?
                                    squiggly?
                                    command?
                                    interpolated?)])))
           (list (list 'literal
                       (get-output-string out)
                       (append '(literal ruby-heredoc-introducer ruby-string-literal)
                               flavor-tags))
                 (match spec
                   [(ruby-heredoc-spec delimiter _ _ command? interpolated?)
                    (ruby-heredoc-spec delimiter
                                       allow-indent?
                                       squiggly?
                                       command?
                                       interpolated?)]))]
          [else
           #f])])]))

;; read-physical-line-string : input-port? -> (or/c string? #f)
;;   Consume one physical line, preserving CRLF and EOF-without-newline.
(define (read-physical-line-string in)
  (define out
    (open-output-string))
  (let loop ([read-any? #f])
    (define next
      (peek-next in))
    (cond
      [(eof-object? next)
       (cond
         [read-any? (get-output-string out)]
         [else      #f])]
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

;; strip-line-ending : string? -> string?
;;   Remove one trailing CRLF, CR, or LF sequence from a physical line.
(define (strip-line-ending line)
  (cond
    [(string-suffix? line "\r\n")
     (substring line 0 (- (string-length line) 2))]
    [(or (string-suffix? line "\r")
         (string-suffix? line "\n"))
     (substring line 0 (- (string-length line) 1))]
    [else
     line]))

;; heredoc-terminator-line? : ruby-heredoc-spec? string? -> boolean?
;;   Determine whether one physical line closes a heredoc body.
(define (heredoc-terminator-line? spec line)
  (define content
    (strip-line-ending line))
  (define delimiter
    (ruby-heredoc-spec-delimiter spec))
  (cond
    [(ruby-heredoc-spec-allow-indent? spec)
     (regexp-match? (regexp (string-append "^[ \t]*"
                                           (regexp-quote delimiter)
                                           "$"))
                    content)]
    [else
     (string=? content delimiter)]))

;; read-heredoc-body-token : input-port? ruby-heredoc-spec? -> (list symbol? string? (listof symbol?))
;;   Consume one heredoc body through its terminator line.
(define (read-heredoc-body-token in spec)
  (define out
    (open-output-string))
  (let loop ()
    (define line
      (read-physical-line-string in))
    (cond
      [(not line)
       (list 'malformed
             (get-output-string out)
             '(literal ruby-heredoc-body ruby-string-literal malformed-token ruby-error))]
      [else
       (display line out)
       (cond
         [(heredoc-terminator-line? spec line)
         (define text
           (get-output-string out))
          (define flavor-tags
            (heredoc-spec->flavor-tags spec))
          (define tags
            (cond
              [(ruby-heredoc-spec-command? spec)
                (cond
                 [(and (ruby-heredoc-spec-interpolated? spec)
                       (text-contains-interpolation? text))
                  (append '(literal ruby-heredoc-body ruby-command-literal ruby-interpolated-literal)
                          flavor-tags)]
                 [else
                  (append '(literal ruby-heredoc-body ruby-command-literal)
                          flavor-tags)])]
              [else
               (cond
                 [(and (ruby-heredoc-spec-interpolated? spec)
                       (text-contains-interpolation? text))
                  (append '(literal ruby-heredoc-body ruby-string-literal ruby-interpolated-literal)
                          flavor-tags)]
                 [else
                  (append '(literal ruby-heredoc-body ruby-string-literal)
                          flavor-tags)])]))
          (list 'literal text tags)]
        [else
         (loop)])])))

;; -----------------------------------------------------------------------------
;; Base dispatch lexer

;; make-ruby-base-lexer : -> (input-port? -> any/c)
;;   Construct the parser-tools lexer used for Ruby token dispatch.
(define (make-ruby-base-lexer)
  (lexer
   [(eof)
    'eof]
   [(:+ ruby-inline-whitespace)
    (list 'whitespace lexeme '(whitespace ruby-whitespace))]
   [ruby-newline
    (list 'whitespace lexeme '(whitespace ruby-whitespace ruby-newline))]
   ["#"
    (let ()
      (define out
        (open-output-string))
      (display lexeme out)
      (read-until-line-end! input-port out)
      (list 'comment
            (get-output-string out)
            '(comment ruby-comment)))]
   [ruby-class-variable
    (list 'identifier lexeme '(identifier ruby-class-variable))]
   [ruby-instance-variable
    (list 'identifier lexeme '(identifier ruby-instance-variable))]
   [ruby-global-variable
    (list 'identifier lexeme '(identifier ruby-global-variable))]
   [ruby-symbol-literal
    (list 'literal lexeme '(literal ruby-symbol-literal))]
   [(:or #\' #\")
    (let ()
      (define delimiter
        (string-ref lexeme 0))
      (define out
        (open-output-string))
      (display lexeme out)
      (define terminated?
        (read-quoted-string! input-port out delimiter))
      (define text
        (get-output-string out))
      (cond
        [terminated?
         (define tags
           (cond
             [(and (char=? delimiter #\")
                   (text-contains-interpolation? text))
              '(literal ruby-string-literal ruby-interpolated-literal)]
             [else
              '(literal ruby-string-literal)]))
         (list 'literal text tags)]
        [else
         (list 'malformed text '(literal malformed-token ruby-string-literal ruby-error))]))]
   [":\""
    (let ()
      (define out
        (open-output-string))
      (display lexeme out)
      (define terminated?
        (read-quoted-string! input-port out #\"))
      (define text
        (get-output-string out))
      (cond
        [terminated?
         (list 'literal text '(literal ruby-symbol-literal ruby-string-literal))]
        [else
         (list 'malformed text '(literal malformed-token ruby-symbol-literal ruby-string-literal ruby-error))]))]
   [":'"
    (let ()
      (define out
        (open-output-string))
      (display lexeme out)
      (define terminated?
        (read-quoted-string! input-port out #\'))
      (define text
        (get-output-string out))
      (cond
        [terminated?
         (list 'literal text '(literal ruby-symbol-literal ruby-string-literal))]
        [else
         (list 'malformed text '(literal malformed-token ruby-symbol-literal ruby-string-literal ruby-error))]))]
   [ruby-float
    (list 'literal lexeme '(literal ruby-number-literal))]
   [ruby-hex-integer
    (list 'literal lexeme '(literal ruby-number-literal))]
   [ruby-binary-integer
    (list 'literal lexeme '(literal ruby-number-literal))]
   [ruby-octal-integer
    (list 'literal lexeme '(literal ruby-number-literal))]
   [ruby-decimal-integer
    (list 'literal lexeme '(literal ruby-number-literal))]
   [ruby-ident-like
    (list 'identifier lexeme (keyword-token-tags lexeme))]
   [ruby-constant-like
    (list 'identifier lexeme '(identifier ruby-constant))]
   [ruby-operator-token
    (list 'operator lexeme (operator-token-tags lexeme))]
   [ruby-delimiter-token
    (list 'delimiter lexeme (operator-token-tags lexeme))]
   [ruby-any-char
    (list 'malformed lexeme '(malformed-token ruby-error))]))

;; -----------------------------------------------------------------------------
;; Public reader

;; make-ruby-derived-reader : -> (input-port? -> (or/c ruby-derived-token? 'eof))
;;   Construct a stateful Ruby derived-token reader.
(define (make-ruby-derived-reader)
  (define base-lexer
    (make-ruby-base-lexer))
  (define at-stream-start?
    #t)
  (define slash-can-start-regexp?
    #t)
  (define pending-heredocs
    '())
  (define pending-derived-tokens
    '())
  (define last-significant-token
    #f)
  (define saw-whitespace-since-significant?
    #f)
  (define method-name-context
    'none)
  (define method-reference-context
    0)
  ;; uppercase-command-regexp-context? : -> boolean?
  ;;   Detect a DSL-like command-word context such as Given /regexp/ do.
  (define (uppercase-command-regexp-context?)
    (cond
      [(not last-significant-token)
       #f]
      [else
       (define text
         (ruby-derived-token-text last-significant-token))
       (and saw-whitespace-since-significant?
            (ruby-derived-token-has-tag? last-significant-token 'ruby-constant)
            (positive? (string-length text))
            (char-upper-case? (string-ref text 0)))]))
  ;; update-slash-context! : ruby-derived-token? -> void?
  ;;   Track whether a following slash can begin a regexp literal.
  (define (update-slash-context! token)
    (define text
      (ruby-derived-token-text token))
    (cond
      [(or (ruby-derived-token-has-tag? token 'comment)
           (ruby-derived-token-has-tag? token 'whitespace))
       (when (ruby-derived-token-has-tag? token 'whitespace)
         (set! saw-whitespace-since-significant? #t))
       (when (ruby-derived-token-has-tag? token 'ruby-newline)
         (set! slash-can-start-regexp? #t))
       (void)]
      [(ruby-derived-token-has-tag? token 'keyword)
       (set! last-significant-token token)
       (set! saw-whitespace-since-significant? #f)
       (set! slash-can-start-regexp?
             (not (member text
                          '("true" "false" "nil" "self" "__FILE__"
                            "__LINE__" "__ENCODING__" "end"))))]
      [(ruby-derived-token-has-tag? token 'operator)
       (set! last-significant-token token)
       (set! saw-whitespace-since-significant? #f)
       (set! slash-can-start-regexp? #t)]
      [(ruby-derived-token-has-tag? token 'delimiter)
       (set! last-significant-token token)
       (set! saw-whitespace-since-significant? #f)
       (set! slash-can-start-regexp?
             (member text '("(" "[" "{" "," ";" ":" "=>" "::")))]
      [else
       (set! last-significant-token token)
       (set! saw-whitespace-since-significant? #f)
       (set! slash-can-start-regexp? #f)]))
  ;; update-method-name-context! : ruby-derived-token? -> void?
  ;;   Track lightweight context after def and singleton receivers.
  (define (update-method-name-context! token)
    (define text
      (ruby-derived-token-text token))
    (cond
      [(or (ruby-derived-token-has-tag? token 'comment)
           (ruby-derived-token-has-tag? token 'whitespace))
       (void)]
      [(eq? method-name-context 'none)
       (when (and (ruby-derived-token-has-tag? token 'ruby-keyword)
                  (string=? text "def"))
         (set! method-name-context 'expect-name))]
      [(eq? method-name-context 'expect-name)
       (cond
         [(and (ruby-derived-token-has-tag? token 'keyword)
               (string=? text "self"))
          (set! method-name-context 'expect-dot)]
         [else
          (set! method-name-context 'none)])]
      [(eq? method-name-context 'expect-dot)
       (cond
         [(or (string=? text ".")
              (string=? text "::"))
          (set! method-name-context 'expect-name)]
         [else
          (set! method-name-context 'none)])]
      [else
       (set! method-name-context 'none)]))
  ;; update-method-reference-context! : ruby-derived-token? -> void?
  ;;   Track lightweight context after alias and undef.
  (define (update-method-reference-context! token)
    (define text
      (ruby-derived-token-text token))
    (cond
      [(or (ruby-derived-token-has-tag? token 'comment)
           (ruby-derived-token-has-tag? token 'whitespace))
       (void)]
      [(zero? method-reference-context)
       (cond
         [(and (ruby-derived-token-has-tag? token 'ruby-keyword)
               (string=? text "alias"))
          (set! method-reference-context 2)]
         [(and (ruby-derived-token-has-tag? token 'ruby-keyword)
               (string=? text "undef"))
          (set! method-reference-context 1)]
         [else
          (void)])]
      [else
       (set! method-reference-context (sub1 method-reference-context))]))
  (define (next-derived-token in)
    (cond
      [(pair? pending-derived-tokens)
       (define token
         (car pending-derived-tokens))
       (set! pending-derived-tokens (cdr pending-derived-tokens))
       (update-method-name-context! token)
       (update-method-reference-context! token)
       (update-slash-context! token)
       token]
      [else
       (define start-pos
         (current-stream-position in))
       (define raw
         (cond
           [(pair? pending-heredocs)
            (define spec
              (car pending-heredocs))
            (set! pending-heredocs (cdr pending-heredocs))
            (read-heredoc-body-token in spec)]
           [else
            (define next
              (peek-next in))
            (cond
              [(eof-object? next)
               'eof]
              [(and (char? next)
                    (char=? next #\/)
                    (or slash-can-start-regexp?
                        (uppercase-command-regexp-context?)))
               (read-regexp-literal in)]
              [(positive? method-reference-context)
               (define raw-token
                 (or (try-read-symbol-literal in)
                     (try-read-identifier-method-name in)
                     (try-read-operator-method-name in)
                     (base-lexer in)))
               (match raw-token
                 [(list _ _ tags)
                  (cond
                    [(or (member 'ruby-symbol-literal tags)
                         (member 'ruby-method-name tags)
                         (member 'ruby-operator-method-name tags)
                         (member 'ruby-identifier tags))
                     (identifier-token->method-reference raw-token)]
                    [else
                     raw-token])])]
              [(and (char? next)
                    (char=? next #\`))
               (read-backtick-command-literal in)]
              [(and (char? next)
                    (char=? next #\:))
               (or (try-read-symbol-literal in)
                   (base-lexer in))]
              [(and (char? next)
                    (char=? next #\%))
               (or (try-read-percent-literal in)
                   (base-lexer in))]
              [(eq? method-name-context 'expect-name)
               (or (try-read-identifier-method-name in)
                   (try-read-operator-method-name in)
                   (base-lexer in))]
              [(and (char? next)
                    (char=? next #\?))
               (or (try-read-character-literal in)
                   (base-lexer in))]
              [(and (char? next)
                    (char=? next #\<))
               (define maybe-heredoc
                 (try-read-heredoc-introducer in))
               (cond
                 [maybe-heredoc
                  (define token
                    (first maybe-heredoc))
                  (define spec
                    (second maybe-heredoc))
                  (set! pending-heredocs
                        (append pending-heredocs (list spec)))
                  token]
                 [else
                  (base-lexer in)])]
              [(and (char? next)
                    (char=? next #\\))
               (read-one-char-token in 'operator '(operator ruby-line-continuation))]
              [else
               (base-lexer in)])]))
       (define end-pos
         (current-stream-position in))
       (cond
         [(eq? raw 'eof)
          'eof]
         [else
          (define text
            (second raw))
          (define base-tags
            (third raw))
          (define tags
            (cond
              [(and at-stream-start?
                    (string-prefix? text "#!"))
               '(comment ruby-comment ruby-shebang-comment)]
              [else
               base-tags]))
          (set! at-stream-start?
                (and at-stream-start?
                     (member 'whitespace base-tags)
                     (not (member 'ruby-newline base-tags))))
          (define base-token
            (make-token-from-text start-pos end-pos text tags))
          (define token
            (retag-keyword-argument-label base-token in))
          (define maybe-chunks
            (split-interpolated-literal-chunks token))
          (cond
            [maybe-chunks
             (define split-tokens
               (split-derived-token-with-chunks token maybe-chunks))
             (set! pending-derived-tokens (cdr split-tokens))
             (set! token (car split-tokens))]
            [else
             (void)])
          (update-method-name-context! token)
          (update-method-reference-context! token)
          (update-slash-context! token)
          token])]))
  next-derived-token)
