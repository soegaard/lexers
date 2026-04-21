#lang racket/base

;;;
;;; Markdown Derived Tokens
;;;
;;
;; Stateful GitHub-flavored Markdown tokenization and reusable Markdown
;; classifications.

;; markdown-derived-token?         : any/c -> boolean?
;;   Recognize a derived Markdown token.
;; markdown-derived-token-text     : markdown-derived-token? -> string?
;;   Extract the source text for one derived Markdown token.
;; markdown-derived-token-start    : markdown-derived-token? -> position?
;;   Extract the starting source position for one derived Markdown token.
;; markdown-derived-token-end      : markdown-derived-token? -> position?
;;   Extract the ending source position for one derived Markdown token.
;; markdown-derived-token-tags     : markdown-derived-token? -> (listof symbol?)
;;   Extract the reusable classification tags for one derived Markdown token.
;; markdown-derived-token-has-tag? : markdown-derived-token? symbol? -> boolean?
;;   Determine whether a derived Markdown token has a given classification tag.
;; make-markdown-derived-reader    : -> (input-port? -> (or/c markdown-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived Markdown tokens.

(provide markdown-derived-token?
         markdown-derived-token-text
         markdown-derived-token-start
         markdown-derived-token-end
         markdown-derived-token-tags
         markdown-derived-token-has-tag?
         make-markdown-derived-reader)

(require parser-tools/lex
         racket/list
         racket/port
         racket/string
         "../csv.rkt"
         "../c.rkt"
         "../cpp.rkt"
         "../css.rkt"
         "../html.rkt"
         "../javascript.rkt"
         "../json.rkt"
         "../makefile.rkt"
         "../plist.rkt"
         "../objc.rkt"
         "../python.rkt"
         "../racket.rkt"
         "../shell.rkt"
         "../scribble.rkt"
         "../swift.rkt"
         "../tsv.rkt"
         "../wat.rkt"
         "../yaml.rkt"
         "parser-tools-compat.rkt"
         "string-compat.rkt")

;; A derived Markdown token with reusable tags and source positions.
(struct markdown-derived-token (kind text start end tags) #:transparent)

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

;; make-md-token : (vectorof exact-nonnegative-integer?) exact-nonnegative-integer? exact-nonnegative-integer? symbol? string? (listof symbol?) -> markdown-derived-token?
;;   Construct one Markdown derived token from explicit bounds and tags.
(define (make-md-token starts start end kind text tags)
  (markdown-derived-token kind
                          text
                          (position-at starts start)
                          (position-at starts end)
                          (remove-duplicates tags)))

;; wrap-derived-tokens : listof-token exact-nonnegative-integer? (position? -> string?) (position? -> position?) (token -> listof symbol?) (token -> string?) (listof symbol?) -> (listof markdown-derived-token?)
;;   Wrap delegated derived tokens into Markdown derived tokens.
(define (delegated-shared-tags text tags)
  (append
   (cond
     [(member 'comment tags)
      '(comment)]
     [(regexp-match? #px"^[ \t\r\n]+$" text)
      '(whitespace)]
     [(member 'keyword tags)
      '(keyword)]
     [(or (member 'identifier tags)
          (member 'declaration-name tags)
          (member 'parameter-name tags)
          (member 'property-name tags)
          (member 'object-key tags)
          (member 'objc-identifier tags)
          (member 'method-name tags)
          (member 'private-name tags)
          (member 'json-object-key tags)
          (member 'cpp-identifier tags)
          (member 'at-rule-name tags)
          (member 'function-name tags)
          (member 'custom-property-name tags)
          (member 'property-name-candidate tags)
          (member 'html-tag-name tags)
          (member 'html-closing-tag-name tags)
          (member 'html-attribute-name tags)
          (member 'jsx-tag-name tags)
          (member 'jsx-closing-tag-name tags)
          (member 'jsx-attribute-name tags)
          (member 'racket-symbol tags)
          (member 'racket-no-color tags)
          (member 'racket-other tags)
          (member 'scribble-symbol tags)
          (member 'scribble-command tags)
          (member 'shell-word tags)
          (member 'csv-field tags)
          (member 'c-identifier tags)
          (member 'swift-identifier tags)
          (member 'tsv-field tags)
          (member 'yaml-anchor tags)
          (member 'yaml-alias tags)
          (member 'yaml-tag tags)
          (member 'wat-identifier tags))
      '(identifier)]
     [(member 'yaml-directive tags)
      '(keyword)]
     [(or (member 'literal tags)
          (member 'string-literal tags)
          (member 'numeric-literal tags)
          (member 'regex-literal tags)
          (member 'template-literal tags)
          (member 'template-chunk tags)
          (member 'color-literal tags)
          (member 'color-function tags)
          (member 'gradient-function tags)
          (member 'length-dimension tags)
          (member 'html-text tags)
          (member 'html-attribute-value tags)
          (member 'html-entity tags)
          (member 'html-doctype tags)
          (member 'json-string tags)
          (member 'json-number tags)
          (member 'json-true tags)
          (member 'json-false tags)
          (member 'json-null tags)
          (member 'racket-string tags)
          (member 'racket-constant tags)
          (member 'racket-hash-colon-keyword tags)
          (member 'scribble-text tags)
          (member 'scribble-string tags)
          (member 'scribble-constant tags)
          (member 'shell-string-literal tags)
          (member 'shell-variable tags)
          (member 'shell-command-substitution tags)
          (member 'shell-option tags)
          (member 'shell-numeric-literal tags)
          (member 'csv-field tags)
          (member 'c-string-literal tags)
          (member 'c-char-literal tags)
          (member 'c-numeric-literal tags)
          (member 'swift-string-literal tags)
          (member 'swift-numeric-literal tags)
          (member 'tsv-field tags)
          (member 'c-header-name tags)
          (member 'yaml-plain-scalar tags)
          (member 'yaml-string-literal tags)
          (member 'yaml-boolean tags)
          (member 'yaml-null tags)
          (member 'yaml-number tags)
          (member 'yaml-block-scalar-content tags)
          (member 'wat-string-literal tags)
          (member 'wat-numeric-literal tags))
      '(literal)]
    [(or (member 'wat-form tags)
          (member 'wat-type tags)
          (member 'wat-instruction tags)
          (member 'c-keyword tags)
          (member 'c-preprocessor-directive tags)
          (member 'shell-keyword tags)
          (member 'shell-builtin tags))
      '(keyword)]
    [(or (member 'delimiter tags)
          (member 'c-delimiter tags)
          (member 'csv-separator tags)
          (member 'csv-row-separator tags)
          (member 'yaml-document-marker tags)
          (member 'yaml-flow-delimiter tags)
          (member 'yaml-value-indicator tags)
          (member 'yaml-key-indicator tags)
          (member 'yaml-sequence-indicator tags)
          (member 'yaml-block-scalar-header tags)
          (member 'racket-parenthesis tags)
          (member 'scribble-parenthesis tags)
          (member 'scribble-command-char tags)
          (member 'scribble-body-delimiter tags)
          (member 'scribble-optional-delimiter tags)
          (member 'shell-punctuation tags)
          (member 'tsv-separator tags)
          (member 'tsv-row-separator tags)
          (regexp-match? #px"^[()\\[\\]{}<>;,:.]$" text))
     '(delimiter)]
     [(or (member 'c-operator tags)
          (member 'swift-operator tags))
      '(operator)]
     [(regexp-match? #px"^[=+\\-*/!&|%^~]+$" text)
      '(operator)]
     [(member 'malformed-token tags)
      '(malformed-token)]
     [else
      '()])))

(define (wrap-derived-tokens delegated-tokens
                             base-index
                             starts
                             text-proc
                             start-proc
                             end-proc
                             tags-proc
                             extra-tags)
  (for/list ([token (in-list delegated-tokens)])
    (define local-start (start-proc token))
    (define local-end   (end-proc token))
    (define global-start
      (+ base-index (sub1 (position-offset local-start))))
    (define global-end
      (+ base-index (sub1 (position-offset local-end))))
    (markdown-derived-token 'delegated
                            (text-proc token)
                            (position-at starts global-start)
                            (position-at starts global-end)
                            (remove-duplicates
                             (append (delegated-shared-tags (text-proc token)
                                                            (tags-proc token))
                                     (tags-proc token)
                                     extra-tags)))))

;; known-fence-language : string? -> (or/c symbol? #f)
;;   Normalize a fenced-code info string to a known embedded language.
(define (known-fence-language info)
  (define parts
    (string-split (string-trim info)))
  (define primary
    (cond
      [(pair? parts)
       (string-downcase (car parts))]
      [else
       ""]))
  (cond
    [(equal? primary "")                 #f]
    [(member primary '("c" "h"))         'c]
    [(member primary '("cpp" "c++" "cc" "cxx" "hpp" "hh" "hxx")) 'cpp]
    [(member primary '("csv"))           'csv]
    [(member primary '("css"))           'css]
    [(member primary '("html"))          'html]
    [(member primary '("javascript" "js")) 'javascript]
    [(member primary '("json"))          'json]
    [(member primary '("make" "makefile" "mk")) 'makefile]
    [(member primary '("plist"))         'plist]
    [(member primary '("objc" "objective-c" "objectivec" "obj-c")) 'objc]
    [(member primary '("python" "py"))   'python]
    [(member primary '("swift"))         'swift]
    [(member primary '("tsv"))           'tsv]
    [(member primary '("yaml" "yml"))    'yaml]
    [(member primary '("jsx"))           'jsx]
    [(member primary '("racket" "rkt"))  'racket]
    [(member primary '("bash" "sh" "shell")) 'bash]
    [(member primary '("zsh"))           'zsh]
    [(member primary '("powershell" "pwsh" "ps1")) 'powershell]
    [(member primary '("scribble" "scrbl")) 'scribble]
    [(member primary '("wat" "wasm"))    'wat]
    [else                                #f]))

;; capture->string : (or/c bytes? string? #f) -> string?
;;   Normalize a regexp capture value into a string.
(define (capture->string v)
  (cond
    [(bytes? v)  (bytes->string/utf-8 v)]
    [(string? v) v]
    [else        ""]))

;; string-index-of : string? string? exact-nonnegative-integer? -> (or/c exact-nonnegative-integer? #f)
;;   Find the first occurrence of needle at or after start in haystack.
(define (string-index-of haystack needle [start 0])
  (define hlen (string-length haystack))
  (define nlen (string-length needle))
  (let loop ([i start])
    (cond
      [(> (+ i nlen) hlen) #f]
      [(substring=? haystack i needle) i]
      [else            (loop (add1 i))])))

;; delegated-fence-tokens : symbol? string? exact-nonnegative-integer? (vectorof exact-nonnegative-integer?) -> (listof markdown-derived-token?)
;;   Tokenize a fenced code body using a delegated lexer when available.
(define (delegated-fence-tokens lang body body-start starts)
  (case lang
    [(c)
     (wrap-derived-tokens (c-string->derived-tokens body)
                          body-start
                          starts
                          c-derived-token-text
                          c-derived-token-start
                          c-derived-token-end
                          c-derived-token-tags
                          '(embedded-c markdown-code-block))]
    [(cpp)
     (wrap-derived-tokens (cpp-string->derived-tokens body)
                          body-start
                          starts
                          cpp-derived-token-text
                          cpp-derived-token-start
                          cpp-derived-token-end
                          cpp-derived-token-tags
                          '(embedded-cpp markdown-code-block))]
    [(csv)
     (wrap-derived-tokens (csv-string->derived-tokens body)
                          body-start
                          starts
                          csv-derived-token-text
                          csv-derived-token-start
                          csv-derived-token-end
                          csv-derived-token-tags
                          '(embedded-csv markdown-code-block))]
    [(css)
     (wrap-derived-tokens (css-string->derived-tokens body)
                          body-start
                          starts
                          css-derived-token-text
                          css-derived-token-start
                          css-derived-token-end
                          css-derived-token-tags
                          '(embedded-css markdown-code-block))]
    [(html)
     (wrap-derived-tokens (html-string->derived-tokens body)
                          body-start
                          starts
                          html-derived-token-text
                          html-derived-token-start
                          html-derived-token-end
                          html-derived-token-tags
                          '(embedded-html markdown-code-block))]
    [(json)
     (wrap-derived-tokens (json-string->derived-tokens body)
                          body-start
                          starts
                          json-derived-token-text
                          json-derived-token-start
                          json-derived-token-end
                          json-derived-token-tags
                          '(embedded-json markdown-code-block))]
    [(makefile)
     (wrap-derived-tokens (makefile-string->derived-tokens body)
                          body-start
                          starts
                          makefile-derived-token-text
                          makefile-derived-token-start
                          makefile-derived-token-end
                          makefile-derived-token-tags
                          '(embedded-makefile markdown-code-block))]
    [(plist)
     (wrap-derived-tokens (plist-string->derived-tokens body)
                          body-start
                          starts
                          plist-derived-token-text
                          plist-derived-token-start
                          plist-derived-token-end
                          plist-derived-token-tags
                          '(embedded-plist markdown-code-block))]
    [(objc)
     (wrap-derived-tokens (objc-string->derived-tokens body)
                          body-start
                          starts
                          objc-derived-token-text
                          objc-derived-token-start
                          objc-derived-token-end
                          objc-derived-token-tags
                          '(embedded-objc markdown-code-block))]
    [(python)
     (wrap-derived-tokens (python-string->derived-tokens body)
                          body-start
                          starts
                          python-derived-token-text
                          python-derived-token-start
                          python-derived-token-end
                          python-derived-token-tags
                          '(embedded-python markdown-code-block))]
    [(yaml)
     (wrap-derived-tokens (yaml-string->derived-tokens body)
                          body-start
                          starts
                          yaml-derived-token-text
                          yaml-derived-token-start
                          yaml-derived-token-end
                          yaml-derived-token-tags
                          '(embedded-yaml markdown-code-block))]
    [(javascript)
     (wrap-derived-tokens (javascript-string->derived-tokens body #:jsx? #f)
                          body-start
                          starts
                          javascript-derived-token-text
                          javascript-derived-token-start
                          javascript-derived-token-end
                          javascript-derived-token-tags
                          '(embedded-javascript markdown-code-block))]
    [(jsx)
     (wrap-derived-tokens (javascript-string->derived-tokens body #:jsx? #t)
                          body-start
                          starts
                          javascript-derived-token-text
                          javascript-derived-token-start
                          javascript-derived-token-end
                          javascript-derived-token-tags
                          '(embedded-javascript markdown-code-block))]
    [(racket)
     (wrap-derived-tokens (racket-string->derived-tokens body)
                          body-start
                          starts
                          racket-derived-token-text
                          racket-derived-token-start
                          racket-derived-token-end
                          racket-derived-token-tags
                          '(embedded-racket markdown-code-block))]
    [(bash)
     (wrap-derived-tokens (shell-string->derived-tokens body #:shell 'bash)
                          body-start
                          starts
                          shell-derived-token-text
                          shell-derived-token-start
                          shell-derived-token-end
                          shell-derived-token-tags
                          '(embedded-shell markdown-code-block))]
    [(zsh)
     (wrap-derived-tokens (shell-string->derived-tokens body #:shell 'zsh)
                          body-start
                          starts
                          shell-derived-token-text
                          shell-derived-token-start
                          shell-derived-token-end
                          shell-derived-token-tags
                          '(embedded-shell markdown-code-block))]
    [(powershell)
     (wrap-derived-tokens (shell-string->derived-tokens body #:shell 'powershell)
                          body-start
                          starts
                          shell-derived-token-text
                          shell-derived-token-start
                          shell-derived-token-end
                          shell-derived-token-tags
                          '(embedded-shell markdown-code-block))]
    [(scribble)
     (wrap-derived-tokens (scribble-string->derived-tokens body)
                          body-start
                          starts
                          scribble-derived-token-text
                          scribble-derived-token-start
                          scribble-derived-token-end
                          scribble-derived-token-tags
                          '(embedded-scribble markdown-code-block))]
    [(swift)
     (wrap-derived-tokens (swift-string->derived-tokens body)
                          body-start
                          starts
                          swift-derived-token-text
                          swift-derived-token-start
                          swift-derived-token-end
                          swift-derived-token-tags
                          '(embedded-swift markdown-code-block))]
    [(tsv)
     (wrap-derived-tokens (tsv-string->derived-tokens body)
                          body-start
                          starts
                          tsv-derived-token-text
                          tsv-derived-token-start
                          tsv-derived-token-end
                          tsv-derived-token-tags
                          '(embedded-tsv markdown-code-block))]
    [(wat)
     (wrap-derived-tokens (wat-string->derived-tokens body)
                          body-start
                          starts
                          wat-derived-token-text
                          wat-derived-token-start
                          wat-derived-token-end
                          wat-derived-token-tags
                          '(embedded-wat markdown-code-block))]
    [else
     null]))

;; delegate-html-region : string? exact-nonnegative-integer? (vectorof exact-nonnegative-integer?) (listof symbol?) -> (listof markdown-derived-token?)
;;   Tokenize a raw HTML region and wrap the delegated derived tokens.
(define (delegate-html-region text start-index starts extra-tags)
  (wrap-derived-tokens (html-string->derived-tokens text)
                       start-index
                       starts
                       html-derived-token-text
                       html-derived-token-start
                       html-derived-token-end
                       html-derived-token-tags
                       (cons 'embedded-html extra-tags)))

;; next-line-range : string? exact-nonnegative-integer? -> (values string? string? exact-nonnegative-integer? exact-nonnegative-integer?)
;;   Extract the next line body, its newline text, and the following index.
(define (next-line-range source index)
  (define len (string-length source))
  (let loop ([i index])
    (cond
      [(>= i len)
       (values (substring source index len) "" len len)]
      [(char=? (string-ref source i) #\return)
       (cond
         [(and (< (add1 i) len)
               (char=? (string-ref source (add1 i)) #\newline))
          (values (substring source index i)
                  "\r\n"
                  i
                  (+ i 2))]
         [else
          (values (substring source index i)
                  "\r"
                  i
                  (add1 i))])]
      [(char=? (string-ref source i) #\newline)
       (values (substring source index i)
               "\n"
               i
               (add1 i))]
      [else
       (loop (add1 i))])))

;; blank-line? : string? -> boolean?
;;   Determine whether a line body is blank or all whitespace.
(define (blank-line? line)
  (regexp-match? #px"^[ \t]*$" line))

;; block-html-tag-name : string? -> (or/c string? #f)
;;   Extract a simple raw HTML tag name from a line, when present.
(define (block-html-tag-name line)
  (cond
    [(regexp-match #px"^[ \t]{0,3}<([A-Za-z][A-Za-z0-9-]*)\\b" line)
     => (lambda (m) (string-downcase (capture->string (cadr m))))]
    [else
     #f]))

;; closing-tag-index : string? string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Find the end index after the matching closing tag, or #f.
(define (closing-tag-index source tag start-index)
  (define pat
    (pregexp (format "(?i:</~a\\s*>)" (regexp-quote tag))))
  (cond
    [(regexp-match-positions pat source start-index)
     => (lambda (m) (cdar m))]
    [else #f]))

;; simple-inline-html-end : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Find a simple inline HTML region end for a tag starting at index.
(define (simple-inline-html-end source start-index)
  (define len (string-length source))
  (cond
    [(regexp-match-positions #px"^<!--" (substring source start-index))
     (define close-pos (string-index-of source "-->" start-index))
     (and close-pos (+ close-pos 3))]
    [(regexp-match #px"^</?([A-Za-z][A-Za-z0-9-]*)\\b" (substring source start-index))
     => (lambda (m)
          (define tag (string-downcase (capture->string (cadr m))))
          (define open-close (string-index-of source ">" start-index))
          (cond
            [(not open-close) #f]
            [(regexp-match? #px"/>" (substring source start-index (add1 open-close)))
             (add1 open-close)]
            [else
             (or (closing-tag-index source tag (add1 open-close))
                 (add1 open-close))]))]
    [else
     #f]))

;; simple-link-title-split : string? -> (values string? (or/c string? #f))
;;   Split a link destination and optional quoted title.
(define (simple-link-title-split inner)
  (cond
    [(regexp-match #px"^([^ \t]+)[ \t]+(\"[^\"]*\"|'[^']*')$" inner)
     => (lambda (m)
          (values (capture->string (cadr m))
                  (capture->string (caddr m))))]
    [else
     (values inner #f)]))

;; parse-inline : string? exact-nonnegative-integer? (vectorof exact-nonnegative-integer?) -> (listof markdown-derived-token?)
;;   Parse inline Markdown structure on one line body.
(define (parse-inline line line-start starts)
  (define len (string-length line))
  (define tokens '())
  ;; emit-many! : (listof markdown-derived-token?) -> void?
  ;;   Prepend a forward-order token list into the reversed accumulator.
  (define (emit-many! more)
    (set! tokens
          (append (reverse more)
                  tokens)))
  (define (emit start end kind text tags)
    (set! tokens
          (cons (make-md-token starts
                               (+ line-start start)
                               (+ line-start end)
                               kind
                               text
                               tags)
                tokens)))
  (let loop ([i 0])
    (cond
      [(>= i len)
       (reverse tokens)]
      [else
       (define ch (string-ref line i))
       (cond
         [(char=? ch #\\)
          (cond
            [(< (add1 i) len)
             (emit i (+ i 2) 'delimiter (substring line i (+ i 2))
                   '(delimiter markdown-escape))
             (loop (+ i 2))]
            [else
             (emit i (add1 i) 'literal "\\"
                   '(literal markdown-text))
             (loop (add1 i))])]
         [(and (<= (+ i 2) len)
               (regexp-match? #px"^~~" (substring line i)))
          (emit i (+ i 2) 'delimiter "~~"
                '(delimiter markdown-strikethrough-delimiter))
          (loop (+ i 2))]
         [(char=? ch #\~)
          (emit i (add1 i) 'literal "~"
                '(literal markdown-text))
          (loop (add1 i))]
         [(and (<= (+ i 2) len)
               (or (regexp-match? #px"^\\*\\*" (substring line i))
                   (regexp-match? #px"^__" (substring line i))))
          (emit i (+ i 2) 'delimiter (substring line i (+ i 2))
                '(delimiter markdown-strong-delimiter))
          (loop (+ i 2))]
         [(or (char=? ch #\*) (char=? ch #\_))
          (emit i (add1 i) 'delimiter (substring line i (add1 i))
                '(delimiter markdown-emphasis-delimiter))
          (loop (add1 i))]
         [(char=? ch #\`)
          (define end
            (or (string-index-of line "`" (add1 i))
                len))
          (define token-end
            (if (= end len) len (add1 end)))
          (emit i token-end 'literal (substring line i token-end)
                '(literal markdown-code-span))
          (loop token-end)]
         [(char=? ch #\!)
          (cond
            [(and (< (add1 i) len)
                  (char=? (string-ref line (add1 i)) #\[))
             (emit i (add1 i) 'delimiter "!"
                   '(delimiter markdown-image-marker))
             (define rb (string-index-of line "]" (+ i 2)))
             (define lp (and rb (< (add1 rb) len)
                             (char=? (string-ref line (add1 rb)) #\()
                             (add1 rb)))
             (define rp (and lp (string-index-of line ")" (add1 lp))))
             (cond
               [(and rb lp rp)
                (emit (add1 i) (+ i 2) 'delimiter "["
                      '(delimiter))
                (emit (+ i 2) rb 'literal (substring line (+ i 2) rb)
                      '(literal markdown-link-text))
                (emit rb (add1 rb) 'delimiter "]"
                      '(delimiter))
                (emit lp (add1 lp) 'delimiter "("
                      '(delimiter))
                (define-values (dest title)
                  (simple-link-title-split (substring line (+ lp 1) rp)))
                (emit (+ lp 1) (+ lp 1 (string-length dest))
                      'literal
                      dest
                      '(literal markdown-link-destination))
                (when title
                  (define title-start
                    (+ (+ lp 1 (string-length dest))
                       (string-length (regexp-replace #px"^([^ \t]+)([ \t]+).*$"
                                                     (substring line (+ lp 1) rp)
                                                     "\\2"))))
                  (emit title-start (+ title-start (string-length title))
                        'literal
                        title
                        '(literal markdown-link-title)))
                (emit rp (add1 rp) 'delimiter ")"
                      '(delimiter))
                (loop (add1 rp))]
               [else
                (emit i (add1 i) 'literal "!"
                      '(literal markdown-text))
                (loop (add1 i))])]
            [else
             (emit i (add1 i) 'literal "!"
                   '(literal markdown-text))
             (loop (add1 i))])]
         [(char=? ch #\[)
          (define rb (string-index-of line "]" (add1 i)))
          (define lp (and rb (< (add1 rb) len)
                          (char=? (string-ref line (add1 rb)) #\()
                          (add1 rb)))
          (define rp (and lp (string-index-of line ")" (add1 lp))))
          (cond
            [(and rb lp rp)
             (emit i (add1 i) 'delimiter "["
                   '(delimiter))
             (emit (add1 i) rb 'literal (substring line (add1 i) rb)
                   '(literal markdown-link-text))
             (emit rb (add1 rb) 'delimiter "]"
                   '(delimiter))
             (emit lp (add1 lp) 'delimiter "("
                   '(delimiter))
             (define-values (dest title)
               (simple-link-title-split (substring line (+ lp 1) rp)))
             (emit (+ lp 1) (+ lp 1 (string-length dest))
                   'literal
                   dest
                   '(literal markdown-link-destination))
             (when title
               (define title-start
                 (+ (+ lp 1 (string-length dest))
                    (string-length (regexp-replace #px"^([^ \t]+)([ \t]+).*$"
                                                  (substring line (+ lp 1) rp)
                                                  "\\2"))))
               (emit title-start (+ title-start (string-length title))
                     'literal
                     title
                     '(literal markdown-link-title)))
             (emit rp (add1 rp) 'delimiter ")"
                   '(delimiter))
             (loop (add1 rp))]
            [else
             (emit i (add1 i) 'literal "["
                   '(literal markdown-text))
             (loop (add1 i))])]
         [(and (char=? ch #\<)
               (or (regexp-match? #px"^<https?://[^>]+>" (substring line i))
                   (regexp-match? #px"^<[^ >]+@[^ >]+>" (substring line i))))
          (define close (or (string-index-of line ">" i) (sub1 len)))
          (emit i (add1 close) 'literal (substring line i (add1 close))
                '(literal markdown-autolink))
          (loop (add1 close))]
         [(char=? ch #\<)
          (define html-end
            (simple-inline-html-end line i))
          (cond
            [html-end
             (emit-many! (delegate-html-region (substring line i html-end)
                                               (+ line-start i)
                                               starts
                                               null))
             (loop html-end)]
            [else
             (emit i (add1 i) 'literal "<"
                   '(literal markdown-text))
             (loop (add1 i))])]
         [else
          (define next-special
            (let loop2 ([j i])
              (cond
                [(>= j len) len]
                [(member (string-ref line j)
                         '(#\\ #\* #\_ #\` #\[ #\! #\< #\~))
                 j]
                [else
                 (loop2 (add1 j))])))
          (emit i next-special 'literal (substring line i next-special)
                '(literal markdown-text))
          (loop next-special)])])))

;; parse-table-row : string? exact-nonnegative-integer? (vectorof exact-nonnegative-integer?) boolean? -> (listof markdown-derived-token?)
;;   Parse one table row or alignment row.
(define (parse-table-row line line-start starts alignment?)
  (define tokens '())
  (define len (string-length line))
  (define (emit start end kind text tags)
    (set! tokens
          (cons (make-md-token starts
                               (+ line-start start)
                               (+ line-start end)
                               kind
                               text
                               tags)
                tokens)))
  (let loop ([i 0] [cell-start 0])
    (cond
      [(>= i len)
       (when (< cell-start len)
         (emit cell-start len
               'literal
               (substring line cell-start len)
               (if alignment?
                   '(literal markdown-table-alignment)
                   '(literal markdown-table-cell))))
       (reverse tokens)]
      [(char=? (string-ref line i) #\|)
       (when (< cell-start i)
         (emit cell-start i
               'literal
               (substring line cell-start i)
               (if alignment?
                   '(literal markdown-table-alignment)
                   '(literal markdown-table-cell))))
       (emit i (add1 i) 'delimiter "|" '(delimiter markdown-table-pipe))
       (loop (add1 i) (add1 i))]
      [else
       (loop (add1 i) cell-start)])))

;; tokenize-markdown-source : string? -> (listof markdown-derived-token?)
;;   Tokenize one entire Markdown source string into derived token values.
(define (tokenize-markdown-source source)
  (define starts (line-starts source))
  (define tokens '())
  (define len (string-length source))
  ;; emit-many! : (listof markdown-derived-token?) -> void?
  ;;   Prepend a forward-order token list into the reversed accumulator.
  (define (emit-many! more)
    (set! tokens
          (append (reverse more)
                  tokens)))
  (define (emit start end kind text tags)
    (set! tokens
          (cons (make-md-token starts start end kind text tags)
                tokens)))
  ;; emit-inline-with-hard-break : string? exact-nonnegative-integer? -> void?
  ;;   Emit inline Markdown tokens, splitting a trailing two-space hard break
  ;;   into its own token without duplicating source text.
  (define (emit-inline-with-hard-break text start)
    (define text-len
      (string-length text))
    (define hard-break?
      (regexp-match? #px"  $" text))
    (define inline-end
      (cond
        [hard-break? (- text-len 2)]
        [else        text-len]))
    (when (positive? inline-end)
      (emit-many! (parse-inline (substring text 0 inline-end)
                                start
                                starts)))
    (when hard-break?
      (emit (+ start inline-end) (+ start text-len)
            'delimiter "  "
            '(delimiter markdown-hard-line-break))))
  (let loop ([index 0])
    (cond
      [(>= index len)
       (reverse tokens)]
      [else
       (define-values (line nl line-end next-index)
         (next-line-range source index))
       (define full-line-end
         (+ line-end (string-length nl)))
       (cond
         [(blank-line? line)
          (emit index full-line-end 'whitespace (substring source index full-line-end)
                '(whitespace))
          (loop full-line-end)]
         [(regexp-match #px"^([ \t]{0,3})([`~]{3,})(.*)$" line)
          => (lambda (m)
               (define indent (capture->string (cadr m)))
               (define fence  (capture->string (caddr m)))
               (define rest   (capture->string (cadddr m)))
               (define info   (string-trim rest))
               (define cursor index)
               (unless (string=? indent "")
                 (emit cursor (+ cursor (string-length indent))
                       'whitespace
                       indent
                       '(whitespace))
                 (set! cursor (+ cursor (string-length indent))))
               (define fence-char (substring fence 0 1))
               (define fence-len (string-length fence))
               (emit cursor (+ cursor (string-length fence))
                     'delimiter
                     fence
                     '(delimiter markdown-code-fence))
               (set! cursor (+ cursor (string-length fence)))
               (define leading-gap
                 (let loop-gap ([i 0])
                   (cond
                     [(>= i (string-length rest))
                      i]
                     [(or (char=? (string-ref rest i) #\space)
                          (char=? (string-ref rest i) #\tab))
                      (loop-gap (add1 i))]
                     [else
                      i])))
               (define trailing-gap
                 (let loop-gap ([i (string-length rest)])
                   (cond
                     [(<= i leading-gap)
                      i]
                     [(or (char=? (string-ref rest (sub1 i)) #\space)
                          (char=? (string-ref rest (sub1 i)) #\tab))
                      (loop-gap (sub1 i))]
                     [else
                      i])))
               (when (positive? leading-gap)
                 (emit cursor (+ cursor leading-gap)
                       'whitespace
                       (substring rest 0 leading-gap)
                       '(whitespace))
                 (set! cursor (+ cursor leading-gap)))
               (when (not (string=? info ""))
                 (emit cursor (+ cursor (string-length info))
                       'identifier
                       info
                       '(identifier markdown-code-info-string)))
               (set! cursor (+ cursor (string-length info)))
               (when (< trailing-gap (string-length rest))
                 (emit cursor (+ cursor (- (string-length rest) trailing-gap))
                       'whitespace
                       (substring rest trailing-gap)
                       '(whitespace)))
               (define header-end
                 (+ index (string-length line)))
               (when (< header-end full-line-end)
                 (emit header-end full-line-end
                       'whitespace
                       (substring source header-end full-line-end)
                       '(whitespace)))
               (define lang (known-fence-language info))
               (define close-rx
                 (pregexp (format "(?m:^[ \t]{0,3}~a{~a,}[ \t]*$)"
                                  (regexp-quote fence-char)
                                  fence-len)))
               (define after-open full-line-end)
               (define raw-close-match
                 (regexp-match-positions close-rx source after-open))
               (define close-match
                 (cond
                   [(and (list? raw-close-match)
                         (pair? raw-close-match)
                         (pair? (car raw-close-match)))
                    raw-close-match]
                   [else
                    #f]))
               (define body-end
                 (if close-match (caar close-match) len))
               (define close-line-end
                 (if close-match (cdar close-match) len))
                 (define body (substring source after-open body-end))
                 (cond
                   [lang
                    (emit-many! (delegated-fence-tokens lang body after-open starts))]
                 [else
                  (when (positive? (string-length body))
                    (emit after-open body-end
                          'literal
                          body
                          '(literal markdown-code-block)))])
               (when close-match
                 (emit body-end close-line-end
                       'delimiter
                       (substring source body-end close-line-end)
                       '(delimiter markdown-code-fence)))
               (loop close-line-end))]
         [(and (regexp-match? #px"^(?: {4}|\t)" line)
               (not (regexp-match? #px"^[ \t]{0,3}([`~]{3,})" line)))
          (define block-start index)
          (define block-end full-line-end)
          (let code-loop ([cursor full-line-end] [end block-end])
            (cond
              [(>= cursor len)
               (emit block-start end
                     'literal
                     (substring source block-start end)
                     '(literal markdown-code-block))
               (loop end)]
              [else
               (define-values (next-line next-nl _le next-cursor)
                 (next-line-range source cursor))
               (if (or (regexp-match? #px"^(?: {4}|\t)" next-line)
                       (blank-line? next-line))
                   (code-loop next-cursor next-cursor)
                   (begin
                     (emit block-start end
                           'literal
                           (substring source block-start end)
                           '(literal markdown-code-block))
                     (loop end)))]))]
         [(and (block-html-tag-name line)
               (not (regexp-match? #px"^ {0,3}<https?://" line)))
          (define tag (block-html-tag-name line))
          (define region-end
                (or (closing-tag-index source tag index)
                    (or (string-index-of source ">" index) full-line-end)))
          (emit-many! (delegate-html-region (substring source index region-end)
                                            index
                                            starts
                                            null))
          (loop region-end)]
         [(and (regexp-match? #px"^[ \t]{0,3}#{1,6}[ \t]+" line))
          (define m (regexp-match #px"^([ \t]{0,3})(#{1,6})([ \t]+)(.*?)([ \t]+#+[ \t]*)?$" line))
          (define indent (capture->string (cadr m)))
          (define marker (capture->string (caddr m)))
          (define gap    (capture->string (cadddr m)))
          (define text
            (capture->string (list-ref m 4)))
          (define cursor index)
          (unless (string=? indent "")
            (emit cursor (+ cursor (string-length indent))
                  'whitespace indent '(whitespace))
            (set! cursor (+ cursor (string-length indent))))
          (emit cursor (+ cursor (string-length marker))
                'delimiter marker '(delimiter markdown-heading-marker))
          (set! cursor (+ cursor (string-length marker)))
          (emit cursor (+ cursor (string-length gap))
                'whitespace gap '(whitespace))
          (set! cursor (+ cursor (string-length gap)))
          (unless (string=? text "")
            (emit cursor (+ cursor (string-length text))
                  'literal text '(literal markdown-heading-text)))
          (unless (string=? nl "")
            (emit line-end full-line-end 'whitespace nl '(whitespace)))
          (loop full-line-end)]
         [(regexp-match? #px"^[ \t]{0,3}(?:\\* *\\* *\\*+|- *- *-+|_ *_ *_+)[ \t]*$" line)
          (emit index full-line-end 'delimiter (substring source index full-line-end)
                '(delimiter markdown-thematic-break))
          (loop full-line-end)]
         [(and (string-contains? line "|")
               (< full-line-end len))
          (define-values (next-line next-nl _next-le _next-cursor)
            (next-line-range source full-line-end))
          (if (regexp-match? #px"^[ \t|:-]+$" next-line)
              (let table-loop ([cursor index] [first? #t] [alignment? #f])
                (define-values (row row-nl row-end row-next)
                  (next-line-range source cursor))
                (cond
                  [(and (not first?)
                        (or (blank-line? row)
                            (not (string-contains? row "|"))))
                   (loop cursor)]
                 [else
                   (emit-many! (parse-table-row row cursor starts alignment?))
                   (unless (string=? row-nl "")
                     (emit row-end row-next 'whitespace row-nl '(whitespace)))
                   (table-loop row-next #f (not first?))]))
              (let* ([list-m (regexp-match #px"^([ \t]{0,3})([-+*]|[0-9]+[.)])([ \t]+)(.*)$" line)]
                     [quote-m (regexp-match #px"^([ \t]{0,3})(> ?)(.*)$" line)])
                (cond
                  [list-m
                   (define indent (capture->string (cadr list-m)))
                   (define marker (capture->string (caddr list-m)))
                   (define gap    (capture->string (cadddr list-m)))
                   (define rest   (capture->string (list-ref list-m 4)))
                   (define cursor index)
                   (unless (string=? indent "")
                     (emit cursor (+ cursor (string-length indent))
                           'whitespace indent '(whitespace))
                     (set! cursor (+ cursor (string-length indent))))
                   (emit cursor (+ cursor (string-length marker))
                         'delimiter marker '(delimiter markdown-list-marker))
                   (set! cursor (+ cursor (string-length marker)))
                   (emit cursor (+ cursor (string-length gap))
                         'whitespace gap '(whitespace))
                   (set! cursor (+ cursor (string-length gap)))
                   (cond
                     [(regexp-match #px"^(\\[(?: |x|X)\\])([ \t]+)(.*)$" rest)
                      => (lambda (tm)
                           (define task (capture->string (cadr tm)))
                           (define task-gap (capture->string (caddr tm)))
                           (define task-rest (capture->string (list-ref tm 3)))
                           (emit cursor (+ cursor (string-length task))
                                 'delimiter task '(delimiter markdown-task-marker))
                           (set! cursor (+ cursor (string-length task)))
                           (emit cursor (+ cursor (string-length task-gap))
                                 'whitespace task-gap '(whitespace))
                           (set! cursor (+ cursor (string-length task-gap)))
                           (emit-many! (parse-inline task-rest cursor starts)))]
                     [else
                      (emit-many! (parse-inline rest cursor starts))])
                   (unless (string=? nl "")
                     (emit line-end full-line-end 'whitespace nl '(whitespace)))
                   (loop full-line-end)]
                  [quote-m
                   (define indent (capture->string (cadr quote-m)))
                   (define marker (capture->string (caddr quote-m)))
                   (define rest   (capture->string (list-ref quote-m 3)))
                   (define cursor index)
                   (unless (string=? indent "")
                     (emit cursor (+ cursor (string-length indent))
                           'whitespace indent '(whitespace))
                     (set! cursor (+ cursor (string-length indent))))
                   (emit cursor (+ cursor (string-length marker))
                         'delimiter marker '(delimiter markdown-blockquote-marker))
                   (set! cursor (+ cursor (string-length marker)))
                   (emit-inline-with-hard-break rest cursor)
                   (unless (string=? nl "")
                     (emit line-end full-line-end 'whitespace nl '(whitespace)))
                   (loop full-line-end)]
                  [else
                   (emit-inline-with-hard-break line index)
                   (unless (string=? nl "")
                     (emit line-end full-line-end 'whitespace nl '(whitespace)))
                   (loop full-line-end)])))]
         [else
          (define list-m (regexp-match #px"^([ \t]{0,3})([-+*]|[0-9]+[.)])([ \t]+)(.*)$" line))
          (define quote-m (regexp-match #px"^([ \t]{0,3})(> ?)(.*)$" line))
          (cond
            [list-m
             (define indent (capture->string (cadr list-m)))
             (define marker (capture->string (caddr list-m)))
             (define gap    (capture->string (cadddr list-m)))
             (define rest   (capture->string (list-ref list-m 4)))
             (define cursor index)
             (unless (string=? indent "")
               (emit cursor (+ cursor (string-length indent))
                     'whitespace indent '(whitespace))
               (set! cursor (+ cursor (string-length indent))))
             (emit cursor (+ cursor (string-length marker))
                   'delimiter marker '(delimiter markdown-list-marker))
             (set! cursor (+ cursor (string-length marker)))
             (emit cursor (+ cursor (string-length gap))
                   'whitespace gap '(whitespace))
             (set! cursor (+ cursor (string-length gap)))
             (cond
               [(regexp-match #px"^(\\[(?: |x|X)\\])([ \t]+)(.*)$" rest)
                => (lambda (tm)
                     (define task (capture->string (cadr tm)))
                     (define task-gap (capture->string (caddr tm)))
                     (define task-rest (capture->string (list-ref tm 3)))
                     (emit cursor (+ cursor (string-length task))
                           'delimiter task '(delimiter markdown-task-marker))
                     (set! cursor (+ cursor (string-length task)))
                     (emit cursor (+ cursor (string-length task-gap))
                           'whitespace task-gap '(whitespace))
                     (set! cursor (+ cursor (string-length task-gap)))
                     (emit-inline-with-hard-break task-rest cursor))]
               [else
                (emit-inline-with-hard-break rest cursor)])
             (unless (string=? nl "")
               (emit line-end full-line-end 'whitespace nl '(whitespace)))
             (loop full-line-end)]
            [quote-m
             (define indent (capture->string (cadr quote-m)))
             (define marker (capture->string (caddr quote-m)))
             (define rest   (capture->string (list-ref quote-m 3)))
             (define cursor index)
             (unless (string=? indent "")
               (emit cursor (+ cursor (string-length indent))
                     'whitespace indent '(whitespace))
               (set! cursor (+ cursor (string-length indent))))
             (emit cursor (+ cursor (string-length marker))
                   'delimiter marker '(delimiter markdown-blockquote-marker))
             (set! cursor (+ cursor (string-length marker)))
             (emit-inline-with-hard-break rest cursor)
             (unless (string=? nl "")
               (emit line-end full-line-end 'whitespace nl '(whitespace)))
             (loop full-line-end)]
            [else
             (emit-inline-with-hard-break line index)
             (unless (string=? nl "")
               (emit line-end full-line-end 'whitespace nl '(whitespace)))
             (loop full-line-end)])])])))

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

;; split-line-ending : string? -> (values string? string?)
;;   Split one preserved line into content and its trailing line ending.
(define (split-line-ending line*)
  (define len
    (string-length line*))
  (cond
    [(and (>= len 2)
          (char=? (string-ref line* (- len 2)) #\return)
          (char=? (string-ref line* (sub1 len)) #\newline))
     (values (substring line* 0 (- len 2))
             "\r\n")]
    [(and (positive? len)
          (char=? (string-ref line* (sub1 len)) #\newline))
     (values (substring line* 0 (sub1 len))
             "\n")]
    [(and (positive? len)
          (char=? (string-ref line* (sub1 len)) #\return))
     (values (substring line* 0 (sub1 len))
             "\r")]
    [else
     (values line* "")]))

;; read-line/preserve : input-port? -> (or/c string? eof-object?)
;;   Read one line while preserving its trailing newline sequence.
(define (read-line/preserve in)
  (define out
    (open-output-string))
  (let loop ()
    (define ch
      (read-char in))
    (cond
      [(eof-object? ch)
       (define line*
         (get-output-string out))
       (cond
         [(string=? line* "")
          eof]
         [else
          line*])]
      [else
       (write-char ch out)
       (cond
         [(char=? ch #\newline)
          (get-output-string out)]
         [(char=? ch #\return)
          (define next
            (peek-char in))
          (when (char=? next #\newline)
            (write-char (read-char in) out))
          (get-output-string out)]
         [else
          (loop)])])))

;; shift-position : position? exact-positive-integer? exact-positive-integer? exact-nonnegative-integer? -> position?
;;   Shift one local token position into the original streamed source.
(define (shift-position pos base-offset base-line base-col)
  (define local-line
    (position-line pos))
  (define local-col
    (position-col pos))
  (make-stream-position (+ base-offset (sub1 (position-offset pos)))
                        (+ base-line (sub1 local-line))
                        (cond
                          [(= local-line 1)
                           (+ base-col local-col)]
                          [else
                           local-col])))

;; shift-token : markdown-derived-token? exact-positive-integer? exact-positive-integer? exact-nonnegative-integer? -> markdown-derived-token?
;;   Shift one local Markdown token into the original streamed source.
(define (shift-token token base-offset base-line base-col)
  (markdown-derived-token (markdown-derived-token-kind token)
                          (markdown-derived-token-text token)
                          (shift-position (markdown-derived-token-start token)
                                          base-offset
                                          base-line
                                          base-col)
                          (shift-position (markdown-derived-token-end token)
                                          base-offset
                                          base-line
                                          base-col)
                          (markdown-derived-token-tags token)))

;; shifted-block-tokens : string? exact-positive-integer? exact-positive-integer? exact-nonnegative-integer? -> (listof markdown-derived-token?)
;;   Tokenize one local Markdown block and shift its positions into the original source.
(define (shifted-block-tokens block-source base-offset base-line base-col)
  (for/list ([token (in-list (tokenize-markdown-source block-source))])
    (shift-token token base-offset base-line base-col)))

;; make-markdown-derived-reader : -> (input-port? -> (or/c markdown-derived-token? 'eof))
;;   Construct a stateful port-based reader for derived Markdown tokens.
(define (make-markdown-derived-reader)
  (define tokens #f)
  (define pending-line #f)

  ;; next-line-record : input-port? -> (or/c (list/c string? position?) #f)
  ;;   Read the next preserved line, with one-line pushback support.
  (define (next-line-record in)
    (cond
      [pending-line
       (define record pending-line)
       (set! pending-line #f)
       record]
      [else
       (define start-pos
         (current-stream-position in))
       (define line*
         (read-line/preserve in))
       (cond
         [(eof-object? line*)
          #f]
         [else
          (list line* start-pos)])]))

  ;; unread-line-record! : (list/c string? position?) -> void?
  ;;   Push one preserved line back for the next block read.
  (define (unread-line-record! record)
    (set! pending-line record))

  ;; read-next-block : input-port? -> (or/c (list/c string? position?) #f)
  ;;   Read one Markdown block with enough lookahead to keep multiline blocks intact.
  (define (read-next-block in)
    (define first-record
      (next-line-record in))
    (cond
      [(not first-record)
       #f]
      [else
       (define first-line*
         (car first-record))
       (define first-pos
         (cadr first-record))
       (define out
         (open-output-string))
       (define (write-record! record)
         (write-string (car record) out))
       (define-values (first-line first-nl)
         (split-line-ending first-line*))
       (write-record! first-record)
       (cond
         [(blank-line? first-line)
          (list (get-output-string out) first-pos)]
         [(regexp-match #px"^[ \t]{0,3}([`~]{3,})(.*)$" first-line)
          => (lambda (m)
               (define fence
                 (capture->string (cadr m)))
               (define fence-char
                 (substring fence 0 1))
               (define fence-len
                 (string-length fence))
               (let loop ()
                 (define record
                   (next-line-record in))
                 (cond
                   [(not record)
                    (list (get-output-string out) first-pos)]
                   [else
                    (define line*
                      (car record))
                    (define-values (line _nl)
                      (split-line-ending line*))
                    (write-record! record)
                    (cond
                      [(regexp-match? (pregexp (format "^[ \t]{0,3}~a{~a,}[ \t]*$"
                                                       (regexp-quote fence-char)
                                                       fence-len))
                                      line)
                       (list (get-output-string out) first-pos)]
                      [else
                       (loop)])])))]
         [(and (regexp-match? #px"^(?: {4}|\t)" first-line)
               (not (regexp-match? #px"^[ \t]{0,3}([`~]{3,})" first-line)))
          (let loop ()
            (define record
              (next-line-record in))
            (cond
              [(not record)
               (list (get-output-string out) first-pos)]
              [else
               (define line*
                 (car record))
               (define-values (line _nl)
                 (split-line-ending line*))
               (cond
                 [(or (blank-line? line)
                      (regexp-match? #px"^(?: {4}|\t)" line))
                  (write-record! record)
                  (loop)]
                 [else
                  (unread-line-record! record)
                  (list (get-output-string out) first-pos)])]))]
         [else
          (let loop ()
            (define record
              (next-line-record in))
            (cond
              [(not record)
               (list (get-output-string out) first-pos)]
              [else
               (define line*
                 (car record))
               (define-values (line _nl)
                 (split-line-ending line*))
               (cond
                 [(blank-line? line)
                  (unread-line-record! record)
                  (list (get-output-string out) first-pos)]
                 [else
                  (write-record! record)
                  (loop)])]))])]))

  (lambda (in)
    (unless (input-port? in)
      (raise-argument-error 'make-markdown-derived-reader "input-port?" in))
    (port-count-lines! in)
    (when (or (not tokens) (null? tokens))
      (define block
        (read-next-block in))
      (set! tokens
            (cond
              [block
               (define block-source
                 (car block))
               (define block-pos
                 (cadr block))
               (shifted-block-tokens block-source
                                     (position-offset block-pos)
                                     (position-line block-pos)
                                     (position-col block-pos))]
              [else
               '()])))
    (cond
      [(null? tokens)
       'eof]
      [else
       (define token (car tokens))
       (set! tokens (cdr tokens))
       token])))

;; markdown-derived-token-has-tag? : markdown-derived-token? symbol? -> boolean?
;;   Determine whether a derived Markdown token has a given classification tag.
(define (markdown-derived-token-has-tag? token tag)
  (member tag (markdown-derived-token-tags token)))
