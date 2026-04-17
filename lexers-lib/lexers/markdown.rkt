#lang racket/base

;;;
;;; Markdown Lexer
;;;
;;
;; Public entry points for the GitHub-flavored Markdown lexer.

;; make-markdown-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Markdown lexer.
;; make-markdown-derived-lexer : -> (input-port? -> (or/c markdown-derived-token? 'eof))
;;   Construct a port-based Markdown lexer that returns derived Markdown token
;;   values.
;; markdown-derived-token?     : any/c -> boolean?
;;   Recognize a derived Markdown token value returned by the derived-token API.
;; markdown-derived-token-tags : markdown-derived-token? -> (listof symbol?)
;;   Extract the Markdown-specific classification tags for one derived token.
;; markdown-derived-token-has-tag? : markdown-derived-token? symbol? -> boolean?
;;   Determine whether a derived Markdown token has a given classification tag.
;; markdown-derived-token-text : markdown-derived-token? -> string?
;;   Extract the source text corresponding to one derived Markdown token.
;; markdown-derived-token-start : markdown-derived-token? -> position?
;;   Extract the starting source position for one derived Markdown token.
;; markdown-derived-token-end  : markdown-derived-token? -> position?
;;   Extract the ending source position for one derived Markdown token.
;; markdown-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Markdown string using the Markdown lexer.
;; markdown-string->derived-tokens : string? -> (listof markdown-derived-token?)
;;   Tokenize an entire Markdown string into derived Markdown token values.
;; markdown-profiles           : immutable-hash?
;;   Profile defaults for the public Markdown lexer.

(provide make-markdown-lexer
         make-markdown-derived-lexer
         markdown-derived-token?
         markdown-derived-token-tags
         markdown-derived-token-has-tag?
         markdown-derived-token-text
         markdown-derived-token-start
         markdown-derived-token-end
         markdown-string->tokens
         markdown-string->derived-tokens
         markdown-profiles)

(require parser-tools/lex
         "private/config.rkt"
         (rename-in "private/markdown-derived.rkt"
                    [markdown-derived-token? private-markdown-derived-token?]
                    [markdown-derived-token-tags private-markdown-derived-token-tags]
                    [markdown-derived-token-has-tag? private-markdown-derived-token-has-tag?]
                    [markdown-derived-token-text private-markdown-derived-token-text]
                    [markdown-derived-token-start private-markdown-derived-token-start]
                    [markdown-derived-token-end private-markdown-derived-token-end]
                    [make-markdown-derived-reader private-make-markdown-derived-reader])
         "private/markdown-tokenize.rkt"
         "private/parser-tools-compat.rkt")

(define markdown-profiles markdown-profile-defaults)

;; markdown-derived-token? : any/c -> boolean?
;;   Recognize a derived Markdown token value returned by the derived-token API.
(define (markdown-derived-token? v)
  (private-markdown-derived-token? v))

;; markdown-derived-token-tags : markdown-derived-token? -> (listof symbol?)
;;   Extract the Markdown-specific classification tags for one derived token.
(define (markdown-derived-token-tags token)
  (private-markdown-derived-token-tags token))

;; markdown-derived-token-has-tag? : markdown-derived-token? symbol? -> boolean?
;;   Determine whether a derived Markdown token has a given classification tag.
(define (markdown-derived-token-has-tag? token tag)
  (private-markdown-derived-token-has-tag? token tag))

;; markdown-derived-token-text : markdown-derived-token? -> string?
;;   Extract the source text corresponding to one derived Markdown token.
(define (markdown-derived-token-text token)
  (private-markdown-derived-token-text token))

;; markdown-derived-token-start : markdown-derived-token? -> position?
;;   Extract the starting source position for one derived Markdown token.
(define (markdown-derived-token-start token)
  (private-markdown-derived-token-start token))

;; markdown-derived-token-end : markdown-derived-token? -> position?
;;   Extract the ending source position for one derived Markdown token.
(define (markdown-derived-token-end token)
  (private-markdown-derived-token-end token))

;; make-markdown-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Markdown lexer.
(define (make-markdown-lexer #:profile          [profile 'coloring]
                             #:trivia           [trivia 'profile-default]
                             #:source-positions [source-positions 'profile-default])
  (define config
    (make-markdown-config #:profile          profile
                          #:trivia           trivia
                          #:source-positions source-positions))
  (make-markdown-token-reader config))

;; make-markdown-derived-lexer : -> (input-port? -> (or/c markdown-derived-token? 'eof))
;;   Construct a port-based Markdown lexer that returns derived token values.
(define (make-markdown-derived-lexer)
  (private-make-markdown-derived-reader))

;; markdown-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Markdown string using the projected token API.
(define (markdown-string->tokens source
                                 #:profile          [profile 'coloring]
                                 #:trivia           [trivia 'profile-default]
                                 #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-markdown-lexer #:profile          profile
                         #:trivia           trivia
                         #:source-positions source-positions))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (cond
      [(eof-token? token) (reverse (cons token tokens))]
      [else               (loop (cons token tokens))])))

;; markdown-string->derived-tokens : string? -> (listof markdown-derived-token?)
;;   Tokenize an entire Markdown string into derived Markdown token values.
(define (markdown-string->derived-tokens source)
  (define lexer (make-markdown-derived-lexer))
  (define in (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token (lexer in))
    (cond
      [(eq? token 'eof)
       (reverse tokens)]
      [else
       (loop (cons token tokens))])))

(module+ test
  (require rackunit
           racket/list)

  ;; first-token-before-rest? : (-> (input-port? -> any)) string? string? -> any
  ;;   Read the first token before the second chunk is written.
  (define (first-token-before-rest? make-lexer first-chunk rest-chunk)
    (define lexer
      (make-lexer))
    (define-values (in out)
      (make-pipe))
    (write-string first-chunk out)
    (flush-output out)
    (define result-channel
      (make-channel))
    (thread (lambda ()
              (channel-put result-channel (lexer in))))
    (define token
      (sync/timeout 1 result-channel))
    (write-string rest-chunk out)
    (close-output-port out)
    token)

  ;; derive-with-timeout : string? real? -> (or/c (listof markdown-derived-token?) #f)
  ;;   Tokenize source in a worker thread and fail fast on accidental hangs.
  (define (derive-with-timeout source seconds)
    (define result-channel
      (make-channel))
    (define worker
      (thread (lambda ()
                (channel-put result-channel
                             (markdown-string->derived-tokens source)))))
    (define result
      (sync/timeout seconds result-channel))
    (unless result
      (kill-thread worker))
    result)

  ;; contiguous-derived-stream? : (listof markdown-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (markdown-derived-token-end left))
         (position-offset (markdown-derived-token-start right)))))

  ;; has-adjacent-texts? : (listof markdown-derived-token?) string? string? -> boolean?
  ;;   Determine whether adjacent token texts occur in order.
  (define (has-adjacent-texts? tokens left-text right-text)
    (for/or ([left  (in-list tokens)]
             [right (in-list (cdr tokens))])
      (and (string=? (markdown-derived-token-text left) left-text)
           (string=? (markdown-derived-token-text right) right-text))))

  (define heading-tokens
    (markdown-string->tokens "# Title\nParagraph  \n"
                             #:profile 'coloring
                             #:source-positions #f))
  (define list-tokens
    (markdown-string->tokens "- [x] done\n1. item\n> quote\n"
                             #:profile 'compiler
                             #:source-positions #f))
  (define inline-tokens
    (markdown-string->tokens
     "See [site](https://example.com \"Title\") and ![alt](img.png) plus `code` and ~~gone~~.\n"
     #:profile 'compiler
     #:source-positions #f))
  (define table-tokens
    (markdown-string->tokens
     "| A | B |\n| :- | -: |\n| 1 | 2 |\n"
     #:profile 'coloring
     #:source-positions #f))
  (define fenced-js-tokens
    (markdown-string->tokens
     "```js\nconst x = 1;\n```\n"
     #:profile 'compiler
     #:source-positions #f))
  (define fenced-unknown-tokens
    (markdown-string->tokens
     "```unknown\nx\n```\n"
     #:profile 'compiler
     #:source-positions #f))
  (define indented-code-tokens
    (markdown-string->tokens
     "    code\n    more\n"
     #:profile 'compiler
     #:source-positions #f))
  (define html-tokens
    (markdown-string->tokens
     "Text <span class=\"x\">hi</span>\n<table><tr><td>x</td></tr></table>\n"
     #:profile 'coloring
     #:source-positions #f))
  (define mixed-derived-tokens
    (markdown-string->derived-tokens
     "# Title\n- [ ] task\n| A | B |\n| :- | -: |\n| 1 | 2 |\n\n```js\nconst x = 1;\n```\n\n```rkt\n(define x 1)\n```\n\n```wat\n(module (func (result i32) (i32.const 42)))\n```\n\n```html\n<style>.x { color: #fff; }</style><script>const y = 2;</script>\n```\n\nText <span class=\"x\">hi</span>\n"))
  (define derived-heading-marker
    (findf (lambda (token)
             (markdown-derived-token-has-tag? token 'markdown-heading-marker))
           mixed-derived-tokens))
  (define derived-heading-text
    (findf (lambda (token)
             (markdown-derived-token-has-tag? token 'markdown-heading-text))
           mixed-derived-tokens))
  (define derived-task-marker
    (findf (lambda (token)
             (markdown-derived-token-has-tag? token 'markdown-task-marker))
           mixed-derived-tokens))
  (define derived-table-pipe
    (findf (lambda (token)
             (markdown-derived-token-has-tag? token 'markdown-table-pipe))
           mixed-derived-tokens))
  (define derived-table-alignment
    (findf (lambda (token)
             (markdown-derived-token-has-tag? token 'markdown-table-alignment))
           mixed-derived-tokens))
  (define derived-js-token
    (findf (lambda (token)
             (and (markdown-derived-token-has-tag? token 'embedded-javascript)
                  (markdown-derived-token-has-tag? token 'keyword)
                  (string=? (markdown-derived-token-text token) "const")))
           mixed-derived-tokens))
  (define derived-racket-token
    (findf (lambda (token)
             (and (markdown-derived-token-has-tag? token 'embedded-racket)
                  (markdown-derived-token-has-tag? token 'racket-definition-form)))
           mixed-derived-tokens))
  (define derived-html-token
    (findf (lambda (token)
             (and (markdown-derived-token-has-tag? token 'embedded-html)
                  (markdown-derived-token-has-tag? token 'html-tag-name)
                  (string=? (markdown-derived-token-text token) "span")))
           mixed-derived-tokens))
  (define derived-css-token
    (findf (lambda (token)
             (and (markdown-derived-token-has-tag? token 'embedded-html)
                  (markdown-derived-token-has-tag? token 'embedded-css)
                  (markdown-derived-token-has-tag? token 'color-literal)))
           mixed-derived-tokens))
  (define derived-wat-token
    (findf (lambda (token)
             (and (markdown-derived-token-has-tag? token 'embedded-wat)
                  (markdown-derived-token-has-tag? token 'wat-instruction)
                  (string=? (markdown-derived-token-text token) "i32.const")))
           mixed-derived-tokens))
  (define derived-hard-break
    (findf (lambda (token)
             (markdown-derived-token-has-tag? token 'markdown-hard-line-break))
           (markdown-string->derived-tokens "a  \nb")))
  (define fenced-bash-derived-tokens
    (markdown-string->derived-tokens "```bash\necho hi\n```\n"))
  (define fenced-racket-derived-tokens
    (markdown-string->derived-tokens "```racket\n(+ 1 2)\n```\n"))
  (define fenced-webracket-derived-tokens
    (markdown-string->derived-tokens "```webracket.rkt\n(test)\n```\n"))
  (define lone-fence-derived-tokens
    (markdown-string->derived-tokens "```"))
  (define lone-fence+newline-derived-tokens
    (markdown-string->derived-tokens "```\n"))
  (define unterminated-fence-with-content-derived-tokens
    (markdown-string->derived-tokens "```js\nconst x = 1;\n"))
  (define closed-fence-derived-tokens
    (markdown-string->derived-tokens "```js\nconst x = 1;\n```\n"))
  (define fenced-bash-newline
    (findf (lambda (token)
             (and (markdown-derived-token-has-tag? token 'whitespace)
                  (string=? (markdown-derived-token-text token) "\n")))
           fenced-bash-derived-tokens))
  (define fenced-racket-newline
    (findf (lambda (token)
             (and (markdown-derived-token-has-tag? token 'whitespace)
                  (string=? (markdown-derived-token-text token) "\n")))
           fenced-racket-derived-tokens))
  (define fenced-webracket-newline
    (findf (lambda (token)
             (and (markdown-derived-token-has-tag? token 'whitespace)
                  (string=? (markdown-derived-token-text token) "\n")))
           fenced-webracket-derived-tokens))
  (define hard-break-source
    "alpha  \nbeta\n")
  (define prose-exclamation-source
    "Compiled and executed the first example will most likely print \"Hello World!\" correctly.\n")
  (define prose-exclamation-derived-tokens
    (derive-with-timeout prose-exclamation-source 2))
  (define prose-tilde-source
    "Ordinary prose with a lone ~ tilde.\n")
  (define prose-tilde-derived-tokens
    (derive-with-timeout prose-tilde-source 2))
  (define prose-backslash-source
    "Trailing slash \\\n")
  (define prose-backslash-derived-tokens
    (derive-with-timeout prose-backslash-source 2))
  (define streaming-first-token
    (first-token-before-rest? make-markdown-derived-lexer
                              "# Title\n\n"
                              "More text.\n"))
  (define nested-fence-source
    "### Minimal example\n\n```racket\n(define x 1)\n```\n\n- item\n  ```racket\n  (define y 2)\n  ```\n")
  (define nested-fence-derived-tokens
    (markdown-string->derived-tokens nested-fence-source))
  (define nested-fence-projected-tokens
    (markdown-string->tokens nested-fence-source
                             #:profile 'coloring
                             #:source-positions #f))
  (define exact-canvas-slice-source
    "### Minimal example\n\n```racket\n(define window   (js-window-window))\n(define document (js-window-document))\n(define canvas   (js-get-element-by-id document \"app-canvas\"))\n(define ctx      (js-canvas-get-context canvas \"2d\" (void)))\n\n(js-set-canvas-width! canvas 640)\n(js-set-canvas-height! canvas 480)\n(js-set-canvas2d-fill-style! ctx \"#4cc9f0\")\n(js-canvas2d-fill-rect ctx 40 40 200 120)\n```\n\nThe remainder of this guide documents every canvas-related binding and shows a short usage example.\n\n## HTMLCanvasElement bindings\n\n- **`(js-canvas-capture-stream canvas fps)`** – Start streaming the canvas contents at `fps` frames per second. Returns a `MediaStream`.\n  ```racket\n  (define stream (js-canvas-capture-stream canvas 30.0))\n  ```\n")
  (define exact-canvas-slice-derived-tokens
    (markdown-string->derived-tokens exact-canvas-slice-source))
  (define hard-break-derived-tokens
    (markdown-string->derived-tokens hard-break-source))
  (define hard-break-projected-tokens
    (markdown-string->tokens hard-break-source
                             #:profile 'coloring
                             #:source-positions #t))
  (define hard-break-token
    (findf (lambda (token)
             (markdown-derived-token-has-tag? token 'markdown-hard-line-break))
           hard-break-derived-tokens))
  (define hard-break-text-token
    (findf (lambda (token)
             (and (markdown-derived-token-has-tag? token 'markdown-text)
                  (string=? (markdown-derived-token-text token) "alpha")))
           hard-break-derived-tokens))
  (define indented-opening-fence-whitespace
    (has-adjacent-texts? nested-fence-derived-tokens
                         "  "
                         "```"))
  (define indented-closing-fence
    (findf (lambda (token)
             (and (markdown-derived-token-has-tag? token 'markdown-code-fence)
                  (string=? (markdown-derived-token-text token) "  ```")))
           nested-fence-derived-tokens))

  (check-equal? (map stream-token-name heading-tokens)
                '(delimiter whitespace literal whitespace literal delimiter whitespace eof))
  (check-equal? (map stream-token-name list-tokens)
                '(delimiter delimiter literal delimiter literal delimiter literal eof))
  (check-not-false (member 'delimiter (map stream-token-name inline-tokens)))
  (check-not-false (member 'literal (map stream-token-name table-tokens)))
  (check-not-false (member 'identifier (map stream-token-name fenced-js-tokens)))
  (check-equal? (map stream-token-name fenced-unknown-tokens)
                '(delimiter identifier literal delimiter eof))
  (check-equal? (map stream-token-name indented-code-tokens)
                '(literal eof))
  (check-not-false (member 'identifier (map stream-token-name html-tokens)))
  (check-true (stream-token-has-positions? (car (markdown-string->tokens "# h"))))
  (check-equal? (map stream-token-name (markdown-string->tokens "# h" #:source-positions #f))
                '(delimiter whitespace literal eof))
  (check-equal? (map stream-token-name
                     (markdown-string->tokens "# h\n"
                                              #:profile 'compiler
                                              #:source-positions #f))
                '(delimiter literal eof))
  (check-not-false derived-heading-marker)
  (check-not-false derived-heading-text)
  (check-not-false derived-task-marker)
  (check-not-false derived-table-pipe)
  (check-not-false derived-table-alignment)
  (check-not-false derived-js-token)
  (check-not-false derived-racket-token)
  (check-not-false derived-html-token)
  (check-not-false derived-css-token)
  (check-not-false derived-wat-token)
  (check-not-false derived-hard-break)
  (check-not-false fenced-bash-newline)
  (check-not-false fenced-racket-newline)
  (check-not-false fenced-webracket-newline)
  (check-not-false hard-break-token)
  (check-not-false hard-break-text-token)
  (check-not-false prose-exclamation-derived-tokens)
  (check-not-false prose-tilde-derived-tokens)
  (check-not-false prose-backslash-derived-tokens)
  (check-not-false streaming-first-token)
  (check-not-false indented-opening-fence-whitespace)
  (check-not-false indented-closing-fence)
  (check-not-false (markdown-derived-token-has-tag? streaming-first-token
                                                    'markdown-heading-marker))
  (check-equal? (markdown-derived-token-text hard-break-token)
                "  ")
  (check-equal? (apply string-append
                       (map markdown-derived-token-text prose-exclamation-derived-tokens))
                prose-exclamation-source)
  (check-equal? (apply string-append
                       (map markdown-derived-token-text prose-tilde-derived-tokens))
                prose-tilde-source)
  (check-equal? (apply string-append
                       (map markdown-derived-token-text prose-backslash-derived-tokens))
                prose-backslash-source)
  (check-equal? (apply string-append
                       (map markdown-derived-token-text nested-fence-derived-tokens))
                nested-fence-source)
  (check-equal? (apply string-append
                       (for/list ([token (in-list nested-fence-projected-tokens)]
                                  #:unless (eof-token? token))
                         (stream-token-value token)))
                nested-fence-source)
  (check-equal? (apply string-append
                       (map markdown-derived-token-text exact-canvas-slice-derived-tokens))
                exact-canvas-slice-source)
  (check-equal? (apply string-append
                       (map markdown-derived-token-text hard-break-derived-tokens))
                hard-break-source)
  (check-equal? (apply string-append
                       (for/list ([token (in-list hard-break-projected-tokens)]
                                  #:unless (eof-token? token))
                         (stream-token-value token)))
                hard-break-source)
  (check-equal? (apply string-append
                       (map markdown-derived-token-text lone-fence-derived-tokens))
                "```")
  (check-equal? (apply string-append
                       (map markdown-derived-token-text lone-fence+newline-derived-tokens))
                "```\n")
  (check-equal? (apply string-append
                       (map markdown-derived-token-text unterminated-fence-with-content-derived-tokens))
                "```js\nconst x = 1;\n")
  (check-equal? (apply string-append
                       (map markdown-derived-token-text closed-fence-derived-tokens))
                "```js\nconst x = 1;\n```\n")
  (check-not-false
   (findf (lambda (token)
            (markdown-derived-token-has-tag? token 'markdown-code-fence))
          lone-fence-derived-tokens))
  (check-not-false
   (findf (lambda (token)
            (markdown-derived-token-has-tag? token 'markdown-code-block))
          unterminated-fence-with-content-derived-tokens))
  (check-true (contiguous-derived-stream? fenced-bash-derived-tokens))
  (check-true (contiguous-derived-stream? fenced-racket-derived-tokens))
  (check-true (contiguous-derived-stream? fenced-webracket-derived-tokens))
  (check-true (contiguous-derived-stream? hard-break-derived-tokens))
  (check-true (contiguous-derived-stream? prose-exclamation-derived-tokens))
  (check-true (contiguous-derived-stream? prose-tilde-derived-tokens))
  (check-true (contiguous-derived-stream? prose-backslash-derived-tokens))
  (check-true (contiguous-derived-stream? nested-fence-derived-tokens))
  (check-true (contiguous-derived-stream? exact-canvas-slice-derived-tokens))
  (check-true (contiguous-derived-stream? lone-fence-derived-tokens))
  (check-true (contiguous-derived-stream? lone-fence+newline-derived-tokens))
  (check-true (contiguous-derived-stream? unterminated-fence-with-content-derived-tokens))
  (check-true (contiguous-derived-stream? closed-fence-derived-tokens)))
