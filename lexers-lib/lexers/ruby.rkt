#lang racket/base

;;;
;;; Ruby Lexer
;;;
;;
;; Public entry points for the Ruby lexer.

;; make-ruby-lexer         : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Ruby lexer.
;; make-ruby-derived-lexer : -> (input-port? -> (or/c ruby-derived-token? 'eof))
;;   Construct a port-based Ruby lexer that returns derived Ruby token values.
;; ruby-derived-token?     : any/c -> boolean?
;;   Recognize a derived Ruby token value returned by the derived-token API.
;; ruby-derived-token-tags : ruby-derived-token? -> (listof symbol?)
;;   Extract the Ruby-specific classification tags for one derived token.
;; ruby-derived-token-has-tag? : ruby-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
;; ruby-derived-token-text : ruby-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
;; ruby-derived-token-start : ruby-derived-token? -> position?
;;   Extract the starting source position for one derived token.
;; ruby-derived-token-end  : ruby-derived-token? -> position?
;;   Extract the ending source position for one derived token.
;; ruby-string->tokens     : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Ruby string using the Ruby lexer.
;; ruby-string->derived-tokens : string? -> (listof ruby-derived-token?)
;;   Tokenize an entire Ruby string into derived Ruby token values.
;; ruby-profiles           : immutable-hash?
;;   Profile defaults for the public Ruby lexer.

(provide make-ruby-lexer
         make-ruby-derived-lexer
         ruby-derived-token?
         ruby-derived-token-tags
         ruby-derived-token-has-tag?
         ruby-derived-token-text
         ruby-derived-token-start
         ruby-derived-token-end
         ruby-string->tokens
         ruby-string->derived-tokens
         ruby-profiles)

(require parser-tools/lex
         racket/list
         racket/string
         "private/config.rkt"
         (rename-in "private/ruby-derived.rkt"
                    [ruby-derived-token? private-ruby-derived-token?]
                    [ruby-derived-token-tags private-ruby-derived-token-tags]
                    [ruby-derived-token-has-tag? private-ruby-derived-token-has-tag?]
                    [ruby-derived-token-text private-ruby-derived-token-text]
                    [ruby-derived-token-start private-ruby-derived-token-start]
                    [ruby-derived-token-end private-ruby-derived-token-end]
                    [make-ruby-derived-reader private-make-ruby-derived-reader])
         "private/ruby-tokenize.rkt"
         "token.rkt")

(define ruby-profiles
  ruby-profile-defaults)

;; ruby-derived-token? : any/c -> boolean?
;;   Recognize a derived Ruby token value returned by the derived-token API.
(define (ruby-derived-token? v)
  (private-ruby-derived-token? v))

;; ruby-derived-token-tags : ruby-derived-token? -> (listof symbol?)
;;   Extract the Ruby-specific classification tags for one derived token.
(define (ruby-derived-token-tags token)
  (private-ruby-derived-token-tags token))

;; ruby-derived-token-has-tag? : ruby-derived-token? symbol? -> boolean?
;;   Determine whether a derived token has a given classification tag.
(define (ruby-derived-token-has-tag? token tag)
  (private-ruby-derived-token-has-tag? token tag))

;; ruby-derived-token-text : ruby-derived-token? -> string?
;;   Extract the source text corresponding to one derived token.
(define (ruby-derived-token-text token)
  (private-ruby-derived-token-text token))

;; ruby-derived-token-start : ruby-derived-token? -> position?
;;   Extract the starting source position for one derived token.
(define (ruby-derived-token-start token)
  (private-ruby-derived-token-start token))

;; ruby-derived-token-end : ruby-derived-token? -> position?
;;   Extract the ending source position for one derived token.
(define (ruby-derived-token-end token)
  (private-ruby-derived-token-end token))

;; make-ruby-lexer : keyword-arguments -> (input-port? -> token-like?)
;;   Construct a port-based Ruby lexer.
(define (make-ruby-lexer #:profile          [profile 'coloring]
                         #:trivia           [trivia 'profile-default]
                         #:source-positions [source-positions 'profile-default])
  (define config
    (make-ruby-config #:profile          profile
                      #:trivia           trivia
                      #:source-positions source-positions))
  (make-ruby-token-reader config))

;; make-ruby-derived-lexer : -> (input-port? -> (or/c ruby-derived-token? 'eof))
;;   Construct a port-based Ruby lexer that returns derived token values.
(define (make-ruby-derived-lexer)
  (private-make-ruby-derived-reader))

;; ruby-string->tokens : string? keyword-arguments -> (listof token-like?)
;;   Tokenize an entire Ruby string using the projected token API.
(define (ruby-string->tokens source
                             #:profile          [profile 'coloring]
                             #:trivia           [trivia 'profile-default]
                             #:source-positions [source-positions 'profile-default])
  (define lexer
    (make-ruby-lexer #:profile          profile
                     #:trivia           trivia
                     #:source-positions source-positions))
  (define in
    (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token
      (lexer in))
    (cond
      [(lexer-token-eof? token)
       (reverse (cons token tokens))]
      [else
       (loop (cons token tokens))])))

;; ruby-string->derived-tokens : string? -> (listof ruby-derived-token?)
;;   Tokenize an entire Ruby string into derived Ruby token values.
(define (ruby-string->derived-tokens source)
  (define lexer
    (make-ruby-derived-lexer))
  (define in
    (open-input-string source))
  (port-count-lines! in)
  (let loop ([tokens '()])
    (define token
      (lexer in))
    (cond
      [(eq? token 'eof)
       (reverse tokens)]
      [else
       (loop (cons token tokens))])))

(module+ test
  (require rackunit)

  ;; contiguous-derived-stream? : (listof ruby-derived-token?) -> boolean?
  ;;   Determine whether adjacent derived tokens cover the source contiguously.
  (define (contiguous-derived-stream? tokens)
    (for/and ([left  (in-list tokens)]
              [right (in-list (cdr tokens))])
      (= (position-offset (ruby-derived-token-end left))
         (position-offset (ruby-derived-token-start right)))))

  ;; first-token-before-rest? : (-> any) string? string? -> any
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
    (thread
     (lambda ()
       (channel-put result-channel (lexer in))))
    (define token
      (sync/timeout 1 result-channel))
    (write-string rest-chunk out)
    (close-output-port out)
    token)

  (define sample-source
    "#!/usr/bin/env ruby\nmodule Demo\n  class Greeter\n    def initialize(name)\n      @name = name\n    end\n\n    def call!\n      puts :hello\n      puts \"Hello, #{@name}\"\n    end\n  end\nend\n")
  (define sample-derived
    (ruby-string->derived-tokens sample-source))
  (define sample-tokens
    (ruby-string->tokens sample-source
                         #:profile 'coloring
                         #:source-positions #f))
  (define compiler-tokens
    (ruby-string->tokens sample-source
                         #:profile 'compiler
                         #:source-positions #f))
  (define malformed-string-derived
    (ruby-string->derived-tokens "puts \"unterminated"))
  (define regexp-source
    "if `flex -V` !~ /^flex \\d+\\.\\d+$/\n  name.gsub(/[-_.\\s]([a-z])/){ $1.upcase } \\\n    .gsub('+', 'x')\nend\n")
  (define regexp-derived
    (ruby-string->derived-tokens regexp-source))
  (define percent-source
    "a = %Q{hello #{name}}\nb = %r{foo/bar}ix\nc = %w[foo bar]\n")
  (define percent-derived
    (ruby-string->derived-tokens percent-source))
  (define extended-literal-source
    "hex = 0xFF\nbin = 0b1010i\noct = 0o755r\nlegacy = 0755\nchar = ?.\nescape = ?\\n\nctrl = ?\\C-a\nmeta = ?\\M-a\nunicode = ?\\u{41}\nhexchar = ?\\x41\nrx = %R{foo}i\ncmd = %X(ls)\nsym = %S{name}\nsyms = %I[foo #{bar}]\n")
  (define extended-literal-derived
    (ruby-string->derived-tokens extended-literal-source))
  (define method-name-source
    "class Demo\n  def self.foo(bar)\n  end\n\n  def value=(x)\n  end\n\n  def <<(item)\n  end\nend\n")
  (define method-name-derived
    (ruby-string->derived-tokens method-name-source))
  (define method-reference-source
    "class Demo\n  alias new_name old_name\n  alias :size? :empty?\n  undef value=\n  undef <<\nend\n")
  (define method-reference-derived
    (ruby-string->derived-tokens method-reference-source))
  (define symbol-method-source
    "return false unless method == :+\nif something == :==\n  qux :[]=, :foo=\nend\n")
  (define symbol-method-derived
    (ruby-string->derived-tokens symbol-method-source))
  (define interpolated-source
    "puts \"Hello, #{name}\"\ncmd = `echo #{$USER}`\nrx = %R{#{prefix}[a-z]+}\nplain = 'no #{interp}'\nconfig = {timeout: 3, mode: :fast}\n")
  (define interpolated-derived
    (ruby-string->derived-tokens interpolated-source))
  (define heredoc-source
    "message = <<-EOF\nhello\n#{name}\nEOF\nvalue = (<<~HTML).gsub(/^  /, '')\n  <p>Hi</p>\nHTML\nplain = <<-'TXT'\n#{name}\nTXT\n")
  (define heredoc-derived
    (ruby-string->derived-tokens heredoc-source))
  (define crlf-source
    "module Demo\r\n  value = :ok\r\nend\r\n")
  (define crlf-derived
    (ruby-string->derived-tokens crlf-source))
  (define first-streaming-token
    (first-token-before-rest? make-ruby-derived-lexer
                              "class "
                              "Demo\nend\n"))

  (define shebang-token
    (findf (lambda (token)
             (ruby-derived-token-has-tag? token 'ruby-shebang-comment))
           sample-derived))
  (define keyword-token
    (findf (lambda (token)
             (ruby-derived-token-has-tag? token 'ruby-keyword))
           sample-derived))
  (define instance-variable-token
    (findf (lambda (token)
             (ruby-derived-token-has-tag? token 'ruby-instance-variable))
           sample-derived))
  (define symbol-token
    (findf (lambda (token)
             (ruby-derived-token-has-tag? token 'ruby-symbol-literal))
           sample-derived))
  (define string-token
    (findf (lambda (token)
             (ruby-derived-token-has-tag? token 'ruby-string-literal))
           sample-derived))
  (define malformed-token
    (findf (lambda (token)
             (ruby-derived-token-has-tag? token 'malformed-token))
           malformed-string-derived))
  (define regexp-token
    (findf (lambda (token)
             (ruby-derived-token-has-tag? token 'ruby-regexp-literal))
           regexp-derived))
  (define backtick-token
    (findf (lambda (token)
             (ruby-derived-token-has-tag? token 'ruby-command-literal))
           regexp-derived))
  (define backreference-token
    (findf (lambda (token)
             (and (ruby-derived-token-has-tag? token 'ruby-global-variable)
                  (string=? (ruby-derived-token-text token) "$1")))
           regexp-derived))
  (define continuation-token
    (findf (lambda (token)
             (ruby-derived-token-has-tag? token 'ruby-line-continuation))
           regexp-derived))
  (define percent-string-token
    (findf (lambda (token)
             (and (ruby-derived-token-has-tag? token 'ruby-percent-literal)
                  (ruby-derived-token-has-tag? token 'ruby-string-literal)))
           percent-derived))
  (define percent-word-token
    (findf (lambda (token)
             (ruby-derived-token-has-tag? token 'ruby-word-list-literal))
           percent-derived))
  (define hex-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "0xFF"))
           extended-literal-derived))
  (define binary-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "0b1010i"))
           extended-literal-derived))
  (define octal-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "0o755r"))
           extended-literal-derived))
  (define legacy-octal-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "0755"))
           extended-literal-derived))
  (define character-token
    (findf (lambda (token)
             (ruby-derived-token-has-tag? token 'ruby-character-literal))
           extended-literal-derived))
  (define control-character-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "?\\C-a"))
           extended-literal-derived))
  (define meta-character-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "?\\M-a"))
           extended-literal-derived))
  (define unicode-character-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "?\\u{41}"))
           extended-literal-derived))
  (define upper-percent-regexp-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "%R{foo}i"))
           extended-literal-derived))
  (define upper-percent-command-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "%X(ls)"))
           extended-literal-derived))
  (define upper-percent-symbol-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "%S{name}"))
           extended-literal-derived))
  (define percent-symbol-list-token
    (findf (lambda (token)
             (and (ruby-derived-token-has-tag? token 'ruby-symbol-list-literal)
                  (string-prefix? (ruby-derived-token-text token) "%I[foo")))
           extended-literal-derived))
  (define singleton-method-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "foo"))
           method-name-derived))
  (define setter-method-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "value="))
           method-name-derived))
  (define operator-method-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "<<"))
           method-name-derived))
  (define alias-new-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "new_name"))
           method-reference-derived))
  (define alias-old-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "old_name"))
           method-reference-derived))
  (define alias-symbol-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) ":size?"))
           method-reference-derived))
  (define undef-setter-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "value="))
           method-reference-derived))
  (define undef-operator-token
    (findf (lambda (token)
             (and (string=? (ruby-derived-token-text token) "<<")
                  (ruby-derived-token-has-tag? token 'ruby-method-reference)))
           method-reference-derived))
  (define plus-symbol-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) ":+"))
           symbol-method-derived))
  (define equals-symbol-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) ":=="))
           symbol-method-derived))
  (define index-assign-symbol-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) ":[]="))
           symbol-method-derived))
  (define setter-symbol-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) ":foo="))
           symbol-method-derived))
  (define interpolated-string-token
    (findf (lambda (token)
             (and (string=? (ruby-derived-token-text token) "\"Hello, ")
                  (ruby-derived-token-has-tag? token 'ruby-string-literal)))
           interpolated-derived))
  (define string-interpolation-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "#{name}"))
           interpolated-derived))
  (define interpolated-command-token
    (findf (lambda (token)
             (and (string=? (ruby-derived-token-text token) "#{$USER}")
                  (ruby-derived-token-has-tag? token 'ruby-interpolation)))
           interpolated-derived))
  (define interpolated-regexp-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "#{prefix}"))
           interpolated-derived))
  (define plain-single-quoted-token
    (findf (lambda (token)
             (string=? (ruby-derived-token-text token) "'no #{interp}'"))
           interpolated-derived))
  (define keyword-argument-label-token
    (findf (lambda (token)
             (and (string=? (ruby-derived-token-text token) "timeout")
                  (ruby-derived-token-has-tag? token 'ruby-keyword-argument-label)))
           interpolated-derived))
  (define second-keyword-argument-label-token
    (findf (lambda (token)
             (and (string=? (ruby-derived-token-text token) "mode")
                  (ruby-derived-token-has-tag? token 'ruby-keyword-argument-label)))
           interpolated-derived))
  (define heredoc-introducer-token
    (findf (lambda (token)
             (ruby-derived-token-has-tag? token 'ruby-heredoc-introducer))
           heredoc-derived))
  (define heredoc-body-token
    (findf (lambda (token)
             (ruby-derived-token-has-tag? token 'ruby-heredoc-body))
           heredoc-derived))
  (define interpolated-heredoc-body-token
    (findf (lambda (token)
             (and (ruby-derived-token-has-tag? token 'ruby-heredoc-body)
                  (string-prefix? (ruby-derived-token-text token) "\nhello\n")))
           heredoc-derived))
  (define plain-heredoc-body-token
    (findf (lambda (token)
             (and (ruby-derived-token-has-tag? token 'ruby-heredoc-body)
                  (string-prefix? (ruby-derived-token-text token) "\n#{name}\n")
                  (ruby-derived-token-has-tag? token 'ruby-noninterpolating-heredoc)))
           heredoc-derived))
  (define heredoc-interpolation-token
    (findf (lambda (token)
             (and (string=? (ruby-derived-token-text token) "#{name}")
                  (ruby-derived-token-has-tag? token 'ruby-interpolation)))
           heredoc-derived))
  (define squiggly-heredoc-introducer-token
    (findf (lambda (token)
             (and (ruby-derived-token-has-tag? token 'ruby-heredoc-introducer)
                  (string=? (ruby-derived-token-text token) "<<~HTML")))
           heredoc-derived))

  (check-equal? (take (map lexer-token-name sample-tokens) 8)
                '(comment whitespace keyword whitespace identifier whitespace whitespace keyword))
  (check-equal? (last compiler-tokens) 'eof)
  (check-equal? (take (map lexer-token-name compiler-tokens) 6)
                '(keyword identifier keyword identifier keyword identifier))
  (check-not-false shebang-token)
  (check-not-false keyword-token)
  (check-not-false instance-variable-token)
  (check-not-false symbol-token)
  (check-not-false string-token)
  (check-not-false malformed-token)
  (check-not-false regexp-token)
  (check-not-false backtick-token)
  (check-not-false backreference-token)
  (check-not-false continuation-token)
  (check-not-false percent-string-token)
  (check-not-false percent-word-token)
  (check-not-false hex-token)
  (check-not-false (ruby-derived-token-has-tag? hex-token 'ruby-number-literal))
  (check-not-false binary-token)
  (check-not-false (ruby-derived-token-has-tag? binary-token 'ruby-number-literal))
  (check-not-false octal-token)
  (check-not-false (ruby-derived-token-has-tag? octal-token 'ruby-number-literal))
  (check-not-false legacy-octal-token)
  (check-not-false (ruby-derived-token-has-tag? legacy-octal-token 'ruby-number-literal))
  (check-not-false character-token)
  (check-not-false control-character-token)
  (check-not-false meta-character-token)
  (check-not-false unicode-character-token)
  (check-not-false upper-percent-regexp-token)
  (check-not-false (ruby-derived-token-has-tag? upper-percent-regexp-token 'ruby-regexp-literal))
  (check-not-false upper-percent-command-token)
  (check-not-false (ruby-derived-token-has-tag? upper-percent-command-token 'ruby-command-literal))
  (check-not-false upper-percent-symbol-token)
  (check-not-false (ruby-derived-token-has-tag? upper-percent-symbol-token 'ruby-symbol-literal))
  (check-not-false percent-symbol-list-token)
  (check-not-false (ruby-derived-token-has-tag? percent-symbol-list-token 'ruby-symbol-list-literal))
  (check-not-false singleton-method-token)
  (check-not-false (ruby-derived-token-has-tag? singleton-method-token 'ruby-method-name))
  (check-not-false setter-method-token)
  (check-not-false (ruby-derived-token-has-tag? setter-method-token 'ruby-method-name))
  (check-not-false operator-method-token)
  (check-not-false (ruby-derived-token-has-tag? operator-method-token 'ruby-operator-method-name))
  (check-not-false alias-new-token)
  (check-not-false (ruby-derived-token-has-tag? alias-new-token 'ruby-method-reference))
  (check-not-false alias-old-token)
  (check-not-false (ruby-derived-token-has-tag? alias-old-token 'ruby-method-reference))
  (check-not-false alias-symbol-token)
  (check-not-false (ruby-derived-token-has-tag? alias-symbol-token 'ruby-method-reference))
  (check-not-false undef-setter-token)
  (check-not-false (ruby-derived-token-has-tag? undef-setter-token 'ruby-method-reference))
  (check-not-false undef-operator-token)
  (check-not-false (ruby-derived-token-has-tag? undef-operator-token 'ruby-method-reference))
  (check-not-false plus-symbol-token)
  (check-not-false (ruby-derived-token-has-tag? plus-symbol-token 'ruby-symbol-literal))
  (check-not-false (ruby-derived-token-has-tag? plus-symbol-token 'ruby-method-symbol-literal))
  (check-not-false equals-symbol-token)
  (check-not-false (ruby-derived-token-has-tag? equals-symbol-token 'ruby-method-symbol-literal))
  (check-not-false index-assign-symbol-token)
  (check-not-false (ruby-derived-token-has-tag? index-assign-symbol-token 'ruby-method-symbol-literal))
  (check-not-false setter-symbol-token)
  (check-not-false (ruby-derived-token-has-tag? setter-symbol-token 'ruby-method-symbol-literal))
  (check-not-false interpolated-string-token)
  (check-not-false string-interpolation-token)
  (check-not-false (ruby-derived-token-has-tag? string-interpolation-token 'ruby-interpolation))
  (check-not-false interpolated-command-token)
  (check-not-false (ruby-derived-token-has-tag? interpolated-command-token 'ruby-interpolation))
  (check-not-false interpolated-regexp-token)
  (check-not-false (ruby-derived-token-has-tag? interpolated-regexp-token 'ruby-interpolation))
  (check-not-false plain-single-quoted-token)
  (check-false (ruby-derived-token-has-tag? plain-single-quoted-token 'ruby-interpolated-literal))
  (check-not-false keyword-argument-label-token)
  (check-not-false second-keyword-argument-label-token)
  (check-not-false heredoc-introducer-token)
  (check-not-false heredoc-body-token)
  (check-not-false interpolated-heredoc-body-token)
  (check-not-false heredoc-interpolation-token)
  (check-not-false (ruby-derived-token-has-tag? interpolated-heredoc-body-token 'ruby-indented-heredoc))
  (check-not-false (ruby-derived-token-has-tag? interpolated-heredoc-body-token 'ruby-interpolating-heredoc))
  (check-not-false plain-heredoc-body-token)
  (check-false (ruby-derived-token-has-tag? plain-heredoc-body-token 'ruby-interpolated-literal))
  (check-not-false (ruby-derived-token-has-tag? plain-heredoc-body-token 'ruby-noninterpolating-heredoc))
  (check-not-false squiggly-heredoc-introducer-token)
  (check-not-false (ruby-derived-token-has-tag? squiggly-heredoc-introducer-token 'ruby-squiggly-heredoc))
  (check-not-false first-streaming-token)
  (check-not-false (ruby-derived-token-has-tag? first-streaming-token 'ruby-keyword))
  (check-true (contiguous-derived-stream? sample-derived))
  (check-equal? sample-source
                (apply string-append (map ruby-derived-token-text sample-derived)))
  (check-equal? regexp-source
                (apply string-append (map ruby-derived-token-text regexp-derived)))
  (check-equal? percent-source
                (apply string-append (map ruby-derived-token-text percent-derived)))
  (check-equal? extended-literal-source
                (apply string-append (map ruby-derived-token-text extended-literal-derived)))
  (check-equal? method-name-source
                (apply string-append (map ruby-derived-token-text method-name-derived)))
  (check-equal? method-reference-source
                (apply string-append (map ruby-derived-token-text method-reference-derived)))
  (check-equal? symbol-method-source
                (apply string-append (map ruby-derived-token-text symbol-method-derived)))
  (check-equal? interpolated-source
                (apply string-append (map ruby-derived-token-text interpolated-derived)))
  (check-equal? heredoc-source
                (apply string-append (map ruby-derived-token-text heredoc-derived)))
  (check-equal? crlf-source
                (apply string-append (map ruby-derived-token-text crlf-derived))))
