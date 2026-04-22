#lang scribble/manual

@(require scribble/example
          (for-label racket/base
                     racket/contract/base
                     parser-tools/lex
                     syntax-color/racket-lexer
                     (only-in syntax-color/scribble-lexer
                              make-scribble-inside-lexer)
                     lexers/c
                     lexers/cpp
                     lexers/csv
                     lexers/css
                     lexers/go
                     lexers/haskell
                     lexers/html
                     lexers/java
                     lexers/json
                     lexers/latex
                     lexers/makefile
                     lexers/markdown
                     lexers/objc
                     lexers/pascal
                     lexers/plist
                     lexers/python
                     lexers/racket
                     lexers/rhombus
                     lexers/rust
                     lexers/shell
                     lexers/scribble
                     lexers/swift
                     lexers/tex
                     lexers/token
                     lexers/tsv
                     lexers/javascript
                     lexers/wat
                     lexers/yaml))

@(define c-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/c))
     the-eval))

@(define cpp-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/cpp))
     the-eval))

@(define css-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/css))
     the-eval))

@(define csv-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/csv))
     the-eval))

@(define go-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/go))
     the-eval))

@(define java-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/java))
     the-eval))

@(define javascript-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/javascript))
     the-eval))

@(define html-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/html))
     the-eval))

@(define haskell-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/haskell))
     the-eval))

@(define markdown-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/markdown))
     the-eval))

@(define json-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/json))
     the-eval))

@(define makefile-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/makefile))
     the-eval))

@(define latex-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/latex))
     the-eval))

@(define plist-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/plist))
     the-eval))

@(define objc-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/objc))
     the-eval))

@(define pascal-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/pascal))
     the-eval))

@(define yaml-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/yaml))
     the-eval))

@(define python-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/python))
     the-eval))

@(define racket-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/racket))
     the-eval))

@(define rust-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/rust))
     the-eval))

@(define shell-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/shell))
     the-eval))

@(define swift-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/swift))
     the-eval))

@(define tex-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/tex))
     the-eval))

@(define tsv-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/tsv))
     the-eval))

@(define wat-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/wat))
     the-eval))

@(define scribble-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/scribble))
     the-eval))

@title{Lexers}
@author[(author+email "Jens Axel Søgaard" "jensaxel@soegaard.net")]

This manual documents the public APIs in the @tt{lexers} packages.

The library currently provides reusable lexers for multiple applications.
Syntax coloring is the first intended application, but the lexer APIs are also
designed to support other consumers.

@table-of-contents[]

@section{Overview}

The public language modules currently available are:

@itemlist[
 @item{@racketmodname[lexers/token]}
 @item{@racketmodname[lexers/c]}
 @item{@racketmodname[lexers/cpp]}
 @item{@racketmodname[lexers/csv]}
 @item{@racketmodname[lexers/css]}
 @item{@racketmodname[lexers/go]}
 @item{@racketmodname[lexers/haskell]}
 @item{@racketmodname[lexers/html]}
 @item{@racketmodname[lexers/java]}
 @item{@racketmodname[lexers/json]}
 @item{@racketmodname[lexers/latex]}
 @item{@racketmodname[lexers/makefile]}
 @item{@racketmodname[lexers/javascript]}
 @item{@racketmodname[lexers/markdown]}
 @item{@racketmodname[lexers/objc]}
 @item{@racketmodname[lexers/pascal]}
 @item{@racketmodname[lexers/plist]}
 @item{@racketmodname[lexers/python]}
 @item{@racketmodname[lexers/racket]}
 @item{@racketmodname[lexers/rhombus]}
 @item{@racketmodname[lexers/rust]}
 @item{@racketmodname[lexers/shell]}
 @item{@racketmodname[lexers/scribble]}
 @item{@racketmodname[lexers/swift]}
 @item{@racketmodname[lexers/tex]}
 @item{@racketmodname[lexers/tsv]}
 @item{@racketmodname[lexers/wat]}
 @item{@racketmodname[lexers/yaml]}]

Each language module currently exposes two related kinds of API:

@itemlist[
 @item{A projected token API intended for general consumers such as syntax
       coloring.}
 @item{A derived-token API intended for richer language-specific inspection and
       testing.}]

The projected APIs are intentionally close to
@racketmodname[parser-tools/lex]. They return bare symbols, @racket[token?]
values, and optional @racket[position-token?] wrappers built from the actual
@racketmodname[parser-tools/lex] structures, so existing parser-oriented tools
can consume them more easily.

The current profile split is:

@itemlist[
 @item{@racket['coloring] --- keeps trivia, emits @racket['unknown] for
       recoverable malformed input, and includes source positions by default.}
 @item{@racket['compiler] --- skips trivia by default, raises on malformed
       input, and includes source positions by default.}]

Across languages, the projected lexer constructors return one-argument port
readers. Create the lexer once, call it repeatedly on the same input port, and
stop when the result is an end-of-file token. The projected category symbols
themselves, such as @racket['identifier], @racket['literal], and
@racket['keyword], are intended to be the stable public API.

@subsection{Token Helpers}

The helper module @racketmodname[lexers/token] provides a small public API for
inspecting wrapped or unwrapped projected token values without reaching
directly into @racketmodname[parser-tools/lex].

@defmodule[lexers/token]

@defproc[(lexer-token-name [token (or/c symbol? token? position-token?)])
         symbol?]{
Extracts the effective token category from a wrapped or unwrapped projected
token value.}

@defproc[(lexer-token-value [token (or/c symbol? token? position-token?)])
         any/c]{
Extracts the effective token payload from a wrapped or unwrapped projected
token value. For the bare end-of-file symbol, the result is @racket[#f].}

@defproc[(lexer-token-has-positions? [token (or/c symbol? token? position-token?)])
         boolean?]{
Determines whether a wrapped or unwrapped projected token value carries source
positions.}

@defproc[(lexer-token-start [token (or/c symbol? token? position-token?)])
         (or/c position? #f)]{
Extracts the starting position from a wrapped projected token value. For
unwrapped values, the result is @racket[#f].}

@defproc[(lexer-token-end [token (or/c symbol? token? position-token?)])
         (or/c position? #f)]{
Extracts the ending position from a wrapped projected token value. For
unwrapped values, the result is @racket[#f].}

@defproc[(lexer-token-eof? [token (or/c symbol? token? position-token?)])
         boolean?]{
Determines whether a wrapped or unwrapped projected token value represents end
of input.}

@subsection{Profiles}

The public projected APIs currently support the same profile names:

@itemlist[
 @item{@racket['coloring]}
 @item{@racket['compiler]}]

The current defaults are:

@tabular[#:sep @hspace[2]
 (list (list @bold{Profile} @bold{Trivia} @bold{Source Positions} @bold{Malformed Input})
       (list @racket['coloring] @racket['keep] @racket[#t] "emit unknown tokens")
       (list @racket['compiler] @racket['skip] @racket[#t] "raise an exception"))]

For the keyword arguments accepted by @racket[make-css-lexer],
@racket[css-string->tokens], @racket[make-html-lexer],
@racket[html-string->tokens], @racket[make-json-lexer],
@racket[json-string->tokens], @racket[make-javascript-lexer],
@racket[javascript-string->tokens], @racket[make-markdown-lexer],
@racket[markdown-string->tokens], @racket[make-objc-lexer],
@racket[objc-string->tokens], @racket[make-python-lexer],
@racket[python-string->tokens], @racket[make-racket-lexer],
@racket[racket-string->tokens], @racket[make-rhombus-lexer],
@racket[rhombus-string->tokens], @racket[make-shell-lexer],
@racket[make-cpp-lexer], @racket[cpp-string->tokens],
@racket[shell-string->tokens], @racket[make-scribble-lexer],
@racket[scribble-string->tokens], @racket[make-swift-lexer],
@racket[swift-string->tokens], @racket[make-wat-lexer], and
@racket[wat-string->tokens]:

@itemlist[
 @item{@racket[#:profile] selects the named default bundle.}
 @item{@racket[#:trivia 'profile-default] means “use the trivia policy from the
       selected profile”.}
 @item{@racket[#:source-positions 'profile-default] means “use the
       source-position setting from the selected profile”.}
 @item{An explicit @racket[#:trivia] or @racket[#:source-positions] value
       overrides the selected profile default.}]

@section{CSS}

@defmodule[lexers/css]

The projected CSS API has two entry points:

@itemlist[
 @item{@racket[make-css-lexer] for streaming tokenization from an input port.}
 @item{@racket[css-string->tokens] for eager tokenization of an entire string.}]

@defproc[(make-css-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                         [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                         [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming CSS lexer.

The result is a procedure of one argument, an input port. Each call reads the
next token from the port and returns one projected token value.

When @racket[#:source-positions] is true, each result is a
@racket[position-token?] whose payload is either a bare symbol such as
@racket['eof] or a @racket[token?] carrying a projected category such as
@racket['identifier], @racket['literal], @racket['comment], or
@racket['unknown].

When @racket[#:source-positions] is false, the result is either a bare symbol
or a @racket[token?] directly.

The intended use is to create the lexer once, then call it repeatedly on the
same port until it returns an end-of-file token.

@examples[#:eval css-eval
(define lexer
  (make-css-lexer #:profile 'coloring))
(define in
  (open-input-string "color: #fff;"))
(port-count-lines! in)
(list (lexer in)
      (lexer in)
      (lexer in)
      (lexer in))
]}

@defproc[(css-string->tokens [source string?]
                             [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                             [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                             [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes an entire CSS string using the projected token API.

This is a convenience wrapper over @racket[make-css-lexer]. It opens a string
port, enables line counting, repeatedly calls the port-based lexer until
end-of-file, and returns the resulting token list.}

@subsection{CSS Returned Tokens}

The projected CSS API returns values in the same general shape as
@racketmodname[parser-tools/lex]:

@itemlist[
 @item{The end of input is reported as @racket['eof], either directly or inside
       a @racket[position-token?].}
 @item{Most ordinary results are @racket[token?] values whose
       @racket[token-name] is a projected category and whose
       @racket[token-value] contains language-specific text or metadata.}
 @item{When @racket[#:source-positions] is true, each result is wrapped in a
       @racket[position-token?].}
 @item{When @racket[#:source-positions] is false, results are returned without
       that outer wrapper.}]

Common projected CSS categories include:

@itemlist[
 @item{@racket['whitespace]}
 @item{@racket['comment]}
 @item{@racket['identifier]}
 @item{@racket['literal]}
 @item{@racket['delimiter]}
 @item{@racket['unknown]}
 @item{@racket['eof]}]

In @racket['coloring] mode, whitespace and comments are kept, and recoverable
malformed input is returned as @racket['unknown]. In @racket['compiler] mode,
whitespace and comments are skipped by default, and malformed input raises an
exception instead of producing an @racket['unknown] token.

For the current CSS scaffold, @racket[token-value] normally preserves the
original source text of the emitted token. In particular:

@itemlist[
 @item{For @racket['identifier], the value is the matched identifier text, such
       as @racket["color"] or @racket["--brand-color"].}
 @item{For @racket['literal], the value is the matched literal text, such as
       @racket["#fff"], @racket["12px"], @racket["url(foo.png)"], or
       @racket["rgb("].}
 @item{For @racket['comment] and @racket['whitespace], the value is the
       original comment or whitespace text when those categories are kept.}
 @item{For @racket['delimiter], the value is the matched delimiter text, such
       as @racket[":"], @racket[";"], or @racket["{"].}
 @item{For @racket['unknown] in tolerant mode, the value is the malformed input
       text that could not be accepted.}]

@examples[#:eval css-eval
(define inspect-lexer
  (make-css-lexer #:profile 'coloring))
(define inspect-in
  (open-input-string "color: #fff;"))
(port-count-lines! inspect-in)
(define first-token
  (inspect-lexer inspect-in))
(lexer-token-has-positions? first-token)
(lexer-token-name first-token)
(lexer-token-value first-token)
(position-offset (lexer-token-start first-token))
(position-offset (lexer-token-end first-token))
]}

@defproc[(make-css-derived-lexer)
         (input-port? . -> . (or/c 'eof css-derived-token?))]{
Constructs a streaming CSS lexer for the derived-token layer.

The result is a procedure of one argument, an input port. Each call reads the
next raw CSS token from the port, computes its CSS-specific derived
classifications, and returns one derived token value. At end of input, it
returns @racket['eof].

The intended use is the same as for @racket[make-css-lexer]: create the lexer
once, then call it repeatedly on the same port until it returns
@racket['eof].

@examples[#:eval css-eval
(define derived-lexer
  (make-css-derived-lexer))
(define derived-in
  (open-input-string "color: #fff;"))
(port-count-lines! derived-in)
(list (derived-lexer derived-in)
      (derived-lexer derived-in)
      (derived-lexer derived-in)
      (derived-lexer derived-in))
]}

@defproc[(css-string->derived-tokens [source string?])
         (listof css-derived-token?)]{
Tokenizes an entire CSS string into derived CSS token values.

This is a convenience wrapper over @racket[make-css-derived-lexer]. It opens a
string port, enables line counting, repeatedly calls the derived lexer until it
returns @racket['eof], and returns the resulting list of derived tokens.}

@defproc[(css-derived-token? [v any/c])
         boolean?]{
Recognizes derived CSS token values returned by
@racket[make-css-derived-lexer] and @racket[css-string->derived-tokens].}

@defproc[(css-derived-token-tags [token css-derived-token?])
         (listof symbol?)]{
Returns the CSS-specific classification tags attached to a derived CSS token.}

@defproc[(css-derived-token-has-tag? [token css-derived-token?]
                                     [tag symbol?])
         boolean?]{
Determines whether a derived CSS token carries a given classification tag.}

@defproc[(css-derived-token-text [token css-derived-token?])
         string?]{
Returns the exact source text corresponding to a derived CSS token.}

@defproc[(css-derived-token-start [token css-derived-token?])
         position?]{
Returns the starting source position for a derived CSS token.}

@defproc[(css-derived-token-end [token css-derived-token?])
         position?]{
Returns the ending source position for a derived CSS token.}

@subsection{CSS Derived Tokens}

A derived CSS token pairs one raw CSS token with a small list of
CSS-specific classification tags. This layer is more precise than the projected
consumer-facing categories and is meant for inspection, testing, and
language-aware tools.

The current CSS scaffold may attach tags such as:

@itemlist[
 @item{@racket['at-rule-name]}
 @item{@racket['color-literal]}
 @item{@racket['color-function]}
 @item{@racket['selector-token]}
 @item{@racket['property-name]}
 @item{@racket['declaration-value-token]}
 @item{@racket['function-name]}
 @item{@racket['gradient-function]}
 @item{@racket['custom-property-name]}
 @item{@racket['property-name-candidate]}
 @item{@racket['string-literal]}
 @item{@racket['numeric-literal]}
 @item{@racket['length-dimension]}
 @item{@racket['malformed-token]}]

@examples[#:eval css-eval
(define derived-tokens
  (css-string->derived-tokens ".foo { color: red; background: rgb(1 2 3); }"))
(map (lambda (token)
       (list (css-derived-token-text token)
             (css-derived-token-tags token)
             (css-derived-token-has-tag? token 'selector-token)
             (css-derived-token-has-tag? token 'property-name)
             (css-derived-token-has-tag? token 'declaration-value-token)
             (css-derived-token-has-tag? token 'color-literal)
             (css-derived-token-has-tag? token 'function-name)
             (css-derived-token-has-tag? token 'color-function)
             (css-derived-token-has-tag? token 'custom-property-name)
             (css-derived-token-has-tag? token 'string-literal)
             (css-derived-token-has-tag? token 'numeric-literal)
             (css-derived-token-has-tag? token 'length-dimension)))
     derived-tokens)
]}

@defthing[css-profiles immutable-hash?]{
The profile defaults used by the CSS lexer.}

@section{HTML}

@defmodule[lexers/html]

The projected HTML API has two entry points:

@itemlist[
 @item{@racket[make-html-lexer] for streaming tokenization from an input port.}
 @item{@racket[html-string->tokens] for eager tokenization of an entire string.}]

@defproc[(make-html-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                          [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                          [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming HTML lexer.

The result is a procedure of one argument, an input port. Each call reads the
next token from the port and returns one projected token value.

The projected HTML token stream includes ordinary markup tokens and inline
delegated tokens from embedded @tt{<style>} and @tt{<script>} bodies.

When @racket[#:source-positions] is true, each result is a
@racket[position-token?]. When it is false, the result is either a bare symbol
or a @racket[token?] directly.

@examples[#:eval html-eval
(define lexer
  (make-html-lexer #:profile 'coloring))
(define in
  (open-input-string "<section id=main>Hi</section>"))
(port-count-lines! in)
(list (lexer in)
      (lexer in)
      (lexer in)
      (lexer in))
]}

@defproc[(html-string->tokens [source string?]
                              [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                              [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                              [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes an entire HTML string using the projected token API.

This is a convenience wrapper over @racket[make-html-lexer].}

@subsection{HTML Returned Tokens}

Common projected HTML categories include:

@itemlist[
 @item{@racket['comment]}
 @item{@racket['keyword]}
 @item{@racket['identifier]}
 @item{@racket['literal]}
 @item{@racket['operator]}
 @item{@racket['delimiter]}
 @item{@racket['unknown]}
 @item{@racket['eof]}]

For the current HTML scaffold:

@itemlist[
 @item{tag names and attribute names project as @racket['identifier]}
 @item{attribute values, text nodes, entities, and delegated CSS/JS literals
       project as @racket['literal]}
 @item{punctuation such as @tt{<}, @tt{</}, @tt{>}, @tt{/>}, and embedded
       interpolation boundaries project as @racket['delimiter] or
       @racket['operator]}
 @item{comments project as @racket['comment]}
 @item{doctype/declaration markup projects as @racket['keyword]}]

@examples[#:eval html-eval
(define inspect-lexer
  (make-html-lexer #:profile 'coloring))
(define inspect-in
  (open-input-string "<!doctype html><main id=\"app\">Hi &amp; bye</main>"))
(port-count-lines! inspect-in)
(define first-token
  (inspect-lexer inspect-in))
(lexer-token-has-positions? first-token)
(lexer-token-name first-token)
(lexer-token-value first-token)
(position-offset (lexer-token-start first-token))
(position-offset (lexer-token-end first-token))
]}

@defproc[(make-html-derived-lexer)
         (input-port? . -> . (or/c 'eof html-derived-token?))]{
Constructs a streaming HTML lexer for the derived-token layer.}

@defproc[(html-string->derived-tokens [source string?])
         (listof html-derived-token?)]{
Tokenizes an entire HTML string into derived HTML token values.}

@defproc[(html-derived-token? [v any/c])
         boolean?]{
Recognizes derived HTML token values returned by
@racket[make-html-derived-lexer] and
@racket[html-string->derived-tokens].}

@defproc[(html-derived-token-tags [token html-derived-token?])
         (listof symbol?)]{
Returns the HTML-specific classification tags attached to a derived HTML token.}

@defproc[(html-derived-token-has-tag? [token html-derived-token?]
                                      [tag symbol?])
         boolean?]{
Determines whether a derived HTML token carries a given classification tag.}

@defproc[(html-derived-token-text [token html-derived-token?])
         string?]{
Returns the exact source text corresponding to a derived HTML token.}

@defproc[(html-derived-token-start [token html-derived-token?])
         position?]{
Returns the starting source position for a derived HTML token.}

@defproc[(html-derived-token-end [token html-derived-token?])
         position?]{
Returns the ending source position for a derived HTML token.}

@subsection{HTML Derived Tokens}

The current HTML scaffold may attach tags such as:

@itemlist[
 @item{@racket['html-tag-name]}
 @item{@racket['html-closing-tag-name]}
 @item{@racket['html-attribute-name]}
 @item{@racket['html-attribute-value]}
 @item{@racket['html-text]}
 @item{@racket['html-entity]}
 @item{@racket['html-doctype]}
 @item{@racket['comment]}
 @item{@racket['embedded-css]}
 @item{@racket['embedded-javascript]}
 @item{@racket['malformed-token]}]

Delegated CSS and JavaScript body tokens keep their reusable semantic tags and
gain an additional language marker such as @racket['embedded-css] or
@racket['embedded-javascript].

@examples[#:eval html-eval
(define derived-tokens
  (html-string->derived-tokens
   "<!doctype html><section id=main class=\"card\">Hi &amp; bye<style>.hero { color: #c33; }</style><script>const root = document.querySelector(\"#app\");</script></section>"))
(map (lambda (token)
       (list (html-derived-token-text token)
             (html-derived-token-tags token)
             (html-derived-token-has-tag? token 'html-tag-name)
             (html-derived-token-has-tag? token 'html-attribute-name)
             (html-derived-token-has-tag? token 'html-attribute-value)
             (html-derived-token-has-tag? token 'html-text)
             (html-derived-token-has-tag? token 'html-entity)
             (html-derived-token-has-tag? token 'embedded-css)
             (html-derived-token-has-tag? token 'embedded-javascript)))
     derived-tokens)
]}

@defthing[html-profiles immutable-hash?]{
The profile defaults used by the HTML lexer.}

@section{C}

@defmodule[lexers/c]

The projected C API has two entry points:

@itemlist[
 @item{@racket[make-c-lexer] for streaming tokenization from an input port.}
 @item{@racket[c-string->tokens] for eager tokenization of an entire string.}]

The first C implementation is a handwritten streaming lexer grounded primarily
in C lexical and preprocessing-token rules. It is preprocessor-aware from the
first slice, so directive lines like @tt{#include} and @tt{#define} are
tokenized directly instead of being flattened into ordinary punctuation and
identifiers. It also recognizes C digraph punctuators and validates string and
character escape sequences so malformed escapes stay inside one malformed
literal token.

@defproc[(make-c-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                       [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                       [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming C lexer.

Projected C categories include @racket['comment], @racket['whitespace],
@racket['keyword], @racket['identifier], @racket['literal],
@racket['operator], @racket['delimiter], and @racket['unknown].

Keywords and preprocessor directive names project as @racket['keyword].
Header names such as @tt{<stdio.h>} and @tt{"local.h"} project as
@racket['literal].

@examples[#:eval c-eval
(define lexer
  (make-c-lexer #:profile 'coloring))
(define in
  (open-input-string "#include <stdio.h>\nint main(void) { return 0; }\n"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(c-string->tokens [source string?]
                           [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                           [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                           [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected C tokens.}

The derived C API provides reusable language-specific structure:

@defproc[(make-c-derived-lexer)
         (input-port? . -> . (or/c c-derived-token? 'eof))]{
Constructs a streaming C lexer that returns derived C tokens.}

@defproc[(c-string->derived-tokens [source string?])
         (listof c-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived C tokens.}

@defproc[(c-derived-token? [v any/c])
         boolean?]{
Recognizes derived C tokens.}

@defproc[(c-derived-token-tags [token c-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(c-derived-token-has-tag? [token c-derived-token?]
                                   [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(c-derived-token-text [token c-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(c-derived-token-start [token c-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(c-derived-token-end [token c-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable C-specific derived tags include:

@itemlist[
 @item{@racket['c-comment]}
 @item{@racket['c-whitespace]}
 @item{@racket['c-keyword]}
 @item{@racket['c-identifier]}
 @item{@racket['c-string-literal]}
 @item{@racket['c-char-literal]}
 @item{@racket['c-numeric-literal]}
 @item{@racket['c-operator]}
 @item{@racket['c-delimiter]}
 @item{@racket['c-preprocessor-directive]}
 @item{@racket['c-header-name]}
 @item{@racket['c-line-splice]}
 @item{@racket['c-error]}
 @item{@racket['malformed-token]}]

Malformed C input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{c} or @tt{h} delegate to
@racketmodname[lexers/c]. Wrapped delegated Markdown tokens preserve C-derived
tags and gain @racket['embedded-c].}

@defthing[c-profiles immutable-hash?]{
The profile defaults used by the C lexer.}

@section{C++}

@defmodule[lexers/cpp]

The projected C++ API has two entry points:

@itemlist[
 @item{@racket[make-cpp-lexer] for streaming tokenization from an input port.}
 @item{@racket[cpp-string->tokens] for eager tokenization of an entire string.}]

The first C++ implementation is a handwritten streaming lexer grounded in C++
lexical structure. It is preprocessor-aware and covers comments, identifiers,
keywords, operator words, character and string literals, raw string literals,
numeric literals, and punctuators such as @tt{::} and @tt{->}.

@defproc[(make-cpp-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                         [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                         [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming C++ lexer.

Projected C++ categories include @racket['comment], @racket['whitespace],
@racket['keyword], @racket['identifier], @racket['literal],
@racket['operator], @racket['delimiter], and @racket['unknown].

@examples[#:eval cpp-eval
(define lexer
  (make-cpp-lexer #:profile 'coloring))
(define in
  (open-input-string "#include <vector>\nstd::string s;\n"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(cpp-string->tokens [source string?]
                             [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                             [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                             [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected C++ tokens.}

The derived C++ API provides reusable language-specific structure:

@defproc[(make-cpp-derived-lexer)
         (input-port? . -> . (or/c cpp-derived-token? 'eof))]{
Constructs a streaming C++ lexer that returns derived C++ tokens.}

@defproc[(cpp-string->derived-tokens [source string?])
         (listof cpp-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived C++ tokens.}

@defproc[(cpp-derived-token? [v any/c])
         boolean?]{
Recognizes derived C++ tokens.}

@defproc[(cpp-derived-token-tags [token cpp-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(cpp-derived-token-has-tag? [token cpp-derived-token?]
                                     [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(cpp-derived-token-text [token cpp-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(cpp-derived-token-start [token cpp-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(cpp-derived-token-end [token cpp-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable C++-specific derived tags include:

@itemlist[
 @item{@racket['cpp-comment]}
 @item{@racket['cpp-whitespace]}
 @item{@racket['cpp-keyword]}
 @item{@racket['cpp-identifier]}
 @item{@racket['cpp-string-literal]}
 @item{@racket['cpp-char-literal]}
 @item{@racket['cpp-numeric-literal]}
 @item{@racket['cpp-operator]}
 @item{@racket['cpp-delimiter]}
 @item{@racket['cpp-preprocessor-directive]}
 @item{@racket['cpp-header-name]}
 @item{@racket['cpp-line-splice]}
 @item{@racket['cpp-error]}
 @item{@racket['malformed-token]}]

Ordinary C++ strings and character literals validate common escape structures
in the derived layer, including simple, octal, hexadecimal, and universal-
character escapes. Invalid escapes and malformed multi-character character
literals remain source-faithful but are tagged with @racket['malformed-token].

Malformed C++ input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{cpp}, @tt{c++}, @tt{cc}, @tt{cxx},
@tt{hpp}, @tt{hh}, or @tt{hxx} delegate to @racketmodname[lexers/cpp].
Wrapped delegated Markdown tokens preserve C++-derived tags and gain
@racket['embedded-cpp].}

@defthing[cpp-profiles immutable-hash?]{
The profile defaults used by the C++ lexer.}

@section{CSV}

@defmodule[lexers/csv]

The projected CSV API has two entry points:

@itemlist[
 @item{@racket[make-csv-lexer] for streaming tokenization from an input port.}
 @item{@racket[csv-string->tokens] for eager tokenization of an entire string.}]

The first CSV implementation is a handwritten streaming lexer for
comma-separated text. It preserves exact source text, including empty fields
and CRLF row separators.

@defproc[(make-csv-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                         [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                         [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming CSV lexer.

Projected CSV categories include @racket['literal], @racket['delimiter], and
@racket['unknown].

Field contents project as @racket['literal]. Field separators and row
separators project as @racket['delimiter].

@examples[#:eval csv-eval
(define lexer
  (make-csv-lexer #:profile 'coloring))
(define in
  (open-input-string "name,age\nAda,37\n"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(csv-string->tokens [source string?]
                             [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                             [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                             [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected CSV tokens.}

The derived CSV API provides reusable structure for delimited text:

@defproc[(make-csv-derived-lexer)
         (input-port? . -> . (or/c csv-derived-token? 'eof))]{
Constructs a streaming CSV lexer that returns derived CSV tokens.}

@defproc[(csv-string->derived-tokens [source string?])
         (listof csv-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived CSV tokens.}

@defproc[(csv-derived-token? [v any/c])
         boolean?]{
Recognizes derived CSV tokens.}

@defproc[(csv-derived-token-tags [token csv-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(csv-derived-token-has-tag? [token csv-derived-token?]
                                     [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(csv-derived-token-text [token csv-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(csv-derived-token-start [token csv-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(csv-derived-token-end [token csv-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable CSV-specific derived tags include:

@itemlist[
 @item{@racket['delimited-field]}
 @item{@racket['delimited-quoted-field]}
 @item{@racket['delimited-unquoted-field]}
 @item{@racket['delimited-empty-field]}
 @item{@racket['delimited-separator]}
 @item{@racket['delimited-row-separator]}
 @item{@racket['delimited-error]}
 @item{@racket['csv-field]}
 @item{@racket['csv-separator]}
 @item{@racket['csv-row-separator]}
 @item{@racket['malformed-token]}]

Malformed CSV input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{csv} delegate to
@racketmodname[lexers/csv]. Wrapped delegated Markdown tokens preserve
CSV-derived tags and gain @racket['embedded-csv].}

@defthing[csv-profiles immutable-hash?]{
The profile defaults used by the CSV lexer.}

@section{JSON}

@defmodule[lexers/json]

The projected JSON API has two entry points:

@itemlist[
 @item{@racket[make-json-lexer] for streaming tokenization from an input port.}
 @item{@racket[json-string->tokens] for eager tokenization of an entire string.}]

@defproc[(make-json-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                          [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                          [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming JSON lexer.

The result is a procedure of one argument, an input port. Each call reads the
next token from the port and returns one projected token value.

Projected JSON categories include @racket['delimiter], @racket['operator],
@racket['identifier], @racket['literal], @racket['whitespace], and
@racket['unknown].

Object keys project as @racket['identifier], while numbers, ordinary strings,
and the JSON keywords @tt{true}, @tt{false}, and @tt{null} project as
@racket['literal].

@examples[#:eval json-eval
(define lexer
  (make-json-lexer #:profile 'coloring))
(define in
  (open-input-string "{\"x\": [1, true]}"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(json-string->tokens [source string?]
                              [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                              [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                              [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected JSON tokens.}

The derived JSON API provides reusable language-specific structure:

@defproc[(make-json-derived-lexer)
         (input-port? . -> . (or/c json-derived-token? 'eof))]{
Constructs a streaming JSON lexer that returns derived JSON tokens.}

@defproc[(json-string->derived-tokens [source string?])
         (listof json-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived JSON tokens.}

@defproc[(json-derived-token? [v any/c])
         boolean?]{
Recognizes derived JSON tokens.}

@defproc[(json-derived-token-tags [token json-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(json-derived-token-has-tag? [token json-derived-token?]
                                      [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(json-derived-token-text [token json-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(json-derived-token-start [token json-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(json-derived-token-end [token json-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable JSON-specific derived tags include:

@itemlist[
 @item{@racket['json-object-key]}
 @item{@racket['json-string]}
 @item{@racket['json-number]}
 @item{@racket['json-true]}
 @item{@racket['json-false]}
 @item{@racket['json-null]}
 @item{@racket['json-object-start]}
 @item{@racket['json-object-end]}
 @item{@racket['json-array-start]}
 @item{@racket['json-array-end]}
 @item{@racket['json-comma]}
 @item{@racket['json-colon]}
 @item{@racket['json-error]}
 @item{@racket['malformed-token]}]

Malformed JSON input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{json} delegate to
@racketmodname[lexers/json]. Wrapped delegated Markdown tokens preserve
JSON-derived tags and gain @racket['embedded-json].}

@section{Makefile}

@defmodule[lexers/makefile]

The projected Makefile API has two entry points:

@itemlist[
 @item{@racket[make-makefile-lexer] for streaming tokenization from an input
       port.}
 @item{@racket[makefile-string->tokens] for eager tokenization of an entire
       string.}]

The first Makefile implementation is a handwritten streaming lexer aimed at
ordinary @tt{Makefile}, @tt{GNUmakefile}, and @tt{.mk} inputs. It covers
comments, directive lines, variable assignments, rule targets, recipe lines,
variable references, delimiters, and CRLF-preserving source fidelity.

@defproc[(make-makefile-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                              [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                              [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming Makefile lexer.

Projected Makefile categories include @racket['comment], @racket['whitespace],
@racket['keyword], @racket['identifier], @racket['literal],
@racket['operator], @racket['delimiter], and @racket['unknown].

Directive words such as @tt{include} project as @racket['keyword]. Assignment
operators such as @tt{:=} and @tt{+=} project as @racket['operator]. Rule
separators such as @tt{:} project as @racket['delimiter].}

@defproc[(makefile-string->tokens [source string?]
                                  [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                                  [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                                  [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected Makefile tokens.}

The derived Makefile API provides reusable language-specific structure:

@defproc[(make-makefile-derived-lexer)
         (input-port? . -> . (or/c makefile-derived-token? 'eof))]{
Constructs a streaming Makefile lexer that returns derived Makefile tokens.}

@defproc[(makefile-string->derived-tokens [source string?])
         (listof makefile-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived Makefile tokens.}

@defproc[(makefile-derived-token? [v any/c])
         boolean?]{
Recognizes derived Makefile tokens.}

@defproc[(makefile-derived-token-tags [token makefile-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(makefile-derived-token-has-tag? [token makefile-derived-token?]
                                          [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(makefile-derived-token-text [token makefile-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(makefile-derived-token-start [token makefile-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(makefile-derived-token-end [token makefile-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable Makefile-specific derived tags include:

@itemlist[
 @item{@racket['makefile-directive]}
 @item{@racket['makefile-variable]}
 @item{@racket['makefile-assignment-operator]}
 @item{@racket['makefile-rule-target]}
 @item{@racket['makefile-rule-delimiter]}
 @item{@racket['makefile-variable-reference]}
 @item{@racket['makefile-paren-variable-reference]}
 @item{@racket['makefile-brace-variable-reference]}
 @item{@racket['makefile-recipe-separator]}
 @item{@racket['makefile-order-only-delimiter]}
 @item{@racket['malformed-token]}]

Markdown fenced code blocks labeled @tt{make}, @tt{makefile}, or @tt{mk}
delegate to @racketmodname[lexers/makefile]. Wrapped delegated Markdown tokens
preserve Makefile-derived tags and gain @racket['embedded-makefile].}

@section{Plist}

@defmodule[lexers/plist]

The projected plist API has two entry points:

@itemlist[
 @item{@racket[make-plist-lexer] for streaming tokenization from an input
       port.}
 @item{@racket[plist-string->tokens] for eager tokenization of an entire
       string.}]

The first plist implementation is a handwritten streaming lexer for XML
property-list files such as @tt{Info.plist}. The first slice deliberately
targets XML plists only; it does not attempt to cover binary @tt{bplist}
files. Because this scope is XML-only, quoted attribute values are treated as
ordinary plist attribute values, while unquoted attribute values are treated as
malformed input.

@defproc[(make-plist-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                           [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                           [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming plist lexer.

Projected plist categories include @racket['comment], @racket['whitespace],
@racket['keyword], @racket['identifier], @racket['literal],
@racket['operator], @racket['delimiter], and @racket['unknown].

XML declarations and plist doctypes project as @racket['keyword]. Element
content such as @tt{CFBundleName} and @tt{Lexers} projects as
@racket['literal].}

@defproc[(plist-string->tokens [source string?]
                               [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                               [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                               [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected plist tokens.}

The derived plist API provides reusable language-specific structure:

@defproc[(make-plist-derived-lexer)
         (input-port? . -> . (or/c plist-derived-token? 'eof))]{
Constructs a streaming plist lexer that returns derived plist tokens.}

@defproc[(plist-string->derived-tokens [source string?])
         (listof plist-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived plist tokens.}

@defproc[(plist-derived-token? [v any/c])
         boolean?]{
Recognizes derived plist tokens.}

@defproc[(plist-derived-token-tags [token plist-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(plist-derived-token-has-tag? [token plist-derived-token?]
                                       [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(plist-derived-token-text [token plist-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(plist-derived-token-start [token plist-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(plist-derived-token-end [token plist-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable plist-specific derived tags include:

@itemlist[
 @item{@racket['plist-processing-instruction]}
 @item{@racket['plist-doctype]}
 @item{@racket['plist-tag-name]}
 @item{@racket['plist-closing-tag-name]}
 @item{@racket['plist-attribute-name]}
 @item{@racket['plist-attribute-value]}
 @item{@racket['plist-entity]}
 @item{@racket['plist-key-text]}
 @item{@racket['plist-string-text]}
 @item{@racket['plist-data-text]}
 @item{@racket['plist-date-text]}
 @item{@racket['plist-integer-text]}
 @item{@racket['plist-real-text]}
 @item{@racket['plist-text]}
 @item{@racket['plist-comment]}
 @item{@racket['malformed-token]}]

Markdown fenced code blocks labeled @tt{plist} delegate to
@racketmodname[lexers/plist]. Wrapped delegated Markdown tokens preserve
plist-derived tags and gain @racket['embedded-plist].}

@section{YAML}

@defmodule[lexers/yaml]

The projected YAML API has two entry points:

@itemlist[
 @item{@racket[make-yaml-lexer] for streaming tokenization from an input port.}
 @item{@racket[yaml-string->tokens] for eager tokenization of an entire string.}]

The first YAML implementation is a handwritten streaming lexer grounded
primarily in the YAML 1.2.2 lexical and structural rules. The first slice is
deliberately parser-lite, but it covers practical block mappings, block
sequences, flow delimiters, directives, document markers, quoted scalars, plain
scalars, comments, and block scalar bodies. Block-scalar headers validate the
compact YAML indicator forms, so malformed headers remain source-faithful but
are tagged with @racket['malformed-token] instead of enabling block-scalar
mode.

@defproc[(make-yaml-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                          [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                          [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming YAML lexer.

Projected YAML categories include @racket['comment], @racket['whitespace],
@racket['keyword], @racket['identifier], @racket['literal],
@racket['delimiter], and @racket['unknown].

Directive lines such as @tt{%YAML 1.2} project as @racket['keyword]. Plain and
quoted scalars project as @racket['literal]. Structural markers such as
@tt{:}, @tt{-}, @tt{[}, @tt{]}, @tt{@(litchar "{")}, @tt{@(litchar "}")}, and document
markers project as @racket['delimiter].

@examples[#:eval yaml-eval
(define lexer
  (make-yaml-lexer #:profile 'coloring))
(define in
  (open-input-string "name: Deploy\non:\n  push:\n"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(yaml-string->tokens [source string?]
                              [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                              [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                              [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected YAML tokens.}

The derived YAML API provides reusable language-specific structure:

@defproc[(make-yaml-derived-lexer)
         (input-port? . -> . (or/c yaml-derived-token? 'eof))]{
Constructs a streaming YAML lexer that returns derived YAML tokens.}

@defproc[(yaml-string->derived-tokens [source string?])
         (listof yaml-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived YAML tokens.}

@defproc[(yaml-derived-token? [v any/c])
         boolean?]{
Recognizes derived YAML tokens.}

@defproc[(yaml-derived-token-tags [token yaml-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(yaml-derived-token-has-tag? [token yaml-derived-token?]
                                      [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(yaml-derived-token-text [token yaml-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(yaml-derived-token-start [token yaml-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(yaml-derived-token-end [token yaml-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable YAML-specific derived tags include:

@itemlist[
 @item{@racket['yaml-comment]}
 @item{@racket['yaml-whitespace]}
 @item{@racket['yaml-directive]}
 @item{@racket['yaml-document-marker]}
 @item{@racket['yaml-sequence-indicator]}
 @item{@racket['yaml-key-indicator]}
 @item{@racket['yaml-value-indicator]}
 @item{@racket['yaml-flow-delimiter]}
 @item{@racket['yaml-anchor]}
 @item{@racket['yaml-alias]}
 @item{@racket['yaml-tag]}
 @item{@racket['yaml-string-literal]}
 @item{@racket['yaml-plain-scalar]}
 @item{@racket['yaml-key-scalar]}
 @item{@racket['yaml-boolean]}
 @item{@racket['yaml-null]}
 @item{@racket['yaml-number]}
 @item{@racket['yaml-block-scalar-header]}
 @item{@racket['yaml-block-scalar-content]}
 @item{@racket['yaml-error]}
 @item{@racket['malformed-token]}]

Double-quoted YAML scalars validate common escape forms in the derived layer,
including simple escapes plus @tt{\xXX}, @tt{\uXXXX}, and @tt{\UXXXXXXXX}
Unicode escapes. Invalid escape sequences remain source-faithful but are tagged
with @racket['malformed-token].

Malformed YAML input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{yaml} or @tt{yml} delegate to
@racketmodname[lexers/yaml]. Wrapped delegated Markdown tokens preserve
YAML-derived tags and gain @racket['embedded-yaml].}

@defthing[yaml-profiles immutable-hash?]{
The profile defaults used by the YAML lexer.}

@section{Markdown}

@defmodule[lexers/markdown]

The projected Markdown API has two entry points:

@itemlist[
 @item{@racket[make-markdown-lexer] for streaming tokenization from an input
       port.}
 @item{@racket[markdown-string->tokens] for eager tokenization of an entire
       string.}]

The first Markdown implementation is a handwritten, parser-lite,
GitHub-flavored Markdown lexer. It is line-oriented and can delegate raw HTML
and known fenced-code languages to the existing C, C++, CSV, HTML, CSS,
JavaScript, JSON, Makefile, Objective-C, plist, Python, Racket, Scribble, shell,
Swift, TSV, WAT, and YAML lexers.

@defproc[(make-markdown-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                              [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                              [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming Markdown lexer.

The result is a procedure of one argument, an input port. Each call reads the
next projected Markdown token from the port and returns one projected token
value.

When @racket[#:source-positions] is true, each result is a
@racket[position-token?]. When it is false, the result is either a bare symbol
or a @racket[token?] directly.

The intended use is to create the lexer once, then call it repeatedly on the
same port until it returns an end-of-file token.

@examples[#:eval markdown-eval
(define lexer
  (make-markdown-lexer #:profile 'coloring))
(define in
  (open-input-string "# Title\n\n```js\nconst x = 1;\n```\n"))
(port-count-lines! in)
(list (lexer in)
      (lexer in)
      (lexer in)
      (lexer in))
]}

@defproc[(markdown-string->tokens [source string?]
                                  [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                                  [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                                  [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes an entire Markdown string using the projected token API.

This is a convenience wrapper over @racket[make-markdown-lexer].}

@subsection{Markdown Returned Tokens}

Common projected Markdown categories include:

@itemlist[
 @item{@racket['whitespace]}
 @item{@racket['identifier]}
 @item{@racket['literal]}
 @item{@racket['keyword]}
 @item{@racket['operator]}
 @item{@racket['delimiter]}
 @item{@racket['comment]}
 @item{@racket['unknown]}
 @item{@racket['eof]}]

For the current Markdown scaffold:

@itemlist[
 @item{ordinary prose, inline code text, code-block text, and link or image
       payload text project mostly as @racket['literal]}
 @item{language names and delegated name-like tokens project as
       @racket['identifier] or @racket['keyword], depending on the delegated
       lexer}
 @item{structural markers such as heading markers, list markers, brackets,
       pipes, backticks, and fence delimiters project as @racket['delimiter]}
 @item{comments only appear through delegated embedded HTML}
 @item{recoverable malformed constructs project as @racket['unknown] in
       @racket['coloring] mode and raise in @racket['compiler] mode}]

For source continuity, the derived Markdown stream preserves the newline after a
fenced-code info string as an explicit whitespace token before the code body.
Incomplete fenced-code blocks are tokenized best-effort instead of raising an
internal error.

@examples[#:eval markdown-eval
(define inspect-lexer
  (make-markdown-lexer #:profile 'coloring))
(define inspect-in
  (open-input-string "# Title\n\nText with <span class=\"x\">hi</span>\n"))
(port-count-lines! inspect-in)
(define first-token
  (inspect-lexer inspect-in))
(lexer-token-has-positions? first-token)
(lexer-token-name first-token)
(lexer-token-value first-token)
(position-offset (lexer-token-start first-token))
(position-offset (lexer-token-end first-token))
]}

@defproc[(make-markdown-derived-lexer)
         (input-port? . -> . (or/c 'eof markdown-derived-token?))]{
Constructs a streaming Markdown lexer for the derived-token layer.}

@defproc[(markdown-string->derived-tokens [source string?])
         (listof markdown-derived-token?)]{
Tokenizes an entire Markdown string into derived Markdown token values.}

@defproc[(markdown-derived-token? [v any/c])
         boolean?]{
Recognizes derived Markdown token values returned by
@racket[make-markdown-derived-lexer] and
@racket[markdown-string->derived-tokens].}

@defproc[(markdown-derived-token-tags [token markdown-derived-token?])
         (listof symbol?)]{
Returns the Markdown-specific classification tags attached to a derived
Markdown token.}

@defproc[(markdown-derived-token-has-tag? [token markdown-derived-token?]
                                          [tag symbol?])
         boolean?]{
Determines whether a derived Markdown token carries a given classification
tag.}

@defproc[(markdown-derived-token-text [token markdown-derived-token?])
         string?]{
Returns the exact source text corresponding to a derived Markdown token.}

@defproc[(markdown-derived-token-start [token markdown-derived-token?])
         position?]{
Returns the starting source position for a derived Markdown token.}

@defproc[(markdown-derived-token-end [token markdown-derived-token?])
         position?]{
Returns the ending source position for a derived Markdown token.}

@subsection{Markdown Derived Tokens}

The current Markdown scaffold may attach tags such as:

@itemlist[
 @item{@racket['markdown-text]}
 @item{@racket['markdown-heading-marker]}
 @item{@racket['markdown-heading-text]}
 @item{@racket['markdown-blockquote-marker]}
 @item{@racket['markdown-list-marker]}
 @item{@racket['markdown-task-marker]}
 @item{@racket['markdown-thematic-break]}
 @item{@racket['markdown-code-span]}
 @item{@racket['markdown-code-fence]}
 @item{@racket['markdown-code-block]}
 @item{@racket['markdown-code-info-string]}
 @item{@racket['markdown-emphasis-delimiter]}
 @item{@racket['markdown-strong-delimiter]}
 @item{@racket['markdown-strikethrough-delimiter]}
 @item{@racket['markdown-link-text]}
 @item{@racket['markdown-link-destination]}
 @item{@racket['markdown-link-title]}
 @item{@racket['markdown-image-marker]}
 @item{@racket['markdown-autolink]}
 @item{@racket['markdown-table-pipe]}
 @item{@racket['markdown-table-alignment]}
 @item{@racket['markdown-table-cell]}
 @item{@racket['markdown-escape]}
 @item{@racket['markdown-hard-line-break]}
 @item{@racket['embedded-html]}
 @item{@racket['embedded-css]}
 @item{@racket['embedded-cpp]}
 @item{@racket['embedded-csv]}
 @item{@racket['embedded-go]}
 @item{@racket['embedded-haskell]}
 @item{@racket['embedded-java]}
 @item{@racket['embedded-javascript]}
 @item{@racket['embedded-json]}
 @item{@racket['embedded-makefile]}
 @item{@racket['embedded-latex]}
 @item{@racket['embedded-objc]}
 @item{@racket['embedded-pascal]}
 @item{@racket['embedded-plist]}
 @item{@racket['embedded-python]}
 @item{@racket['embedded-racket]}
 @item{@racket['embedded-rust]}
 @item{@racket['embedded-shell]}
 @item{@racket['embedded-scribble]}
 @item{@racket['embedded-swift]}
 @item{@racket['embedded-tex]}
 @item{@racket['embedded-tsv]}
 @item{@racket['embedded-wat]}
 @item{@racket['embedded-yaml]}
 @item{@racket['malformed-token]}]

Delegated raw HTML and recognized fenced-code languages keep their reusable
derived tags and gain Markdown embedding markers such as
@racket['embedded-html], @racket['embedded-cpp], @racket['embedded-csv],
@racket['embedded-go],
@racket['embedded-haskell],
@racket['embedded-java],
@racket['embedded-javascript], @racket['embedded-json],
@racket['embedded-latex], @racket['embedded-makefile], @racket['embedded-objc],
@racket['embedded-pascal], @racket['embedded-plist], @racket['embedded-python],
@racket['embedded-racket], @racket['embedded-rust], @racket['embedded-shell],
@racket['embedded-swift], @racket['embedded-tex], @racket['embedded-tsv],
@racket['embedded-wat], or @racket['embedded-yaml].

@examples[#:eval markdown-eval
(define derived-tokens
  (markdown-string->derived-tokens
   "# Title\n\n- [x] done\n\n```js\nconst x = 1;\n```\n\nText <span class=\"x\">hi</span>\n"))
(map (lambda (token)
       (list (markdown-derived-token-text token)
             (markdown-derived-token-tags token)))
     derived-tokens)
]}

@defthing[markdown-profiles immutable-hash?]{
The profile defaults used by the Markdown lexer.}

@section{Go}

@defmodule[lexers/go]

The projected Go API has two entry points:

@itemlist[
 @item{@racket[make-go-lexer] for streaming tokenization from an input port.}
 @item{@racket[go-string->tokens] for eager tokenization of an entire string.}]

The first Go implementation is a handwritten streaming lexer grounded in the
official Go lexical specification. It covers whitespace, line and general
comments, identifiers, keywords, string and rune literals, numeric and
imaginary literals, operators, and delimiters. In the @racket['compiler]
profile, the projected token stream also performs Go semicolon insertion at the
newline and EOF boundaries required by the specification, while the
@racket['coloring] profile remains source-faithful.

@defproc[(make-go-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                        [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                        [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming Go lexer.

Projected Go categories include @racket['comment], @racket['whitespace],
@racket['keyword], @racket['identifier], @racket['literal],
@racket['operator], @racket['delimiter], and @racket['unknown].}

@examples[#:eval go-eval
(define lexer
  (make-go-lexer #:profile 'coloring))
(define in
  (open-input-string "package main\nfunc main() {}\n"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(go-string->tokens [source string?]
                            [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                            [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                            [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected Go tokens.}

The derived Go API provides reusable language-specific structure:

@defproc[(make-go-derived-lexer)
         (input-port? . -> . (or/c go-derived-token? 'eof))]{
Constructs a streaming Go lexer that returns derived Go tokens.}

@defproc[(go-string->derived-tokens [source string?])
         (listof go-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived Go tokens.}

@defproc[(go-derived-token? [v any/c])
         boolean?]{
Recognizes derived Go tokens.}

@defproc[(go-derived-token-tags [token go-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(go-derived-token-has-tag? [token go-derived-token?]
                                    [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(go-derived-token-text [token go-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(go-derived-token-start [token go-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(go-derived-token-end [token go-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable Go-specific derived tags include:

@itemlist[
 @item{@racket['go-comment]}
 @item{@racket['go-line-comment]}
 @item{@racket['go-general-comment]}
 @item{@racket['go-whitespace]}
 @item{@racket['go-keyword]}
 @item{@racket['go-identifier]}
 @item{@racket['go-string-literal]}
 @item{@racket['go-raw-string-literal]}
 @item{@racket['go-rune-literal]}
 @item{@racket['go-numeric-literal]}
 @item{@racket['go-imaginary-literal]}
 @item{@racket['go-operator]}
 @item{@racket['go-delimiter]}
 @item{@racket['malformed-token]}]

Malformed Go input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{go} and @tt{golang} delegate to
@racketmodname[lexers/go]. Wrapped delegated Markdown tokens preserve
Go-derived tags and gain @racket['embedded-go].}

@defthing[go-profiles immutable-hash?]{
The profile defaults used by the Go lexer.}

@section{Java}

@defmodule[lexers/java]

The projected Java API has two entry points:

@itemlist[
 @item{@racket[make-java-lexer] for streaming tokenization from an input port.}
 @item{@racket[java-string->tokens] for eager tokenization of an entire string.}]

The first Java implementation is a handwritten streaming lexer grounded in the
Java lexical grammar. It covers whitespace, line and block comments,
identifiers, keywords, the contextual keyword @tt{non-sealed}, string literals, text blocks, char literals, numeric
literals, operators, and delimiters. It also recognizes Java Unicode escapes
for lexical classification while preserving exact source slices in the emitted
tokens, validates Java escape sequences including octal escapes, and recognizes
text blocks only when the opening delimiter has the JLS-required trailing line
terminator. Numeric literals validate their required digit-bearing parts, so
malformed forms such as @tt{1e}, @tt{0x}, and @tt{0b} remain source-faithful
but are tagged with @racket['malformed-token].

@defproc[(make-java-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                          [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                          [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming Java lexer.

Projected Java categories include @racket['comment], @racket['whitespace],
@racket['keyword], @racket['identifier], @racket['literal],
@racket['operator], @racket['delimiter], and @racket['unknown].}

@examples[#:eval java-eval
(define lexer
  (make-java-lexer #:profile 'coloring))
(define in
  (open-input-string "class Example {\n    String s = \"hi\";\n}\n"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(java-string->tokens [source string?]
                              [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                              [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                              [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected Java tokens.}

The derived Java API provides reusable language-specific structure:

@defproc[(make-java-derived-lexer)
         (input-port? . -> . (or/c java-derived-token? 'eof))]{
Constructs a streaming Java lexer that returns derived Java tokens.}

@defproc[(java-string->derived-tokens [source string?])
         (listof java-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived Java tokens.}

@defproc[(java-derived-token? [v any/c])
         boolean?]{
Recognizes derived Java tokens.}

@defproc[(java-derived-token-tags [token java-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(java-derived-token-has-tag? [token java-derived-token?]
                                      [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(java-derived-token-text [token java-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(java-derived-token-start [token java-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(java-derived-token-end [token java-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable Java-specific derived tags include:

@itemlist[
 @item{@racket['java-comment]}
 @item{@racket['java-line-comment]}
 @item{@racket['java-block-comment]}
 @item{@racket['java-doc-comment]}
 @item{@racket['java-whitespace]}
 @item{@racket['java-keyword]}
 @item{@racket['java-identifier]}
 @item{@racket['java-annotation-marker]}
 @item{@racket['java-annotation-name]}
 @item{@racket['java-string-literal]}
 @item{@racket['java-text-block]}
 @item{@racket['java-char-literal]}
 @item{@racket['java-numeric-literal]}
 @item{@racket['java-boolean-literal]}
 @item{@racket['java-true-literal]}
 @item{@racket['java-false-literal]}
 @item{@racket['java-null-literal]}
 @item{@racket['java-operator]}
 @item{@racket['java-delimiter]}
 @item{@racket['malformed-token]}]

Malformed Java input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{java} delegate to
@racketmodname[lexers/java]. Wrapped delegated Markdown tokens preserve
Java-derived tags and gain @racket['embedded-java].}

@defthing[java-profiles immutable-hash?]{
The profile defaults used by the Java lexer.}

@section{Haskell}

@defmodule[lexers/haskell]

The projected Haskell API has two entry points:

@itemlist[
 @item{@racket[make-haskell-lexer] for streaming tokenization from an input
       port.}
 @item{@racket[haskell-string->tokens] for eager tokenization of an entire
       string.}]

The first Haskell implementation is a handwritten streaming lexer grounded in
the Haskell lexical-structure specification, with a small set of practical
GHC-era additions such as pragmas. It covers whitespace, line comments,
nested comments, pragmas, identifiers, operators, strings, characters,
numeric literals, and delimiters. In the @racket['compiler] profile, the
projected token stream also inserts ordinary Haskell layout tokens for
@tt{let}, @tt{where}, @tt{do}, and @tt{of}, while the @racket['coloring]
profile remains source-faithful.

@defproc[(make-haskell-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                             [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                             [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming Haskell lexer.

Projected Haskell categories include @racket['comment],
@racket['whitespace], @racket['keyword], @racket['identifier],
@racket['literal], @racket['operator], @racket['delimiter], and
@racket['unknown].}

@examples[#:eval haskell-eval
(define lexer
  (make-haskell-lexer #:profile 'coloring))
(define in
  (open-input-string "{-# LANGUAGE OverloadedStrings #-}\nmodule Main where\n"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(haskell-string->tokens [source string?]
                                 [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                                 [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                                 [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected Haskell
tokens.}

The derived Haskell API provides reusable language-specific structure:

@defproc[(make-haskell-derived-lexer)
         (input-port? . -> . (or/c haskell-derived-token? 'eof))]{
Constructs a streaming Haskell lexer that returns derived Haskell tokens.}

@defproc[(haskell-string->derived-tokens [source string?])
         (listof haskell-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived Haskell tokens.}

@defproc[(haskell-derived-token? [v any/c])
         boolean?]{
Recognizes derived Haskell tokens.}

@defproc[(haskell-derived-token-tags [token haskell-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(haskell-derived-token-has-tag? [token haskell-derived-token?]
                                         [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(haskell-derived-token-text [token haskell-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(haskell-derived-token-start [token haskell-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(haskell-derived-token-end [token haskell-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable Haskell-specific derived tags include:

@itemlist[
 @item{@racket['haskell-comment]}
 @item{@racket['haskell-line-comment]}
 @item{@racket['haskell-nested-comment]}
 @item{@racket['haskell-pragma]}
 @item{@racket['haskell-whitespace]}
 @item{@racket['haskell-keyword]}
 @item{@racket['haskell-variable-identifier]}
 @item{@racket['haskell-constructor-identifier]}
 @item{@racket['haskell-variable-operator]}
 @item{@racket['haskell-constructor-operator]}
 @item{@racket['haskell-string-literal]}
 @item{@racket['haskell-char-literal]}
 @item{@racket['haskell-numeric-literal]}
 @item{@racket['haskell-delimiter]}
 @item{@racket['malformed-token]}]

Malformed Haskell input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{haskell}, @tt{hs}, and @tt{lhs}
delegate to @racketmodname[lexers/haskell]. Wrapped delegated Markdown
tokens preserve Haskell-derived tags and gain @racket['embedded-haskell].}

@defthing[haskell-profiles immutable-hash?]{
The profile defaults used by the Haskell lexer.}

@section{Objective-C}

@defmodule[lexers/objc]

The projected Objective-C API has two entry points:

@itemlist[
 @item{@racket[make-objc-lexer] for streaming tokenization from an input port.}
 @item{@racket[objc-string->tokens] for eager tokenization of an entire string.}]

The first Objective-C implementation is a handwritten streaming lexer grounded
in the language's lexical surface and existing Objective-C lexer prior art. It
is preprocessor-aware and covers comments, identifiers, C / Objective-C
keywords, at-sign Objective-C keywords, Objective-C strings, object-literal
introducers, numbers, operators, and delimiters.

@defproc[(make-objc-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                          [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                          [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming Objective-C lexer.

Projected Objective-C categories include @racket['comment],
@racket['whitespace], @racket['keyword], @racket['identifier],
@racket['literal], @racket['operator], @racket['delimiter], and
@racket['unknown].

@examples[#:eval objc-eval
(define lexer
  (make-objc-lexer #:profile 'coloring))
(define in
  (open-input-string "@interface Foo : NSObject\n@property NSString *name;\n@end\n"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(objc-string->tokens [source string?]
                              [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                              [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                              [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected Objective-C
tokens.}

The derived Objective-C API provides reusable language-specific structure:

@defproc[(make-objc-derived-lexer)
         (input-port? . -> . (or/c objc-derived-token? 'eof))]{
Constructs a streaming Objective-C lexer that returns derived Objective-C
tokens.}

@defproc[(objc-string->derived-tokens [source string?])
         (listof objc-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived Objective-C
tokens.}

@defproc[(objc-derived-token? [v any/c])
         boolean?]{
Recognizes derived Objective-C tokens.}

@defproc[(objc-derived-token-tags [token objc-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(objc-derived-token-has-tag? [token objc-derived-token?]
                                      [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(objc-derived-token-text [token objc-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(objc-derived-token-start [token objc-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(objc-derived-token-end [token objc-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable Objective-C-specific derived tags include:

@itemlist[
 @item{@racket['objc-comment]}
 @item{@racket['objc-whitespace]}
 @item{@racket['objc-keyword]}
 @item{@racket['objc-at-keyword]}
 @item{@racket['objc-identifier]}
 @item{@racket['objc-string-literal]}
 @item{@racket['objc-char-literal]}
 @item{@racket['objc-numeric-literal]}
 @item{@racket['objc-operator]}
 @item{@racket['objc-delimiter]}
 @item{@racket['objc-preprocessor-directive]}
 @item{@racket['objc-header-name]}
 @item{@racket['objc-literal-introducer]}
 @item{@racket['objc-line-splice]}
 @item{@racket['objc-error]}
 @item{@racket['malformed-token]}]

Ordinary Objective-C strings, @tt{@"..."} strings, and character literals
validate common escape structures in the derived layer, including simple,
octal, hexadecimal, and universal-character escapes. Invalid escapes and
malformed multi-character character literals remain source-faithful but are
tagged with @racket['malformed-token].

Malformed Objective-C input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{objc}, @tt{objective-c},
@tt{objectivec}, or @tt{obj-c} delegate to @racketmodname[lexers/objc].
Wrapped delegated Markdown tokens preserve Objective-C-derived tags and gain
@racket['embedded-objc].}

@defthing[objc-profiles immutable-hash?]{
The profile defaults used by the Objective-C lexer.}

@section{Pascal}

@defmodule[lexers/pascal]

The projected Pascal API has two entry points:

@itemlist[
 @item{@racket[make-pascal-lexer] for streaming tokenization from an input port.}
 @item{@racket[pascal-string->tokens] for eager tokenization of an entire string.}]

The first Pascal implementation is a handwritten streaming lexer grounded in
the Free Pascal token reference. It covers whitespace, three comment forms,
identifiers, escaped reserved-word identifiers, reserved words, numeric
literals, strings, control-string fragments, operators, and delimiters.

@defproc[(make-pascal-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                            [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                            [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming Pascal lexer.

Projected Pascal categories include @racket['comment], @racket['whitespace],
@racket['keyword], @racket['identifier], @racket['literal],
@racket['operator], @racket['delimiter], and @racket['unknown].}

@examples[#:eval pascal-eval
(define lexer
  (make-pascal-lexer #:profile 'coloring))
(define in
  (open-input-string "program Test;\nvar &do: Integer;\nbegin\nend.\n"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(pascal-string->tokens [source string?]
                                [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                                [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                                [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected Pascal tokens.}

The derived Pascal API provides reusable language-specific structure:

@defproc[(make-pascal-derived-lexer)
         (input-port? . -> . (or/c pascal-derived-token? 'eof))]{
Constructs a streaming Pascal lexer that returns derived Pascal tokens.}

@defproc[(pascal-string->derived-tokens [source string?])
         (listof pascal-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived Pascal tokens.}

@defproc[(pascal-derived-token? [v any/c])
         boolean?]{
Recognizes derived Pascal tokens.}

@defproc[(pascal-derived-token-tags [token pascal-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(pascal-derived-token-has-tag? [token pascal-derived-token?]
                                        [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(pascal-derived-token-text [token pascal-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(pascal-derived-token-start [token pascal-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(pascal-derived-token-end [token pascal-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable Pascal-specific derived tags include:

@itemlist[
 @item{@racket['pascal-comment]}
 @item{@racket['pascal-compiler-directive]}
 @item{@racket['pascal-whitespace]}
 @item{@racket['pascal-keyword]}
 @item{@racket['pascal-identifier]}
 @item{@racket['pascal-escaped-identifier]}
 @item{@racket['pascal-string-literal]}
 @item{@racket['pascal-control-string]}
 @item{@racket['pascal-numeric-literal]}
 @item{@racket['pascal-operator]}
 @item{@racket['pascal-delimiter]}
 @item{@racket['malformed-token]}]

Compiler directives inside brace or star comments, such as @tt{{$mode objfpc}}
and @tt{(*$ifdef DEBUG*)}, remain comments in the projected stream while the
derived layer preserves @racket['pascal-compiler-directive].

Malformed Pascal input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{pascal}, @tt{pas}, @tt{delphi}, and
@tt{objectpascal} delegate to @racketmodname[lexers/pascal]. Wrapped
delegated Markdown tokens preserve Pascal-derived tags and gain
@racket['embedded-pascal].}

@defthing[pascal-profiles immutable-hash?]{
The profile defaults used by the Pascal lexer.}

@section{Python}

@defmodule[lexers/python]

The projected Python API has two entry points:

@itemlist[
 @item{@racket[make-python-lexer] for streaming tokenization from an input port.}
 @item{@racket[python-string->tokens] for eager tokenization of an entire string.}]

The first Python implementation is a handwritten streaming lexer grounded in
Python's lexical-analysis rules. It tracks indentation-sensitive line starts,
physical and logical newlines, names, comments, strings, numbers, operators,
and delimiters.

@defproc[(make-python-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                            [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                            [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming Python lexer.

Projected Python categories include @racket['comment], @racket['whitespace],
@racket['keyword], @racket['identifier], @racket['literal],
@racket['operator], @racket['delimiter], and @racket['unknown].

Soft keywords currently project as @racket['keyword], while the derived layer
keeps the more specific @racket['python-soft-keyword] tag.

@examples[#:eval python-eval
(define lexer
  (make-python-lexer #:profile 'coloring))
(define in
  (open-input-string "def answer(x):\n    return x\n"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(python-string->tokens [source string?]
                                [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                                [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                                [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected Python tokens.}

The derived Python API provides reusable language-specific structure:

@defproc[(make-python-derived-lexer)
         (input-port? . -> . (or/c python-derived-token? 'eof))]{
Constructs a streaming Python lexer that returns derived Python tokens.}

@defproc[(python-string->derived-tokens [source string?])
         (listof python-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived Python tokens.}

@defproc[(python-derived-token? [v any/c])
         boolean?]{
Recognizes derived Python tokens.}

@defproc[(python-derived-token-tags [token python-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(python-derived-token-has-tag? [token python-derived-token?]
                                        [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(python-derived-token-text [token python-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(python-derived-token-start [token python-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(python-derived-token-end [token python-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable Python-specific derived tags include:

@itemlist[
 @item{@racket['python-comment]}
 @item{@racket['python-whitespace]}
 @item{@racket['python-newline]}
 @item{@racket['python-nl]}
 @item{@racket['python-line-join]}
 @item{@racket['python-keyword]}
 @item{@racket['python-soft-keyword]}
 @item{@racket['python-identifier]}
 @item{@racket['python-string-literal]}
 @item{@racket['python-bytes-literal]}
 @item{@racket['python-f-string-literal]}
 @item{@racket['python-t-string-literal]}
 @item{@racket['python-raw-string-literal]}
 @item{@racket['python-numeric-literal]}
 @item{@racket['python-operator]}
 @item{@racket['python-delimiter]}
 @item{@racket['python-indent]}
 @item{@racket['python-dedent]}
 @item{@racket['python-error]}
 @item{@racket['malformed-token]}]

Ordinary, bytes, formatted, template, and raw-prefixed Python strings all
project as @racket['literal], while the derived layer preserves the more
specific string-literal tags above.

Malformed Python input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{python} and @tt{py} delegate to
@racketmodname[lexers/python]. Wrapped delegated Markdown tokens preserve
Python-derived tags and gain @racket['embedded-python].}

@section{Shell}

@defmodule[lexers/shell]

The projected shell API has two entry points:

@itemlist[
 @item{@racket[make-shell-lexer] for streaming tokenization from an input
       port.}
 @item{@racket[shell-string->tokens] for eager tokenization of an entire
       string.}]

The first shell implementation is a handwritten lexer for reusable shell
tokenization. It currently supports Bash, Zsh, and PowerShell. The public API
defaults to Bash and accepts @racket[#:shell 'bash], @racket[#:shell 'zsh],
and @racket[#:shell 'powershell] (with @racket['pwsh] accepted as an alias).
The derived layer also distinguishes pipelines, logical operators,
redirections, and heredoc introducers instead of exposing them only through
generic shell punctuation tags.

@defproc[(make-shell-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                           [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                           [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default]
                           [#:shell shell (or/c 'bash 'zsh 'powershell 'pwsh) 'bash])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming shell lexer.}

@defproc[(shell-string->tokens [source string?]
                               [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                               [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                               [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default]
                               [#:shell shell (or/c 'bash 'zsh 'powershell 'pwsh) 'bash])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes an entire shell source string using the projected token API.}

@subsection{Shell Returned Tokens}

Common projected shell categories include:

@itemlist[
 @item{@racket['whitespace]}
 @item{@racket['comment]}
 @item{@racket['keyword]}
 @item{@racket['identifier]}
 @item{@racket['literal]}
 @item{@racket['delimiter]}
 @item{@racket['unknown]}
 @item{@racket['eof]}]

For the current shell scaffold:

@itemlist[
 @item{keywords and builtins project as @racket['keyword]}
 @item{remaining words project as @racket['identifier]}
 @item{strings, variables, command substitutions, options, and numeric
       literals project as @racket['literal]}
 @item{operators and punctuation project as @racket['delimiter]}
 @item{malformed string or substitution input projects as @racket['unknown] in
       @racket['coloring] mode and raises in @racket['compiler] mode}]

Projected and derived shell token text preserve the exact consumed source
slice, including comments, whitespace, and CRLF line endings.

@examples[#:eval shell-eval
(define lexer
  (make-shell-lexer #:profile 'coloring #:shell 'bash))
(define in
  (open-input-string "export PATH\necho $PATH\n"))
(port-count-lines! in)
(list (lexer in)
      (lexer in)
      (lexer in))
]}

@defproc[(make-shell-derived-lexer [#:shell shell (or/c 'bash 'zsh 'powershell 'pwsh) 'bash])
         (input-port? . -> . (or/c 'eof shell-derived-token?))]{
Constructs a streaming shell lexer for the derived-token layer.}

@defproc[(shell-string->derived-tokens [source string?]
                                       [#:shell shell (or/c 'bash 'zsh 'powershell 'pwsh) 'bash])
         (listof shell-derived-token?)]{
Tokenizes an entire shell source string into derived shell token values.}

@defproc[(shell-derived-token? [v any/c])
         boolean?]{
Recognizes derived shell token values returned by
@racket[make-shell-derived-lexer] and
@racket[shell-string->derived-tokens].}

@defproc[(shell-derived-token-tags [token shell-derived-token?])
         (listof symbol?)]{
Returns the shell-specific classification tags attached to a derived shell
token.}

@defproc[(shell-derived-token-has-tag? [token shell-derived-token?]
                                       [tag symbol?])
         boolean?]{
Determines whether a derived shell token carries a given classification tag.}

@defproc[(shell-derived-token-text [token shell-derived-token?])
         string?]{
Returns the exact source text corresponding to a derived shell token.}

@defproc[(shell-derived-token-start [token shell-derived-token?])
         position?]{
Returns the starting source position for a derived shell token.}

@defproc[(shell-derived-token-end [token shell-derived-token?])
         position?]{
Returns the ending source position for a derived shell token.}

@subsection{Shell Derived Tokens}

The current shell scaffold may attach tags such as:

@itemlist[
 @item{@racket['shell-keyword]}
 @item{@racket['shell-builtin]}
 @item{@racket['shell-word]}
 @item{@racket['shell-string-literal]}
 @item{@racket['shell-ansi-string-literal]}
 @item{@racket['shell-variable]}
 @item{@racket['shell-command-substitution]}
 @item{@racket['shell-comment]}
 @item{@racket['shell-option]}
 @item{@racket['shell-numeric-literal]}
 @item{@racket['shell-punctuation]}
 @item{@racket['shell-pipeline-operator]}
 @item{@racket['shell-logical-operator]}
 @item{@racket['shell-redirection-operator]}
 @item{@racket['shell-heredoc-operator]}
 @item{@racket['malformed-token]}]

In Bash and Zsh, ANSI-C quoted strings such as @tt{$'line\n'} project as
@racket['literal] while the derived layer preserves
@racket['shell-ansi-string-literal].

Markdown fenced code blocks delegate to @racketmodname[lexers/shell] for
@tt{bash}, @tt{sh}, @tt{shell}, @tt{zsh}, @tt{powershell}, @tt{pwsh}, and
@tt{ps1} info strings. Delegated Markdown tokens keep the shell tags and gain
@racket['embedded-shell].

@examples[#:eval shell-eval
(define derived-tokens
  (shell-string->derived-tokens "printf \"%s\\n\" $(pwd)\n# done\n"))
(map (lambda (token)
       (list (shell-derived-token-text token)
             (shell-derived-token-tags token)))
     derived-tokens)
]}

@defthing[shell-profiles immutable-hash?]{
The profile defaults used by the shell lexer.}

@section{Rust}

@defmodule[lexers/rust]

The projected Rust API has two entry points:

@itemlist[
 @item{@racket[make-rust-lexer] for streaming tokenization from an input port.}
 @item{@racket[rust-string->tokens] for eager tokenization of an entire string.}]

The first Rust implementation is a handwritten streaming lexer grounded in the
Rust lexical structure reference. It covers whitespace, line and nested block
comments, identifiers, raw identifiers, keywords, lifetimes, strings, raw
strings, character and byte literals, numeric literals, punctuation, and
delimiters.

@defproc[(make-rust-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                          [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                          [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming Rust lexer.

Projected Rust categories include @racket['comment], @racket['whitespace],
@racket['keyword], @racket['identifier], @racket['literal],
@racket['operator], @racket['delimiter], and @racket['unknown].}

@examples[#:eval rust-eval
(define lexer
  (make-rust-lexer #:profile 'coloring))
(define in
  (open-input-string "fn main() {\n    let r#type = 42u32;\n}\n"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(rust-string->tokens [source string?]
                              [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                              [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                              [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected Rust tokens.}

The derived Rust API provides reusable language-specific structure:

@defproc[(make-rust-derived-lexer)
         (input-port? . -> . (or/c rust-derived-token? 'eof))]{
Constructs a streaming Rust lexer that returns derived Rust tokens.}

@defproc[(rust-string->derived-tokens [source string?])
         (listof rust-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived Rust tokens.}

@defproc[(rust-derived-token? [v any/c])
         boolean?]{
Recognizes derived Rust tokens.}

@defproc[(rust-derived-token-tags [token rust-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(rust-derived-token-has-tag? [token rust-derived-token?]
                                      [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(rust-derived-token-text [token rust-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(rust-derived-token-start [token rust-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(rust-derived-token-end [token rust-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable Rust-specific derived tags include:

@itemlist[
 @item{@racket['rust-comment]}
 @item{@racket['rust-doc-comment]}
 @item{@racket['rust-whitespace]}
 @item{@racket['rust-keyword]}
 @item{@racket['rust-identifier]}
 @item{@racket['rust-raw-identifier]}
 @item{@racket['rust-lifetime]}
 @item{@racket['rust-string-literal]}
 @item{@racket['rust-raw-string-literal]}
 @item{@racket['rust-char-literal]}
 @item{@racket['rust-byte-literal]}
 @item{@racket['rust-byte-string-literal]}
 @item{@racket['rust-c-string-literal]}
 @item{@racket['rust-numeric-literal]}
 @item{@racket['rust-punctuation]}
 @item{@racket['rust-delimiter]}
 @item{@racket['malformed-token]}]

Ordinary Rust strings, byte strings, chars, bytes, and C strings validate
their escape structure in the derived layer. Invalid escapes and malformed
multi-character char literals remain source-faithful but are tagged with
@racket['malformed-token].

Malformed Rust input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{rust} or @tt{rs} delegate to
@racketmodname[lexers/rust]. Wrapped delegated Markdown tokens preserve
Rust-derived tags and gain @racket['embedded-rust].}

@defthing[rust-profiles immutable-hash?]{
The profile defaults used by the Rust lexer.}

@section{Swift}

@defmodule[lexers/swift]

The projected Swift API has two entry points:

@itemlist[
 @item{@racket[make-swift-lexer] for streaming tokenization from an input port.}
 @item{@racket[swift-string->tokens] for eager tokenization of an entire string.}]

The first Swift implementation is a handwritten streaming lexer grounded in
Swift lexical structure. It covers whitespace, line comments, nested block
comments, identifiers, keywords, attributes, pound directives, strings,
numbers, operators, and delimiters.

@defproc[(make-swift-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                           [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                           [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming Swift lexer.

Projected Swift categories include @racket['comment], @racket['whitespace],
@racket['keyword], @racket['identifier], @racket['literal],
@racket['operator], @racket['delimiter], and @racket['unknown].

@examples[#:eval swift-eval
(define lexer
  (make-swift-lexer #:profile 'coloring))
(define in
  (open-input-string "import UIKit\n@IBOutlet weak var label: UILabel!\n"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(swift-string->tokens [source string?]
                               [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                               [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                               [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected Swift tokens.}

The derived Swift API provides reusable language-specific structure:

@defproc[(make-swift-derived-lexer)
         (input-port? . -> . (or/c swift-derived-token? 'eof))]{
Constructs a streaming Swift lexer that returns derived Swift tokens.}

@defproc[(swift-string->derived-tokens [source string?])
         (listof swift-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived Swift tokens.}

@defproc[(swift-derived-token? [v any/c])
         boolean?]{
Recognizes derived Swift tokens.}

@defproc[(swift-derived-token-tags [token swift-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(swift-derived-token-has-tag? [token swift-derived-token?]
                                       [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(swift-derived-token-text [token swift-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(swift-derived-token-start [token swift-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(swift-derived-token-end [token swift-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable Swift-specific derived tags include:

@itemlist[
 @item{@racket['swift-comment]}
 @item{@racket['swift-whitespace]}
 @item{@racket['swift-keyword]}
 @item{@racket['swift-identifier]}
 @item{@racket['swift-string-literal]}
 @item{@racket['swift-raw-string-literal]}
 @item{@racket['swift-numeric-literal]}
 @item{@racket['swift-attribute]}
 @item{@racket['swift-pound-directive]}
 @item{@racket['swift-operator]}
 @item{@racket['swift-delimiter]}
 @item{@racket['swift-error]}
 @item{@racket['malformed-token]}]

Both ordinary Swift strings and raw strings with @tt{#} delimiters project as
@racket['literal], while the derived layer preserves
@racket['swift-raw-string-literal] for the raw forms.

Malformed Swift input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{swift} delegate to
@racketmodname[lexers/swift]. Wrapped delegated Markdown tokens preserve
Swift-derived tags and gain @racket['embedded-swift].}

@section{TeX}

@defmodule[lexers/tex]

The projected TeX API has two entry points:

@itemlist[
 @item{@racket[make-tex-lexer] for streaming tokenization from an input port.}
 @item{@racket[tex-string->tokens] for eager tokenization of an entire string.}]

The first TeX implementation is a handwritten streaming lexer grounded in TeX's
tokenization model, but it intentionally stays within a practical static
subset. It covers comments, whitespace, control words, control symbols, group
and optional delimiters, math shifts, parameter markers, and plain text runs.
The derived layer distinguishes inline-vs-display math shifts and gives
reusable tags to the common special characters @tt{&}, @tt{_}, @tt{^}, and
@tt{~}. It also gives reusable tags to common control-symbol spacing commands
such as @tt{\ }, @tt{\,}, @tt{\;}, @tt{\!}, and @tt{\/}. Common accent
control symbols such as @tt{\'} and @tt{\"} also receive their own reusable
tag, and group/optional delimiters distinguish opening-vs-closing roles. The
plain-TeX paragraph command @tt{\par} also receives its own reusable tag.

@defproc[(make-tex-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                         [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                         [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming TeX lexer.

Projected TeX categories include @racket['comment], @racket['whitespace],
@racket['identifier], @racket['literal], @racket['delimiter], and
@racket['unknown].

@examples[#:eval tex-eval
(define lexer
  (make-tex-lexer #:profile 'coloring))
(define in
  (open-input-string "\\section{Hi}\n$x+y$\n"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(tex-string->tokens [source string?]
                             [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                             [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                             [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected TeX tokens.}

The derived TeX API provides reusable language-specific structure:

@defproc[(make-tex-derived-lexer)
         (input-port? . -> . (or/c tex-derived-token? 'eof))]{
Constructs a streaming TeX lexer that returns derived TeX tokens.}

@defproc[(tex-string->derived-tokens [source string?])
         (listof tex-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived TeX tokens.}

@defproc[(tex-derived-token? [v any/c])
         boolean?]{
Recognizes derived TeX tokens.}

@defproc[(tex-derived-token-tags [token tex-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(tex-derived-token-has-tag? [token tex-derived-token?]
                                     [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(tex-derived-token-text [token tex-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(tex-derived-token-start [token tex-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(tex-derived-token-end [token tex-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable TeX-specific derived tags include:

@itemlist[
 @item{@racket['tex-comment]}
 @item{@racket['tex-whitespace]}
 @item{@racket['tex-control-word]}
 @item{@racket['tex-control-symbol]}
 @item{@racket['tex-paragraph-command]}
 @item{@racket['tex-parameter]}
 @item{@racket['tex-parameter-reference]}
 @item{@racket['tex-parameter-escape]}
 @item{@racket['tex-parameter-marker]}
 @item{@racket['tex-text]}
 @item{@racket['tex-math-shift]}
 @item{@racket['tex-inline-math-shift]}
 @item{@racket['tex-display-math-shift]}
 @item{@racket['tex-group-delimiter]}
 @item{@racket['tex-open-group-delimiter]}
 @item{@racket['tex-close-group-delimiter]}
 @item{@racket['tex-optional-delimiter]}
 @item{@racket['tex-open-optional-delimiter]}
 @item{@racket['tex-close-optional-delimiter]}
 @item{@racket['tex-special-character]}
 @item{@racket['tex-alignment-tab]}
 @item{@racket['tex-subscript-mark]}
 @item{@racket['tex-superscript-mark]}
 @item{@racket['tex-unbreakable-space]}
 @item{@racket['tex-control-space]}
 @item{@racket['tex-accent-command]}
 @item{@racket['tex-spacing-command]}
 @item{@racket['tex-italic-correction]}
 @item{@racket['malformed-token]}]

Markdown fenced code blocks labeled @tt{tex} delegate to
@racketmodname[lexers/tex]. Wrapped delegated Markdown tokens preserve
TeX-derived tags and gain @racket['embedded-tex].}

@defthing[tex-profiles immutable-hash?]{
The profile defaults used by the TeX lexer.}

@section{LaTeX}

@defmodule[lexers/latex]

The projected LaTeX API has two entry points:

@itemlist[
 @item{@racket[make-latex-lexer] for streaming tokenization from an input port.}
 @item{@racket[latex-string->tokens] for eager tokenization of an entire string.}]

The first LaTeX implementation builds on the TeX lexer and adds a lightweight
classification layer for common LaTeX commands such as @tt{\section},
@tt{\begin}, and @tt{\end}. Environment names in forms such as
@tt{\begin\{itemize\}} receive their own derived tag, and @tt{\verb|...|}
spans receive a dedicated verbatim-literal tag. The common LaTeX line-break
command @tt{\\} also receives its own derived tag.

@defproc[(make-latex-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                           [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                           [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming LaTeX lexer.

Projected LaTeX categories include @racket['comment], @racket['whitespace],
@racket['keyword], @racket['identifier], @racket['literal],
@racket['delimiter], and @racket['unknown].}

@defproc[(latex-string->tokens [source string?]
                               [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                               [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                               [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected LaTeX tokens.}

The derived LaTeX API reuses the TeX token representation and adds LaTeX tags
where applicable:

@defproc[(make-latex-derived-lexer)
         (input-port? . -> . (or/c latex-derived-token? 'eof))]{
Constructs a streaming LaTeX lexer that returns derived LaTeX tokens.}

@defproc[(latex-string->derived-tokens [source string?])
         (listof latex-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived LaTeX tokens.}

@defproc[(latex-derived-token? [v any/c])
         boolean?]{
Recognizes derived LaTeX tokens.}

@defproc[(latex-derived-token-tags [token latex-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(latex-derived-token-has-tag? [token latex-derived-token?]
                                       [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(latex-derived-token-text [token latex-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(latex-derived-token-start [token latex-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(latex-derived-token-end [token latex-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

Common additional LaTeX-oriented derived tags include:

@itemlist[
 @item{@racket['latex-command]}
 @item{@racket['latex-environment-command]}
 @item{@racket['latex-environment-name]}
 @item{@racket['latex-verbatim-literal]}
 @item{@racket['latex-line-break-command]}]

Markdown fenced code blocks labeled @tt{latex} delegate to
@racketmodname[lexers/latex]. Wrapped delegated Markdown tokens preserve
LaTeX-derived tags and gain @racket['embedded-latex].}

@defthing[latex-profiles immutable-hash?]{
The profile defaults used by the LaTeX lexer.}

@section{TSV}

@defmodule[lexers/tsv]

The projected TSV API has two entry points:

@itemlist[
 @item{@racket[make-tsv-lexer] for streaming tokenization from an input port.}
 @item{@racket[tsv-string->tokens] for eager tokenization of an entire string.}]

The first TSV implementation is a handwritten streaming lexer for
tab-separated text. It preserves exact source text, including literal tab
separators, empty fields, and CRLF row separators.

@defproc[(make-tsv-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                         [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                         [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming TSV lexer.

Projected TSV categories include @racket['literal], @racket['delimiter], and
@racket['unknown].

Field contents project as @racket['literal]. Field separators and row
separators project as @racket['delimiter].

@examples[#:eval tsv-eval
(define lexer
  (make-tsv-lexer #:profile 'coloring))
(define in
  (open-input-string "name\tage\nAda\t37\n"))
(port-count-lines! in)
(list (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in))
      (lexer-token-name (lexer in)))
]}

@defproc[(tsv-string->tokens [source string?]
                             [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                             [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                             [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes all of @racket[source] eagerly and returns projected TSV tokens.}

The derived TSV API provides reusable structure for delimited text:

@defproc[(make-tsv-derived-lexer)
         (input-port? . -> . (or/c tsv-derived-token? 'eof))]{
Constructs a streaming TSV lexer that returns derived TSV tokens.}

@defproc[(tsv-string->derived-tokens [source string?])
         (listof tsv-derived-token?)]{
Tokenizes all of @racket[source] eagerly and returns derived TSV tokens.}

@defproc[(tsv-derived-token? [v any/c])
         boolean?]{
Recognizes derived TSV tokens.}

@defproc[(tsv-derived-token-tags [token tsv-derived-token?])
         (listof symbol?)]{
Returns the derived-token tags for @racket[token].}

@defproc[(tsv-derived-token-has-tag? [token tsv-derived-token?]
                                     [tag symbol?])
         boolean?]{
Determines whether @racket[token] carries @racket[tag].}

@defproc[(tsv-derived-token-text [token tsv-derived-token?])
         string?]{
Returns the exact source text covered by @racket[token].}

@defproc[(tsv-derived-token-start [token tsv-derived-token?])
         position?]{
Returns the starting source position of @racket[token].}

@defproc[(tsv-derived-token-end [token tsv-derived-token?])
         position?]{
Returns the ending source position of @racket[token].}

The first reusable TSV-specific derived tags include:

@itemlist[
 @item{@racket['delimited-field]}
 @item{@racket['delimited-quoted-field]}
 @item{@racket['delimited-unquoted-field]}
 @item{@racket['delimited-empty-field]}
 @item{@racket['delimited-separator]}
 @item{@racket['delimited-row-separator]}
 @item{@racket['delimited-error]}
 @item{@racket['tsv-field]}
 @item{@racket['tsv-separator]}
 @item{@racket['tsv-row-separator]}
 @item{@racket['malformed-token]}]

Malformed TSV input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{tsv} delegate to
@racketmodname[lexers/tsv]. Wrapped delegated Markdown tokens preserve
TSV-derived tags and gain @racket['embedded-tsv].}

@defthing[tsv-profiles immutable-hash?]{
The profile defaults used by the TSV lexer.}

@section{WAT}

@defmodule[lexers/wat]

The projected WAT API has two entry points:

@itemlist[
 @item{@racket[make-wat-lexer] for streaming tokenization from an input port.}
 @item{@racket[wat-string->tokens] for eager tokenization of an entire string.}]

The first WAT implementation is a handwritten lexer for WebAssembly text
format. It targets WAT only, not binary @tt{.wasm} files.

@defproc[(make-wat-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                         [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                         [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming WAT lexer.

The result is a procedure of one argument, an input port. Each call reads the
next projected WAT token from the port and returns one projected token value.

When @racket[#:source-positions] is true, each result is a
@racket[position-token?]. When it is false, the result is either a bare symbol
or a @racket[token?] directly.

The intended use is to create the lexer once, then call it repeatedly on the
same port until it returns an end-of-file token.

The streaming port readers emit tokens incrementally. They do not buffer the
entire remaining input before producing the first token.

@examples[#:eval wat-eval
(define lexer
  (make-wat-lexer #:profile 'coloring))
(define in
  (open-input-string "(module (func (result i32) (i32.const 42)))"))
(port-count-lines! in)
(list (lexer in)
      (lexer in)
      (lexer in)
      (lexer in))
]}

@defproc[(wat-string->tokens [source string?]
                             [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                             [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                             [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes an entire WAT string using the projected token API.

This is a convenience wrapper over @racket[make-wat-lexer].}

@subsection{WAT Returned Tokens}

Common projected WAT categories include:

@itemlist[
 @item{@racket['whitespace]}
 @item{@racket['comment]}
 @item{@racket['identifier]}
 @item{@racket['keyword]}
 @item{@racket['literal]}
 @item{@racket['delimiter]}
 @item{@racket['unknown]}
 @item{@racket['eof]}]

For the current WAT scaffold:

@itemlist[
 @item{form names, type names, and instruction names project as
       @racket['keyword]}
 @item{$-prefixed names and remaining word-like names project as
       @racket['identifier]}
 @item{strings and numeric literals project as @racket['literal]}
 @item{parentheses project as @racket['delimiter]}
 @item{comments project as @racket['comment]}
 @item{malformed input projects as @racket['unknown] in @racket['coloring]
       mode and raises in @racket['compiler] mode}]

Projected and derived token text preserve the exact source slice, including
whitespace and comments.

@examples[#:eval wat-eval
(define inspect-lexer
  (make-wat-lexer #:profile 'coloring))
(define inspect-in
  (open-input-string ";; line comment\n(module (func (result i32) (i32.const 42)))"))
(port-count-lines! inspect-in)
(define first-token
  (inspect-lexer inspect-in))
(lexer-token-has-positions? first-token)
(lexer-token-name first-token)
(lexer-token-value first-token)
(position-offset (lexer-token-start first-token))
(position-offset (lexer-token-end first-token))
]}

@defproc[(make-wat-derived-lexer)
         (input-port? . -> . (or/c 'eof wat-derived-token?))]{
Constructs a streaming WAT lexer for the derived-token layer.}

@defproc[(wat-string->derived-tokens [source string?])
         (listof wat-derived-token?)]{
Tokenizes an entire WAT string into derived WAT token values.}

@defproc[(wat-derived-token? [v any/c])
         boolean?]{
Recognizes derived WAT token values returned by
@racket[make-wat-derived-lexer] and
@racket[wat-string->derived-tokens].}

@defproc[(wat-derived-token-tags [token wat-derived-token?])
         (listof symbol?)]{
Returns the WAT-specific classification tags attached to a derived WAT token.}

@defproc[(wat-derived-token-has-tag? [token wat-derived-token?]
                                     [tag symbol?])
         boolean?]{
Determines whether a derived WAT token carries a given classification tag.}

@defproc[(wat-derived-token-text [token wat-derived-token?])
         string?]{
Returns the exact source text corresponding to a derived WAT token.}

@defproc[(wat-derived-token-start [token wat-derived-token?])
         position?]{
Returns the starting source position for a derived WAT token.}

@defproc[(wat-derived-token-end [token wat-derived-token?])
         position?]{
Returns the ending source position for a derived WAT token.}

@subsection{WAT Derived Tokens}

The current WAT scaffold may attach tags such as:

@itemlist[
 @item{@racket['wat-form]}
 @item{@racket['wat-type]}
 @item{@racket['wat-instruction]}
 @item{@racket['wat-identifier]}
 @item{@racket['wat-string-literal]}
 @item{@racket['wat-numeric-literal]}
 @item{@racket['comment]}
 @item{@racket['whitespace]}
 @item{@racket['malformed-token]}]

@examples[#:eval wat-eval
(define derived-tokens
  (wat-string->derived-tokens
   "(module (func $answer (result i32) i32.const 42))"))
(map (lambda (token)
       (list (wat-derived-token-text token)
             (wat-derived-token-tags token)))
     derived-tokens)
]}

@defthing[wat-profiles immutable-hash?]{
The profile defaults used by the WAT lexer.}

@section{Racket}

@defmodule[lexers/racket]

The projected Racket API has two entry points:

@itemlist[
 @item{@racket[make-racket-lexer] for streaming tokenization from an input
       port.}
 @item{@racket[racket-string->tokens] for eager tokenization of an entire
       string.}]

This lexer is adapter-backed. It uses the lexer from
@racketmodname[syntax-color/racket-lexer] as its raw engine and adapts that
output into the public @tt{lexers} projected and derived APIs.

When a source starts with @racket["#lang at-exp"], the adapter switches to
the Scribble lexer family in Racket mode so that @tt|{@litchar["@"]}| forms
are tokenized as Scribble escapes instead of ordinary symbol text.

@defproc[(make-racket-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                            [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                            [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming Racket lexer.

The result is a procedure of one argument, an input port. Each call reads the
next token from the port and returns one projected token value.

When @racket[#:source-positions] is true, each result is a
@racket[position-token?]. When it is false, the result is either a bare symbol
or a @racket[token?] directly.

The intended use is to create the lexer once, then call it repeatedly on the
same port until it returns an end-of-file token.

@examples[#:eval racket-eval
(define lexer
  (make-racket-lexer #:profile 'coloring))
(define in
  (open-input-string "#:x \"hi\""))
(port-count-lines! in)
(list (lexer in)
      (lexer in)
      (lexer in))
]}

@defproc[(racket-string->tokens [source string?]
                                [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                                [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                                [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes an entire Racket string using the projected token API.

This is a convenience wrapper over @racket[make-racket-lexer].}

@subsection{Racket Returned Tokens}

Common projected Racket categories include:

@itemlist[
 @item{@racket['whitespace]}
 @item{@racket['comment]}
 @item{@racket['identifier]}
 @item{@racket['literal]}
 @item{@racket['delimiter]}
 @item{@racket['unknown]}
 @item{@racket['eof]}]

For the current adapter:

@itemlist[
 @item{comments and sexp comments project as @racket['comment]}
 @item{whitespace projects as @racket['whitespace]}
 @item{strings, constants, and hash-colon keywords project as
       @racket['literal]}
 @item{symbols, @tt{other}, and @tt{no-color} tokens project as
       @racket['identifier]}
 @item{parentheses project as @racket['delimiter]}
 @item{lexical errors project as @racket['unknown] in @racket['coloring] mode
       and raise in @racket['compiler] mode}]

Projected and derived Racket token text preserve the exact consumed source
slice, including multi-semicolon comment headers such as @tt{;;;}.

@examples[#:eval racket-eval
(define inspect-lexer
  (make-racket-lexer #:profile 'coloring))
(define inspect-in
  (open-input-string "#;(+ 1 2) #:x"))
(port-count-lines! inspect-in)
(define first-token
  (inspect-lexer inspect-in))
(lexer-token-has-positions? first-token)
(lexer-token-name first-token)
(lexer-token-value first-token)
]}

@defproc[(make-racket-derived-lexer)
         (input-port? . -> . (or/c 'eof racket-derived-token?))]{
Constructs a streaming Racket lexer for the derived-token layer.}

@defproc[(racket-string->derived-tokens [source string?])
         (listof racket-derived-token?)]{
Tokenizes an entire Racket string into derived Racket token values.}

@defproc[(racket-derived-token? [v any/c])
         boolean?]{
Recognizes derived Racket token values returned by
@racket[make-racket-derived-lexer] and
@racket[racket-string->derived-tokens].}

@defproc[(racket-derived-token-tags [token racket-derived-token?])
         (listof symbol?)]{
Returns the Racket-specific classification tags attached to a derived Racket
token.}

@defproc[(racket-derived-token-has-tag? [token racket-derived-token?]
                                        [tag symbol?])
         boolean?]{
Determines whether a derived Racket token carries a given classification tag.}

@defproc[(racket-derived-token-text [token racket-derived-token?])
         string?]{
Returns the exact source text corresponding to a derived Racket token.}

@defproc[(racket-derived-token-start [token racket-derived-token?])
         position?]{
Returns the starting source position for a derived Racket token.}

@defproc[(racket-derived-token-end [token racket-derived-token?])
         position?]{
Returns the ending source position for a derived Racket token.}

@subsection{Racket Derived Tokens}

The current Racket adapter may attach tags such as:

@itemlist[
 @item{@racket['racket-comment]}
 @item{@racket['racket-sexp-comment]}
 @item{@racket['racket-whitespace]}
 @item{@racket['racket-constant]}
 @item{@racket['racket-string]}
 @item{@racket['racket-symbol]}
 @item{@racket['racket-parenthesis]}
 @item{@racket['racket-hash-colon-keyword]}
 @item{@racket['racket-commented-out]}
 @item{@racket['racket-datum]}
 @item{@racket['racket-open]}
 @item{@racket['racket-close]}
 @item{@racket['racket-continue]}
 @item{@racket['racket-usual-special-form]}
 @item{@racket['racket-definition-form]}
 @item{@racket['racket-binding-form]}
 @item{@racket['racket-conditional-form]}
 @item{@racket['racket-error]}
 @item{@racket['scribble-text] for @racket["#lang at-exp"] text regions}
 @item{@racket['scribble-command-char] for @tt|{@litchar["@"]}| in
       @racket["#lang at-exp"] sources}
 @item{@racket['scribble-command] for command names such as
       @tt|{@litchar["@"]bold}| in @racket["#lang at-exp"] sources}
 @item{@racket['scribble-body-delimiter]}
 @item{@racket['scribble-optional-delimiter]}
 @item{@racket['scribble-racket-escape]}]

The `usual special form` tags are heuristic. They are meant to help ordinary
Racket tooling recognize common built-in forms such as @racket[define],
@racket[define-values], @racket[if], and @racket[let], but they are not
guarantees about expanded meaning. In particular, a token whose text is
@racket["define"] may still receive @racket['racket-usual-special-form] even in
a program where @racket[define] has been rebound, because the lexer does not
perform expansion or binding resolution.

@examples[#:eval racket-eval
(define derived-tokens
  (racket-string->derived-tokens "#;(+ 1 2) #:x \"hi\""))
(map (lambda (token)
       (list (racket-derived-token-text token)
             (racket-derived-token-tags token)))
     derived-tokens)

(define at-exp-derived-tokens
  (racket-string->derived-tokens "#lang at-exp racket\n(define x @bold{hi})\n"))
(map (lambda (token)
       (list (racket-derived-token-text token)
             (racket-derived-token-tags token)))
     at-exp-derived-tokens)
]}

@defthing[racket-profiles immutable-hash?]{
The profile defaults used by the Racket lexer.}

@section{Rhombus}

@defmodule[lexers/rhombus]

The projected Rhombus API has two entry points:

@itemlist[
 @item{@racket[make-rhombus-lexer] for streaming tokenization from an input
       port.}
 @item{@racket[rhombus-string->tokens] for eager tokenization of an entire
       string.}]

This lexer is adapter-backed. It uses the lexer from @tt{rhombus/private/syntax-color}
as its raw engine and adapts that
output into the public @tt{lexers} projected and derived APIs.

Rhombus support is optional. When @tt{rhombus/private/syntax-color} is not
available, the module still loads, but calling the Rhombus lexer raises an
error explaining that Rhombus support requires @tt{rhombus-lib} on
@tt{base >= 8.14}.

@defproc[(make-rhombus-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                             [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                             [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming Rhombus lexer.}

@defproc[(rhombus-string->tokens [source string?]
                                 [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                                 [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                                 [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes an entire Rhombus string using the projected token API.}

@subsection{Rhombus Returned Tokens}

Common projected Rhombus categories include:

@itemlist[
 @item{@racket['whitespace]}
 @item{@racket['comment]}
 @item{@racket['identifier]}
 @item{@racket['keyword]}
 @item{@racket['literal]}
 @item{@racket['operator]}
 @item{@racket['delimiter]}
 @item{@racket['unknown]}
 @item{@racket['eof]}]

For the current adapter:

@itemlist[
 @item{ordinary whitespace projects as @racket['whitespace]}
 @item{line comments project as @racket['comment]}
 @item{Rhombus keywords and builtins project as @racket['keyword]}
 @item{remaining identifiers project as @racket['identifier]}
 @item{literals project as @racket['literal]}
 @item{openers, closers, and separators project as @racket['delimiter]}
 @item{operators such as @tt{+}, @tt{:}, and @tt{,} project as
       @racket['operator]}
 @item{recoverable malformed input projects as @racket['unknown] in
       @racket['coloring] mode and raises in @racket['compiler] mode}]

Projected and derived Rhombus token text preserve the exact consumed source
slice, including CRLF line endings when Rhombus support is available.

@defproc[(make-rhombus-derived-lexer)
         (input-port? . -> . (or/c 'eof rhombus-derived-token?))]{
Constructs a streaming Rhombus lexer for the derived-token layer.}

@defproc[(rhombus-string->derived-tokens [source string?])
         (listof rhombus-derived-token?)]{
Tokenizes an entire Rhombus string into derived Rhombus token values.}

@defproc[(rhombus-derived-token? [v any/c])
         boolean?]{
Recognizes derived Rhombus token values returned by
@racket[make-rhombus-derived-lexer] and
@racket[rhombus-string->derived-tokens].}

@defproc[(rhombus-derived-token-tags [token rhombus-derived-token?])
         (listof symbol?)]{
Returns the Rhombus-specific classification tags attached to a derived Rhombus
token.}

@defproc[(rhombus-derived-token-has-tag? [token rhombus-derived-token?]
                                         [tag symbol?])
         boolean?]{
Determines whether a derived Rhombus token carries a given classification tag.}

@defproc[(rhombus-derived-token-text [token rhombus-derived-token?])
         string?]{
Returns the exact source text corresponding to a derived Rhombus token.}

@defproc[(rhombus-derived-token-start [token rhombus-derived-token?])
         position?]{
Returns the starting source position for a derived Rhombus token.}

@defproc[(rhombus-derived-token-end [token rhombus-derived-token?])
         position?]{
Returns the ending source position for a derived Rhombus token.}

@subsection{Rhombus Derived Tokens}

The current Rhombus adapter may attach tags such as:

@itemlist[
 @item{@racket['rhombus-comment]}
 @item{@racket['rhombus-whitespace]}
 @item{@racket['rhombus-string]}
 @item{@racket['rhombus-constant]}
 @item{@racket['rhombus-literal]}
 @item{@racket['rhombus-identifier]}
 @item{@racket['rhombus-keyword]}
 @item{@racket['rhombus-builtin]}
 @item{@racket['rhombus-operator]}
 @item{@racket['rhombus-block-operator]}
 @item{@racket['rhombus-comma-operator]}
 @item{@racket['rhombus-opener]}
 @item{@racket['rhombus-closer]}
 @item{@racket['rhombus-parenthesis]}
 @item{@racket['rhombus-separator]}
 @item{@racket['rhombus-at]}
 @item{@racket['rhombus-fail]}
 @item{@racket['rhombus-error]}
 @item{@racket['malformed-token]}]

The adapter preserves Rhombus-specific keyword and builtin guesses from
@tt{rhombus/private/syntax-color}. Since the shared projected stream
does not have a separate builtin category, builtins currently project as
@racket['keyword], while the derived-token layer keeps the more specific
@racket['rhombus-builtin] tag.

@defthing[rhombus-profiles immutable-hash?]{
The profile defaults used by the Rhombus lexer.}

@section{Scribble}

@defmodule[lexers/scribble]

The projected Scribble API has two entry points:

@itemlist[
 @item{@racket[make-scribble-lexer] for streaming tokenization from an input
       port.}
 @item{@racket[scribble-string->tokens] for eager tokenization of an entire
       string.}]

This lexer is adapter-backed. It uses
@racketmodname[syntax-color/scribble-lexer] as its raw engine and adapts that
output into the public @tt{lexers} projected and derived APIs.

The first implementation defaults to Scribble's inside/text mode via
@racket[make-scribble-inside-lexer]. Command-character customization is
intentionally deferred.

@defproc[(make-scribble-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                              [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                              [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming Scribble lexer.

The result is a procedure of one argument, an input port. Each call reads the
next token from the port and returns one projected token value.

When @racket[#:source-positions] is true, each result is a
@racket[position-token?]. When it is false, the result is either a bare symbol
or a @racket[token?] directly.

The intended use is to create the lexer once, then call it repeatedly on the
same port until it returns an end-of-file token.

@examples[#:eval scribble-eval
(define lexer
  (make-scribble-lexer #:profile 'coloring))
(define in
  (open-input-string "@title{Hi}\nText"))
(port-count-lines! in)
(list (lexer in)
      (lexer in)
      (lexer in)
      (lexer in))
]}

@defproc[(scribble-string->tokens [source string?]
                                  [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                                  [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                                  [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes an entire Scribble string using the projected token API.

This is a convenience wrapper over @racket[make-scribble-lexer].}

@subsection{Scribble Returned Tokens}

Common projected Scribble categories include:

@itemlist[
 @item{@racket['whitespace]}
 @item{@racket['comment]}
 @item{@racket['identifier]}
 @item{@racket['literal]}
 @item{@racket['delimiter]}
 @item{@racket['unknown]}
 @item{@racket['eof]}]

For the current adapter:

@itemlist[
 @item{text, strings, and constants project as @racket['literal]}
 @item{whitespace projects as @racket['whitespace]}
 @item{symbol and @tt{other} tokens project as @racket['identifier]}
 @item{parentheses, the command character, and body or optional delimiters
       project as @racket['delimiter]}
 @item{lexical errors project as @racket['unknown] in @racket['coloring] mode
       and raise in @racket['compiler] mode}]

For source fidelity, the Scribble adapter preserves the exact source slice for
projected and derived token text, including whitespace spans that contain one
or more newlines.

@examples[#:eval scribble-eval
(define inspect-lexer
  (make-scribble-lexer #:profile 'coloring))
(define inspect-in
  (open-input-string "@title{Hi}"))
(port-count-lines! inspect-in)
(define first-token
  (inspect-lexer inspect-in))
(lexer-token-has-positions? first-token)
(lexer-token-name first-token)
(lexer-token-value first-token)
]}

@defproc[(make-scribble-derived-lexer)
         (input-port? . -> . (or/c 'eof scribble-derived-token?))]{
Constructs a streaming Scribble lexer for the derived-token layer.}

@defproc[(scribble-string->derived-tokens [source string?])
         (listof scribble-derived-token?)]{
Tokenizes an entire Scribble string into derived Scribble token values.}

@defproc[(scribble-derived-token? [v any/c])
         boolean?]{
Recognizes derived Scribble token values returned by
@racket[make-scribble-derived-lexer] and
@racket[scribble-string->derived-tokens].}

@defproc[(scribble-derived-token-tags [token scribble-derived-token?])
         (listof symbol?)]{
Returns the Scribble-specific classification tags attached to a derived
Scribble token.}

@defproc[(scribble-derived-token-has-tag? [token scribble-derived-token?]
                                          [tag symbol?])
         boolean?]{
Determines whether a derived Scribble token carries a given classification
tag.}

@defproc[(scribble-derived-token-text [token scribble-derived-token?])
         string?]{
Returns the exact source text corresponding to a derived Scribble token.}

@defproc[(scribble-derived-token-start [token scribble-derived-token?])
         position?]{
Returns the starting source position for a derived Scribble token.}

@defproc[(scribble-derived-token-end [token scribble-derived-token?])
         position?]{
Returns the ending source position for a derived Scribble token.}

@subsection{Scribble Derived Tokens}

The current Scribble adapter may attach tags such as:

@itemlist[
 @item{@racket['scribble-comment]}
 @item{@racket['scribble-whitespace]}
 @item{@racket['scribble-text]}
 @item{@racket['scribble-string]}
 @item{@racket['scribble-constant]}
 @item{@racket['scribble-symbol]}
 @item{@racket['scribble-parenthesis]}
 @item{@racket['scribble-other]}
 @item{@racket['scribble-error]}
 @item{@racket['scribble-command]}
 @item{@racket['scribble-command-char]}
 @item{@racket['scribble-body-delimiter]}
 @item{@racket['scribble-optional-delimiter]}
 @item{@racket['scribble-racket-escape]}]

These tags describe reusable Scribble structure, not presentation. In
particular, @racket['scribble-command] only means that a symbol-like token is
being used as a command name after @racket["@"]. It does not mean the lexer has
inferred higher-level document semantics for commands such as
@racket[title] or @racket[itemlist].

@examples[#:eval scribble-eval
(define derived-tokens
  (scribble-string->derived-tokens
   "@title{Hi}\n@racket[(define x 1)]"))
(map (lambda (token)
       (list (scribble-derived-token-text token)
             (scribble-derived-token-tags token)))
     derived-tokens)
]}

@defthing[scribble-profiles immutable-hash?]{
The profile defaults used by the Scribble lexer.}

@section{JavaScript}

@defmodule[lexers/javascript]

The projected JavaScript API has two entry points:

@itemlist[
 @item{@racket[make-javascript-lexer] for streaming tokenization from an input
       port.}
 @item{@racket[javascript-string->tokens] for eager tokenization of an entire
       string.}]

@defproc[(make-javascript-lexer [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                                [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                                [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default]
                                [#:jsx? jsx? boolean? #f])
         (input-port? . -> . (or/c symbol? token? position-token?))]{
Constructs a streaming JavaScript lexer.

The result is a procedure of one argument, an input port. Each call reads the
next token from the port and returns one projected token value.

When @racket[#:source-positions] is true, each result is a
@racket[position-token?] whose payload is either a bare symbol such as
@racket['eof] or a @racket[token?] carrying a projected category such as
@racket['keyword], @racket['identifier], @racket['literal],
@racket['operator], @racket['comment], or @racket['unknown].

When @racket[#:source-positions] is false, the result is either a bare symbol
or a @racket[token?] directly.

The intended use is to create the lexer once, then call it repeatedly on the
same port until it returns an end-of-file token.

When @racket[#:jsx?] is true, the lexer accepts a small JSX extension inside
JavaScript expressions. The projected token categories remain the same, while
the derived-token API exposes JSX-specific structure.

The current JavaScript slice includes broader modern numeric literals, such as
hexadecimal, binary, and octal prefixed integers, decimal exponents, numeric
separators, and integer @tt{BigInt} suffixes.

@examples[#:eval javascript-eval
(define lexer
  (make-javascript-lexer #:profile 'coloring))
(define in
  (open-input-string "const x = 1;"))
(port-count-lines! in)
(list (lexer in)
      (lexer in)
      (lexer in)
      (lexer in))
]}

@defproc[(javascript-string->tokens [source string?]
                                    [#:profile profile (or/c 'coloring 'compiler) 'coloring]
                                    [#:trivia trivia (or/c 'profile-default 'keep 'skip) 'profile-default]
                                    [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default]
                                    [#:jsx? jsx? boolean? #f])
         (listof (or/c symbol? token? position-token?))]{
Tokenizes an entire JavaScript string using the projected token API.

This is a convenience wrapper over @racket[make-javascript-lexer]. It opens a
string port, enables line counting, repeatedly calls the port-based lexer until
end-of-file, and returns the resulting token list.}

@subsection{JavaScript Returned Tokens}

The projected JavaScript API uses the same output shape:

@itemlist[
 @item{The end of input is reported as @racket['eof], either directly or inside
       a @racket[position-token?].}
 @item{Ordinary results are usually @racket[token?] values whose
       @racket[token-name] is a projected category and whose
       @racket[token-value] contains language-specific text or metadata.}
 @item{When @racket[#:source-positions] is true, each result is wrapped in a
       @racket[position-token?].}
 @item{When @racket[#:source-positions] is false, results are returned without
       that outer wrapper.}]

Common projected JavaScript categories include:

@itemlist[
 @item{@racket['whitespace]}
 @item{@racket['comment]}
 @item{@racket['keyword]}
 @item{@racket['identifier]}
 @item{@racket['literal]}
 @item{@racket['operator]}
 @item{@racket['delimiter]}
 @item{@racket['unknown]}
 @item{@racket['eof]}]

In @racket['coloring] mode, whitespace and comments are kept, and recoverable
malformed input is returned as @racket['unknown]. In @racket['compiler] mode,
whitespace and comments are skipped by default, and malformed input raises an
exception instead of producing an @racket['unknown] token.

For the current JavaScript scaffold, @racket[token-value] also preserves the
original source text of the emitted token. In particular:

@itemlist[
 @item{For @racket['keyword] and @racket['identifier], the value is the matched
       identifier text, such as @racket["const"] or @racket["name"].}
 @item{For @racket['literal], the value is the matched literal text, such as
       @racket["1"], @racket["1_000"], @racket["123n"], or
       @racket["\"hello\""].}
 @item{For @racket['comment] and @racket['whitespace], the value is the
       original comment or whitespace text when those categories are kept.}
 @item{For @racket['operator] and @racket['delimiter], the value is the matched
       character text, such as @racket["="], @racket[";"], or @racket["("].}
 @item{For @racket['unknown] in tolerant mode, the value is the malformed input
       text that could not be accepted.}]

@examples[#:eval javascript-eval
(define inspect-lexer
  (make-javascript-lexer #:profile 'coloring))
(define inspect-in
  (open-input-string "const x = 1;"))
(port-count-lines! inspect-in)
(define first-token
  (inspect-lexer inspect-in))
(lexer-token-has-positions? first-token)
(lexer-token-name first-token)
(lexer-token-value first-token)
(position-offset (lexer-token-start first-token))
(position-offset (lexer-token-end first-token))
]}

@defproc[(make-javascript-derived-lexer [#:jsx? jsx? boolean? #f])
         (input-port? . -> . (or/c 'eof javascript-derived-token?))]{
Constructs a streaming JavaScript lexer for the derived-token layer.

The result is a procedure of one argument, an input port. Each call reads the
next raw JavaScript token from the port, computes its JavaScript-specific
derived classifications, and returns one derived token value. At end of input,
it returns @racket['eof].

The intended use is the same as for @racket[make-javascript-lexer]: create the
lexer once, then call it repeatedly on the same port until it returns
@racket['eof].

@examples[#:eval javascript-eval
(define derived-lexer
  (make-javascript-derived-lexer))
(define derived-in
  (open-input-string "const x = 1;"))
(port-count-lines! derived-in)
(list (derived-lexer derived-in)
      (derived-lexer derived-in)
      (derived-lexer derived-in)
      (derived-lexer derived-in))
]}

@defproc[(javascript-string->derived-tokens [source string?]
                                            [#:jsx? jsx? boolean? #f])
         (listof javascript-derived-token?)]{
Tokenizes an entire JavaScript string into derived JavaScript token values.

This is a convenience wrapper over @racket[make-javascript-derived-lexer]. It
opens a string port, enables line counting, repeatedly calls the derived lexer
until it returns @racket['eof], and returns the resulting list of derived
tokens.}

@defproc[(javascript-derived-token? [v any/c])
         boolean?]{
Recognizes derived JavaScript token values returned by
@racket[make-javascript-derived-lexer] and
@racket[javascript-string->derived-tokens].}

@defproc[(javascript-derived-token-tags [token javascript-derived-token?])
         (listof symbol?)]{
Returns the JavaScript-specific classification tags attached to a derived
JavaScript token.}

@defproc[(javascript-derived-token-has-tag? [token javascript-derived-token?]
                                            [tag symbol?])
         boolean?]{
Determines whether a derived JavaScript token carries a given classification
tag.}

@defproc[(javascript-derived-token-text [token javascript-derived-token?])
         string?]{
Returns the exact source text corresponding to a derived JavaScript token.}

@defproc[(javascript-derived-token-start [token javascript-derived-token?])
         position?]{
Returns the starting source position for a derived JavaScript token.}

@defproc[(javascript-derived-token-end [token javascript-derived-token?])
         position?]{
Returns the ending source position for a derived JavaScript token.}

@subsection{JavaScript Derived Tokens}

A derived JavaScript token pairs one raw JavaScript token with a small list of
JavaScript-specific classification tags. This layer is more precise than the
projected consumer-facing categories and is meant for inspection, testing, and
language-aware tools.

The current JavaScript scaffold may attach tags such as:

@itemlist[
 @item{@racket['keyword]}
 @item{@racket['identifier]}
 @item{@racket['declaration-name]}
 @item{@racket['parameter-name]}
 @item{@racket['object-key]}
 @item{@racket['property-name]}
 @item{@racket['method-name]}
 @item{@racket['private-name]}
 @item{@racket['static-keyword-usage]}
 @item{@racket['string-literal]}
 @item{@racket['numeric-literal]}
 @item{@racket['bigint-literal]}
 @item{@racket['numeric-separator-literal]}
 @item{@racket['regex-literal]}
 @item{@racket['template-literal]}
 @item{@racket['template-chunk]}
 @item{@racket['template-interpolation-boundary]}
 @item{@racket['jsx-tag-name]}
 @item{@racket['jsx-closing-tag-name]}
 @item{@racket['jsx-attribute-name]}
 @item{@racket['jsx-text]}
 @item{@racket['jsx-interpolation-boundary]}
 @item{@racket['jsx-fragment-boundary]}
 @item{@racket['comment]}
 @item{@racket['malformed-token]}]

@examples[#:eval javascript-eval
(define derived-tokens
  (javascript-string->derived-tokens
   "class Box { static create() { return this.value; } #secret = 1; }\nfunction wrap(name) { return name; }\nconst item = obj.run();\nconst data = { answer: 42 };\nconst greeting = `a ${name} b`;\nreturn /ab+c/i;"))
(map (lambda (token)
       (list (javascript-derived-token-text token)
             (javascript-derived-token-tags token)
             (javascript-derived-token-has-tag? token 'keyword)
             (javascript-derived-token-has-tag? token 'identifier)
             (javascript-derived-token-has-tag? token 'declaration-name)
             (javascript-derived-token-has-tag? token 'parameter-name)
             (javascript-derived-token-has-tag? token 'object-key)
             (javascript-derived-token-has-tag? token 'property-name)
             (javascript-derived-token-has-tag? token 'method-name)
             (javascript-derived-token-has-tag? token 'private-name)
             (javascript-derived-token-has-tag? token 'static-keyword-usage)
             (javascript-derived-token-has-tag? token 'numeric-literal)
             (javascript-derived-token-has-tag? token 'regex-literal)
             (javascript-derived-token-has-tag? token 'template-literal)
             (javascript-derived-token-has-tag? token 'template-chunk)
             (javascript-derived-token-has-tag? token 'template-interpolation-boundary)))
     derived-tokens)
]}

@examples[#:eval javascript-eval
(define jsx-derived-tokens
  (javascript-string->derived-tokens
   "const el = <Button kind=\"primary\">Hello {name}</Button>;\nconst frag = <>ok</>;"
   #:jsx? #t))
(map (lambda (token)
       (list (javascript-derived-token-text token)
             (javascript-derived-token-tags token)
             (javascript-derived-token-has-tag? token 'jsx-tag-name)
             (javascript-derived-token-has-tag? token 'jsx-closing-tag-name)
             (javascript-derived-token-has-tag? token 'jsx-attribute-name)
             (javascript-derived-token-has-tag? token 'jsx-text)
             (javascript-derived-token-has-tag? token 'jsx-interpolation-boundary)
             (javascript-derived-token-has-tag? token 'jsx-fragment-boundary)))
     jsx-derived-tokens)
]}

@defthing[javascript-profiles immutable-hash?]{
The profile defaults used by the JavaScript lexer.}
