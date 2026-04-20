#lang scribble/manual

@(require scribble/example
          (for-label racket/base
                     racket/contract/base
                     parser-tools/lex
                     syntax-color/racket-lexer
                     (only-in syntax-color/scribble-lexer
                              make-scribble-inside-lexer)
                     lexers/c
                     lexers/csv
                     lexers/css
                     lexers/html
                     lexers/json
                     lexers/markdown
                     lexers/python
                     lexers/racket
                     lexers/rhombus
                     lexers/shell
                     lexers/scribble
                     lexers/swift
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
 @item{@racketmodname[lexers/csv]}
 @item{@racketmodname[lexers/css]}
 @item{@racketmodname[lexers/html]}
 @item{@racketmodname[lexers/json]}
 @item{@racketmodname[lexers/javascript]}
 @item{@racketmodname[lexers/markdown]}
 @item{@racketmodname[lexers/python]}
 @item{@racketmodname[lexers/racket]}
 @item{@racketmodname[lexers/rhombus]}
 @item{@racketmodname[lexers/shell]}
 @item{@racketmodname[lexers/scribble]}
 @item{@racketmodname[lexers/swift]}
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
@racket[markdown-string->tokens], @racket[make-python-lexer],
@racket[python-string->tokens], @racket[make-racket-lexer],
@racket[racket-string->tokens], @racket[make-rhombus-lexer],
@racket[rhombus-string->tokens], @racket[make-shell-lexer],
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
identifiers.

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
scalars, comments, and block scalar bodies.

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
and known fenced-code languages to the existing C, CSV, HTML, CSS,
JavaScript, JSON, Python, Racket, Scribble, shell, TSV, WAT, and YAML
lexers.

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
 @item{@racket['embedded-csv]}
 @item{@racket['embedded-javascript]}
 @item{@racket['embedded-json]}
 @item{@racket['embedded-python]}
 @item{@racket['embedded-racket]}
 @item{@racket['embedded-shell]}
 @item{@racket['embedded-scribble]}
 @item{@racket['embedded-swift]}
 @item{@racket['embedded-tsv]}
 @item{@racket['embedded-wat]}
 @item{@racket['embedded-yaml]}
 @item{@racket['malformed-token]}]

Delegated raw HTML and recognized fenced-code languages keep their reusable
derived tags and gain Markdown embedding markers such as
@racket['embedded-html], @racket['embedded-csv],
@racket['embedded-javascript], @racket['embedded-json],
@racket['embedded-python], @racket['embedded-racket], @racket['embedded-shell],
@racket['embedded-swift], @racket['embedded-tsv], @racket['embedded-wat], or
@racket['embedded-yaml].

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
 @item{@racket['python-numeric-literal]}
 @item{@racket['python-operator]}
 @item{@racket['python-delimiter]}
 @item{@racket['python-indent]}
 @item{@racket['python-dedent]}
 @item{@racket['python-error]}
 @item{@racket['malformed-token]}]

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
 @item{@racket['shell-variable]}
 @item{@racket['shell-command-substitution]}
 @item{@racket['shell-comment]}
 @item{@racket['shell-option]}
 @item{@racket['shell-numeric-literal]}
 @item{@racket['shell-punctuation]}
 @item{@racket['malformed-token]}]

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
 @item{@racket['swift-numeric-literal]}
 @item{@racket['swift-attribute]}
 @item{@racket['swift-pound-directive]}
 @item{@racket['swift-operator]}
 @item{@racket['swift-delimiter]}
 @item{@racket['swift-error]}
 @item{@racket['malformed-token]}]

Malformed Swift input is handled using the shared profile rules:

@itemlist[
 @item{In the @racket['coloring] profile, malformed input projects as
       @racket['unknown].}
 @item{In the @racket['compiler] profile, malformed input raises a read
       exception.}]

Markdown fenced code blocks labeled @tt{swift} delegate to
@racketmodname[lexers/swift]. Wrapped delegated Markdown tokens preserve
Swift-derived tags and gain @racket['embedded-swift].}

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
       @racket["1"] or @racket["\"hello\""].}
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
