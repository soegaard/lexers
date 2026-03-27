#lang scribble/manual

@(require scribble/example
          (for-label racket/base
                     racket/contract/base
                     parser-tools/lex
                     lexers/categories
                     lexers/css
                     lexers/token
                     lexers/javascript))

@(define css-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/css))
     the-eval))

@(define javascript-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parser-tools/lex
                         lexers/token
                         lexers/javascript))
     the-eval))

@title{Lexers}

This manual documents the public APIs in the @tt{lexers} packages.

The library currently provides reusable lexers for multiple applications.
Syntax coloring is the first intended application, but the lexer APIs are also
designed to support other consumers.

@table-of-contents[]

@section{Overview}

The public language modules currently available are:

@itemlist[
 @item{@racketmodname[lexers/categories]}
 @item{@racketmodname[lexers/token]}
 @item{@racketmodname[lexers/css]}
 @item{@racketmodname[lexers/javascript]}]

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
stop when the result is an end-of-file token.

@subsection{Category Constants}

The module @racketmodname[lexers/categories] exports the stable projected
category names so consumers do not need to hardcode symbols.

@defmodule[lexers/categories]

@defthing[lexer-category-whitespace symbol?]{The public category for whitespace tokens.}
@defthing[lexer-category-comment symbol?]{The public category for comment tokens.}
@defthing[lexer-category-identifier symbol?]{The public category for identifier tokens.}
@defthing[lexer-category-keyword symbol?]{The public category for keyword tokens.}
@defthing[lexer-category-literal symbol?]{The public category for literal tokens.}
@defthing[lexer-category-operator symbol?]{The public category for operator tokens.}
@defthing[lexer-category-delimiter symbol?]{The public category for delimiter tokens.}
@defthing[lexer-category-unknown symbol?]{The public category for recoverable unknown tokens.}

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

Both the CSS and JavaScript projected APIs currently support the same profile
names:

@itemlist[
 @item{@racket['coloring]}
 @item{@racket['compiler]}]

The current defaults are:

@tabular[#:sep @hspace[2]
 (list (list @bold{Profile} @bold{Trivia} @bold{Source Positions} @bold{Malformed Input})
       (list @racket['coloring] @racket['keep] @racket[#t] "emit unknown tokens")
       (list @racket['compiler] @racket['skip] @racket[#t] "raise an exception"))]

For the keyword arguments accepted by @racket[make-css-lexer],
@racket[css-string->tokens], @racket[make-javascript-lexer], and
@racket[javascript-string->tokens]:

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

@defproc[(css-derived-token-has-tag? [token css-derived-token?]
                                     [tag symbol?])
         boolean?]{
Determines whether a derived CSS token carries a given classification tag.}

@subsection{CSS Derived Tokens}

A derived CSS token pairs one raw CSS token with a small list of
CSS-specific classification tags. This layer is more precise than the projected
consumer-facing categories and is meant for inspection, testing, and
language-aware tools.

The current CSS scaffold may attach tags such as:

@itemlist[
 @item{@racket['color-literal]}
 @item{@racket['color-function]}
 @item{@racket['gradient-function]}
 @item{@racket['custom-property-name]}
 @item{@racket['property-name-candidate]}
 @item{@racket['length-dimension]}
 @item{@racket['malformed-token]}]

@examples[#:eval css-eval
(define derived-tokens
  (css-string->derived-tokens "#fff rgb( --brand-color 12px"))
(map (lambda (token)
       (list (css-derived-token-has-tag? token 'color-literal)
             (css-derived-token-has-tag? token 'color-function)
             (css-derived-token-has-tag? token 'custom-property-name)
             (css-derived-token-has-tag? token 'length-dimension)))
     derived-tokens)
]}

@defthing[css-profiles immutable-hash?]{
The profile defaults used by the CSS lexer.}

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
                                [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
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
                                    [#:source-positions source-positions (or/c 'profile-default boolean?) 'profile-default])
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

@defproc[(make-javascript-derived-lexer)
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

@defproc[(javascript-string->derived-tokens [source string?])
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

@defproc[(javascript-derived-token-has-tag? [token javascript-derived-token?]
                                            [tag symbol?])
         boolean?]{
Determines whether a derived JavaScript token carries a given classification
tag.}

@subsection{JavaScript Derived Tokens}

A derived JavaScript token pairs one raw JavaScript token with a small list of
JavaScript-specific classification tags. This layer is more precise than the
projected consumer-facing categories and is meant for inspection, testing, and
language-aware tools.

The current JavaScript scaffold may attach tags such as:

@itemlist[
 @item{@racket['keyword]}
 @item{@racket['identifier]}
 @item{@racket['string-literal]}
 @item{@racket['numeric-literal]}
 @item{@racket['comment]}
 @item{@racket['malformed-token]}]

@examples[#:eval javascript-eval
(define derived-tokens
  (javascript-string->derived-tokens "const name = 1"))
(map (lambda (token)
       (list (javascript-derived-token-has-tag? token 'keyword)
             (javascript-derived-token-has-tag? token 'identifier)
             (javascript-derived-token-has-tag? token 'numeric-literal)))
     derived-tokens)
]}

@defthing[javascript-profiles immutable-hash?]{
The profile defaults used by the JavaScript lexer.}
