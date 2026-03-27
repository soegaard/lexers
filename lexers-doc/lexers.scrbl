#lang scribble/manual

@(require scribble/example
          (for-label racket/base
                     racket/contract/base
                     parser-tools/lex
                     lexers/css
                     lexers/javascript))

@(define css-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         lexers/css))
     the-eval))

@(define javascript-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
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
 @item{@racketmodname[lexers/css]}
 @item{@racketmodname[lexers/javascript]}]

Each language module currently exposes two related kinds of API:

@itemlist[
 @item{A projected token API intended for general consumers such as syntax
       coloring.}
 @item{A derived-token API intended for richer language-specific inspection and
       testing.}]

The current profile split is:

@itemlist[
 @item{@racket['coloring] --- keeps trivia, emits @racket['unknown] for
       recoverable malformed input, and includes source positions by default.}
 @item{@racket['compiler] --- skips trivia by default, raises on malformed
       input, and includes source positions by default.}]

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

@defthing[javascript-profiles immutable-hash?]{
The profile defaults used by the JavaScript lexer.}
