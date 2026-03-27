#lang scribble/manual

@(require (for-label racket/base
                     parser-tools/lex
                     lexers/css
                     lexers/javascript))

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

@defproc[(make-css-lexer [#:profile profile symbol? 'coloring]
                         [#:trivia trivia any/c 'profile-default]
                         [#:source-positions source-positions any/c 'profile-default])
         procedure?]{
Constructs a port-based CSS lexer that returns the projected token stream.}

@defproc[(css-string->tokens [source string?]
                             [#:profile profile symbol? 'coloring]
                             [#:trivia trivia any/c 'profile-default]
                             [#:source-positions source-positions any/c 'profile-default])
         list?]{
Tokenizes an entire CSS string using the projected token API.}

@defproc[(make-css-derived-lexer)
         procedure?]{
Constructs a port-based CSS lexer that returns derived CSS token values.}

@defproc[(css-string->derived-tokens [source string?])
         list?]{
Tokenizes an entire CSS string into derived CSS token values.}

@defthing[css-profiles hash?]{
The profile defaults used by the CSS lexer.}

@section{JavaScript}

@defmodule[lexers/javascript]

@defproc[(make-javascript-lexer [#:profile profile symbol? 'coloring]
                                [#:trivia trivia any/c 'profile-default]
                                [#:source-positions source-positions any/c 'profile-default])
         procedure?]{
Constructs a port-based JavaScript lexer that returns the projected token
stream.}

@defproc[(javascript-string->tokens [source string?]
                                    [#:profile profile symbol? 'coloring]
                                    [#:trivia trivia any/c 'profile-default]
                                    [#:source-positions source-positions any/c 'profile-default])
         list?]{
Tokenizes an entire JavaScript string using the projected token API.}

@defproc[(make-javascript-derived-lexer)
         procedure?]{
Constructs a port-based JavaScript lexer that returns derived JavaScript token
values.}

@defproc[(javascript-string->derived-tokens [source string?])
         list?]{
Tokenizes an entire JavaScript string into derived JavaScript token values.}

@defthing[javascript-profiles hash?]{
The profile defaults used by the JavaScript lexer.}
