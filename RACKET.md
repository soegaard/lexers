# Racket

This document contains design notes for a Racket lexer in the `lexers`
library.

## Scope

The first implementation of `lexers/racket` is adapter-backed rather than a
new handwritten lexer.

It uses the Racket lexer from `syntax-color/racket-lexer` as the underlying
token engine and adapts that output into the public `lexers` API shape:

- a projected token API
- a derived-token API

The first milestone targets ordinary Racket source and also recognizes
`#lang at-exp ...` sources, where `@` switches from Racket mode into
Scribble-style text mode.

## Why An Adapter

The syntax-color Racket lexer already provides:

- matched text
- token classes such as comment, string, constant, symbol, and parenthesis
- source offsets
- datum/open/close/continue status
- and, through the `*` protocol, information about code commented out with
  `#;`

That makes it a good raw foundation for `lexers/racket`.

## Derived Tags

The first reusable Racket-specific derived tags include:

- `racket-comment`
- `racket-sexp-comment`
- `racket-whitespace`
- `racket-constant`
- `racket-string`
- `racket-symbol`
- `racket-parenthesis`
- `racket-hash-colon-keyword`
- `racket-no-color`
- `racket-other`
- `racket-error`
- `racket-commented-out`
- `racket-datum`
- `racket-open`
- `racket-close`
- `racket-continue`

When a source starts with `#lang at-exp`, the adapter uses the Scribble lexer
family from `syntax-color` in Racket mode. In that case, derived Racket tokens
may also carry reusable Scribble structure tags such as:

- `scribble-text`
- `scribble-command-char`
- `scribble-command`
- `scribble-body-delimiter`
- `scribble-optional-delimiter`
- `scribble-racket-escape`

These tags describe reusable lexical and reader roles, not presentation.

The derived layer may also attach a small heuristic layer for identifiers that
are usually special forms in ordinary Racket code. These tags are deliberately
not claims about expanded meaning, since macro expansion can redefine names
such as `define` or `if`.

The first heuristic tags are:

- `racket-usual-special-form`
- `racket-definition-form`
- `racket-binding-form`
- `racket-conditional-form`

These tags are only added when the token is already a symbol-like datum token
and its source text matches one of the configured “usual” form names.

For example, the token text `define` may still be tagged as a usual special
form even in a program where `define` has been rebound, because the lexer does
not know expansion-time bindings.

## Deferred Work

The first Racket milestone still defers:

- special handling of `racket-nobar-lexer`
- renderer-facing distinctions

Standalone Scribble documents still live in the separate `lexers/scribble`
module built on the Scribble lexer family from `syntax-color`.
