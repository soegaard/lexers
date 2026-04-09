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

The first milestone targets ordinary Racket source, not Scribble-specific
`@` notation.

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

These tags describe reusable lexical and reader roles, not presentation.

## Deferred Work

The first Racket milestone defers:

- Scribble-specific `@` tokenization
- special handling of `racket-nobar-lexer`
- renderer-facing distinctions

If Scribble support is needed later, it should be added as a separate
`lexers/scribble` module built on the Scribble lexer family from
`syntax-color`.
