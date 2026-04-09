# Scribble

This document contains design notes for a Scribble lexer in the `lexers`
library.

## Scope

The first implementation of `lexers/scribble` is adapter-backed rather than a
new handwritten lexer.

It uses the Scribble lexer family from `syntax-color/scribble-lexer` as the
underlying token engine and adapts that output into the public `lexers` API
shape:

- a projected token API
- a derived-token API

The first milestone targets ordinary Scribble documents with the normal `@`
command character.

## Why A Separate Module

Scribble is intentionally separate from `lexers/racket`.

Although Scribble can contain Racket escapes, its practical language surface
is different:

- it defaults to text mode rather than ordinary Racket mode
- it has `@` command structure
- it has body and optional delimiters with document-oriented meaning

So `lexers/scribble` is a separate adapter-backed module rather than a mode of
`lexers/racket`.

## Adapter Basis

The first implementation uses `make-scribble-inside-lexer`, which starts in
inside/text mode. This matches current `scribble-tools` practice and fits the
most common use of Scribble documents.

The public API does not expose `#:command-char` in the first milestone. It
assumes the ordinary `@` command character.

## Derived Tags

The first reusable Scribble-specific derived tags include:

- `scribble-comment`
- `scribble-whitespace`
- `scribble-text`
- `scribble-string`
- `scribble-constant`
- `scribble-symbol`
- `scribble-parenthesis`
- `scribble-other`
- `scribble-error`
- `scribble-command`
- `scribble-command-char`
- `scribble-body-delimiter`
- `scribble-optional-delimiter`
- `scribble-racket-escape`

These tags describe reusable lexical and structural roles, not presentation.

The first structure layer is intentionally parser-lite:

- `@` gets `scribble-command-char`
- the next symbol-like token after `@` gets `scribble-command`
- `{` and `}` get `scribble-body-delimiter`
- `[` and `]` get `scribble-optional-delimiter`
- tokens inside bracket-delimited `@command[...]` escapes may also get
  `scribble-racket-escape`

The lexer does not try to infer higher-level document semantics such as
whether a command is a title command, a block command, or an item command.

## Deferred Work

The first Scribble milestone defers:

- command-character customization
- a public starting-mode option
- higher-level document command semantics
- presentation-specific distinctions
