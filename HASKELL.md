# Haskell

`lexers/haskell` provides a reusable streaming lexer for a practical first slice
of Haskell lexical structure.

The implementation is grounded primarily in Chapter 2 of the Haskell 2010
report, with a small modern-GHC practical layer for pragma comments and a few
commonly seen newer keywords.

The first slice covers:

- whitespace with CRLF preservation
- `--` line comments
- `{- -}` nested comments
- `{-# #-}` pragma comments
- variable and constructor identifiers
- reserved identifiers and reserved operators
- symbolic operators
- numeric literals
- character and string literals
- common delimiters and specials

Projected Haskell categories are:

- `comment`
- `whitespace`
- `keyword`
- `identifier`
- `literal`
- `operator`
- `delimiter`
- `unknown`
- `eof`

The first reusable Haskell-specific derived tags include:

- `haskell-comment`
- `haskell-line-comment`
- `haskell-nested-comment`
- `haskell-pragma`
- `haskell-whitespace`
- `haskell-keyword`
- `haskell-variable-identifier`
- `haskell-constructor-identifier`
- `haskell-variable-operator`
- `haskell-constructor-operator`
- `haskell-string-literal`
- `haskell-char-literal`
- `haskell-numeric-literal`
- `haskell-delimiter`
- `malformed-token`

This module is intended to be source-faithful and streaming. Layout handling is
now profile-sensitive:

- in `coloring`, the projected stream remains source-faithful and does not
  synthesize layout tokens
- in `compiler`, the projected stream inserts virtual layout `{`, `;`, and `}`
  tokens for ordinary `let`, `where`, `do`, and `of` layouts

The derived-token API remains source-faithful and does not synthesize virtual
layout tokens. Full escape-gap support and broader GHC-extension surfaces can
still be added later within the same architecture.
