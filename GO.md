# Go

`lexers/go` provides a reusable streaming lexer for a practical first slice of
Go lexical structure.

The implementation is grounded primarily in the official Go specification,
especially the lexical sections on comments, tokens, identifiers, keywords,
operators and punctuation, and literals.

The first slice covers:

- line and general comments
- whitespace with CRLF preservation
- identifiers
- keywords
- interpreted and raw string literals
- rune literals
- numeric and imaginary literals
- operators and delimiters

Projected Go categories are:

- `comment`
- `whitespace`
- `keyword`
- `identifier`
- `literal`
- `operator`
- `delimiter`
- `unknown`
- `eof`

The first reusable Go-specific derived tags include:

- `go-comment`
- `go-line-comment`
- `go-general-comment`
- `go-whitespace`
- `go-keyword`
- `go-identifier`
- `go-string-literal`
- `go-raw-string-literal`
- `go-rune-literal`
- `go-numeric-literal`
- `go-imaginary-literal`
- `go-operator`
- `go-delimiter`
- `malformed-token`

This module is intended to be source-faithful and streaming. Semicolon
insertion is intentionally deferred for now, because the first lexer slice is
focused on exact-source tokenization instead of synthesized parser-facing
tokens.
