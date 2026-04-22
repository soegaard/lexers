# Rust

`lexers/rust` provides a reusable streaming lexer for a practical first slice
of Rust lexical structure.

The implementation is grounded primarily in the Rust Reference sections on
identifiers, comments, tokens, lifetimes, and literals. The first version aims
for a useful real-language subset, not complete specification coverage.

The first slice covers:

- line and nested block comments, including doc comments
- whitespace with CRLF preservation
- identifiers and raw identifiers such as `r#type`
- keywords
- lifetimes such as `'a` and `'static`
- string, raw string, byte string, char, and byte literals
- numeric literals with suffixes
- common Rust punctuation and delimiters

The current implementation also validates the ordinary escape structure of
string, byte string, char, byte, and C string literals. Invalid escape
sequences and malformed multi-character char literals remain source-faithful
but are tagged as malformed.

Projected Rust categories are:

- `comment`
- `whitespace`
- `keyword`
- `identifier`
- `literal`
- `operator`
- `delimiter`
- `unknown`
- `eof`

The first reusable Rust-specific derived tags include:

- `rust-comment`
- `rust-doc-comment`
- `rust-whitespace`
- `rust-keyword`
- `rust-identifier`
- `rust-raw-identifier`
- `rust-lifetime`
- `rust-string-literal`
- `rust-raw-string-literal`
- `rust-char-literal`
- `rust-byte-literal`
- `rust-byte-string-literal`
- `rust-c-string-literal`
- `rust-numeric-literal`
- `rust-punctuation`
- `rust-delimiter`
- `malformed-token`

This module is intended to be source-faithful and streaming. More complete
Unicode identifier handling and the remaining reserved-token corners can be
added later within the same architecture.
