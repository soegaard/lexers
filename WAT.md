# WAT

This document contains design notes for a WebAssembly text-format lexer in the
`lexers` library.

## Scope

The public module is `lexers/wat`.

It targets WebAssembly text format (WAT), not binary `.wasm` files.

The first implementation is a handwritten lexer that follows the shared
`lexers` architecture:

- a projected token API
- a derived-token API

## Baseline Coverage

The first reusable scope includes:

- parentheses
- whitespace
- line comments starting with `;;`
- nested block comments using `(; ... ;)`
- strings with escapes
- numeric literals, including hex and `nan`/`inf` forms
- form keywords such as `module`, `func`, `param`, and `result`
- type keywords such as `i32`, `i64`, `f32`, and `f64`
- instruction keywords such as `local.get`, `global.set`, and `i32.add`
- `$`-prefixed identifiers and remaining word-like names

## Derived Tags

The first reusable WAT-specific derived tags include:

- `wat-form`
- `wat-type`
- `wat-instruction`
- `wat-identifier`
- `wat-string-literal`
- `wat-numeric-literal`
- `comment`
- `whitespace`
- `malformed-token`

These tags describe reusable WAT syntax roles, not renderer behavior.

## Nested Comments

The lexer supports nested block comments in the same style as the prior-art WAT
tokenizer in `scribble-tools`.

Unterminated strings and block comments are treated as malformed input:

- `unknown` in the `'coloring` profile
- raised read errors in the `'compiler` profile

## Markdown Delegation

Markdown fenced code blocks labeled `wat` or `wasm` delegate to `lexers/wat`.

Wrapped delegated Markdown tokens preserve WAT-derived tags and gain the
embedding tag `embedded-wat`.
