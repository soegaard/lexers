# JSON

This document contains design notes for a reusable JSON lexer in `lexers`.

## Scope

The public module is `lexers/json`.

The first implementation is a small handwritten JSON lexer that follows the
shared `lexers` architecture:

- a projected token API
- a derived-token API

It is intentionally parser-lite, but it keeps enough structure to distinguish
object keys from ordinary string literals.

## Baseline Coverage

The first reusable scope includes:

- whitespace
- object and array delimiters
- commas and colons
- string literals with JSON escapes
- numeric literals
- the JSON keywords `true`, `false`, and `null`
- malformed strings, numbers, and keywords

This lexer targets ordinary JSON, not JSON5 or JavaScript object literals.

## Derived Tags

The first reusable JSON-specific derived tags include:

- `json-whitespace`
- `json-object-start`
- `json-object-end`
- `json-array-start`
- `json-array-end`
- `json-comma`
- `json-colon`
- `json-string`
- `json-object-key`
- `json-number`
- `json-true`
- `json-false`
- `json-null`
- `json-keyword`
- `json-error`
- `malformed-token`

These tags describe reusable JSON syntax roles, not renderer behavior.

## Projected Categories

The projected JSON lexer maps into the shared stream categories:

- `whitespace`
- `delimiter`
- `operator`
- `identifier`
- `literal`
- `unknown`

Object keys project as `identifier`, while ordinary strings, numbers, and the
JSON keywords project as `literal`.

## Markdown Delegation

`lexers/markdown` delegates fenced code blocks labeled `json` to
`lexers/json`.

Wrapped delegated Markdown tokens preserve JSON-derived tags and gain the
embedding tag `embedded-json`.
