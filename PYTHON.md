# Python

This document contains design notes for a reusable Python lexer in `lexers`.

## Scope

The public module is `lexers/python`.

The first implementation is a handwritten streaming lexer that follows the
shared `lexers` architecture:

- a projected token API
- a derived-token API

The lexer is grounded primarily in Python's lexical-analysis rules and token
inventory, not the full parser grammar.

## Baseline Coverage

The first reusable scope includes:

- whitespace and comments
- physical newlines and line continuations
- indentation-sensitive line starts
- identifiers, keywords, and soft keywords
- numeric literals
- ordinary and triple-quoted strings
- operators and delimiters
- malformed tokens

This is intentionally a lexer, not a full Python parser. It keeps enough state
to distinguish logical-line endings from non-significant newlines and to track
indentation changes.

## Derived Tags

The first reusable Python-specific derived tags include:

- `python-comment`
- `python-whitespace`
- `python-newline`
- `python-nl`
- `python-line-join`
- `python-keyword`
- `python-soft-keyword`
- `python-identifier`
- `python-string-literal`
- `python-bytes-literal`
- `python-numeric-literal`
- `python-operator`
- `python-delimiter`
- `python-indent`
- `python-dedent`
- `python-error`
- `malformed-token`

These tags describe reusable syntax roles, not renderer behavior.

## Projected Categories

The projected Python lexer maps into the shared stream categories:

- `comment`
- `whitespace`
- `keyword`
- `identifier`
- `literal`
- `operator`
- `delimiter`
- `unknown`

Soft keywords currently project as `keyword`, while the derived layer preserves
the more specific `python-soft-keyword` tag.

## Markdown Delegation

`lexers/markdown` delegates fenced code blocks labeled `python` or `py` to
`lexers/python`.

Wrapped delegated Markdown tokens preserve Python-derived tags and gain the
embedding tag `embedded-python`.
