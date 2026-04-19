# Rhombus

`lexers/rhombus` is adapter-backed.

The implementation uses `rhombus/private/syntax-color` as the raw lexer engine.
That layer already builds on Shrubbery tokenization and adds Rhombus-specific
semantic guesses for keywords and builtins.

The public `lexers/rhombus` API follows the same split as the existing
adapter-backed lexers:

- a projected token API for general consumers
- a derived-token API for richer language-specific inspection

Rhombus support is optional at runtime.

The `lexers/rhombus` module is meant to load on older Racket installations, but
actually lexing Rhombus source requires `rhombus-lib`, which currently implies
`base >= 8.14`.

The projected layer keeps the shared `lexers` categories small:

- whitespace
- comment
- identifier
- keyword
- literal
- operator
- delimiter
- unknown

The derived layer preserves reusable Rhombus distinctions such as:

- `rhombus-keyword`
- `rhombus-builtin`
- `rhombus-identifier`
- `rhombus-literal`
- `rhombus-operator`
- `rhombus-block-operator`
- `rhombus-comma-operator`
- `rhombus-opener`
- `rhombus-closer`
- `rhombus-at`

As with the other adapter-backed lexers, source fidelity comes from recovering
token text from the exact consumed source slice instead of trusting any
normalized token text returned by the underlying syntax-color layer.
