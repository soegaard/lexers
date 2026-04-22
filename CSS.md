# CSS

This document contains design notes for a CSS lexer in the `lexers` library.

The CSS lexer should follow the general principles in `DESIGN.md`:

- the lexer should be reusable across multiple applications
- syntax coloring is the first application, but not the only one
- consumer-specific behavior should be controlled through profiles or explicit
  parameters
- compatibility with existing interfaces may require projecting precise
  internal categories into broader external ones

## Prior Art

An existing CSS lexer is available in `scribble-tools`.

That lexer is useful as prior art, especially because it was written for syntax
coloring and therefore reflects practical experience with real input.

However, it should not determine the new library design by itself.

The new CSS lexer should be informed both by:

- the existing lexer in `scribble-tools`
- the CSS specification

This is important because the existing lexer was designed for syntax coloring
only, while the new lexer is intended to be reusable across multiple
applications.

The current prior-art implementation appears in
`scribble-tools/cli-css-highlight.rkt`.

That implementation is useful because it shows a practical coloring-oriented
tokenization strategy:

- it tokenizes a complete string eagerly
- it returns coarse token categories such as `comment`, `keyword`, `value`,
  `name`, `punct`, and `plain`
- it tracks broad context such as selector mode versus declaration mode
- it preserves whitespace as `plain`
- it treats comments as first-class output tokens
- it layers color-specific analysis, such as color swatches and custom-property
  color resolution, on top of the token stream

These choices make good sense for a highlighter, but they also show where the
new lexer will need a cleaner separation between lexical recognition and
consumer-specific projection.

## Design Direction

The CSS lexer should aim for a richer internal token vocabulary than a pure
syntax-coloring lexer normally needs.

For example, it may be useful to distinguish:

- identifiers
- at-keywords
- hash tokens
- strings
- numbers
- percentages
- dimensions
- delimiters
- comments
- whitespace
- color literals

Some of these distinctions may be useful only to certain consumers, but the
lexer should still be able to recognize them internally when that improves
precision.

Compared to the current `scribble-tools` lexer, the new design should avoid
collapsing too many distinctions into the coarse categories `keyword`, `value`,
`name`, and `plain` at the primary lexer boundary.

For example:

- selector-oriented identifiers should not automatically be classified the same
  way as property names
- hash-prefixed forms should be distinguished more carefully than "color or
  punctuation"
- numeric forms such as numbers, percentages, and dimensions should remain
  distinguishable internally
- color literals should be representable as a more precise category when that
  improves downstream use

The CSS Syntax specification provides a strong starting point for this internal
vocabulary. Its tokenization model is already phrased as a token stream, and it
explicitly allows tokenization to be performed on demand, one token at a time.
That aligns well with the library's port-based lexer interface.

The spec-grounded internal token categories should therefore begin from the CSS
Syntax token model, including:

- ident-token
- function-token
- at-keyword-token
- hash-token
- string-token
- bad-string-token
- url-token
- bad-url-token
- delim-token
- number-token
- percentage-token
- dimension-token
- whitespace-token
- CDO-token
- CDC-token
- colon-token
- semicolon-token
- comma-token
- bracket, parenthesis, and brace tokens

The specification also gives some tokens additional structure that should be
preserved internally when useful:

- hash tokens have a type flag such as `id` versus `unrestricted`
- number and dimension tokens have a numeric type such as `integer` versus
  `number`
- dimension tokens carry a unit
- tokens may need to preserve enough source representation information for
  downstream consumers that care about exact spelling

This spec-grounded vocabulary should be the baseline for the internal CSS lexer
design, even when some consumer-facing interfaces choose to collapse several of
these distinctions into broader categories.

## Raw Tokens and Derived Classifications

The CSS design should distinguish between a raw token layer and a derived
classification layer.

The public API should expose both layers:

- a raw-token API suitable for parser consumers
- a derived-token API suitable for language-aware tools that want both the raw
  token and additional CSS-specific classifications

### Raw Token Layer

The raw token layer should stay close to CSS Syntax Level 3.

These token categories should generally survive unchanged at the raw level:

- ident-token
- function-token
- at-keyword-token
- hash-token
- string-token
- bad-string-token
- url-token
- bad-url-token
- delim-token
- number-token
- percentage-token
- dimension-token
- whitespace-token
- CDO-token
- CDC-token
- punctuation, bracket, parenthesis, and brace tokens

This layer should be spec-grounded and stable. It provides the cleanest basis
for correctness, for later parser-oriented use, and for comparison against the
CSS specification.

The public API for this layer should be:

- `make-css-raw-lexer`
- `css-string->raw-tokens`
- `css-raw-token?`
- `css-raw-token-kind`
- `css-raw-token-text`
- `css-raw-token-start`
- `css-raw-token-end`

### Derived Classification Layer

On top of the raw token layer, the lexer design may support derived
classifications that are more useful to consumers.

Examples include:

- color literal
- color function
- gradient function
- custom property name
- property name candidate
- known CSS property
- known CSS keyword
- unit class such as length, angle, time, or percentage-like quantity

These classifications should not replace the raw token layer. Instead, they
should enrich it.

The derived-token API should also expose the wrapped raw token directly, so
consumers that start from derived tokens can still inspect spec-grounded raw
token kinds without guessing from projected categories.

For example:

- a `hash-token` may also be classified as a color literal
- an `ident-token` may also be classified as a known color keyword or a custom
  property name
- a `function-token` may also be classified as a color function or gradient
  function
- a `dimension-token` may also carry a derived unit class

This two-layer design allows the lexer to remain faithful to the CSS Syntax
specification while still supporting richer downstream uses such as syntax
coloring, semantic highlighting, documentation tools, and future analyzers.

## Worked Mapping Examples

The following examples illustrate the intended relationship between raw tokens,
derived classifications, and a coloring-oriented projection.

| Source text | Raw token | Possible derived classification | Coloring projection |
| --- | --- | --- | --- |
| `#ff00aa` | `hash-token` | color literal | literal |
| `rgb(10 20 30 / 50%)` | `function-token` + punctuation + numeric tokens | color function | literal |
| `linear-gradient(...)` | `function-token` + punctuation + inner tokens | gradient function | literal |
| `--brand-color` | `ident-token` | custom property name | identifier |
| `color` | `ident-token` | property name candidate or known CSS property | identifier |
| `12px` | `dimension-token` | dimension with length unit | literal |
| `50%` | `percentage-token` | percentage literal | literal |
| `@media` | `at-keyword-token` | at-rule keyword | keyword |
| `/* note */` | comment handling outside the CSS Syntax token stream | comment | comment |
| whitespace | `whitespace-token` | trivia | whitespace |
| malformed string | `bad-string-token` | malformed token | unknown |
| malformed url | `bad-url-token` | malformed token | unknown |

These examples are illustrative rather than exhaustive, but they show the
intended design pattern:

- raw tokens stay close to the specification
- derived classifications add CSS-aware meaning
- coloring projections may collapse several precise categories into broader
  classes used by highlighters

## Comments and Trivia

CSS Syntax Level 3 does not treat comments as ordinary tokens in the same way
it treats tokens such as `ident-token` or `dimension-token`.

For this library, however, comments should still be treated as first-class
reusable lexer output when trivia retention is enabled. This is important for
syntax coloring and may also be useful for documentation-oriented or
presentation-oriented tools.

The design should therefore distinguish between:

- the spec-grounded CSS token model
- the library's reusable token stream model

The reusable token stream model may include explicit comment items in addition
to the spec-grounded CSS tokens.

Comments should be treated as trivia, but they should remain distinct from
whitespace. Profiles or options should determine whether comments are:

- emitted
- skipped
- normalized

This lets the library stay close to CSS Syntax while still supporting the needs
of consumers that care about comments as visible source text.

## Consumer Projections

A CSS lexer for syntax coloring may not need to expose every precise internal
distinction.

Instead, it may project internal categories into broader external categories
expected by an existing color-lexer interface.

For example:

- several literal forms may be presented as a general literal category
- a precise internal category such as color literal may be projected to a
  broader `literal` category
- several punctuation-like spec tokens may be projected to a more general
  `delimiter` or `operator` category
- trivia such as whitespace and comments may be retained for coloring-oriented
  consumers

This projection step should be treated as part of the consumer-facing profile
or compatibility layer, not as a reason to make the core lexer less precise.

The existing `scribble-tools` lexer can be viewed as an example of such a
projection layer. Its coarse categories are likely closer to the output needed
by a syntax-coloring tool than to the full internal vocabulary that a reusable
CSS lexer should maintain.

## Profile Differences

The CSS lexer should support at least the following profile-level differences.

### Coloring Profile

The coloring profile should:

- retain comments and whitespace when needed
- tolerate incomplete or malformed syntax
- return an `unknown` token when useful
- project precise internal categories into broader coloring categories when
  compatibility requires it

### Strict Profile

The strict profile should:

- raise errors on lexically invalid input
- allow trivia to be skipped when not needed
- preserve precise token distinctions that are useful to parser-oriented or
  analysis-oriented consumers

The spec's malformed-token distinctions are especially relevant here. A
tolerant profile may choose to surface malformed forms such as bad strings or
bad URLs as recoverable output categories, while a strict profile may treat the
same situations as lexical errors. This supports consumer-specific behavior
without requiring different lexical definitions for CSS.

## Open Questions

The following CSS-specific questions remain for later design work:

- which token distinctions should always be preserved internally
- which precise categories should be projected into broader coloring categories
- which malformed CSS forms should produce `unknown` tokens in tolerant mode
- how closely the lexer should follow the CSS specification's own tokenization
  model
- which parts of the current `scribble-tools` lexer should be kept as practical
  prior art, and which parts should be treated as coloring-specific projection

## References

The following specifications are the primary external references for the CSS
lexer design:

- [CSS Snapshot 2026](https://www.w3.org/TR/css-2026/)
- [CSS Syntax Module Level 3](https://www.w3.org/TR/css-syntax-3/)
- [CSS Color Module Level 4](https://www.w3.org/TR/css-color-4/)

The snapshot provides the current published overview, while CSS Syntax Level 3
is the main reference for tokenization and parsing-oriented lexical behavior.
CSS Color Level 4 is especially relevant when deciding how precise the lexer
should be about color literals and color-related functions.
