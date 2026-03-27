# JavaScript

This document contains design notes for a JavaScript lexer in the `lexers`
library.

The JavaScript lexer should follow the general principles in `DESIGN.md`:

- the lexer should be reusable across multiple applications
- syntax coloring is the first application, but not the only one
- consumer-specific behavior should be controlled through profiles or explicit
  parameters
- compatibility with existing interfaces may require projecting precise
  internal categories into broader external ones
- the targeted language version or baseline should be documented explicitly

## Version Baseline

JavaScript is an evolving language, so the lexer design must be explicit about
which specification baseline it targets.

The initial JavaScript lexer should begin with a deliberately small baseline
feature set rather than attempting broad ECMAScript coverage immediately.

The first implementation should be described as targeting a small,
well-documented JavaScript subset that is compatible with modern ECMAScript in
the covered areas, while leaving many later features for future work.

Concrete later decisions may choose a named ECMAScript edition or a more
precisely defined baseline, but the initial design should prioritize clarity
over premature completeness.

## Initial Subset

The first JavaScript implementation should include a deliberately small
real-language subset in order to validate the shared lexer architecture.

The initial subset should include:

- whitespace
- line comments
- block comments
- identifiers
- a small keyword set
- string literals
- numeric literals
- delimiters such as parentheses, brackets, braces, comma, semicolon, and dot
- common operators such as `=`, `+`, `-`, `*`, and `/`
- recoverable `unknown` output in tolerant mode
- lexical errors in strict mode for malformed strings and similar malformed
  input

## Deferred Features

The following JavaScript features should be deferred from the first
implementation:

- regular expression literals
- template strings
- Unicode identifier edge cases
- automatic semicolon insertion concerns
- bigint literals
- numeric separators
- JSX
- TypeScript extensions

This keeps the first JavaScript implementation focused on validating the shared
architecture rather than on immediate breadth.

## Raw Tokens and Derived Classifications

As with CSS, the JavaScript design should distinguish between:

- a raw token layer
- a derived classification layer
- a projected consumer-facing token stream

The raw token layer should capture JavaScript-oriented lexical categories as
faithfully as possible within the chosen subset.

The derived layer may add classifications that are useful for consumers, such
as:

- keyword
- identifier
- string literal
- numeric literal
- comment
- malformed token

The projected consumer-facing layer should map those distinctions into the
shared stream vocabulary from `DESIGN.md`.

## Profile Differences

The JavaScript lexer should support at least the same initial profile split as
the CSS lexer.

### Coloring Profile

The coloring profile should:

- retain comments and whitespace when needed
- tolerate incomplete or malformed input
- emit `unknown` when malformed input should remain recoverable
- include source positions by default

### Compiler Profile

The compiler profile should:

- skip trivia when requested by default
- raise lexical errors for malformed input
- include source positions by default

## Open Questions

The following JavaScript-specific questions remain for later design work:

- which keyword set should be included in the initial subset
- how soon regular expression literals should be introduced
- how the lexer should distinguish `/` as division from future regex syntax
- when template strings should become part of the supported baseline
- when the targeted ECMAScript baseline should be named more precisely
