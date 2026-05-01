# Mathematica Lexer

`lexers/mathematica` targets plain Wolfram Language linear source first:

- `.wl`
- `.wls`

The implementation is grounded primarily in the Wolfram syntax documentation,
especially the linear input forms documented under Input Syntax and Operator
Input Forms. Existing Wolfram parser implementations are useful secondary prior
art, and real local Wolfram-language files are used for corpus validation.

## First slice

The first handwritten slice covers:

- whitespace with CRLF fidelity
- first-line `#!` script comments for `.wls`-style sources
- nested `(* ... *)` comments
- symbols and context-qualified names
- strings
- practical numeric literals including decimal, base-form, exponent, and
  backquote precision fragments
- character-entry forms such as `\[Alpha]`, `\:03B1`, `\.7A`, and `\|01F600`
- group delimiters:
  - `()`
  - `[]`
  - `{}`
- part delimiters:
  - `[[`
  - `]]`
- association delimiters:
  - `<|`
  - `|>`
- pattern markers:
  - `_`
  - `__`
  - `___`
  - `_.`
- slots:
  - `#`
  - `##`
  - `#name`
- named-character long-name forms like `\[Alpha]`
- a practical operator surface for rules, replacements, pattern tests, and
  common symbolic operators, including composition and string-pattern forms

The derived layer also preserves a few especially reusable Wolfram roles:

- package forms like `BeginPackage`, `Begin`, `EndPackage`, and `End`
- scoping forms like `Module`, `Block`, `With`, and `Function`
- definition forms like `Set`, `SetDelayed`, `TagSet`, and `TagSetDelayed`
- assignment operators like `=` and `:=`
- rewrite operators like `->`, `:>`, `/.`, and `//.`
- pattern-condition operators like `/;`
- composition operators like `@*` and `/*`
- string-pattern operators like `~~` and `~~>`
- function-arrow operators like `|->`
- numeric subcategories for integer, real, base-form, precision, and exponent
  literals

## Deliberate boundaries

The first version intentionally does not try to match a full parser-grade
token-kind taxonomy or notebook box syntax. The goal is a reusable, streaming,
source-faithful lexer for ordinary textual Wolfram Language source.

The ambiguous `.m` extension is also deferred at the dispatch layer. The lexer
itself can tokenize Wolfram-like `.m` source, but automatic extension mapping
should be decided by the consumer.
