# Pascal

`lexers/pascal` provides a reusable streaming lexer for a practical first slice
of Free Pascal / Object Pascal lexical structure.

The implementation is grounded primarily in Chapter 1 of the Free Pascal
reference, especially the token sections for symbols, comments, reserved
words, identifiers, numbers, and character strings.

The first slice covers:

- whitespace with CRLF preservation
- `{ ... }`, `(* ... *)`, and `// ...` comments
- nested brace and star comments
- identifiers and escaped reserved-word identifiers such as `&do`
- common Pascal and Object Pascal reserved words
- decimal, hexadecimal, octal, binary, and real literals
- single-quoted strings with doubled-quote escapes
- `#<digits>` control-string fragments
- common Pascal operators, separators, and delimiters
- compiler directives inside `{...}` and `(*...*)` comments such as
  `{$mode objfpc}` and `(*$ifdef DEBUG*)`

Projected Pascal categories are:

- `comment`
- `whitespace`
- `keyword`
- `identifier`
- `literal`
- `operator`
- `delimiter`
- `unknown`
- `eof`

The first reusable Pascal-specific derived tags include:

- `pascal-comment`
- `pascal-compiler-directive`
- `pascal-whitespace`
- `pascal-keyword`
- `pascal-identifier`
- `pascal-escaped-identifier`
- `pascal-string-literal`
- `pascal-control-string`
- `pascal-numeric-literal`
- `pascal-operator`
- `pascal-delimiter`
- `malformed-token`

This module is intended to be source-faithful and streaming. Mode-aware nested-
comment differences and remaining numeric/string corners can still be added
later within the same architecture.
