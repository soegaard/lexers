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

This module is intended to be source-faithful and streaming. More dialect-
specific distinctions such as compiler directives inside comments, mode-aware
nested-comment differences, and fuller numeric/string corners can be added
later within the same architecture.
