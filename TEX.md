# TeX

`lexers/tex` provides a reusable streaming lexer for a practical first slice of
TeX source.

The implementation is grounded primarily in TeX's tokenization model as
described in *The TeXbook* and *TeX by Topic*, but the first version is
deliberately static. It does not attempt to model dynamically changing
category codes.

The first slice covers:

- `%` line comments
- whitespace, including CRLF preservation
- control words such as `\section`
- control symbols such as `\%`
- group delimiters `{` and `}`
- optional delimiters `[` and `]`
- math delimiters such as `$`, `$$`, `\(`, `\)`, `\[`, and `\]`
- parameter markers such as `#1` and `##`
- plain text runs
- malformed trailing control escapes

Projected TeX categories are:

- `comment`
- `whitespace`
- `identifier`
- `literal`
- `delimiter`
- `unknown`
- `eof`

The first reusable TeX-specific derived tags include:

- `tex-comment`
- `tex-whitespace`
- `tex-control-word`
- `tex-control-symbol`
- `tex-parameter`
- `tex-text`
- `tex-math-shift`
- `tex-inline-math-shift`
- `tex-display-math-shift`
- `tex-group-delimiter`
- `tex-optional-delimiter`
- `tex-special-character`
- `tex-alignment-tab`
- `tex-subscript-mark`
- `tex-superscript-mark`
- `tex-unbreakable-space`
- `malformed-token`

This module is intended to be source-faithful and streaming. More complete
TeX-specific behavior, especially catcode-sensitive tokenization, can be added
later without changing the basic architecture. The derived layer now also
distinguishes display-vs-inline math shifts and the common special characters
`&`, `_`, `^`, and `~` instead of exposing them only through the generic
`tex-special-character` tag.
