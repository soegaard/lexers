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
- `tex-paragraph-command`
- `tex-parameter`
- `tex-parameter-reference`
- `tex-parameter-escape`
- `tex-parameter-marker`
- `tex-text`
- `tex-math-shift`
- `tex-inline-math-shift`
- `tex-display-math-shift`
- `tex-group-delimiter`
- `tex-open-group-delimiter`
- `tex-close-group-delimiter`
- `tex-optional-delimiter`
- `tex-open-optional-delimiter`
- `tex-close-optional-delimiter`
- `tex-special-character`
- `tex-alignment-tab`
- `tex-subscript-mark`
- `tex-superscript-mark`
- `tex-unbreakable-space`
- `tex-control-space`
- `tex-accent-command`
- `tex-spacing-command`
- `tex-italic-correction`
- `malformed-token`

This module is intended to be source-faithful and streaming. More complete
TeX-specific behavior, especially catcode-sensitive tokenization, can be added
later without changing the basic architecture. The derived layer now also
distinguishes display-vs-inline math shifts and the common special characters
`&`, `_`, `^`, and `~` instead of exposing them only through the generic
`tex-special-character` tag, and it now gives reusable tags to common
control-symbol spacing constructs such as `\ `, `\,`, `\;`, `\!`, and `\/`.
Common accent control symbols such as `\'`, `\"`, `\~`, and `\c` also receive
their own reusable `tex-accent-command` tag. Group and optional delimiters also
distinguish opening-vs-closing roles in the derived layer.
The plain-TeX paragraph command `\par` also receives its own reusable tag.
