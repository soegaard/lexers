# Makefile Lexer

`lexers/makefile` is a reusable lexer for Makefiles and `.mk` include files.

The first slice is deliberately small and source-faithful. It covers:

- comments
- whitespace and CRLF preservation
- directive lines such as `include`, `ifdef`, and `endif`
- variable assignment operators such as `=`, `:=`, `?=`, `+=`, and `!=`
- rule targets and `:` delimiters
- special targets such as `.PHONY`
- variable references such as `$(CC)` and `${VAR}`, including form-specific
  derived tags
- recipe lines starting with a tab

Escaped `#` characters such as `\#` are kept as ordinary source text instead
of starting a comment, so variable values and rule text that contain literal
hash signs remain source-faithful.

The derived-token layer also preserves more Makefile structure than the
projected stream, including:

- `makefile-rule-delimiter` and `makefile-double-colon-delimiter`
- `makefile-recipe-separator`
- `makefile-order-only-delimiter`
- `makefile-paren-variable-reference`
- `makefile-brace-variable-reference`

The lexer is streaming and intended to work well for preview-oriented tools
such as `peek`, while still exposing richer derived-token tags for testing and
other reusable consumers.
