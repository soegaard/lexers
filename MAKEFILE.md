# Makefile Lexer

`lexers/makefile` is a reusable lexer for Makefiles and `.mk` include files.

The first slice is deliberately small and source-faithful. It covers:

- comments
- whitespace and CRLF preservation
- directive lines such as `include`, `ifdef`, and `endif`
- variable assignment operators such as `=`, `:=`, `?=`, `+=`, and `!=`
- rule targets and `:` delimiters
- variable references such as `$(CC)` and `${VAR}`
- recipe lines starting with a tab

The lexer is streaming and intended to work well for preview-oriented tools
such as `peek`, while still exposing richer derived-token tags for testing and
other reusable consumers.
