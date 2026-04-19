# C Lexer Notes

`lexers/c` is a handwritten streaming lexer for C source.

The first slice is grounded primarily in the C11 lexical and preprocessing-token
rules from `C_Standard_2011-n1570.pdf`, with parser grammars treated as
secondary context.

Current scope:

- whitespace and comments
- identifiers and C11 keywords
- preprocessing directives such as `#include` and `#define`
- include header names like `<stdio.h>` and `"local.h"`
- numeric literals
- string and character literals
- operators and delimiters
- backslash-newline line splicing
- malformed unterminated comment and literal handling

The lexer is preprocessor-aware from the beginning, because real C files are not
very useful without directive lines and include targets.

The derived-token layer currently uses tags such as:

- `c-comment`
- `c-whitespace`
- `c-keyword`
- `c-identifier`
- `c-string-literal`
- `c-char-literal`
- `c-numeric-literal`
- `c-operator`
- `c-delimiter`
- `c-preprocessor-directive`
- `c-header-name`
- `c-line-splice`
- `c-error`

The projected layer stays in the shared token vocabulary used by the rest of
the package.
