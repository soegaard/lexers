# C++

`lexers/cpp` is a reusable streaming lexer for C++ source.

The first version is deliberately small and lexically focused. It follows the
preprocessor-aware shape of `lexers/c`, then adds the C++ surface that matters
most for reusable preview and coloring consumers:

- preprocessing directives and header names
- line comments and block comments
- identifiers and C++ keywords
- operator words such as `and` and `or`
- numeric literals, including user-defined literal suffixes
- character and string literals
- raw string literals such as `R"(text)"`
- operators and punctuators such as `::`, `->`, and `.*`

Ordinary C++ string and character literals now validate common escape
structures such as simple escapes, octal escapes, hexadecimal escapes, and
universal-character escapes. Malformed escapes and malformed multi-character
character literals remain source-faithful but are tagged as malformed.

The lexer is source-faithful and streaming. Consumers can reconstruct the exact
input by concatenating token texts.

Derived tags in the first slice include:

- `cpp-comment`
- `cpp-whitespace`
- `cpp-keyword`
- `cpp-identifier`
- `cpp-string-literal`
- `cpp-char-literal`
- `cpp-numeric-literal`
- `cpp-operator`
- `cpp-delimiter`
- `cpp-preprocessor-directive`
- `cpp-header-name`
- `cpp-line-splice`
- `cpp-error`

Markdown fenced code blocks labeled `cpp`, `c++`, `cc`, `cxx`, `hpp`, `hh`,
or `hxx` delegate to `lexers/cpp`.
