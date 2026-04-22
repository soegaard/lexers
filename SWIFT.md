# Swift

`lexers/swift` is a reusable streaming lexer for Swift source.

The first version is grounded in Swift lexical structure, but deliberately
small. It focuses on the parts that are most useful for syntax-coloring and
preview consumers:

- whitespace
- line comments and nested block comments
- identifiers, including backticked identifiers and `$0`-style identifiers
- reserved keywords
- attributes such as `@IBOutlet`
- pound directives and conditionals such as `#if` and `#available`
- string literals, including multiline strings
- raw string literals with `#` delimiters, including multiline raw strings
- numeric literals
- operators and delimiters
- malformed unterminated comments and strings

The lexer is source-faithful and streaming. Consumers can reconstruct the exact
input by concatenating token texts.

Derived tags in the first slice include:

- `swift-comment`
- `swift-whitespace`
- `swift-keyword`
- `swift-identifier`
- `swift-string-literal`
- `swift-raw-string-literal`
- `swift-numeric-literal`
- `swift-attribute`
- `swift-pound-directive`
- `swift-operator`
- `swift-delimiter`
- `swift-error`

Markdown fenced code blocks labeled `swift` delegate to `lexers/swift`.
