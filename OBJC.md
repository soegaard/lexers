# Objective-C

`lexers/objc` is a reusable streaming lexer for Objective-C source.

The first version reuses the preprocessor-aware C-style lexer shape and adds the
Objective-C lexical surface most useful to syntax-coloring and preview
consumers:

- preprocessing directives and header names
- line comments and block comments
- identifiers and C / Objective-C keywords
- `@`-keywords such as `@interface`, `@property`, and `@end`
- Objective-C string literals such as `@"text"`
- object-literal introducers such as `@[]`, `@{}`, and `@()`
- numeric literals, operators, and delimiters

The lexer is source-faithful and streaming. Consumers can reconstruct the exact
input by concatenating token texts.

Derived tags in the first slice include:

- `objc-comment`
- `objc-whitespace`
- `objc-keyword`
- `objc-at-keyword`
- `objc-identifier`
- `objc-string-literal`
- `objc-char-literal`
- `objc-numeric-literal`
- `objc-operator`
- `objc-delimiter`
- `objc-preprocessor-directive`
- `objc-header-name`
- `objc-literal-introducer`
- `objc-line-splice`
- `objc-error`

Markdown fenced code blocks labeled `objc`, `objective-c`, `objectivec`, or
`obj-c` delegate to `lexers/objc`.
