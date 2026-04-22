# Java

`lexers/java` provides a reusable streaming lexer for a practical first slice
of Java lexical structure.

The implementation is grounded primarily in the Java Language Specification,
especially the lexical grammar and lexical-structure chapters.

The first slice covers:

- line and block comments
- whitespace with CRLF preservation
- identifiers
- keywords
- string literals
- text blocks
- char literals
- numeric literals
- operators and delimiters

Projected Java categories are:

- `comment`
- `whitespace`
- `keyword`
- `identifier`
- `literal`
- `operator`
- `delimiter`
- `unknown`
- `eof`

The first reusable Java-specific derived tags include:

- `java-comment`
- `java-line-comment`
- `java-block-comment`
- `java-doc-comment`
- `java-whitespace`
- `java-keyword`
- `java-identifier`
- `java-annotation-marker`
- `java-annotation-name`
- `java-string-literal`
- `java-text-block`
- `java-char-literal`
- `java-numeric-literal`
- `java-operator`
- `java-delimiter`
- `malformed-token`

This module is intended to be source-faithful and streaming. The current first
slice deliberately does not implement the JLS Unicode-escape preprocessing
translation yet, so Java source is tokenized literally as written.
