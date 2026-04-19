# YAML Lexer Notes

`lexers/yaml` is a handwritten streaming lexer for practical YAML 1.2 source.

The first slice is grounded primarily in the YAML 1.2.2 lexical and structural
rules, with an emphasis on the parts that show up in real local corpus files:

- comments
- directives and document markers
- mappings and sequence indicators
- flow delimiters
- plain scalars
- quoted scalars
- block scalar headers and content
- anchors, aliases, and tags

The first version is deliberately parser-lite. It focuses on reusable token and
derived-tag structure rather than full YAML loading semantics.

Current derived tags include:

- `yaml-comment`
- `yaml-whitespace`
- `yaml-directive`
- `yaml-document-marker`
- `yaml-sequence-indicator`
- `yaml-key-indicator`
- `yaml-value-indicator`
- `yaml-flow-delimiter`
- `yaml-anchor`
- `yaml-alias`
- `yaml-tag`
- `yaml-string-literal`
- `yaml-plain-scalar`
- `yaml-key-scalar`
- `yaml-boolean`
- `yaml-null`
- `yaml-number`
- `yaml-block-scalar-header`
- `yaml-block-scalar-content`
- `yaml-error`

Markdown fenced code blocks labeled `yaml` or `yml` delegate to
`lexers/yaml`.
