# TSV

`lexers/tsv` is a reusable streaming lexer for tab-separated text.

The first version intentionally mirrors the CSV architecture:

- tabs are field separators
- `\n`, `\r`, and `\r\n` are row separators
- quoted fields use `"`
- doubled quotes `""` are preserved inside quoted fields
- malformed quoted fields recover as `malformed-token` in `'coloring`
  and raise in `'compiler`

The lexer keeps exact source text, including tabs and Windows line endings, so
downstream consumers can reconstruct the original file without re-slicing the
source.

Derived tags are shared with CSV where possible:

- `delimited-field`
- `delimited-quoted-field`
- `delimited-unquoted-field`
- `delimited-empty-field`
- `delimited-separator`
- `delimited-row-separator`
- `delimited-error`

TSV-specific tags add:

- `tsv-field`
- `tsv-separator`
- `tsv-row-separator`
