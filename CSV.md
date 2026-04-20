# CSV

`lexers/csv` is a reusable streaming lexer for comma-separated text.

The first version is deliberately small and source-faithful:

- commas are field separators
- `\n`, `\r`, and `\r\n` are row separators
- quoted fields use `"`
- doubled quotes `""` are preserved inside quoted fields
- malformed quoted fields recover as `malformed-token` in `'coloring`
  and raise in `'compiler`

The lexer is parser-lite. It does not try to infer schema, header semantics, or
numeric types. It preserves exact source text so consumers such as `peek` can
round-trip the file by concatenating token texts.

Derived tags are shared with TSV where possible:

- `delimited-field`
- `delimited-quoted-field`
- `delimited-unquoted-field`
- `delimited-empty-field`
- `delimited-separator`
- `delimited-row-separator`
- `delimited-error`

CSV-specific tags add:

- `csv-field`
- `csv-separator`
- `csv-row-separator`
