# Property List Lexer

`lexers/plist` is a reusable lexer for XML property-list files such as
`Info.plist`.

The first slice deliberately targets XML plists only. It does not attempt to
cover binary `bplist` files.

The current lexer covers:

- XML declarations
- plist doctypes
- comments
- element names and delimiters
- attribute names and quoted attribute values
- plist value text for `key`, `string`, `data`, `date`, `integer`, and `real`
- CRLF-preserving source fidelity

The lexer is streaming and designed to work well for preview-oriented tools
such as `peek`, while still exposing richer plist-specific derived tags.

Because this lexer deliberately targets XML plists, unquoted attribute values
are treated as malformed input instead of ordinary attribute-value tokens.
