# Assembly Lexer Design

The reusable assembly lexer owns assembly lexical decisions. Terminal preview,
file-type detection, and other consumers must use this implementation rather
than introducing their own assembler tokenizers.

## Implemented foundation

`lexers/asm` currently provides an intentionally conservative generic profile.
It preserves source bytes and distinguishes only syntax that does not require a
dialect claim:

- whitespace;
- symbol fragments;
- structural delimiters; and
- operators, including `;` and `#`, without treating either as a comment.

The generic profile does not identify instructions, directives, comment forms,
quoted strings, a CPU, an assembler implementation, or an output format.
Unknown source remains source; it is not an error merely because a specific
assembler has not been selected.

The derived-token API owns immutable raw byte fragments. Their zero-based,
half-open byte spans partition the input exactly, including byte-order marks,
CRLF, invalid encoded bytes, and missing final newlines. Decoded text and
parser-tools-compatible positions are separate from byte spans. Invalid input
uses U+FFFD in decoded text and carries a recoverable diagnostic while keeping
the original bytes unchanged.

The first byte source supports UTF-8 and BOM-selected UTF-16LE/UTF-16BE. It
decodes incrementally and retains only bounded decoder lookahead. The lexer
emits fragments no larger than one decoded scalar value, so an arbitrary source
token or line is never buffered as a whole.

## Profiles and selection

The profile registry records three distinct states:

- `generic` is implemented;
- known future profiles, such as `ca65`, `acme`, `gas`, and `nasm`, are
  recognized but unavailable; and
- unlisted names are unknown.

An unavailable profile is reported as unavailable instead of being silently
mapped to generic syntax. `lexers/asm/detect` exposes versioned, bounded
selection records. Automatic dialect detection is intentionally disabled until
individual profiles have specification-backed rules and calibration tests.

## Follow-on profiles

Profiles must be implemented one at a time from their language specifications.
Each profile owns its comment, string, label, macro, preprocessing, case, and
state-change rules. CPU names and directive tables alone do not define a source
language. A profile may report ambiguity or unsupported features; it must not
invent certainty from a compatible subset of source.

Before adding a profile, add its focused RackUnit coverage for raw-byte
round-tripping, malformed recovery, chunk boundaries, restart state, and its
specific lexical constructs. Add a corpus checker only once reviewed local
source is available; it must skip cleanly when the optional corpus directory is
absent.
