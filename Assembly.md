# Assembly Lexer Design

The reusable assembly lexer owns assembly lexical decisions. Terminal preview,
file-type detection, and other consumers must use this implementation rather
than introducing their own assembler tokenizers.

## Implemented lexer foundation (M0–M6)

`lexers/asm` provides an intentionally conservative generic profile and
explicit, source-preserving profile selection for the M4–M6 dialects. The
generic profile distinguishes only syntax that does not require a dialect
claim:

- whitespace;
- symbol fragments;
- structural delimiters; and
- operators, including `;` and `#`, without treating either as a comment.

The generic profile does not identify instructions, directives, comment forms,
quoted strings, a CPU, an assembler implementation, or an output format.
Unknown source remains source; it is not an error merely because a specific
assembler has not been selected.

Explicit selections currently provide these profile-owned lexical decisions:

- `ca65`: directives, CPU-related declarations, cheap-local and anonymous
  labels, macros, numeric literals, and the supported `.feature c_comments`
  change;
- `acme`: bang directives, zones, macro calls, literals, and semicolon
  comments;
- `native-motorola`, `asmone`, and `devpac`: a conservative m68k baseline for
  mnemonic suffixes, registers, directives, labels, and semicolon comments;
- `kick`: C-style comments, script braces, directives, and semicolon statement
  separators rather than semicolon comments;
- `64tass`: source directives, namespace and encoding declarations, macro
  boundaries, literals, and semicolon comments;
- `gas`, which requires `m68k`, `x86`, `arm`, or `aarch64` explicitly: target
  comment rules, directives, macros, registers, numeric local references, and
  documented syntax or instruction-mode changes; and
- `nasm`: preprocessor and macro directives, macro parameters and local names,
  `BITS` modes, directives, registers, and data forms.

The deliberately small instruction and directive tables classify documented
lexical structures; they do not attempt complete assembler emulation or infer a
tool identity from shared source.

The derived-token API owns immutable raw byte fragments. Their zero-based,
half-open byte spans partition the input exactly, including byte-order marks,
CRLF, invalid encoded bytes, and missing final newlines. Decoded text and
parser-tools-compatible positions are separate from byte spans. Invalid input
uses U+FFFD in decoded text and carries a recoverable diagnostic while keeping
the original bytes unchanged.

The first byte source supports UTF-8 and BOM-selected UTF-16LE/UTF-16BE. It
decodes incrementally and retains only bounded decoder lookahead. The lexer
emits fragments no larger than the configured cap, so an arbitrary source token
or line is never buffered as a whole. Consecutive fragments of a capped logical
word share a continuation identifier.

## Profiles and selection

The profile registry records implemented names (`generic`, `ca65`, `acme`,
`native-motorola`, `asmone`, `devpac`, `kick`, `64tass`, `gas`, and `nasm`) and
unknown names separately. No selection is silently mapped to generic syntax.

`gas` has no default architecture. Both the lexer and `lexers/asm/detect`
reject a GNU selection without an explicit supported target. The detection API
exposes bounded, versioned selection records. Its M7 probe runs the existing
profile scanners over a fixed byte prefix, records rule-based evidence and
comment-rule conflicts with byte spans, and scores dialect and architecture
independently. It retains compatible alternatives and reports macro or
conditional context rather than treating those observations as unconditional
file-level settings.

Automatic dialect selection remains disabled until a profile has a held-out
calibration record. A high or probable probe result is an inspectable hypothesis
only; callers must keep generic or explicit selection until that gate is met.

## Follow-on work

M7 automatic enablement is calibrated one profile at a time only after audited
real-source samples and detection counterexamples are available. The reusable
probe and its score bands are implemented, but no profile is yet qualified for
automatic use because no held-out corpus has been admitted in this repository.
M8 is additional dialects and representation adapters. Existing profiles must
grow from their published lexical references and fixture evidence; CPU names and
directive tables alone do not define a source language.

The M4–M6 checks cover profile-specific structures, raw-byte reconstruction,
malformed recovery, bounded fragments, and GNU target selection. A corpus
checker will be added once reviewed local source is admitted; it must skip
cleanly when the optional corpus directory is absent.
