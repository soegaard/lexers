# AGENTS.md

The repo contains a collection of lexers written in Racket.

The package contains reusable lexers.
The lexers are intended to support multiple applications.
Syntax coloring is the first planned application, but other uses are expected as well.

- When given rules, always ask whether they should be added to `AGENTS.md`.

Coding guidelines for Racket code in `lexers`:

1. Prefer `cond` over nested `if` or `let` patterns when branching logic is non-trivial.
2. Prefer internal `define` forms inside `cond` branches instead of wrapping branch bodies in `let` or `let*`.
3. Align right-hand-side expressions when it improves readability.
4. Avoid hanging parentheses; keep closing `))` on the same line as the final expression when practical.
5. Prefer named struct fields over numeric indices.
6. Add function comments for helpers and exported functions:
   - `;; name : contract -> result`
   - `;;   Brief purpose sentence.`
7. Add inline comments for parameter types and optional/default parameters when relevant.
8. Add a comment header before blocks of constant definitions.
9. If a constant or definition name is not self-explanatory, add an end-of-line comment explaining its meaning or purpose.
10. When symbols are used for enumeration, use `case` instead of `cond` for branching.
11. At the top of each file, add a header comment in this form:
    - `;;;`
    - `;;; Title`
    - `;;;`
    - ``
    - `;; An explanation of the contents of the source file.`
12. Use `for` constructs instead of explicit loops.
13. Use `match` for destructuring.
14. For each file export, add a comment near the top of the file, after the header and file explanation, with the export identifier and a one-line explanation. Align the start of the explanations when practical.
15. If you find an error in libraries or folders outside this repo, first make a minimal reproduction of the error, then ask how to proceed.
16. Do not add workarounds. Investigate and fix the root cause instead. Ask for help if needed.

17. Use `rackunit` for tests.
18. For consecutive calls with the same callee and simple arguments, align argument columns to improve scanability.
19. For public API design, prefer exposing information through derived-token accessors before making raw-token representations public.
20. Derived-token tags should describe syntax role or reusable language meaning, not presentation.
19. When an existing lexer is available in another library, use it as prior art,
    but redesign from the language specification when needed so the result is a
    reusable lexer rather than one tied to a single application.
20. When starting a lexer for a new language, begin with a deliberately small
    real-language subset in order to validate the shared architecture before
    expanding toward broader specification coverage.
21. Keep `README.md` lightweight. Long-term public-facing documentation belongs
    in `lexers-doc`.
22. When generating local Scribble HTML output, place it outside the packages,
    and use `+m`; for example:
    `raco scribble +m --htmls --dest html/ lexers-doc/lexers.scrbl`.
23. In documentation, prefer explicit input and output types over a plain
    `procedure?`.
24. In documentation, prefer precise types over broad placeholders such as
    `any/c`.
25. In user-facing documentation, avoid internal staging language such as
    "first implementation", "current slice", or similar project-internal
    phrasing.
25. In hot paths, prefer shared non-allocating substring comparison helpers
    over patterns such as `(string=? (substring ...) ...)`.
26. When optional language integrations depend on packages or Racket versions
    that may be unavailable in supported environments, prefer lazy
    `dynamic-require` at runtime over hard module or package dependencies.
27. When a new lexer has enough real local source available, add a corpus
    checker script in `tools/` instead of relying only on unit tests.
28. Corpus checker scripts in `tools/` must skip cleanly when their local
    `/tmp` corpus directory is unavailable, so package-server tests do not
    fail when the corpus is absent.
29. When newline handling is relevant for a public lexer, add an explicit CRLF
    round-trip regression to the public lexer module.
30. When multiple languages need local corpus builders, prefer a shared helper
    in `tools/private/` over duplicating one-off builder logic in each script.
28. For languages with an official lexical-analysis reference, treat that
    lexical specification as the primary lexer source. Use parser grammars and
    implementation files as secondary context.
26. When adding a new public lexer module, update
    `lexers-doc/lexers.scrbl` in the same turn so the manual matches the
    shipped API.

## Packages

There are 3 packages:

- `lexers-lib` - The implementation of the lexers
- `lexers-doc` - The documentation of the lexers, written in Scribble
- `lexers` - Installs both `lexers-lib` and `lexers-doc`

## Design Notes

1. Shared notes are in DESIGN.md.
2. Each programming language gets a separate file for design notes.
3. The notes for CSS are in `CSS.md`.

## Assembly Support

These rules supplement the repository conventions when working on assembly
source support.

1. **Own the grammar in `lexers`.** The detector and renderer must share the
   same assembly language definitions. Do not introduce a second tokenizer or
   a local lexer workaround in `peek`.
2. **Keep classification dimensions separate.** Represent artifact kind,
   architecture, CPU variant/features, instruction mode, syntax, dialect,
   implementation/version, processing layers, platform/ABI, actual binary
   format, source output intent, and source encoding/container independently.
   Validate combinations against a profile's capabilities.
3. **Permit uncertainty.** Unknown, ambiguous, conflicting, conditional, and
   unsupported results are valid. Compatible source is not proof of a
   particular assembler executable, version, CPU variant, or output format.
4. **Preserve authoritative bytes.** Token fragments must partition the input
   exactly, including BOMs, whitespace, original newlines, invalid sequences,
   and a missing final newline. Keep byte spans separate from Racket positions,
   decoded character positions, and terminal columns.
5. **Stream with bounded storage.** Bound the probe, token fragments, lexical
   lookahead, conditional alternatives, nesting, caches, and diagnostics.
   A port API that buffers an entire token or line is insufficient.
6. **Use profile-specific lexical states.** Comments, strings, labels, macro
   parameters, case rules, and preprocessing differ between dialects and CPU
   targets. CPU tables alone do not define an assembler language.
7. **Respect state changes and their context.** Apply source syntax/mode
   changes locally. Do not execute a directive inside an unexpanded macro as
   a file-level change. Join conditional branches conservatively.
8. **Recover without losing source.** Unknown instructions and macros are not
   automatically errors. Malformed input yields preserved fragments and bounded
   diagnostics. Every non-EOF step must consume input or report a genuine
   need for more input.
9. **Keep presentation separate.** Escape terminal-active source controls and
   undecodable bytes for display. Preserve original bytes through the lexer
   and inspection interfaces. Disabling color does not disable escaping.
10. **Honor explicit choices.** Record them as forced, not as confidence 100.
    Reject contradictory explicit dimensions. Source disagreements produce
    diagnostics without silently changing the forced dialect.
11. **Treat suffixes as hints.** Preserve extension case. `.S` does not prove
    CPP; `.inc` does not prove assembly; `.a` may be source or an archive.
    Binary objects, listings, disk images, and tokenized source have separate
    input paths.
12. **Do not execute source during preview.** Do not expand macros, invoke CPP
    or assemblers, run build scripts, load source-selected plugins, or traverse
    arbitrary includes. Declarative configuration must be explicitly enabled.
13. **Freeze automatic selection before ordinary output.** Later evidence may
    produce diagnostics; it cannot recolor an emitted prefix. Genuine in-file
    language switches remain supported.
14. **Keep decisions inspectable.** Explanations go to stderr. Detection JSON
    and token JSONL are explicit output modes with stable, versioned schemas.
15. **Use realistic tests and provenance.** Keep persistent `peek` tests and
    fixtures under `peek-lib/peek/test/`. Use RackUnit. Every assembly corpus
    file has a hard 120-second deadline and failures identify the file/profile.
16. **Preserve existing terminal behavior.** Default color stays enabled;
    `--pager` continues to support `less -R`; `--list-file-types` stays the
    canonical spelling. Any inserted visible text participates in width
    calculations.
17. **Document the proper boundary.** Shared preview decisions belong in
    `DESIGN.md`; assembly decisions belong in `Assembly.md`. Public behavior
    belongs in `peek-doc/peek.scrbl`; reusable lexer APIs also require
    `lexers-doc` updates.
18. **Fix causes at their owner.** If an existing external lexer/library defect
    is discovered, first make a minimal reproduction, then ask how to proceed.
    Missing assembly functionality is a planned dependency, not evidence of an
    existing defect.
