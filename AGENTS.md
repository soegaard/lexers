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
25. In hot paths, prefer shared non-allocating substring comparison helpers
    over patterns such as `(string=? (substring ...) ...)`.
26. When optional language integrations depend on packages or Racket versions
    that may be unavailable in supported environments, prefer lazy
    `dynamic-require` at runtime over hard module or package dependencies.
27. When a new lexer has enough real local source available, add a corpus
    checker script in `tools/` instead of relying only on unit tests.
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
