# lexers

Reusable lexers for multiple consumers.

The library currently provides public lexer modules for:

- `lexers/css`
- `lexers/c`
- `lexers/cpp`
- `lexers/csv`
- `lexers/html`
- `lexers/json`
- `lexers/javascript`
- `lexers/markdown`
- `lexers/objc`
- `lexers/python`
- `lexers/racket`
- `lexers/rhombus`
- `lexers/shell`
- `lexers/scribble`
- `lexers/swift`
- `lexers/tsv`
- `lexers/wat`
- `lexers/yaml`

These modules expose:

- a projected token API for general consumers such as syntax coloring
- a derived-token API for richer language-specific classification

## Packages

This repository contains three packages:

- `lexers`
  Meta-package that installs the library and documentation
- `lexers-lib`
  Lexer implementations
- `lexers-doc`
  Scribble manual

## Documentation

The long-form public documentation lives in `lexers-doc`.

Published documentation:

- [docs.racket-lang.org/lexers](https://docs.racket-lang.org/lexers/index.html)

To build the local manual:

```sh
raco scribble +m --htmls --dest html/ lexers-doc/lexers.scrbl
```

## Status

The current lexers are intended to be reusable across tools instead of being
tied to a single renderer or editor integration.

Several lexers are handwritten, while the Racket and Scribble support is
adapter-backed on top of `syntax-color`, and Rhombus support is adapter-backed
on top of `rhombus/private/syntax-color`.

Rhombus support is optional at runtime. The package remains installable on
older Racket versions, and `lexers/rhombus` becomes usable when
`rhombus-lib` is available on `base >= 8.14`.

## License

MIT. See [LICENSE](LICENSE).
