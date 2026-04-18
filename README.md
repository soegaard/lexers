# lexers

Reusable lexers for multiple consumers.

The library currently provides public lexer modules for:

- `lexers/css`
- `lexers/html`
- `lexers/javascript`
- `lexers/markdown`
- `lexers/racket`
- `lexers/scribble`
- `lexers/wat`

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

To build the local manual:

```sh
raco scribble +m --htmls --dest html/ lexers-doc/lexers.scrbl
```

## Status

The current lexers are intended to be reusable across tools instead of being
tied to a single renderer or editor integration.

Several lexers are handwritten, while the Racket and Scribble support is
adapter-backed on top of `syntax-color`.

## License

MIT. See [LICENSE](LICENSE).
