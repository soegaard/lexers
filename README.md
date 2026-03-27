# lexers

A collection of lexers for a few select languages.

Currently implemented language modules include:

- `lexers/css`
- `lexers/javascript`

The long-term public-facing documentation is intended to live in `lexers-doc`.

## CSS

The first implemented lexer is available at the public module path
`lexers/css`.

The module currently exports:

- `make-css-lexer`
- `css-string->tokens`
- `make-css-derived-lexer`
- `css-string->derived-tokens`
- `css-profiles`

### Projected Tokens

Use `css-string->tokens` for the consumer-facing token stream:

```racket
(require lexers/css)

(css-string->tokens "/* c */ color: #fff;" #:profile 'coloring)
```

The projected stream is intended for general consumers such as syntax coloring.
The current common categories include values such as `comment`, `whitespace`,
`identifier`, `keyword`, `literal`, `delimiter`, and `unknown`.

### Derived Tokens

Use `css-string->derived-tokens` when you want richer CSS-specific
classifications:

```racket
(require lexers/css)

(css-string->derived-tokens "#fff rgb( --brand-color")
```

The derived layer can currently distinguish tags such as `color-literal`,
`color-function`, and `custom-property-name`.

### Profiles

Two profiles are currently supported:

- `'coloring`
  Keeps trivia, emits `unknown` for malformed input, and includes source
  positions by default.
- `'compiler`
  Skips trivia, raises on malformed input, and includes source positions by
  default.
