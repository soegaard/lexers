# HTML

This document contains design notes for an HTML lexer in the `lexers`
library.

The HTML lexer should follow the general principles in `DESIGN.md`:

- the lexer should be reusable across multiple applications
- syntax coloring is the first application, but not the only one
- consumer-specific behavior should be controlled through profiles or explicit
  parameters
- compatibility with existing interfaces may require projecting precise
  internal categories into broader external ones
- the targeted language version or baseline should be documented explicitly

## Scope

The first HTML implementation targets HTML-style markup, not a general XML or
XHTML lexer.

The initial reusable scope includes:

- tag parsing
- attribute names and values
- text nodes
- HTML entities
- comments
- doctype and declaration-style markup
- malformed-tag recovery
- embedded `<style>` and `<script>` handling

The lexer should provide reusable markup semantics rather than documentation or
preview behavior.

## Embedded Languages

HTML contains embedded language regions that are important to support in the
first milestone.

The design rule is:

- `<style>` bodies delegate to `lexers/css`
- `<script>` bodies delegate to `lexers/javascript`

The delegated tokens should be wrapped into the HTML derived-token layer, so
consumers can inspect one uniform HTML token stream while still seeing embedded
CSS and JavaScript semantics through tags such as `embedded-css` and
`embedded-javascript`.

This preserves the shared lexer architecture and avoids duplicating CSS or
JavaScript tokenization logic inside the HTML lexer.

## Derived Tags

The first reusable HTML-specific derived tags should include:

- `html-tag-name`
- `html-closing-tag-name`
- `html-attribute-name`
- `html-attribute-value`
- `html-text`
- `html-entity`
- `html-doctype`
- `comment`
- `malformed-token`

Embedded bodies may also carry delegated tags from CSS or JavaScript, together
with:

- `embedded-css`
- `embedded-javascript`

Derived-token tags should describe reusable syntax roles or language meaning,
not presentation.

## Deferred Items

The first HTML milestone defers:

- inline event-handler attributes such as `onclick="..."`
- `<script type="module">` nuance
- XML/XHTML generalization
- renderer-facing concepts such as MDN links, swatches, tooltips, or preview
  widgets
