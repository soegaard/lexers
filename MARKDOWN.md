# Markdown

This document contains design notes for a Markdown lexer in the `lexers`
library.

## Scope

The first implementation of `lexers/markdown` is a handwritten
GitHub-flavored Markdown lexer.

It follows the shared `lexers` architecture:

- a projected token API
- a derived-token API

The first milestone is parser-lite and line-oriented rather than a full
CommonMark parser.

## GFM Baseline

`lexers/markdown` targets GitHub-flavored Markdown by default.

The first reusable scope includes:

- ATX headings
- paragraphs and plain text
- block quotes
- ordered and unordered lists
- fenced and indented code blocks
- inline code spans
- emphasis, strong emphasis, and strikethrough
- links, images, and autolinks
- thematic breaks
- GFM tables
- task-list markers
- raw HTML regions
- backslash escapes

## Embedded Languages

Markdown contains embedded regions that should delegate to existing lexers when
that improves reusable semantics.

The first design rule is:

- raw HTML delegates to `lexers/html`
- fenced code blocks delegate to known language lexers when the info string is
  recognized

The first recognized fenced language names are:

- `css`
- `html`
- `javascript`, `js`
- `jsx`
- `racket`, `rkt`
- `scribble`, `scrbl`

Delegated tokens are wrapped into the Markdown derived-token layer and gain
language markers such as:

- `embedded-html`
- `embedded-css`
- `embedded-javascript`
- `embedded-racket`
- `embedded-scribble`

This preserves one uniform Markdown token stream while still exposing reusable
semantics from the delegated languages.

## Derived Tags

The first reusable Markdown-specific derived tags include:

- `markdown-text`
- `markdown-heading-marker`
- `markdown-heading-text`
- `markdown-blockquote-marker`
- `markdown-list-marker`
- `markdown-task-marker`
- `markdown-thematic-break`
- `markdown-code-span`
- `markdown-code-fence`
- `markdown-code-block`
- `markdown-code-info-string`
- `markdown-emphasis-delimiter`
- `markdown-strong-delimiter`
- `markdown-strikethrough-delimiter`
- `markdown-link-text`
- `markdown-link-destination`
- `markdown-link-title`
- `markdown-image-marker`
- `markdown-autolink`
- `markdown-table-pipe`
- `markdown-table-alignment`
- `markdown-table-cell`
- `markdown-escape`
- `markdown-hard-line-break`
- `malformed-token`

These tags describe reusable Markdown syntax roles, not renderer behavior.

## Deferred Work

The first Markdown milestone defers:

- document-wide reference-link resolution
- footnotes
- alerts or admonitions
- full CommonMark ambiguity resolution in every corner case
- renderer-facing concepts
