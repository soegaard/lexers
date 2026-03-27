# Design

This document describes the shared design goals for the `lexers` library.

The library is intended to provide reusable lexers that can serve multiple
applications. Syntax coloring is the first planned application, but it is not
the defining one. The core design should support other consumers as well,
including parser-oriented tools.

## Core Principle

The main design principle is to separate lexical recognition from consumer
policy.

A lexer should recognize the lexical structure of a language once. Different
consumers can then choose how strict the lexer should be, whether trivia should
be preserved, and whether source positions should be attached to returned
tokens.

This means the library should not define separate incompatible lexer families
for syntax coloring and compilation. Instead, it should define one reusable
core design with explicit profiles or parameters that control behavior.

## Consumer Profiles

The initial design should account for at least two consumer profiles.

### Coloring Profile

The coloring profile is tolerant.

When the lexer encounters unknown or malformed syntax, it may return an
`unknown` token and allow the consumer to decide how to display it. For
example, a syntax colorer may choose to render such regions in a muted style or
in red.

The coloring profile also needs access to whitespace and comments. These are
important parts of the source text for highlighting and presentation, even when
they are not semantically significant to a compiler.

### Compiler Profile

The compiler profile is strict.

When the lexer encounters unknown or malformed syntax, it should signal a
lexical error instead of returning a placeholder token and continuing.

The compiler profile may also choose to skip trivia such as whitespace and
comments when those tokens are not needed by downstream consumers.

## `parser-tools` Compatibility

Compatibility with `parser-tools` is a hard design target.

The `parser-tools` library is distributed with Racket and already defines a
widely used lexer and parser interface. If this library preserves that
interface, its lexers can be used by existing tools.

The main external callable shape should therefore remain:

- a lexer is a procedure that reads from an input port
- each call returns the next token-like value

The compatibility target for returned values is:

- plain mode returns `symbol | token`
- source-position mode returns `position-token` containing `symbol | token`

Here, `token`, `position`, and `position-token` refer to the actual structures
exported by `parser-tools/lex`.

This point is important because structures in Racket are generative. A
structure defined locally with the same field names and apparent layout is not
the same structure as the one exported by `parser-tools`. Existing consumers
such as `parser-tools/yacc` use the predicates associated with the original
structures. Therefore, direct compatibility requires importing and constructing
the actual `parser-tools` token structures, not local lookalikes.

This compatibility goal also implies retaining the familiar distinction between
empty tokens represented by symbols and valued tokens represented by `token`
structures.

## Token and Trivia Policy

Whitespace and comments are first-class lexical categories in the design.

They should be recognized by the reusable core lexer, even when some consumers
later choose to suppress them.

In some languages, the specification's own token model may not treat comments
as ordinary tokens. Even in such cases, the library may still include explicit
comment items in its reusable token stream model when that improves support for
consumers such as syntax colorers, documentation tools, and other
presentation-oriented applications.

This means the design should distinguish between:

- a specification-grounded raw token model
- the library's reusable token stream model

The reusable token stream model may extend the raw token model with retained
trivia such as comments, and with recoverable error items such as `unknown`,
while still preserving a clear connection to the underlying language
specification.

The design should support caller-configurable trivia retention. At a minimum,
the policy should allow consumers to choose whether trivia is:

- emitted as tokens
- skipped entirely
- normalized before being returned

This allows syntax coloring and other presentation-oriented tools to preserve
layout-sensitive information without forcing compiler-oriented users to process
trivia they do not need.

## Error Policy

Unknown or malformed input should be handled as a policy decision, not as a
difference in the lexical definition of the language.

The lexer should still embody one lexical understanding of the language, while
profiles or parameters determine how failures are reported.

In particular:

- a coloring-oriented profile may emit an `unknown` token and continue
- a compiler-oriented profile should raise a lexical error

At the library level, recoverable `unknown` items should be treated as part of
the reusable token stream model rather than as part of the
specification-grounded raw token model.

This distinction is important because syntax coloring can remain useful even
when the source text is incomplete or temporarily invalid, while a compiler
must reject invalid input.

## Token Vocabulary

The library design should distinguish between common token categories and
language-specific token categories.

Common token categories provide a shared vocabulary across lexers. They are
useful for generic consumers such as syntax colorers, token inspectors, and
testing utilities.

The standardized consumer-facing projection vocabulary should initially
include:

- whitespace
- comment
- identifier
- keyword
- operator
- delimiter
- literal
- unknown

These common names are intended as shared design guidance, not as a claim that
every language will use every category in exactly the same way.

At the reusable token stream boundary, the categories `whitespace`,
`comment`, and `unknown` should be treated as especially important common
categories across languages.

This standardized projection vocabulary is intentionally small. It does not
currently include categories such as `function`, which may remain available in
language-specific raw or derived layers until there is a clear cross-language
need to standardize them.

Individual language lexers should also be free to define language-specific
token categories where that improves precision or usability. Examples might
include tokens for particular numeric forms, string forms, interpolation
markers, or domain-specific keywords.

For some languages, the lexer may need to distinguish categories more precisely
than a downstream consumer does. For example, a CSS lexer may wish to
distinguish color literals from other literal forms.

This suggests an additional design principle:

- lexers may recognize precise internal or language-specific token categories
- consumer-facing interfaces may project these precise categories into broader
  external categories when compatibility requires it

In particular, a lexer compatible with existing Racket color-lexer interfaces
may map precise internal categories into the broader classes expected by those
interfaces, while still preserving the more precise distinctions for other
applications.

The design should therefore support both:

- a common vocabulary for broadly useful token classes
- language-specific vocabulary for distinctions that matter only within a
  particular lexer

When a lexer retains trivia, whitespace and comments should use the common
vocabulary unless a language-specific distinction is clearly needed.

When a lexer is operating in a tolerant profile, unknown or malformed input
should be represented using the common `unknown` category.

## Language Versions

For evolving languages and standards, lexer design must account for versioned
specifications.

This concern should be handled at the language level, not as part of the
shared cross-language consumer profiles such as `'coloring` and `'compiler`.

The general design rule is:

- the shared lexer architecture remains stable across languages
- each language lexer documents the specification version, edition, snapshot,
  or baseline feature set it targets
- version-specific differences are handled in the language-specific design
  notes and implementation

Each language note should therefore record the intended baseline version and,
when relevant, how later or earlier language variants may affect tokenization.

## Public Interface Direction

The intended primary interface is a lexer procedure that consumes an input port
and returns the next token-like value.

The design should expose a core lexer constructor, such as `make-lexer`, that
produces this port-based lexer procedure.

Lexer construction, configuration, or invocation should support explicit
behavior settings for at least:

- error handling policy
- trivia retention policy
- optional source-position wrapping

The preferred direction is that these settings are chosen when the lexer is
constructed. The same settings should also be available through convenience
interfaces such as `string->tokens`, which should internally use the same core
lexer design.

The design should include predefined profiles, such as `'coloring` and
`'compiler`, that supply sensible defaults for these settings.

Profiles should define defaults, while explicit options override those
defaults. This makes the API predictable while still allowing specific tools to
adjust behavior when needed.

The language-specific lexical knowledge should remain fixed. Profiles and
options should control reporting and token-retention behavior, not the lexical
definition of the language itself.

For parser-facing consumers, `parser-tools` compatibility should be the default
external token contract.

In addition to the primary port-based interface, the library should also
provide a convenience function such as `string->tokens` for tokenizing an
entire string without requiring callers to work with string ports directly.

This convenience interface should be defined as a thin wrapper over the
port-based lexer interface, so that it shares the same lexical behavior,
profiles, and compatibility guarantees.

## Deferred Items

The following items are intentionally left open for later design work:

- incremental lexing
- a separate internal token hierarchy distinct from `parser-tools`
- editor-specific styling classes as part of the core token API

These may become useful later, but they should not be assumed in the initial
library design.
