# Shell Lexer Notes

`lexers/shell` provides a reusable shell-script lexer aimed at syntax coloring
and other token-oriented consumers.

## Scope

The first implementation targets three dialects:

- Bash
- Zsh
- PowerShell

The public API defaults to Bash and accepts `#:shell 'bash`, `#:shell 'zsh`,
or `#:shell 'powershell`/`'pwsh`.

## Public Shape

Like the other language modules, `lexers/shell` exposes:

- a projected token API
- a derived-token API

The projected API stays intentionally small. The main projected categories are:

- `whitespace`
- `comment`
- `keyword`
- `identifier`
- `literal`
- `delimiter`
- `unknown`

The derived layer carries reusable shell-specific tags such as:

- `shell-keyword`
- `shell-builtin`
- `shell-word`
- `shell-string-literal`
- `shell-ansi-string-literal`
- `shell-variable`
- `shell-command-substitution`
- `shell-comment`
- `shell-punctuation`
- `shell-pipeline-operator`
- `shell-logical-operator`
- `shell-redirection-operator`
- `shell-heredoc-operator`

## First Slice

The first shell lexer covers:

- whitespace
- `#` line comments in ordinary shell comment positions
- single-quoted and double-quoted strings
- Bash/Zsh ANSI-C strings such as `$'line\n'`
- backtick and `$(...)` command substitution
- `$name` and `${...}` variables
- common shell punctuation and operators, including distinct tags for
  pipelines, logical operators, redirections, and heredoc introducers
- shared Bourne-style keywords and builtins
- Zsh builtins
- PowerShell keywords and verb-noun command names

This is intentionally a parser-lite lexical layer, not a full shell parser.

## Markdown Delegation

`lexers/markdown` delegates fenced code blocks to `lexers/shell` for these
info strings:

- `bash`
- `sh`
- `shell`
- `zsh`
- `powershell`
- `pwsh`
- `ps1`

Delegated Markdown tokens keep the shell derived tags and gain
`embedded-shell`.
