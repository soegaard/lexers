# LaTeX

`lexers/latex` currently builds on the `lexers/tex` core.

The first version shares TeX tokenization and adds a small LaTeX-oriented layer
for common command names such as `\section`, `\begin`, `\end`, `\item`,
`\textbf`, and `\usepackage`.

This means:

- projected LaTeX tokens use the same reusable categories as TeX
- known LaTeX commands can project as `keyword`
- derived tokens keep TeX tags and may additionally carry:
  - `latex-command`
  - `latex-environment-command`

The first version is intentionally conservative. It aims to make ordinary
LaTeX source useful for preview and syntax-coloring consumers without trying to
parse the macro language.
