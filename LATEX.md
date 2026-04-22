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
  - `latex-environment-name`
  - `latex-verbatim-literal`
  - `latex-line-break-command`

The first version is intentionally conservative. It aims to make ordinary
LaTeX source useful for preview and syntax-coloring consumers without trying to
parse the macro language. Environment names in forms such as
`\begin{itemize}` and `\end{itemize}` now receive their own derived tag instead
of remaining plain TeX text. LaTeX `\verb|...|` spans now also receive a
distinct verbatim-literal tag, and `\\` receives a dedicated line-break tag.
