#lang info

(define collection 'multi)
(define pkg-desc
  "Reusable lexers for C, CSV, CSS, HTML, JavaScript, JSON, Markdown, Python, Racket, Rhombus, Scribble, shell scripts, TSV, WebAssembly text, and YAML.")
(define pkg-authors '(soegaard))
(define license 'MIT)
(define deps '("base" "parser-tools-lib" "syntax-color-lib"))
(define build-deps '("rackunit-lib"))
