#lang info

(define collection 'multi)
(define pkg-desc
  "Reusable lexers for CSS, HTML, JavaScript, Markdown, Racket, Rhombus, Scribble, shell scripts, and WebAssembly text.")
(define pkg-authors '(soegaard))
(define license 'MIT)
(define deps '("base" "parser-tools-lib" "syntax-color-lib"))
(define build-deps '("rackunit-lib"))
