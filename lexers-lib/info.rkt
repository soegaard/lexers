#lang info

(define collection 'multi)
(define pkg-desc
  "Reusable lexers for C, C++, CSV, CSS, Go, Haskell, HTML, Java, JavaScript, JSON, LaTeX, Makefiles, Markdown, Objective-C, Pascal, plist files, Python, Racket, Rhombus, Rust, Scribble, shell scripts, Swift, TeX, TSV, WebAssembly text, and YAML.")
(define pkg-authors '(soegaard))
(define license 'MIT)
(define deps '("base" "parser-tools-lib" "syntax-color-lib"))
(define build-deps '("rackunit-lib"))
