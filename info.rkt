#lang info

(define pkg-desc "Meta-package for the lexers library and documentation.")
(define pkg-authors '(soegaard))
(define license 'MIT)

(define deps '("base"
               "lexers-lib"
               "lexers-doc"
               "parser-tools-lib"
               "syntax-color-lib"))
(define build-deps '("parser-tools-doc"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-lib"
                     "syntax-color-doc"))
