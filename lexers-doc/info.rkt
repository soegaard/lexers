#lang info

(define collection 'multi)
(define pkg-desc "Scribble documentation for the lexers library.")
(define pkg-authors '(soegaard))
(define license 'MIT)
(define deps '("base" "scribble-lib" "racket-doc" "parser-tools-doc" "lexers-lib"))
(define build-deps '())
(define scribblings '(("lexers.scrbl" () (library))))
