#lang info

(define collection "pisemble")

(define version "0.1")

(define deps
  '("base"
    "draw-lib"
    "rackunit-lib"))

(define build-deps
  '("scribble-lib"
    "racket-doc"))

(define pkg-desc "Racket assembler and linker for AArch64 (#lang pisemble)")

(define pkg-authors '("pezi_pink@pinksquirrellabs.com"))

(define license 'MIT)

(define scribblings '(("scribblings/pisemble.scrbl" ())))


