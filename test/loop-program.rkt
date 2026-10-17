#lang pisemble
;; Emits one of each loop form so loop-test.rkt can check the branch words.
(require "../lib/expr.rkt")

(aarch64 "loop-program.img" []{
  (/whilenz x1 {add x2 x2 @1})
  (/do-whilenz x1 {add x2 x2 @1})
  (/whilez x1 {add x2 x2 @1})
  (/do-whilez x1 {add x2 x2 @1})
  (/while (x1 ne @0) {add x2 x2 @1})
  (/do-while (x1 ne @0) {add x2 x2 @1})
  (/for {mov x3 @0} (x3 lt @2) {add x3 x3 @1} {add x2 x2 @1})
  :here
  b.hs here-
  b.lo here-
  b.cs here-
  b.cc here-
  (/do-while (x1 hs x2) {add x3 x3 @1})
  (/do-while (x1 lo x2) {add x3 x3 @1})
  })
