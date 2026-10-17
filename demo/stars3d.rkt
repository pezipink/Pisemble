#lang pisemble
;; A minimal 3D starfield built on platform.rkt. Stars fly towards the viewer:
;; each frame a star's depth shrinks and its screen position is offset / depth.
;; Build from this folder with: racket stars3d.rkt
(require "../lib/platform.rkt")

(define star-count 200)
(define max-x-offset (- (quotient width 2) 3))   ; keeps a 2x2 star on screen
(define max-y-offset (- (quotient height 2) 3))

;; Each star is 5 bytes: |x|, |y|, starting depth, x negative?, y negative?
(random-seed 3)
(define star-data
  (append*
   (for/list ([i (in-range star-count)])
     (list (add1 (random 250)) (add1 (random 250)) (random 256)
           (random 2) (random 2)))))

(demo {

(subr Demo-init () [] {
  (debug-str "3d starfield" #t)
})

(subr Draw-frame
      ([page x0] [frame x1] [saved-page x9] [travel x10] [stars x11]
       [remaining x12] [mask x13] [x-negative x14] [y-negative x15] [z x16]
       [offset-x x6] [offset-y x7] [plus x17] [minus x18])
      [] {
  mov saved-page page
  lsl travel frame @1                ; two depth units per frame
  mov w1 @0
  bl clear-screen:

  adr stars star-data:
  mov remaining @star-count
  mov mask @255
:next-star
  ldrb w6 [stars] @1
  ldrb w7 [stars] @1
  ldrb w16 [stars] @1
  ldrb w14 [stars] @1
  ldrb w15 [stars] @1

  ; z = ((start - travel) & 255) + 1, so 1..256 and wrapping back to far
  sub z z travel
  and z z mask
  add z z @1

  ; project: offset * 64 / z, skipping stars that have left the screen
  lsl offset-x offset-x @6
  udiv offset-x offset-x z
  cmp offset-x @max-x-offset
  b.hi skip-star+
  lsl offset-y offset-y @6
  udiv offset-y offset-y z
  cmp offset-y @max-y-offset
  b.hi skip-star+

  mov x1 @(quotient width 2)
  add plus x1 offset-x
  sub minus x1 offset-x
  cmp x-negative @0
  csel x1 plus minus eq
  mov x2 @(quotient height 2)
  add plus x2 offset-y
  sub minus x2 offset-y
  cmp y-negative @0
  csel x2 plus minus eq

  ; far stars are dim single pixels, near ones bright 2x2 blocks
  mov x3 @1
  mov x4 @1
  (load-immediate w5 (rgb 80 80 100))
  cmp z @170
  b.hi draw-star+
  (load-immediate w5 (rgb 170 170 200))
  cmp z @85
  b.hi draw-star+
  mov x3 @2
  mov x4 @2
  (load-immediate w5 (rgb 255 255 255))
:draw-star
  mov x0 saved-page
  bl fill-rect:
:skip-star
  sub remaining remaining @1
  cbnz remaining next-star-
})

:star-data
(write-values star-data)
/= 4
})
