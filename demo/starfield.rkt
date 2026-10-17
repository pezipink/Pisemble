#lang pisemble
;; A simple 3D starfield built on platform.rkt.
(require "../lib/platform.rkt")

(define star-count 80)
(define star-data
  (append*
   (for/list ([i (in-range star-count)])
     (list (+ 8 (modulo (* i 37) 88))
           (+ 5 (modulo (* i 29) 60))
           (modulo (+ (* i 73) 19) 256)
           (modulo i 2)
           (modulo (quotient i 2) 2)))))

(demo {

(subr Demo-init () [] {
  (debug-str "starfield" #t)
})

(subr Draw-frame
      ([page x0] [input-frame x1] [frame x19] [stars x20] [remaining x15]
       [x-offset x4] [y-offset x5] [depth x6] [x-sign x7] [y-sign x8]
       [z x9] [projected-x x10] [projected-y x11] [coordinate x12]
       [size x13] [colour w14])
      [x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x19 x20] {
  mov frame input-frame
  mov x0 page
  (load-immediate w1 (rgb 0 0 12))
  bl clear-screen:

  adr stars star-data:
  mov coordinate @255
  and frame frame coordinate
  mov remaining @star-count
:next-star
  ldrb w4 [stars] @1
  ldrb w5 [stars] @1
  ldrb w6 [stars] @1
  ldrb w7 [stars] @1
  ldrb w8 [stars] @1

  sub z depth frame
  and z z coordinate
  add z z @1

  mov coordinate @64
  mul projected-x x-offset coordinate
  udiv projected-x projected-x z
  cmp projected-x @160
  b.cs advance-star+

  mul projected-y y-offset coordinate
  udiv projected-y projected-y z
  cmp projected-y @100
  b.cs advance-star+

  mov coordinate @160
  cbz x-sign x-positive+
  sub x10 coordinate projected-x
  b x-ready+
:x-positive
  add x10 coordinate projected-x
:x-ready
  mov coordinate @100
  cbz y-sign y-positive+
  sub x11 coordinate projected-y
  b y-ready+
:y-positive
  add x11 coordinate projected-y
:y-ready

  cmp z @24
  b.hi distant-star+
  mov size @3
  (load-immediate w14 (rgb 255 255 255))
  b draw-star+
:distant-star
  mov size @1
  (load-immediate w14 (rgb 150 190 255))
:draw-star
  mov x0 page
  mov x1 projected-x
  mov x2 projected-y
  mov x3 size
  mov x4 size
  mov w5 colour
  bl fill-rect:
:advance-star
:advance-star
  sub remaining remaining @1
  cbnz remaining next-star-
})

:star-data
(write-values star-data)
})
