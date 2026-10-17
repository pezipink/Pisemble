#lang pisemble
;; A small three-ball animation built on platform.rkt.
;; Build from this folder with: racket bouncing.rkt
(require "../lib/platform.rkt")

(define radius 9)
(define diameter (add1 (* 2 radius)))
(define x-range (- width diameter))
(define y-range (- height diameter))
(define circle-half-widths
  (for/list ([dy (in-range (- radius) (add1 radius))])
    (inexact->exact (floor (sqrt (- (sqr radius) (sqr dy)))))))

(demo {

(subr Demo-init () [] {
  (breakpoint)
  (debug-str "bouncing circles" #t)
})

(subr Draw-frame ([page x0] [frame x1]) [x14 x15] {
  mov x14 frame
  mov x15 page
  mov x0 page
  mov w1 @0
  bl clear-screen:

  mov x0 x15
  mov x1 x14
  mov x2 @x-range
  mov x3 @y-range
  mov x4 @2
  mov x5 @1
  mov x6 @0
  mov x7 @0
  (load-immediate w8 (rgb 255 72 92))
  bl draw-ball:

  mov x0 x15
  mov x1 x14
  mov x2 @x-range
  mov x3 @y-range
  mov x4 @1
  mov x5 @2
  mov x6 @95
  mov x7 @38
  (load-immediate w8 (rgb 64 210 255))
  bl draw-ball:

  mov x0 x15
  mov x1 x14
  mov x2 @x-range
  mov x3 @y-range
  mov x4 @3
  mov x5 @2
  mov x6 @203
  mov x7 @121
  (load-immediate w8 (rgb 255 220 72))
  bl draw-ball:
})

; Return the bouncing coordinate in x0. The path has a linear motion and
; reflects at each end of the range.
(subr bounce-axis
      ([frame x1] [range x2] [speed x3] [phase x4] [margin x5]
       [position x6] [period x7])
      [x6 x7] {
  mul position frame speed
  add position position phase
  lsl period range @1
  udiv x0 position period
  mul x0 x0 period
  sub position position x0
  cmp position range
  b.ls in-first-half+
  sub position period position
:in-first-half
  add x0 margin position
})

(subr draw-ball
      ([page x0] [frame x1] [x-range x2] [y-range x3]
       [x-speed x4] [y-speed x5] [x-phase x6] [y-phase x7] [colour w8]
       [saved-page x9] [cx x10] [cy x11] [saved-y-range x12] [saved-y-speed x13])
      [x9 x10 x11 x12 x13] {
  mov saved-page page
  mov saved-y-range y-range
  mov saved-y-speed y-speed
  mov x2 x-range
  mov x3 x-speed
  mov x4 x-phase
  mov x5 @radius
  bl bounce-axis:
  mov cx x0

  mov x2 saved-y-range
  mov x3 saved-y-speed
  mov x4 y-phase
  mov x5 @radius
  bl bounce-axis:
  mov cy x0

  mov x0 saved-page
  mov x1 cx
  mov x2 cy
  mov w3 colour
  bl draw-circle:
})

(subr draw-circle
      ([page x0] [cx x1] [cy x2] [colour w3]
       [spans x11] [remaining x12] [half-width x6]
       [left x7] [row-y x8] [row-width x9] [saved-colour w5] [centre x10])
      [x5 x6 x7 x8 x9 x10 x11 x12] {
  adr spans circle-half-widths:
  mov saved-colour colour
  ; fill-rect overwrites x1 and x2, so keep the centre in x10
  mov centre cx
  mov remaining @diameter
  sub row-y cy @radius
:next-circle-row
  ldrb w6 [spans] @1
  sub left centre half-width
  lsl row-width half-width @1
  add row-width row-width @1
  mov x1 left
  mov x2 row-y
  mov x3 row-width
  mov x4 @1
  mov w5 saved-colour
  bl fill-rect:
  add row-y row-y @1
  sub remaining remaining @1
  cbnz remaining next-circle-row-
})

:circle-half-widths
(write-values circle-half-widths)
/= 4
})
