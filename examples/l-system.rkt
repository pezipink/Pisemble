#lang pisemble
;; =====================================================================
;; L-SYSTEM - a growing, swaying fractal plant built on pi2d.rkt
;; =====================================================================
;; The plant is an L-system with axiom X and rules
;;   X -> F+[[X]-X]-F[-FX]+X     F -> FF
;; Racket expands the rules for a few generations and keeps the drawing
;; commands (F + - [ ]) as bytes. It also scales the plant to fit the
;; screen and builds fixed point tables of segment vectors for all 1024
;; headings. Each frame the tree is drawn again from its first command
;; up to min(frame * rate, total), so it grows and then sways as the
;; branch angle changes with the frame counter.
;;
;; #:render     x0 = back buffer (width x height, 32 bit pixels)
;;              x1 = frame number
;;
;; Build:  racket l-system.rkt          -> l-system.img, .gdb, .elf, .obj
;; Run:    qemu-system-aarch64 -M raspi3b -kernel l-system.img -serial null -serial stdio
(require "../lib/pi2d.rkt")

(define generations 5)
(define turn-units 1024)             ; headings per full turn
(define base-turn 71)                ; branch angle, about 25 degrees
(define sway-amp 12)                 ; extra angle added by the sway, in turn units
(define grow-frames 300)           ; frames taken to draw the full plant
(define sky-colour (rgb 12 16 36))
(define ground-colour (rgb 70 50 25))

(define (u32 v) (bitwise-and v #xFFFFFFFF))
(define (round-int x) (inexact->exact (round x)))

(define (expand-lsystem)
  (for/fold ([s "X"]) ([_ (in-range generations)])
    (list->string
     (apply append
            (for/list ([c (in-string s)])
              (case c
                [(#\X) (string->list "F+[[X]-X]-F[-FX]+X")]
                [(#\F) (list #\F #\F)]
                [else (list c)]))))))

(define commands
  (filter (λ (c) (memv c '(#\F #\+ #\- #\[ #\])))
          (string->list (expand-lsystem))))
(define command-bytes (map char->integer commands))
(define command-count (length command-bytes))

;; end point of every F move, for a unit segment, using the same turns as
;; the render hook
(define (turtle-points)
  (let loop ([cs commands] [x 0.0] [y 0.0] [h 0] [stack '()] [pts '()])
    (cond
      [(null? cs) pts]
      [else
       (define c (car cs))
       (define rest (cdr cs))
       (case c
         [(#\F)
          (define th (* 2 pi (/ h turn-units)))
          (define nx (+ x (sin th)))
          (define ny (- y (cos th)))
          (loop rest nx ny h stack (cons (cons nx ny) pts))]
         [(#\+) (loop rest x y (modulo (+ h base-turn) turn-units) stack pts)]
         [(#\-) (loop rest x y (modulo (- h base-turn) turn-units) stack pts)]
         [(#\[) (loop rest x y h (cons (list x y h) stack) pts)]
         [(#\]) (let ([s (car stack)])
                  (loop rest (car s) (cadr s) (caddr s) (cdr stack) pts))])])))

;; scale so the plant fits the screen, with its base on the ground line
(define points (turtle-points))
(define minx (apply min 0.0 (map car points)))
(define maxx (apply max 0.0 (map car points)))
(define miny (apply min 0.0 (map cdr points)))
(define seg-len
  (* 0.9 (min (/ (- width 16) (- maxx minx))
              (/ (- height 8) (- miny)))))
(define base-x (- (/ width 2) (* seg-len (/ (+ minx maxx) 2))))
(define base-y (- height 4))

(define start-x-bits (round-int (* base-x 65536)))
(define start-y-bits (round-int (* base-y 65536)))

;; per heading, the x and y step of one segment in 16.16 fixed point
(define direction-bits
  (for*/list ([h turn-units]
              [v (let ([th (* 2 pi (/ h turn-units))])
                   (list (* (sin th) seg-len 65536)
                         (* (- (cos th)) seg-len 65536)))])
    (u32 (round-int v))))

;; per frame change of the branch angle, 1024 entries
(define sway-bits
  (for/list ([i turn-units])
    (u32 (round-int (* sway-amp (sin (* 2 pi (/ i turn-units))))))))

(define (lerp a b t) (+ a (* t (- b a))))
;; 16 depth colours, trunk brown to leaf green
(define palette-bits
  (for/list ([i 16])
    (define t (/ i 15.0))
    (rgb (round-int (lerp 120 90 t))
         (round-int (lerp 80 220 t))
         (round-int (lerp 40 90 t)))))

(define total command-count)
(define rate (ceiling (/ total grow-frames)))

(pi2d

#:init () [] {
  (debug-str "l-system" #t)
}

#:render
      ([page x0] [frame x1]) [] {
  ; x2 command pointer, x3 commands left, x4/x5 turtle x/y (16.16),
  ; x6 heading (1024 per turn), x7 depth, x8 command byte, x9 page,
  ; x10 branch angle step, x11 direction table, x12 sway table,
  ; x13 palette, x14 row bytes, x15 scratch, x16 dx then colour,
  ; x17 dy, x19/x20 next x/y, x21/x22 abs dx/dy, x23 steps,
  ; x24/x25 step size, x26/x27 pixel x/y, x28 pixel address
  mov x9 x0
  mov x15 @rate
  mul x3 x1 x15
  (load-immediate x15 total)
  cmp x3 x15
  csel x3 x15 x3 hi

  ; branch angle changes with the frame counter
  and x15 x1 @1023
  lsl x15 x15 @2
  adr x12 sway-table:
  add x15 x12 x15
  ldrsw x15 [x15 @0]
  mov x10 @base-turn
  add x10 x10 x15

  ; the library calls below take their arguments in x0-x3, so keep the command count on the stack
  (PUSH x3)
  (load-immediate w1 sky-colour)
  mov x0 x9
  bl clear-screen:

  mov x0 x9
  mov x1 @0
  mov x2 @(- height 4)
  mov x3 @width
  mov x4 @4
  (load-immediate w5 ground-colour)
  bl fill-rect:
  (POP x3)

  (load-immediate x4 start-x-bits)
  (load-immediate x5 start-y-bits)
  mov x6 @0
  mov x7 @0
  mov x14 @(* 4 width)
  adr x2 commands:
  adr x11 direction-table:
  adr x13 palette-data:
  ; the command loop draws up to x3 commands this frame
:cmd-loop
  cbz x3 cmd-done+
  sub x3 x3 @1
  ldrb w8 [x2] @1
  cmp x8 @70
  b.eq do-forward+
  cmp x8 @43
  b.eq do-right+
  cmp x8 @45
  b.eq do-left+
  cmp x8 @91
  b.eq do-push+
  cmp x8 @93
  b.eq do-pop+
  b cmd-loop-

:do-right
  add x6 x6 x10
  and x6 x6 @1023
  b cmd-loop-

:do-left
  sub x6 x6 x10
  and x6 x6 @1023
  b cmd-loop-

:do-push
  (PUSH x4 x5 x6 x7)
  add x7 x7 @1
  b cmd-loop-

:do-pop
  (POP x7 x6 x5 x4)
  b cmd-loop-

:do-forward
  ; step vector for the current heading, end point in x19/x20
  lsl x15 x6 @3
  add x15 x11 x15
  ldrsw x16 [x15 @0]
  ldrsw x17 [x15 @4]
  add x19 x4 x16
  add x20 x5 x17

  ; number of pixel steps: (max(|dx|, |dy|) >> 16) + 1
  cmp x16 @0
  cneg x21 x16 lt
  cmp x17 @0
  cneg x22 x17 lt
  cmp x21 x22
  csel x15 x21 x22 hi
  asr x23 x15 @16
  add x23 x23 @1
  sdiv x24 x16 x23
  sdiv x25 x17 x23

  ; colour from the depth, capped at 15
  mov x15 @15
  cmp x7 @15
  csel x15 x7 x15 lo
  lsl x15 x15 @2
  add x15 x13 x15
  ldr w16 [x15 @0]

:seg-step
  add x4 x4 x24
  add x5 x5 x25
  asr x26 x4 @16
  asr x27 x5 @16
  cmp x26 @width
  b.hs seg-skip+
  cmp x27 @height
  b.hs seg-skip+
  madd x28 x27 x14 x9
  add x28 x28 x26 LSL @2
  str w16 [x28 @0]
:seg-skip
  sub x23 x23 @1
  cbnz x23 seg-step-
  mov x4 x19
  mov x5 x20
  b cmd-loop-

; a frame can stop inside a branch, so unwind the pushes left on the stack
; (each POP restores the depth in x7 to its value before that push)
:cmd-done
  cbz x7 unwound+
  (POP x7 x6 x5 x4)
  b cmd-done-
:unwound
}

{
/= 4
:direction-table
(for ([v direction-bits]) (write-value-32 v))
/= 4
:sway-table
(for ([v sway-bits]) (write-value-32 v))
/= 4
:palette-data
(for ([c palette-bits]) (write-value-32 c))
:commands
(write-values command-bytes)
/= 4
})
