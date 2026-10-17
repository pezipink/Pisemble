#lang pisemble
;; =====================================================================
;; PALETTE CYCLE - 8 bit palette mode on pi2d.rkt
;; =====================================================================
;; Each pixel is one byte: an index into a 256 colour palette.
;; The picture is drawn into both pages once (frames 0 and 1). After that
;; nothing is drawn: each frame just loads a rotated palette, which moves
;; the colours through the whole picture.
;;
;; Build:  racket cycle.rkt             -> cycle.obj + cycle.img (beside this file)
;; Run:    qemu-system-aarch64 -M raspi3b -kernel cycle.img -serial null -serial stdio
(require "../lib/pi2d.rkt")

;; 256 entry sine, values 0..63, so four of them add up to 0..252
(define sine-bytes
  (for/list ([i 256])
    (exact-round (* 31.5 (+ 1 (sin (/ (* 2 pi i) 256)))))))

;; a smooth looping palette, stored twice so any 256 entry window is contiguous
(define palette-colours
  (for/list ([i 256])
    (define (channel phase)
      (exact-round (* 127.5 (+ 1 (sin (+ phase (/ (* 2 pi i) 256)))))))
    (rgb (channel 0) (channel 2.094) (channel 4.189))))

(pi2d #:bpp 8

#:init () [] {
  (debug-str "palette cycle" #t)
}

#:render ([vptr x0] [t x1]) [] {
  cmp t @2
  b.cs cycle+
  bl draw-picture:
:cycle
  ; set-palette(0, 256, palette + (t & 255) * 4)
  mov x2 @255
  and x2 t x2
  lsl x2 x2 @2
  adr x3 palette:
  add x2 x3 x2
  mov x0 @0
  mov x1 @256
  bl set-palette:
}

{
; x0 = page.  index(x, y) = sin(3x) + sin(x + y) + sin(2y) + sin(y)
(subr draw-picture
      ([vptr x0] [sines x2] [mask x4] [ny x5] [nx x6] [ya x7] [yb x8]
       [xa x9] [xd x10] [xd-row x11] [row-sum x12] [tmp x13] [v x14])
      [x0 x2 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14] {
  adr sines sine-table:
  mov mask @255
  mov ya @0
  mov yb @0
  mov xd-row @0
  mov ny @height
:next-row
  and tmp ya mask
  add tmp sines tmp
  ldrb w13 [x13 @0]
  mov row-sum tmp
  and tmp yb mask
  add tmp sines tmp
  ldrb w13 [x13 @0]
  add row-sum row-sum tmp
  mov xa @0
  mov xd xd-row
  mov nx @width
:next-pixel
  and tmp xa mask
  add tmp sines tmp
  ldrb w13 [x13 @0]
  add v row-sum tmp
  and tmp xd mask
  add tmp sines tmp
  ldrb w13 [x13 @0]
  add v v tmp
  strb w14 [x0] @1
  add xa xa @3
  add xd xd @1
  sub nx nx @1
  cbnz nx next-pixel-
  add ya ya @2
  add yb yb @1
  add xd-row xd-row @1
  sub ny ny @1
  cbnz ny next-row-
})

:sine-table
(write-values sine-bytes)
/= 4
:palette
(for ([c (append palette-colours palette-colours)])
  (write-value-32 c))
})
