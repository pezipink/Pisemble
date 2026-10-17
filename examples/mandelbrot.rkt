#lang pisemble
;; =====================================================================
;; MANDELBROT ZOOMER - a single precision Mandelbrot zoom built on pi2d.rkt
;; =====================================================================
;; The zoom path (centre, span and pixel step for each frame) is computed
;; in Racket at build time. Each frame the render loop iterates
;; z = z*z + c for every pixel, up to max-iter, and colours the pixel from
;; a palette indexed by the escape count. The zoom eases in on the
;; Seahorse Valley boundary and back out over 512 frames.
;;
;; #:render     x0 = back buffer (width x height, 32 bit pixels)
;;              x1 = frame number
;;
;; Build:  racket mandelbrot.rkt        -> mandelbrot.img, .gdb, .elf, .obj
;; Run:    qemu-system-aarch64 -M raspi3b -kernel mandelbrot.img -serial null -serial stdio
(require "../lib/pi2d.rkt")

(define max-iter 64)
(define zoom-frames 512)

(define (f32 x)
  (integer-bytes->integer (real->floating-point-bytes x 4 #f) #f #f))

;; each entry is x0, y0 (centre of the top-left pixel) and the pixel step.
;; The centre stays on the Seahorse Valley boundary: zooming towards the
;; interior would leave the view solid black at max-iter.
(define (zoom-entry f)
  (define t (/ (- 1 (cos (* 2 pi (/ f zoom-frames)))) 2))
  (define span (* 3.5 (expt 2 (* -11 t))))
  (define cx -0.743643887037151)
  (define cy 0.13182590420533)
  (define step (/ span width))
  (list (- cx (* (/ width 2) step))
        (- cy (* (/ height 2) step))
        step
        0.0))

(define zoom-bits
  (for*/list ([f zoom-frames] [v (zoom-entry f)]) (f32 v)))

(define (channel x)
  (inexact->exact (round (* 255 (+ 0.5 (* 0.5 (sin x)))))))

;; max-iter smooth colours, then black for points that never escape
(define palette-bits
  (append
   (for/list ([i max-iter])
     (define p (* 4 pi (/ i max-iter)))
     (rgb (channel p)
          (channel (+ p 2.1))
          (channel (+ p 4.2))))
   (list (rgb 0 0 0))))

(pi2d

#:init () [] {
  (debug-str "mandelbrot" #t)
}

#:render
      ([vptr x0] [frame x1] [ztab x2] [palette x3] [row x4] [col x5]
       [count x6] [tmp x7]) [] {
  adr ztab zoom-table:
  adr palette palette-data:

  ; x0 -> s16, y0 -> s17, step -> s18 for this frame
  and tmp frame @511
  lsl tmp tmp @4
  add tmp ztab tmp
  ldr s16 [tmp @0]
  ldr s17 [tmp @4]
  ldr s18 [tmp @8]
  fmov s29 @2.0
  fmov s30 @4.0

  mov row @0
:next-row
  ; ci -> s23 = y0 + row * step
  scvtf s23 row
  fmul s23 s23 s18
  fadd s23 s23 s17
  mov col @0
:next-col
  ; cr -> s24 = x0 + col * step
  scvtf s24 col
  fmul s24 s24 s18
  fadd s24 s24 s16

  ; z = 0
  mov count @0
  scvtf s25 count
  scvtf s26 count
:iterate
  fmul s27 s25 s25
  fmul s28 s26 s26
  fadd s31 s27 s28
  fcmp s31 s30
  b.gt escaped+
  cmp count @max-iter
  b.hs escaped+
  ; zi' = 2*zr*zi + ci, zr' = zr*zr - zi*zi + cr
  fmul s31 s25 s26
  fmul s31 s31 s29
  fadd s26 s31 s23
  fsub s25 s27 s28
  fadd s25 s25 s24
  add count count @1
  b iterate-
:escaped
  lsl tmp count @2
  add tmp palette tmp
  ldr w8 [tmp @0]
  str w8 [vptr] @4
  add col col @1
  cmp col @width
  b.lo next-col-
  add row row @1
  cmp row @height
  b.lo next-row-
}

{
/= 4
:zoom-table
(for ([c zoom-bits]) (write-value-32 c))
/= 4
:palette-data
(for ([c palette-bits]) (write-value-32 c))
})
