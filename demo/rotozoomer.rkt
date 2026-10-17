#lang pisemble
;; =====================================================================
;; ROTOZOOMER - a floating point rotozoomer built on platform.rkt
;; =====================================================================
;; A 64x64 image is tiled across the screen, rotated and zoomed over
;; time. The image is loaded at assembly time by texture.rkt, which
;; draws it into a texture with racket/draw and writes the texels into
;; the binary. See docs/tutorials/rotozoomer.md for the maths.
;;
;; Draw-frame   x0 = back buffer (width x height, 32 bit pixels)
;;              x1 = frame number
;;
;; Build:  racket rotozoomer.rkt        -> rotozoomer.img, .gdb, .elf, .obj
;; Run:    qemu-system-aarch64 -M raspi3b -kernel rotozoomer.img -serial null -serial stdio
(require "../lib/platform.rkt"
         "texture.rkt"
         racket/runtime-path)

(define-runtime-path squirrel-path "pink-squirrel.png")

(define tex-size 64)
(define texels (load-texture squirrel-path tex-size))

(define (f32 x)
  (integer-bytes->integer (real->floating-point-bytes x 4 #f) #f #f))

;; one full turn of the sine table, 1024 entries
(define sine-bits
  (for/list ([i 1024]) (f32 (sin (* 2 pi (/ i 1024))))))

;; zoom base and amplitude (scale runs 0.2 to 1.4 texels per pixel), pivot
;; centre x and y, texture centre, the bias, and the orbit radii for the pivot
(define const-bits
  (list (f32 0.8) (f32 0.6) (f32 160.0) (f32 100.0) (f32 32.0) (f32 640.0)
        (f32 90.0) (f32 60.0)))

(demo {

(subr Demo-init () [] {
  (debug-str "rotozoomer" #t)
})

(subr Draw-frame
      ([vptr x0] [frame x1] [sines x2] [tex x3] [ang x4] [tmp x16]
       [tmp2 x17] [ny x19] [nx x20] [consts x21]) [] {
  adr sines sine-table:
  adr tex texture:
  adr consts consts:
  lsl ang frame @1
  and ang ang @1023

  ; cos -> s0, sin -> s1
  add tmp ang @256
  and tmp tmp @1023
  lsl tmp tmp @2
  add tmp sines tmp
  ldr s0 [tmp @0]
  lsl tmp ang @2
  add tmp sines tmp
  ldr s1 [tmp @0]

  ; z -> s2 = 0.8 + 0.6 * sine[(4 * frame) & 1023]
  lsl tmp frame @2
  and tmp tmp @1023
  lsl tmp tmp @2
  add tmp sines tmp
  ldr s2 [tmp @0]
  ldr s3 [consts @4]
  fmul s2 s2 s3
  ldr s3 [consts @0]
  fadd s2 s2 s3

  ; per-pixel steps in texels: dudx s10, dvdx s11, dudy s14, dvdy s15
  fmul s10 s2 s0
  fmul s11 s2 s1
  fneg s11 s11
  fmul s14 s2 s1
  fmul s15 s2 s0

  ; pin texture centre (32,32) to a screen point that orbits (160,100),
  ; offset = (AX * sin, AY * cos) using the same sin/cos as the rotation
  ; p = cos*Sx + sin*Sy, q = sin*Sx - cos*Sy, then scale by z
  ldr s16 [consts @24]
  fmul s16 s16 s1
  ldr s17 [consts @28]
  fmul s17 s17 s0
  ldr s18 [consts @8]
  fadd s4 s18 s16
  ldr s19 [consts @12]
  fadd s5 s19 s17
  fmul s21 s0 s4
  fmul s7 s1 s5
  fadd s21 s21 s7
  fmul s22 s1 s4
  fmul s7 s0 s5
  fsub s22 s22 s7
  fmul s21 s21 s2
  fmul s22 s22 s2
  ldr s6 [consts @16]
  fsub s12 s6 s21
  fadd s13 s6 s22

  ldr s20 [consts @20]
  mov ny @height
:next-row
  fmov s8 s12
  fmov s9 s13
  mov nx @width
:next-pixel
  ; bias keeps the value positive so fcvtzs (truncate toward zero) floors it
  fadd s7 s8 s20
  fcvtzs tmp2 s7
  and tmp2 tmp2 @63
  fadd s7 s9 s20
  fcvtzs tmp s7
  and tmp tmp @63
  lsl tmp tmp @6
  add tmp tmp tmp2
  lsl tmp tmp @2
  add tmp tex tmp
  ldr w17 [x16 @0]
  str w17 [vptr] @4
  fadd s8 s8 s10
  fadd s9 s9 s11
  sub nx nx @1
  cbnz nx next-pixel-
  fadd s12 s12 s14
  fadd s13 s13 s15
  sub ny ny @1
  cbnz ny next-row-
})

/= 4
:consts
(for ([c const-bits]) (write-value-32 c))
/= 4
:sine-table
(for ([s sine-bits]) (write-value-32 s))
/= 4
:texture
(for ([c texels]) (write-value-32 c))
})
