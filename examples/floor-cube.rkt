#lang pisemble
;; =====================================================================
;; FLOOR CUBE - a spinning textured cube over a floor caster
;; =====================================================================
;; Each pixel casts a ray into cube local space and runs a slab test
;; against the unit box. The face is the axis with the largest entry t,
;; and the texture coordinates come from the entry point on the other
;; two axes. Rows below the horizon are a floor caster (depth = 160/dy).
;; Single precision FPU maths, s0-s29 are used.
;;
;; Build from a scratch directory, not from examples/:
;;   racket <abs path>\floor-cube.rkt   -> floor-cube.obj + floor-cube.img
(require "../lib/pi2d.rkt")

(define (f32 x)
  (integer-bytes->integer (real->floating-point-bytes x 4 #f) #f #f))

(define (clamp8 x) (max 0 (min 255 (inexact->exact (round x)))))

(define (brick-rgb u v)
  (define row (quotient v 8))
  (define off (if (even? row) 0 8))
  (define bx (modulo (+ u off) 16))
  (define by (modulo v 8))
  (if (or (= by 0) (= bx 0))
      (values 200 196 185)
      (let ([n (modulo (+ (* u 7) (* v 13) (* row 29)) 23)])
        (values (+ 150 n) (+ 60 (quotient n 2)) (+ 40 (quotient n 3))))))

;; three 64x64 copies, one per face, index = face<<12 | v<<6 | u
(define face-shade '(1.0 0.8 0.6))

(define tex-words
  (for*/list ([s face-shade] [v 64] [u 64])
    (define-values (r g b) (brick-rgb u v))
    (rgb (clamp8 (* r s)) (clamp8 (* g s)) (clamp8 (* b s)))))

(define sine-words
  (for/list ([i 256]) (f32 (sin (/ (* 2 3.141592653589793 i) 256)))))

;; byte offsets: 0 1.0, 4 -1.0, 8 0.0, 12 1/160, 16 -160, 20 100, 24 -3,
;; 28 31.5, 32 160, 36 0.01, 40 256, 44 floor light, 48 floor dark, 52 sky
(define const-words
  (list (f32 1.0) (f32 -1.0) (f32 0.0) (f32 (/ 1.0 160)) (f32 -160.0)
        (f32 100.0) (f32 -3.0) (f32 31.5) (f32 160.0) (f32 0.01)
        (f32 256.0)
        (rgb 120 110 90) (rgb 50 45 40) (rgb 20 30 70)))

(pi2d

#:init () [] {
  (debug-str "floor cube" #t)
}

;; register map for the per pixel loop
;;   s8-s10  ray position in local space (cx cy cz)
;;   s11-s13 ray origin in local space
;;   s14-s19 slab limits a = -1 - o and b = 1 - o
;;   s20-s25 per pixel step (ax) and per row step (ay)
;;   s26-s28 row start position
;;   s6, s7  floor u step and u
;;   s29     1.0
#:render
      ([vptr x0] [t x1] [tex x2] [xc x3] [yr x4] [tmp x5] [ui x6] [vi x7]
       [face x8] [addr x9] [col x10] [rows x11] [consts x12] [mask x13]
       [vz x14] [sines x15]) [] {
  adr tex tex:
  adr consts consts:
  adr sines sines:

  ; sY, cY from angle index 3t, sX, cX from index 2t
  mov vz @255
  add tmp t t
  add tmp tmp t
  and tmp tmp vz
  lsl ui tmp @2
  add ui ui sines
  ldr s0 [ui @0]
  add tmp tmp @64
  and tmp tmp vz
  lsl ui tmp @2
  add ui ui sines
  ldr s1 [ui @0]
  add tmp t t
  and tmp tmp vz
  lsl ui tmp @2
  add ui ui sines
  ldr s2 [ui @0]
  add tmp tmp @64
  and tmp tmp vz
  lsl ui tmp @2
  add ui ui sines
  ldr s3 [ui @0]

  ; R = Ry * Rx.  R0 = (cY, sY sX, sY cX), R1 = (0, cX, -sX), R2 = (-sY, cY sX, cY cX)
  ldr s4 [consts @12]
  fmul s20 s1 s4
  fmul s5 s0 s2
  fmul s21 s5 s4
  fmul s5 s0 s3
  fmul s22 s5 s4
  ldr s23 [consts @8]
  fmul s24 s3 s4
  fmul s5 s2 s4
  fneg s25 s5

  ; origin o = R2 * -3
  ldr s6 [consts @24]
  fneg s5 s0
  fmul s11 s5 s6
  fmul s5 s1 s2
  fmul s12 s5 s6
  fmul s5 s1 s3
  fmul s13 s5 s6

  ; a = -1 - o, b = 1 - o
  ldr s5 [consts @4]
  fsub s14 s5 s11
  fsub s15 s5 s12
  fsub s16 s5 s13
  ldr s5 [consts @0]
  fsub s17 s5 s11
  fsub s18 s5 s12
  fsub s19 s5 s13
  fmov s29 s5

  ; row start = R0 * -160/160 + R1 * 100/160 + R2
  ldr s6 [consts @16]
  ldr s7 [consts @20]
  fmul s26 s20 s6
  fneg s5 s0
  fadd s26 s26 s5
  fmul s27 s21 s6
  fmul s5 s24 s7
  fadd s27 s27 s5
  fmul s5 s1 s2
  fadd s27 s27 s5
  fmul s28 s22 s6
  fmul s5 s25 s7
  fadd s28 s28 s5
  fmul s5 s1 s3
  fadd s28 s28 s5

  mov yr @0
  mov rows @200
:row-loop
  ; floor depth dist = 160 / (y - 100), u step = dist / 160,
  ; u = 256 - dist (kept positive for the parity test), v = dist + 0.01 t
  sub tmp yr @100
  scvtf s0 tmp
  ldr s1 [consts @32]
  fdiv s1 s1 s0
  ldr s2 [consts @12]
  fmul s6 s1 s2
  ldr s7 [consts @40]
  fsub s7 s7 s1
  scvtf s0 t
  ldr s2 [consts @36]
  fmul s0 s0 s2
  fadd s0 s0 s1
  fcvtzs vz s0

  mov xc @320
  fmov s8 s26
  fmov s9 s27
  fmov s10 s28

:px-loop
  ; ----- slab test on X -----
  fdiv s0 s29 s8
  fcmp s0 @0
  b.gt x-pos+
  fmul s1 s17 s0
  fmul s2 s14 s0
  b x-done+
:x-pos
  fmul s1 s14 s0
  fmul s2 s17 s0
:x-done
  fmov s3 s1
  fmov s4 s2
  mov face @0

  ; ----- slab test on Y -----
  fdiv s0 s29 s9
  fcmp s0 @0
  b.gt y-pos+
  fmul s1 s18 s0
  fmul s2 s15 s0
  b y-done+
:y-pos
  fmul s1 s15 s0
  fmul s2 s18 s0
:y-done
  fcmp s1 s3
  b.le y-no-near+
  fmov s3 s1
  mov face @1
:y-no-near
  fcmp s2 s4
  b.ge y-no-far+
  fmov s4 s2
:y-no-far

  ; ----- slab test on Z -----
  fdiv s0 s29 s10
  fcmp s0 @0
  b.gt z-pos+
  fmul s1 s19 s0
  fmul s2 s16 s0
  b z-done+
:z-pos
  fmul s1 s16 s0
  fmul s2 s19 s0
:z-done
  fcmp s1 s3
  b.le z-no-near+
  fmov s3 s1
  mov face @2
:z-no-near
  fcmp s2 s4
  b.ge z-no-far+
  fmov s4 s2
:z-no-far

  ; hit when tfar > 0 and tnear <= tfar
  fcmp s4 @0
  b.le bg-pixel+
  fcmp s3 s4
  b.gt bg-pixel+

  ; ----- hit: entry point q = (p + 1) * 31.5 on each axis -----
  mov mask @63
  ldr s5 [consts @28]
  fmul s0 s3 s8
  fadd s0 s0 s11
  fmul s0 s0 s5
  fadd s0 s0 s5
  fcvtzs tmp s0
  fmul s0 s3 s9
  fadd s0 s0 s12
  fmul s0 s0 s5
  fadd s0 s0 s5
  fcvtzs ui s0
  fmul s0 s3 s10
  fadd s0 s0 s13
  fmul s0 s0 s5
  fadd s0 s0 s5
  fcvtzs vi s0

  ; choose (u, v) from the two axes that are not the face axis
  cbnz face face-not-x+
  mov tmp ui
  mov ui vi
  b uv-done+
:face-not-x
  sub addr face @1
  cbnz addr uv-done+
  mov ui vi
:uv-done

  and tmp tmp mask
  and ui ui mask
  lsl ui ui @6
  add addr tmp ui
  lsl face face @12
  add addr addr face
  lsl addr addr @2
  add addr addr tex
  ldr w10 [addr @0]
  b px-store+

:bg-pixel
  sub tmp yr @101
  lsr tmp tmp @63
  cbnz tmp bg-sky+
  mov mask @1
  fcvtzs tmp s7
  add tmp tmp vz
  and tmp tmp mask
  cbnz tmp bg-dark+
  ldr w10 [consts @44]
  b px-store+
:bg-dark
  ldr w10 [consts @48]
  b px-store+
:bg-sky
  ldr w10 [consts @52]

:px-store
  str w10 [vptr] @4
  fadd s8 s8 s20
  fadd s9 s9 s21
  fadd s10 s10 s22
  fadd s7 s7 s6
  sub xc xc @1
  cbnz xc px-loop-

  fsub s26 s26 s23
  fsub s27 s27 s24
  fsub s28 s28 s25
  add yr yr @1
  sub rows rows @1
  cbnz rows row-loop-
}

{
/= 4
:consts
(for ([w const-words]) (write-value-32 w))
:sines
(for ([w sine-words]) (write-value-32 w))
:tex
(for ([w tex-words]) (write-value-32 w))
})
