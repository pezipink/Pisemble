#lang pisemble
;; =====================================================================
;; TEXTURED CUBE OVER A FLOOR CASTER - a demo built on platform.rkt
;; =====================================================================
;; The background is a Wolfenstein style floor caster. Each row below the
;; horizon is a ground distance, and each pixel samples a stone texture at
;; the matching world position while the camera drifts forward and sways.
;; A cube tumbles over it. Its visible faces are triangles with perspective
;; correct texture mapping: u/z, v/z and 1/z are interpolated across each
;; triangle, and one division per pixel recovers u and v.
;;
;; The AArch64 side only has mul, asr and udiv here, so fixed point is used
;; throughout: sines are scaled by 256 and 1/z is stored as 2^24 / z.
;; Build from this folder with: racket textured-cube.rkt
(require "platform.rkt")

(define horizon 88)                  ; last sky row; the floor starts below it
(define cam-height 48)               ; camera height above the floor, in texels
(define floor-focal 160)             ; 90 degree field of view
(define cube-half 45)                ; world units from the cube centre to a face
(define cube-distance 200)           ; cube centre to camera, along z
(define cube-focal 140)
(define cube-cx 160)
(define cube-cy 100)
(define sky-colour (rgb 20 20 60))

(define (signed64 n) (bitwise-and n #xFFFFFFFFFFFFFFFF))

(define sine-values
  (for/list ([k 256])
    (exact-round (* 256 (sin (* 2 pi (/ k 256)))))))

;; vertex i: bit 0 = x, bit 1 = y, bit 2 = z, set means positive
(define (corner bit) (if bit cube-half (- cube-half)))
(define vertex-list
  (for/list ([i 8])
    (list (corner (bitwise-bit-set? i 0))
          (corner (bitwise-bit-set? i 1))
          (corner (bitwise-bit-set? i 2)))))
(define (vertex i) (list-ref vertex-list i))

(define (vec- a b) (for/list ([x a] [y b]) (- x y)))
(define (dot a b) (for/sum ([x a] [y b]) (* x y)))
(define (cross a b)
  (list (- (* (list-ref a 1) (list-ref b 2)) (* (list-ref a 2) (list-ref b 1)))
        (- (* (list-ref a 2) (list-ref b 0)) (* (list-ref a 0) (list-ref b 2)))
        (- (* (list-ref a 0) (list-ref b 1)) (* (list-ref a 1) (list-ref b 0)))))

;; Wind each face so its normal points into the cube. Front faces then give
;; a positive screen-space area, and the assembly culls on that sign.
(define (inward-loop loop)
  (define pts (map vertex loop))
  (define normal (cross (vec- (list-ref pts 1) (list-ref pts 0))
                        (vec- (list-ref pts 2) (list-ref pts 0))))
  (define centre (for/list ([k 3]) (for/sum ([p pts]) (list-ref p k))))
  (if (positive? (dot normal centre))
      (cons (list-ref loop 0) (reverse (cdr loop)))
      loop))

(define face-loops '((0 1 3 2) (4 5 7 6) (0 2 6 4) (1 5 7 3) (0 4 5 1) (2 3 7 6)))
(define uv-corners '((0 0) (64 0) (64 64) (0 64)))

;; each triangle is three (vertex u v) corners, 9 bytes in the table
(define triangle-list
  (append*
   (for/list ([loop (map inward-loop face-loops)])
     (define c (for/list ([v loop] [uv uv-corners])
                 (list v (list-ref uv 0) (list-ref uv 1))))
     (list (list (list-ref c 0) (list-ref c 1) (list-ref c 2))
           (list (list-ref c 0) (list-ref c 2) (list-ref c 3))))))

;; Check the culling convention at identity rotation: only the -z face
;; (nearest the camera) is front facing, so exactly its triangles have a
;; positive screen area.
(define (project-identity i)
  (define p (vertex i))
  (define zc (+ (list-ref p 2) cube-distance))
  (list (+ cube-cx (/ (* cube-focal (list-ref p 0)) zc))
        (+ cube-cy (/ (* cube-focal (list-ref p 1)) zc))))
(for ([tri triangle-list])
  (define pts (for/list ([c tri]) (project-identity (list-ref c 0))))
  (define (px k) (list-ref (list-ref pts k) 0))
  (define (py k) (list-ref (list-ref pts k) 1))
  (define area2 (- (* (- (px 1) (px 0)) (- (py 2) (py 0)))
                   (* (- (py 1) (py 0)) (- (px 2) (px 0)))))
  (define front? (for/and ([c tri]) (= (list-ref (vertex (list-ref c 0)) 2) (- cube-half))))
  (unless (eq? (positive? area2) front?)
    (error "cube winding convention broken")))

(define (floor-texel x y)
  (define cell (modulo (+ (quotient x 16) (quotient y 16)) 2))
  (cond [(or (zero? (modulo x 16)) (zero? (modulo y 16))) (rgb 52 46 42)]
        [(zero? cell) (rgb 150 132 112)]
        [else (rgb 124 108 92)]))

(define (cube-texel x y)
  (if (or (< x 3) (> x 60) (< y 3) (> y 60))
      (rgb 245 245 235)
      (rgb (+ 30 (* 3 x)) (+ 30 (* 3 y)) (- 255 (* 2 (+ x y))))))

; Rows this close to the horizon sample the floor texture at distances so
; large that neighbouring rows jump across the texture. Those rows are drawn
; as a haze that fades from the sky colour into the floor's average colour.
(define sky-rgb '(20 20 60))
(define fog-rows 64)
(define floor-average
  (for/list ([shift '(0 8 16)])
    (quotient (for*/sum ([y 64] [x 64])
                (bitwise-and (arithmetic-shift (floor-texel x y) (- shift)) 255))
              (* 64 64))))
(define (fog-rgb i)
  (for/list ([s sky-rgb] [f floor-average])
    (+ s (quotient (* (- f s) (+ i 1)) fog-rows))))

(demo {

(subr Demo-init () [] {
  (debug-str "textured cube" #t)
})

; x0 = page, x1 = frame. Paints the sky, then casts the floor.
(subr draw-floor ([vptr x0] [frame x1] [sines x2] [m255 x3] [m63 x4] [pz x5] [pxf x6]
                  [row x7] [dist x8] [dr x9] [rowoff x10] [ptr x11] [col x12] [lat x13]
                  [t1 x14] [t2 x15] [hf x16] [kr x17]) [] {
  mov m255 @255
  mov m63 @63
  mov t1 @96
  mul pz frame t1                  ; camera moves 0.375 texel per frame
  and t2 frame m255
  adr sines sine-table:
  lsl t2 t2 @3
  add t2 sines t2
  ldr pxf [t2]
  lsl pxf pxf @4                   ; sway of +-16 texels
  (load-immediate hf (* cam-height floor-focal 256))
  mov kr @6554                     ; 2^20 / 160, the half width in pixels

  mov ptr vptr
  (load-immediate t2 sky-colour)
  (load-immediate t1 (* (+ horizon 1) width))
:sky
  str w15 [ptr] @4
  sub t1 t1 @1
  cbnz t1 sky-

  mov row @(+ horizon 1)
  adr t2 fog-table:
:fog-row
  sub t1 row @(+ horizon 1)
  lsl t1 t1 @2
  add t1 t2 t1
  ldr w9 [t1]
  mov t1 @(* width 4)
  mul ptr row t1
  add ptr ptr vptr
  mov col @width
:fog-px
  str w9 [ptr] @4
  sub col col @1
  cbnz col fog-px-
  add row row @1
  cmp row @(+ horizon fog-rows 1)
  b.ne fog-row-

  mov row @(+ horizon fog-rows 1)
:floor-row
  sub t1 row @horizon
  udiv dist hf t1                  ; distance in texels, scaled by 256
  mul dr dist kr
  add t1 pz dist
  asr t1 t1 @8
  and t1 t1 m63
  lsl rowoff t1 @6                 ; texel row of this ground line, times 64
  mov t1 @(* width 4)
  mul ptr row t1
  add ptr ptr vptr
  mov col @0
:floor-col
  sub lat col @(quotient width 2)
  mul t1 lat dr
  asr t1 t1 @20                    ; lateral offset in texels, scaled by 256
  add t1 t1 pxf
  asr t1 t1 @8
  and t1 t1 m63
  add t1 t1 rowoff
  lsl t1 t1 @2
  adr t2 floor-tex:
  add t2 t2 t1
  ldr w14 [t2]
  str w14 [ptr] @4
  add col col @1
  cmp col @width
  b.ne floor-col-
  add row row @1
  cmp row @height
  b.ne floor-row-
})

; Rotates the eight cube corners (Y then X) and projects them.
; Each vertex record in vbuf is 32 bytes: screen x, screen y, and 1/z.
(subr project-cube ([frame x1] [sines x2] [m255 x3] [ay x4] [ax x5] [siny x6] [cosy x7]
                    [sinx x8] [cosx x9] [vt x10] [vb x11] [count x12] [vx x13] [vy x14]
                    [vz x15] [xr x16] [zr x17] [yr x18] [zz x19] [zc x20] [ir x21]
                    [kk x22] [kf x23] [sxr x24] [syr x25] [t1 x26] [t2 x27]) [] {
  adr sines sine-table:
  mov m255 @255
  lsl ay frame @1
  and ay ay m255
  and ax frame m255

  lsl t1 ay @3
  add t1 sines t1
  ldr siny [t1]
  add t2 ay @64
  and t2 t2 m255
  lsl t2 t2 @3
  add t2 sines t2
  ldr cosy [t2]

  lsl t1 ax @3
  add t1 sines t1
  ldr sinx [t1]
  add t2 ax @64
  and t2 t2 m255
  lsl t2 t2 @3
  add t2 sines t2
  ldr cosx [t2]

  adr vt vertex-table:
  adr vb vbuf:
  mov count @8
  (load-immediate kk (expt 2 24))
  mov kf @cube-focal
:vertex
  ldr vx [vt] @8
  ldr vy [vt] @8
  ldr vz [vt] @8

  ; rotate about Y
  mul t1 cosy vx
  mul t2 siny vz
  add xr t1 t2
  asr xr xr @8
  mul t1 cosy vz
  mul t2 siny vx
  sub zr t1 t2
  asr zr zr @8

  ; rotate about X
  mul t1 cosx vy
  mul t2 sinx zr
  sub yr t1 t2
  asr yr yr @8
  mul t1 sinx vy
  mul t2 cosx zr
  add zz t1 t2
  asr zz zz @8

  add zc zz @cube-distance
  udiv ir kk zc
  mul t1 xr kf
  mul t1 t1 ir
  asr t1 t1 @24
  add sxr t1 @cube-cx
  mul t2 yr kf
  mul t2 t2 ir
  asr t2 t2 @24
  add syr t2 @cube-cy

  str sxr [vb] @8
  str syr [vb] @8
  str ir [vb] @16
  sub count count @1
  cbnz count vertex-
})

; Draws one triangle of the cube (x19 = its 9 byte record, x21 = page).
; Pixels inside the triangle get u = sum(e*u*ir) / sum(e*ir), and the same for v.
; Triangles with area <= 0 are back facing and skipped.
(subr draw-triangle ([tp x19] [page x21] [tb x0] [t1 x2] [px x3] [py x4] [e0 x5]
                     [ax x6] [ay x7] [bx x8] [by x9] [cx x10] [cy x11] [s1 x12] [s2 x13]
                     [e1 x14] [e2 x15] [nu x16] [nv x17] [vb x18] [den x1] [rowb x22])
                    [x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x21 x22] {
  adr tb tri-scratch:
  adr vb vbuf:
  (for ([k 3])
    {
      ldrb w2 [tp] @1
      ldrb w3 [tp] @1
      ldrb w4 [tp] @1
      lsl t1 t1 @5
      add t1 t1 vb
      ldr x5 [t1]
      ldr x14 [t1 @8]
      ldr x15 [t1 @16]
      str x5 [tb @(* 8 k)]
      str x14 [tb @(+ 24 (* 8 k))]
      str x15 [tb @(+ 96 (* 8 k))]
      mul x16 x15 px
      str x16 [tb @(+ 48 (* 8 k))]
      mul x16 x15 py
      str x16 [tb @(+ 72 (* 8 k))]
    })
  ldr ax [tb @0]
  ldr bx [tb @8]
  ldr cx [tb @16]
  ldr ay [tb @24]
  ldr by [tb @32]
  ldr cy [tb @40]

  ; twice the signed area; back faces have it <= 0
  sub s1 bx ax
  sub s2 cy ay
  mul nu s1 s2
  sub s1 by ay
  sub s2 cx ax
  mul s1 s1 s2
  sub nu nu s1
  cmp nu @0
  b.le tri-skip+

  ; bounding box
  cmp ax bx
  csel s1 ax bx lt
  cmp s1 cx
  csel s1 s1 cx lt
  str s1 [tb @120]
  cmp ax bx
  csel s1 ax bx gt
  cmp s1 cx
  csel s1 s1 cx gt
  str s1 [tb @128]
  cmp ay by
  csel s1 ay by lt
  cmp s1 cy
  csel s1 s1 cy lt
  str s1 [tb @136]
  cmp ay by
  csel s1 ay by gt
  cmp s1 cy
  csel s1 s1 cy gt
  str s1 [tb @144]

  mov rowb @(* width 4)
  ldr py [tb @136]
:tri-row
  ldr px [tb @120]
:tri-col
  ; edge functions: each is positive on the side of its opposite vertex
  sub s1 cx bx
  sub s2 py by
  mul e0 s1 s2
  sub s1 cy by
  sub s2 px bx
  mul s1 s1 s2
  sub e0 e0 s1

  sub s1 ax cx
  sub s2 py cy
  mul e1 s1 s2
  sub s1 ay cy
  sub s2 px cx
  mul s1 s1 s2
  sub e1 e1 s1

  sub s1 bx ax
  sub s2 py ay
  mul e2 s1 s2
  sub s1 by ay
  sub s2 px ax
  mul s1 s1 s2
  sub e2 e2 s1

  cmp e0 @0
  b.lt tri-next+
  cmp e1 @0
  b.lt tri-next+
  cmp e2 @0
  b.lt tri-next+

  ldr s1 [tb @48]
  mul nu e0 s1
  ldr s1 [tb @56]
  mul s1 e1 s1
  add nu nu s1
  ldr s1 [tb @64]
  mul s1 e2 s1
  add nu nu s1
  ldr s1 [tb @72]
  mul nv e0 s1
  ldr s1 [tb @80]
  mul s1 e1 s1
  add nv nv s1
  ldr s1 [tb @88]
  mul s1 e2 s1
  add nv nv s1
  ldr s1 [tb @96]
  mul den e0 s1
  ldr s1 [tb @104]
  mul s1 e1 s1
  add den den s1
  ldr s1 [tb @112]
  mul s1 e2 s1
  add den den s1

  udiv s1 nu den
  udiv s2 nv den
  mov e1 @63
  cmp s1 @63
  csel s1 e1 s1 hi
  cmp s2 @63
  csel s2 e1 s2 hi
  lsl s2 s2 @6
  add s2 s2 s1
  lsl s2 s2 @2
  adr s1 cube-tex:
  add s1 s1 s2
  ldr w14 [s1]

  mul s1 py rowb
  add s1 s1 page
  lsl s2 px @2
  add s1 s1 s2
  str w14 [s1]
:tri-next
  add px px @1
  ldr s1 [tb @128]
  cmp px s1
  b.le tri-col-
  add py py @1
  ldr s1 [tb @144]
  cmp py s1
  b.le tri-row-
:tri-skip
})

; x0 = page, x1 = frame
(subr Draw-frame ([vptr x0] [frame x1] [tri x25] [tcount x26]) [] {
  bl draw-floor:
  bl project-cube:
  adr tri tri-table:
  mov tcount @12
:tri-loop
  mov x21 vptr
  mov x19 tri
  bl draw-triangle:
  add tri tri @9
  sub tcount tcount @1
  cbnz tcount tri-loop-
})

/= 8
:sine-table
(for ([v sine-values]) (write-value-64 (signed64 v)))

:vertex-table
(for* ([v vertex-list] [c v]) (write-value-64 (signed64 c)))

:tri-table
(for* ([t triangle-list] [c t]) (write-values c))

/= 4
:vbuf
(reserve (* 8 32))
:tri-scratch
(reserve 160)

:floor-tex
(for* ([y 64] [x 64]) (write-value-32 (floor-texel x y)))

:fog-table
(for ([i fog-rows]) (write-value-32 (apply rgb (fog-rgb i))))

:cube-tex
(for* ([y 64] [x 64]) (write-value-32 (cube-texel x y)))
})
