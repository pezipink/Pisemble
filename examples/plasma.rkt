#lang pisemble
;; =====================================================================
;; PLASMA - a 2D program built on pi2d.rkt
;; =====================================================================
;; Copy this file to start a new program. You write hooks with pi2d:
;;   #:init     called once, after the framebuffer and MMU are up
;;   #:render   x0 = back buffer (width x height, 32 bit pixels made with rgb)
;;              x1 = frame number
;; The platform flips the page and waits for the next 1/60s tick after
;; each render. Tables can be computed in Racket at assembly time.
;;
;; Build:  racket plasma.rkt            -> plasma.obj + plasma.img (beside this file)
;; Run:    qemu-system-aarch64 -M raspi3b -kernel plasma.img -serial null -serial stdio
(require "../lib/pi2d.rkt")

;; 256 entry sine, values 0..63
(define sine-bytes
  (for/list ([i 256])
    (exact-round (* 31.5 (+ 1 (sin (/ (* 2 pi i) 256)))))))

;; 256 colours, three sines out of phase
(define palette-colours
  (for/list ([i 256])
    (define (channel phase)
      (exact-round (* 127.5 (+ 1 (sin (+ phase (/ (* 2 pi i) 256)))))))
    (rgb (channel 0) (channel 2.094) (channel 4.189))))

(pi2d

#:init () [] {
  (debug-str "plasma" #t)
}

; colour(x, y) = palette[ sin(ya) + sin(yb) + t + sin(xa) + sin(xd) ]
#:render
      ([vptr x0] [t x1] [sines x2] [colours x3] [mask x4] [ny x5] [nx x6]
       [ya x7] [yb x8] [xa x9] [xd x10] [xd-row x11] [row-sum x12]
       [tmp x13] [v x14]) [] {
  adr sines sine-table:
  adr colours palette:
  mov mask @255
  mov ya t
  add yb t t
  add yb yb t                 ; 3t
  mov xd-row @0
  sub xd-row xd-row t         ; -t
  mov ny @height
:next-row
  ; row-sum = sin(ya) + sin(yb) + t
  and tmp ya mask
  add tmp sines tmp
  ldrb w13 [x13 @0]
  add row-sum tmp t
  and tmp yb mask
  add tmp sines tmp
  ldrb w13 [x13 @0]
  add row-sum row-sum tmp
  add xa t t                  ; 2t
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
  and v v mask
  lsl v v @2
  add v colours v
  ldr w14 [x14 @0]            ; palette[v]
  str w14 [vptr] @4
  add xa xa @3
  add xd xd @1
  sub nx nx @1
  cbnz nx next-pixel-
  add ya ya @2
  add yb yb @1
  add xd-row xd-row @2
  sub ny ny @1
  cbnz ny next-row-
}

{
:sine-table
(write-values sine-bytes)
/= 4
:palette
(for ([c palette-colours])
  (write-value-32 c))
}
)
