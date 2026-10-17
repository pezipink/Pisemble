#lang pisemble
;; =====================================================================
;; RINGS - concentric coloured rings at 1024x768, built on pi2d.rkt
;; =====================================================================
;; Uses structured control flow (/whilenz, /cond), the struct and stack
;; helpers, the barrel shifter (add x1 x2 x3 LSL @n) and NEON (movi, st1).
;;
;; Sixteen filled discs are drawn from the largest to the smallest. Each
;; disc is one colour from a 64 colour plasma palette, and the palette
;; rotates one step every FRAMES-PER-STEP frames, so the rings appear to move outwards.
;;
;; The half-widths of each disc are computed in Racket when the program is
;; assembled. The table is a flat array of `row` records, one for every row
;; of every disc, laid out by the /struct below.
;;
;; Build:  racket rings.rkt            -> rings.obj + rings.img (beside this file)
;; Run:    qemu-system-aarch64 -M raspi3b -kernel rings.img -serial null -serial stdio
(require "../lib/pi2d.rkt")
(require "../lib/struct.rkt")

(define disc-radii
  (for/list ([k (in-range 16)])
    (* 23 (- 16 k))))

; a plasma style palette: three sine waves a third of a cycle apart, so the
; last entry blends back into the first
(define colour-count 64)
(define TAU (* 8 (atan 1)))
(define colour-table
  (for/list ([i (in-range colour-count)])
    (define t (/ (* TAU i) colour-count))
    (define (channel phase)
      (inexact->exact (round (+ 128 (* 127 (sin (+ t phase)))))))
    (rgb (channel 0) (channel (/ TAU 3)) (channel (* 2 (/ TAU 3))))))

; frames per colour step: 15 at 60 FPS is four steps a second
(define FRAMES-PER-STEP 15)

; one record per row: the disc it belongs to, its dy, and its half-width.
; Accessors: (row/disc-get reg base), (row/dy-get reg base), (row/half-get reg base).
; They must be parenthesised inside { }, like load-immediate.
(/struct row ([disc 4] [dy 4] [half 4]))

; (list disc dy half-width) for every row of every disc, in table order
(define disc-rows
  (for*/list ([k (in-range (length disc-radii))]
              [r (in-value (list-ref disc-radii k))]
              [dy (in-range (+ r 1))])
    (list k dy (integer-sqrt (- (* r r) (* dy dy))))))

(pi2d #:width 1024 #:height 768 #:bpp 32

#:init () [] {
  (debug-str "rings" #t)
}

; x0 = back page, x1 = frame number. Centre is (512, 384), so the largest disc spans rows 16..752.
#:render () [] {
  ; clear the page to black, 16 bytes at a time
  movi v0.2d @0                          ; NEON: v0 = 128 bits of zero
  mov x12 x0
  (load-immediate x13 196608)            ; 3145728 bytes / 16
  (/whilenz x13 {
    st1 {v0.16b} [x12]                   ; NEON: store all 16 bytes of v0
    add x12 x12 @16
    sub x13 x13 @1
  })

  (load-immediate x5 FRAMES-PER-STEP)
  udiv x1 x1 x5                          ; phase = frame / FRAMES-PER-STEP
  adr x2 rows:
  (load-immediate x3 (length disc-rows))  ; rows left
  mov x15 @63                             ; colour mask, colour-count - 1

  (/whilenz x3 {
    (row/disc-get w4 x2)                  ; k
    (row/dy-get w7 x2)                    ; dy
    (row/half-get w8 x2)                  ; half-width
    add x2 x2 @12                         ; sizeof row
    ; colour = colours[(k + phase) & 3]
    add x14 x4 x1
    and x14 x14 x15
    adr x9 colour-table:
    add x9 x9 x14 LSL @2
    ldr w9 [x9 @0]
    ; row above is 384 - dy, row below is 384 + dy, pitch 4096 bytes
    mov x14 @384
    sub x14 x14 x7
    add x10 x0 x14 LSL @12
    mov x14 @384
    add x14 x14 x7
    add x11 x0 x14 LSL @12
    ; span from x = 512 - w to 512 + w, 4 bytes a pixel
    mov x14 @512
    sub x14 x14 x8
    add x12 x10 x14 LSL @2
    add x13 x8 x8
    add x13 x13 @1
    bl span-fill:
    add x12 x11 x14 LSL @2
    add x13 x8 x8
    add x13 x13 @1
    bl span-fill:
    sub x3 x3 @1
  })
}

; x9 = colour, x12 = first pixel, x13 = pixel count (at least 1)
(subr span-fill () [] {
  (/whilenz x13 {
    str w9 [x12] @4
    sub x13 x13 @1
  })
})

{
:colour-table
(for ([c colour-table])
  (write-value-32 c))
:rows
(for ([r disc-rows])
  (write-value-32 (car r))               ; disc
  (write-value-32 (cadr r))              ; dy
  (write-value-32 (caddr r)))            ; half
}
)
