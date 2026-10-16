#lang pisemble
;; =====================================================================
;; PLATFORM - everything that isn't your graphics code (QEMU raspi3b, EL2)
;; =====================================================================
;; Boots core 0, brings up the mini UART, asks the GPU for a double-height
;; 32 bit (or 8 bit palette) framebuffer, turns on the MMU and caches, then
;; runs the frame loop:
;;
;;     Demo-init                       once
;;     loop:  Draw-frame  x0 = back page, x1 = frame number
;;            flip to the page just drawn
;;            wait for the next 1/FPS tick (generic timer)
;;
;; Usage, in your demo file:
;;     (require "platform.rkt")
;;     (demo { ...code and data, defining Demo-init and Draw-frame... })
;;     (demo #:bpp 8 { ... })    for 8 bit palette mode: 1 byte per pixel
;; (demo ...) assembles your code into demo.obj, then builds kernel8.img from
;; this platform code + demo.obj. Both go to the current directory.
;; ROW-BYTES and PAGE-BYTES follow the chosen depth.
;;
;; Rules for demo code:
;;  * Demo-init and Draw-frame may use x0-x28 freely.
;;  * Labels starting with a capital letter are global. Only use capitals for
;;    Demo-init and Draw-frame, and don't call platform globals: the linker
;;    refuses a demo.obj that references a symbol defined in the platform.
;;  * demo.obj is linked at a 4KB boundary, so /= up to $1000 works as expected.
;;
;; Also assembled into demo.obj for you (lowercase, call with bl):
;;     clear-screen   x0 = page, w1 = colour (palette index in 8 bit mode)
;;     fill-rect      x0 = page, x1 = x, x2 = y, x3 = w, x4 = h, w5 = colour
;;     set-palette    x0 = first index, x1 = count, x2 = address of rgb words
;;     send-char      so debug-str / debug-reg / debug-str-reg work
;; All preserve registers.
;;
;; Image layout:  $80000 platform | vectors | page tables | demo.obj
;; =====================================================================
(require (for-syntax syntax/parse racket/stxparam racket/syntax))
(require syntax/parse/define)
(require racket/stxparam)
(require "../wolf3d/periph.rkt" "../wolf3d/stack.rkt" "../wolf3d/struct.rkt" "../wolf3d/expr.rkt")
(provide (all-defined-out)
         (all-from-out "../wolf3d/periph.rkt" "../wolf3d/stack.rkt"
                       "../wolf3d/struct.rkt" "../wolf3d/expr.rkt"))

;; ---- screen ----
(define width 320)
(define height 200)
(define FPS 60)
;; bits per pixel: 32 (direct colour) or 8 (palette). Set with (demo #:bpp 8 ...).
(define BPP 32)
(define ROW-BYTES (* width 4))
(define PAGE-BYTES (* width height 4))
(define (set-bpp! n)
  (unless (memv n '(8 32)) (error "bpp must be 8 or 32, got" n))
  (set! BPP n)
  (set! ROW-BYTES (* width (quotient n 8)))
  (set! PAGE-BYTES (* ROW-BYTES height)))

;; 32 bit pixel, or a palette entry in 8 bit mode. RGB order, so red is the low byte.
(define (rgb r g b)
  (+ #xFF000000 (arithmetic-shift b 16) (arithmetic-shift g 8) r))

;; ---- memory map ----
(define ATTR-DEV 0)   ; Device-nGnRnE
(define ATTR-NC  2)   ; Normal non-cacheable: the framebuffer
(define ATTR-WB  3)   ; Normal write-back: RAM
(define L2-GPU-START    (- 512 (quotient 80 2)))   ; 2MB blocks from here up are GPU memory
(define L2-PERIPH-START 504)                       ; 0x3F000000

;; QEMU -kernel enters at EL2. Patches op1 of an EL1 system register opcode to EL2.
(define (el-op base) (+ base (arithmetic-shift 4 16)))
(define (block-desc attr) (+ $741 (* attr 4)))
(define (attr-flip from to) (* 4 (bitwise-xor from to)))

;; =====================================================================
;; debug output over the mini UART (protocol understood by comms.fsx)
;; =====================================================================
(define-syntax (debug-reg stx)
  (syntax-parse stx
    [(_ r:register)
     #'{
        (PUSH x0)
        (PUSH r)
        mov w0 @(if `r.is32 2 3)
        bl send-char:
        (POP x0)
        bl send-char:
        (for ([_ (in-range (if `r.is32 3 7))])
          { lsr x0 x0 @8
            bl send-char: })
        (POP x0) }]
    [(_ r:register rn:register ...+)
     #'(begin (debug-reg r)
              (debug-reg rn ...))]))

(define-syntax-parser debug-str-reg
  [(_ str:string reg:register)
   #'{
      (debug-str str #f)
      (debug-reg reg)
     }])

;; send-char with its own lowercase literal, so demo.obj has no global references
(define-syntax-parser local-send-char
  [(_) #'{
     (subr send-char () [x0 x1 x2 x3] {
        ldr  x1 uart-base:
        mov  x2 @$20
     :wait
        ldr  w3 (x1 @$54)       ; AUX_MU_LSR: bit 5 = transmitter empty
        and  w3 w2 w3
        cbz  w3 wait-
        strb w0 [x1 @$40]})     ; AUX_MU_IO
     /= 8
     :uart-base (write-value-64 (+ PERIPH-BASE $215000))
  }])

;; =====================================================================
;; helpers assembled into demo.obj
;; =====================================================================
(define-syntax-parser gfx-library
  [(_) #'{
; x0 = page, w1 = colour (palette index in 8 bit mode)
(subr clear-screen ([vptr x0] [colour w1]) [x0 x1 x2 x3] {
  mov w3 colour
  (when (= BPP 8)
    { mov x2 @$FF
      and x3 x3 x2
      lsl x2 x3 @8
      orr x3 x3 x2
      lsl x2 x3 @16
      orr x3 x3 x2 })
  lsl x2 x3 @32
  orr x3 x3 x2
  (load-immediate x2 (quotient PAGE-BYTES 64))
:next
  stp x3 x3 [vptr @0]
  stp x3 x3 [vptr @16]
  stp x3 x3 [vptr @32]
  stp x3 x3 [vptr @48]
  add vptr vptr @64
  sub x2 x2 @1
  cbnz x2 next-
})

; x0 = page, x1 = x, x2 = y, x3 = w, x4 = h, w5 = colour. No clipping, w and h >= 1.
(subr fill-rect
      ([vptr x0] [px x1] [py x2] [rw x3] [rh x4] [colour w5]
       [pitch x6] [cnt x7] [row x8])
      [x0 x1 x2 x3 x4 x6 x7 x8] {
  (load-immediate pitch ROW-BYTES)
  mul py py pitch
  add vptr vptr py
  (when (= BPP 32) { lsl px px @2 })
  add vptr vptr px
  (/whilenz rh {
    mov row vptr
    mov cnt rw
    (/whilenz cnt {
      (if (= BPP 8)
          { strb w5 [x8] @1 }
          { str w5 [x8] @4 })
      sub cnt cnt @1
    })
    add vptr vptr pitch
    sub rh rh @1
  })
})

; 8 bit mode: load palette entries.  x0 = first index, x1 = count (1-256),
; x2 = address of count 32 bit colours made with rgb.
(subr set-palette ([first x0] [count x1] [src x2] [msg x3] [dst x4] [n x6]) [x0 x1 x2 x3 x4 x5 x6 x8] {
  adr msg palette-msg:
  mov w5 @0
  str w5 [msg @4]               ; the GPU overwrites these with its response
  str w5 [msg @16]
  str w0 [msg @20]
  str w1 [msg @24]
  add dst msg @28
  mov n count
:copy
  ldr w5 [x2] @4
  str w5 [x4] @4
  sub n n @1
  cbnz n copy-
  mov x8 msg
  bl send-vcore-msg:
})

(mailbox-routine)
(local-send-char)

/= 16
:palette-msg
  (write-value-32 (* 264 4))
  (write-value-32 0)
  (write-value-32 $4800B)       ; set palette
  (write-value-32 (* 258 4))
  (write-value-32 0)
  (write-value-32 0)            ; first index
  (write-value-32 0)            ; count
  (reserve (* 256 4))           ; colours
  (write-value-32 0)            ; end tag
  }])

;; send-vcore-msg: x8 = 16 byte aligned property message.  Used by the platform
;; and by set-palette.  Cleans the whole message from the cache first.
(define-syntax-parser mailbox-routine
  [(_) #'{
(subr send-vcore-msg () [x0 x1 x2 x3 x8] {
     mov x0 x8
     ldr w1 [x8 @0]              ; message size
     add x1 x8 x1
:clean
     (write-value-32 $d50b7e20)  ; dc civac, x0
     add x0 x0 @64
     cmp x0 x1
     b.cc clean-
     (write-value-32 $d5033f9f)  ; dsb sy
     ldr x0 vc-mbox:
:wait
     ldr w1 (x0 @$18)            ; status
     mov x2 @1
     lsl x2 x2 @31               ; full
     and x1 x1 x2
     cbnz x1 wait-
     mov x2 x8
     mov x3 @8                   ; property channel
     orr x2 x2 x3
     str w2 (x0 @$20)            ; write
:wait
     ldr w1 (x0 @$18)
     mov x2 @1
     lsl x2 x2 @30               ; empty
     and x1 x1 x2
     cbnz x1 wait-
     ldr w1 (x0 @$0)             ; read
     mov x2 @%1111
     and x1 x1 x2
     sub x1 x1 @8
     cbnz x1 wait-
})
/= 8
:vc-mbox (write-value-64 (+ PERIPH-BASE $00B880))
  }])

;; =====================================================================
;; the demo wrapper
;; =====================================================================
(define-syntax-parser demo
  [(_ (~optional (~seq #:bpp bpp:expr) #:defaults ([bpp #'32])) body ...)
   #'(begin
       (set-bpp! bpp)
       (aarch64 "demo.obj" [] body ... { (gfx-library) })
       (platform-image))])

;; =====================================================================
;; MMU: identity map, RAM write-back, GPU memory non-cacheable, peripherals device
;; =====================================================================
;; in: x0 = table pointer, x2 = descriptor, x5 = 2MB.  n must be > 0.
(define-syntax-parser fill-blocks
  [(_ n:expr)
   #'{
      (load-immediate x4 n)
    :fill
      str x2 [x0] @8
      add x2 x2 x5
      sub x4 x4 @1
      cbnz x4 fill-
     }])

(define-syntax-parser setup-mmu
  [(_) #'{
    (load-immediate x0 $80803520)         ; T0SZ=32 (4GB), WB/WB, inner shareable, 4KB granule
    (write-value-32 (el-op $d5182040))    ; msr tcr_el2, x0
    (load-immediate x0 $FF440400)         ; attr0 nGnRnE, attr1 nGnRE, attr2 NC, attr3 WB
    (write-value-32 (el-op $d518a200))    ; msr mair_el2, x0
    adr x0 id_pgd:
    (write-value-32 (el-op $d5182000))    ; msr ttbr0_el2, x0

    ;; level 1: [0] -> level 2 table, [1] device (ARM local peripherals)
    adr x0 id_pgd:
    (load-immediate x3 $1000)
    add x10 x0 x3
    mov x3 @3
    orr x1 x10 x3
    str x1 [x0] @8
    (load-immediate x2 $40000741)
    str x2 [x0] @8

    ;; level 2: 512 x 2MB blocks = first 1GB
    mov x0 x10
    (load-immediate x2 (block-desc ATTR-WB))
    (load-immediate x5 $200000)
    (fill-blocks L2-GPU-START)
    (load-immediate x6 (attr-flip ATTR-WB ATTR-NC))
    eor x2 x2 x6
    (fill-blocks (- L2-PERIPH-START L2-GPU-START))
    (load-immediate x6 (attr-flip ATTR-NC ATTR-DEV))
    eor x2 x2 x6
    (fill-blocks (- 512 L2-PERIPH-START))

    (write-value-32 $d5033f9f)            ; dsb sy
    (write-value-32 (el-op $d508871f))    ; tlbi alle2
    (write-value-32 $d5033f9f)            ; dsb sy
    (write-value-32 $d5033fdf)            ; isb

    (write-value-32 (el-op $d5381000))    ; mrs x0, sctlr_el2
    (load-immediate x1 $1005)             ; I | C | M
    orr x0 x0 x1
    (write-value-32 (el-op $d5181000))    ; msr sctlr_el2, x0
    (write-value-32 $d5033f9f)            ; dsb sy
    (write-value-32 $d5033fdf)            ; isb
  }])

;; =====================================================================
;; the image
;; =====================================================================
(define-syntax-parser platform-image
  [(_) #'(aarch64 "kernel8.img" ["demo.obj"] {

     ; only core 0 runs
     mrs x0 mpidr_el1
     mov x1 @$3
     and x0 x0 x1
     cbz x0 main:
:hang
     wfe
     b hang-

:main
    ldr x1 stack-top:
    mov sp x1
    (init-uart)

    (write-value-32 $d5384240)            ; mrs x0, CurrentEL
    cmp x0 @8
    b.ne wrong-el+

    adr x0 vectors:
    (write-value-32 (el-op $d518c000))    ; msr vbar_el2, x0

    ; framebuffer request (MMU still off)
    adr x8 mbox-msg:
    bl send-vcore-msg:
    adr x1 mbox-msg:
    ldr w0 [x1 @4]
    mov x2 @1
    lsl x2 x2 @31
    cmp x0 x2
    b.ne no-fb+
    adr x0 fb:
    ldr w0 [x0]
    ldr x1 bus-mask:
    and x0 x0 x1                          ; GPU bus address -> ARM physical
    cbz x0 no-fb+
    adr x1 vmem:
    str x0 [x1]

    (setup-mmu)
    (debug-str "platform up" #t)

    bl Demo-init:
:frame-loop
    bl back-buffer:
    ldr x1 frame-count:
    bl Draw-frame:
    bl flip:
    bl wait-frame:
    ldr x1 frame-count:
    add x1 x1 @1
    adr x2 frame-count:
    str x1 [x2]
    b frame-loop-

:wrong-el
    (debug-str-reg "expected EL2, CurrentEL is " x0)
    b hang-
:no-fb
    (debug-str "framebuffer request failed" #t)
    b hang-

(create-send-char)

;; ---------------------------------------------------------------------
;; frame handling
;; ---------------------------------------------------------------------

; x0 = the page that is not on screen
(subr back-buffer () [x1 x2] {
    ldr x0 vmem:
    ldr x1 front-y:
    cbnz x1 done+             ; page 1 shown -> draw page 0
    ldr x2 page-bytes:
    add x0 x0 x2
:done
})

; show the page that was just drawn
(subr flip () [x0 x1 x2 x3 x8] {
  ldr x1 front-y:
  mov x3 @0
  cbnz x1 show+
  mov x3 @height
:show
  adr x1 front-y:
  str x3 [x1]
  adr x8 flip-msg:
  mov w2 @0
  str w2 [x8 @4]              ; the GPU overwrites the response words
  str w2 [x8 @16]
  str w3 [x8 @24]             ; y offset
  bl send-vcore-msg:
})

; sleep until the next 1/FPS tick of the generic timer
(subr wait-frame () [x0 x1 x2 x3] {
  ldr x1 next-tick:
:spin
  (write-value-32 $d53be020)  ; mrs x0, cntpct_el0
  cmp x0 x1
  b.cc spin-
  (write-value-32 $d53be002)  ; mrs x2, cntfrq_el0
  mov x3 @FPS
  udiv x2 x2 x3
  add x1 x1 x2
  cmp x1 x0
  b.hi ok+
  add x1 x0 x2                ; running late: don't try to catch up
:ok
  adr x3 next-tick:
  str x1 [x3]
})

(mailbox-routine)

;; ---------------------------------------------------------------------
;; faults: print vector number, ESR, ELR, FAR, then disassemble from ELR
;; ---------------------------------------------------------------------
:fault
  (write-value-32 (el-op $d5385201))   ; mrs x1, esr_el2
  (write-value-32 (el-op $d5384022))   ; mrs x2, elr_el2
  (write-value-32 (el-op $d5386003))   ; mrs x3, far_el2
  (debug-str "FAULT: vector esr elr far" #t)
  (debug-reg x0 x1 x2 x3)
  mov x1 x2
  mov x2 @32
  bl disassemble-dump:
:fault-hang
  b fault-hang-

; x1 = address, w2 = instruction count
(subr disassemble-dump ([address x1] [instruction-count w2]) [x0] {
    mov w0 @4
    bl send-char:
    mov x0 x1
    bl send-char:
    (for ([_ (in-range 7)])
      { lsr x0 x0 @8
        bl send-char: })
    mov x0 x2
    bl send-char:
    (for ([_ (in-range 3)])
      { lsr x0 x0 @8
        bl send-char: })
    lsr instruction-count instruction-count @1
    (/whilenz instruction-count {
      ldr x0 [x1] @8
      bl send-char:
      (for ([_ (in-range 7)])
        { lsr x0 x0 @8
          bl send-char: })
      sub instruction-count instruction-count @1
    })
})

; 16 entries of $80 bytes, each passes its index to fault
/= $800
:vectors
(for ([i (in-range 16)])
  { /= $80
    mov x0 @i
    b fault: })

;; ---------------------------------------------------------------------
;; data
;; ---------------------------------------------------------------------
/= 8
(periph-addresses)
:stack-top   (write-value-64 $80000)
:bus-mask    (write-value-64 $3FFFFFFF)
:page-bytes  (write-value-64 PAGE-BYTES)
:vmem        (write-value-64 0)     ; base of the double-height framebuffer
:front-y     (write-value-64 0)     ; y offset of the page on screen (0 or height)
:frame-count (write-value-64 0)
:next-tick   (write-value-64 0)

/= 16
:mbox-msg
    (write-value-32 (* 35 4))
    (write-value-32 0)
      (write-value-32 $48003)   ; physical size
      (write-value-32 8)
      (write-value-32 0)
      (write-value-32 width)
      (write-value-32 height)
      (write-value-32 $48004)   ; virtual size: two pages
      (write-value-32 8)
      (write-value-32 8)
      (write-value-32 width)
      (write-value-32 (* 2 height))
      (write-value-32 $48009)   ; virtual offset
      (write-value-32 8)
      (write-value-32 0)
      (write-value-32 0)
      (write-value-32 0)
      (write-value-32 $48005)   ; depth
      (write-value-32 4)
      (write-value-32 4)
      (write-value-32 BPP)
      (write-value-32 $48006)   ; pixel order: 1 = RGB
      (write-value-32 4)
      (write-value-32 4)
      (write-value-32 1)
      (write-value-32 $40001)   ; allocate
      (write-value-32 8)
      (write-value-32 8)
:fb   (write-value-32 4096)     ; response: bus address
      (write-value-32 0)
    (write-value-32 0)          ; end tag

/= 64
:flip-msg
    (write-value-32 (* 8 4))
    (write-value-32 0)
    (write-value-32 $48009)     ; set virtual offset
    (write-value-32 8)
    (write-value-32 0)
    (write-value-32 0)          ; x
    (write-value-32 0)          ; y
    (write-value-32 0)          ; end tag

/= $1000
:id_pgd
(reserve $2000)
})])
