#lang pisemble
(require (for-syntax syntax/parse racket/stxparam))
(require syntax/parse/define)
(require racket/stxparam)
(require "periph.rkt" "stack.rkt")

(define-syntax (wait stx)
  (syntax-parse stx
    [(_ value)
     #'{ldr x2 value
        :inner
        sub x2 x2 @1
        cbnz x2 inner- }]))

(define-syntax (debug-reg stx)
  (syntax-parse stx
    [(_ r:register)
     #'{
        ; preserve x0, it is used for passing char to send-char
        (PUSH x0) 
        (PUSH r)
        ; first send code indicating whether a 32 or 64 bit number will be sent
        mov w0 @(if `r.is32 2 3)
        bl send-char:
        ; restore r into x0 and send first char
        (POP x0)   
        bl  send-char:
        ; shift and send the remainig 3 or 7 bytes
        (for ([_ (in-range (if `r.is32 3 7))])
          { lsr x0 x0 @8
            bl send-char: })
        ; restore x0 original value
        (POP x0) }]
    [(_ r:register rn:register ...+)
     #'(begin (debug-reg r)
              (debug-reg rn ...))]))


(define-syntax (dump-all-regs stx)
  (syntax-parse stx
    [(_)
     #:with (reg ...) (for/list ([i (in-range 0 31)])
                   (datum->syntax this-syntax (string->symbol (format "X~a"i))))
     #:with (name ...) (for/list ([i (in-range 0 31)])
                   (datum->syntax this-syntax (format "X~a "i)))
     #'(begin (begin (debug-str name #f) (debug-reg reg)) ...)]))
(define-syntax-parser load-immediate ;todo: probably want this as a core macro in pisemble itself?
  [(_ target:register value:expr)
   #'(cond
       [(< value (expt 2 16))
        { mov target @value  }]
       [(< value (expt 2 32))
        {
         mov target @(bitwise-and value $FFFF)
         movk target @(arithmetic-shift value -16) LSL @16
         }]
       [else (error (format "load-immediate: value too large, not currently supported ~a" value))])])
(define EL 2)   ; 2 for QEMU. Check the real Pi with CurrentEL (it may differ)

; patch an EL1 opcode for the current EL (op1 field, bits 18:16: EL1=0 EL2=4 EL3=6)
(define (el-op base)
  (+ base (arithmetic-shift (case EL [(1) 0] [(2) 4] [(3) 6]) 16)))

(define-syntax-parser setup-mmu
  [(_) #'{
    ;; ---- level 1 table at $30000: four 1GB entries ----
    (load-immediate x0 $30000)
    (load-immediate x1 $31003)      ; [0] table -> level 2 at $31000
    str x1 [x0] @8
    (load-immediate x2 $40000741)   ; [1] 0x40000000 Device (ARM local peripherals)
    str x2 [x0] @8
    (load-immediate x2 $80000741)   ; [2]
    str x2 [x0] @8
    (load-immediate x2 $C0000741)   ; [3]
    str x2 [x0] @8

    ;; ---- level 2 table at $31000: 512 x 2MB blocks ----
    (load-immediate x0 $31000)
    (load-immediate x2 $74D)        ; entry 0: addr 0, AttrIdx 3 (write-back)
    (load-immediate x5 $200000)     ; 2MB step
    mov x4 @0
 :l2loop
    str x2 [x0] @8
    add x2 x2 x5
    add x4 x4 @1                    ; x4 = index of the NEXT entry
    cmp x4 @472                     ; GPU memory starts: AttrIdx 3 -> 2
    b.ne l2nc+
    mov x6 @4
    eor x2 x2 x6
 :l2nc
    cmp x4 @504                     ; peripherals start (0x3F000000): AttrIdx 2 -> 0
    b.ne l2dev+
    mov x6 @8
    eor x2 x2 x6
 :l2dev
    cmp x4 @512
    b.ne l2loop-

    ;; ---- translation registers ----
    (load-immediate x1 $80803520)   ; T0SZ=32, WB/WB, inner shareable, 4KB, PS=32bit, RES1 bits 23/31
    (write-value-32 (el-op $d5182041))   ; msr tcr_elN, x1
    (load-immediate x1 $ff440400)        ; attr0 nGnRnE, attr1 nGnRE, attr2 NC, attr3 WB
    (write-value-32 (el-op $d518a201))   ; msr mair_elN, x1
    (load-immediate x0 $30000)
    (write-value-32 (el-op $d5182000))   ; msr ttbr0_elN, x0
    (write-value-32 $d5033f9f)           ; dsb sy
    (write-value-32 (el-op $d508871f))   ; tlbi alleN
    (write-value-32 $d5033f9f)           ; dsb sy
    (write-value-32 $d5033fdf)           ; isb

    ;; ---- SMPEN (Cortex-A53), unchanged from your file ----
    (write-value-32 $d539f220)           ; mrs x0, cpuectlr_el1
    (write-value-32 $b27a0000)           ; orr x0, x0, #(1<<6)
    (write-value-32 $d519f220)           ; msr cpuectlr_el1, x0

    ;; ---- enable D-cache, I-cache, MMU ----
    (write-value-32 (el-op $d5381000))   ; mrs x0, sctlr_elN
    (write-value-32 $b27e0000)           ; orr x0, x0, #(1<<2)  C
    (write-value-32 $b2740000)           ; orr x0, x0, #(1<<12) I
    (write-value-32 $b2400000)           ; orr x0, x0, #(1<<0)  M
    (write-value-32 (el-op $d5181000))   ; msr sctlr_elN, x0
    (write-value-32 $d5033f9f)           ; dsb sy
    (write-value-32 $d5033fdf)           ; isb
  }])

;; =====================================================================
;; timing
;; =====================================================================
;; (time-it label bytes reps body) runs body reps times, keeps the fastest
;; run (in cntpct_el0 ticks) and sends a Timing record to comms.fsx:
;;   5, label chars, 0, bytes (u64), ticks (u64), cntfrq (u64)
;; body may trash any register except sp.

(define (send-label str)
  { mov w0 @5
    bl send-char: }
  (for ([c str])
    { mov w0 @(char->integer c)
      bl send-char: })
  { mov w0 @0
    bl send-char: })

(define-syntax-parser time-it
  [(_ label:expr bytes:expr reps:expr body:expr)
   #'{
      (load-immediate x0 reps)
      bl timing-reset:
    :time-it-loop
      bl timing-begin:
      body
      bl timing-end:
      cbnz x0 time-it-loop-
      (send-label label)
      (load-immediate x1 bytes)
      bl timing-send:
     }])

;; read / write size bytes starting at the address stored at base-label, passes times.
;; size must be a multiple of 128.
(define-syntax-parser mem-read
  [(_ base:label-targ size:expr passes:expr)
   #'{
      (load-immediate x3 passes)
    :mem-pass
      ldr x0 base
      (load-immediate x1 (quotient size 128))
    :mem-blk
      ldp x4 x5 [x0 @0]
      ldp x4 x5 [x0 @16]
      ldp x4 x5 [x0 @32]
      ldp x4 x5 [x0 @48]
      ldp x4 x5 [x0 @64]
      ldp x4 x5 [x0 @80]
      ldp x4 x5 [x0 @96]
      ldp x4 x5 [x0 @112]
      add x0 x0 @128
      sub x1 x1 @1
      cbnz x1 mem-blk-
      sub x3 x3 @1
      cbnz x3 mem-pass-
     }])

(define-syntax-parser mem-write
  [(_ base:label-targ size:expr passes:expr)
   #'{
      (load-immediate x4 $FF0000FF)
      mov x5 x4
      (load-immediate x3 passes)
    :mem-pass
      ldr x0 base
      (load-immediate x1 (quotient size 128))
    :mem-blk
      stp x4 x5 [x0 @0]
      stp x4 x5 [x0 @16]
      stp x4 x5 [x0 @32]
      stp x4 x5 [x0 @48]
      stp x4 x5 [x0 @64]
      stp x4 x5 [x0 @80]
      stp x4 x5 [x0 @96]
      stp x4 x5 [x0 @112]
      add x0 x0 @128
      sub x1 x1 @1
      cbnz x1 mem-blk-
      sub x3 x3 @1
      cbnz x3 mem-pass-
     }])

;; read + write sweep over buf for each size (KB); each run moves about total bytes
(define-syntax-parser mem-sweep
  [(_ name:expr buf:label-targ sizes-kb:expr total:expr reps:expr)
   #'(for ([kb sizes-kb])
       (define size (* kb 1024))
       (define passes (max 1 (quotient total size)))
       (time-it (format "read  ~a ~aKB" name kb) (* size passes) reps
                (mem-read buf size passes))
       (time-it (format "write ~a ~aKB" name kb) (* size passes) reps
                (mem-write buf size passes)))])

(define TEST-SIZES '(4 16 32 64 128 256 512 1024 2048))   ; KB. L1 = 32KB, L2 = 512KB
(define FB-BYTES (* 320 200 4))                            ; 256000, a multiple of 128

(aarch64 "kernel8.img" [] {

     ; check cpu core
     mrs x0 mpidr_el1
     mov x1 @$3
     and x0 x0 x1
     cbz x0 main:
 
:hang ; all cores except 1 end up here
     wfe
     b hang-     

:main
    ldr x1 START:
    mov sp x1

    (init-uart)
    (debug-str "uart up" #t)
    (write-value-32 $d5384240) 
    (debug-reg x0)

    ;; ---- framebuffer (MMU still off, so no cache maintenance worries) ----
    adr x8 MBOX-MSG:
    bl send-vcore-msg:
    adr x0 fb:
    ldr w0 [x0]
    ldr x1 CONVERT:
    and x0 x0 x1                    ; GPU bus address -> ARM physical
    adr x1 VMEM:
    str x0 [x1]
    (debug-str "framebuffer at " #f)
    (debug-reg x0)
    cbnz x0 fb-ok+
    (debug-str "no framebuffer" #t)
    b hang:
 :fb-ok

    ;; ---- memory speed: MMU and caches off (everything is Device memory) ----
    (debug-str "speed test, mmu off" #t)
    (mem-sweep "nommu" BUF-BASE: '(4 64 256) (* 256 1024) 2)
    (time-it "write fb nommu" FB-BYTES 2 (mem-write VMEM: FB-BYTES 1))

    (setup-mmu)
    (debug-str "mnu up" #t)

    ;; ---- memory speed: RAM write-back cached, framebuffer non-cacheable ----
    (debug-str "speed test, mmu on" #t)
    (mem-sweep "wb" BUF-BASE: TEST-SIZES (* 8 1024 1024) 3)
    (time-it "read  fb nc" (* FB-BYTES 8) 3 (mem-read VMEM: FB-BYTES 8))
    (time-it "write fb nc" (* FB-BYTES 8) 3 (mem-write VMEM: FB-BYTES 8))
    (debug-str "speed test done" #t)
    
    bl dump-regs:     
    (debug-str "heres a number" #t)
    mov x0 @$BAD
    lsl x0 x0 @16
    movk x0 @$F00D
    lsl x0 x0 @16
    movk x0 @$DEAD
    lsl x0 x0 @16
    movk x0 @$bEEF

    (debug-reg x0 w0)
    

    width = 320
     height = 200      

     ;mailbox offsets
     mbox-read = $0
     Mbox-poll = $10
     mbox-sender = $14
     mbox-status = $18
     mbox-config = $1c
     mbox-write = $20
     mbox-response = $80000000
     mbox-full     = $80000000
     mbox-empty    = $40000000

     ;Mailbox channels
     mbox-ch-prop = $8  ; request to VC from ARM

 :loop
     b loop-

;; inline the code and label for the send-char subroutine     
(create-send-char)

:dump-regs
  (PUSH x30)
  (dump-all-regs)
  (POP x30)
  ret x30

;; ---- timing support for time-it ----
; x0 = number of runs
(subr timing-reset () [x1] {
  adr x1 T-REPS:
  str x0 [x1]
  mov x0 @0
  sub x0 x0 @1                    ; best = max
  adr x1 T-BEST:
  str x0 [x1]
})

(subr timing-begin () [x0 x1] {
  (write-value-32 $d5033f9f)      ; dsb sy
  (write-value-32 $d5033fdf)      ; isb
  (write-value-32 $d53be020)      ; mrs x0, cntpct_el0
  adr x1 T-START:
  str x0 [x1]
})

; keeps the fastest run, returns runs remaining in x0
(subr timing-end () [x1 x2] {
  (write-value-32 $d5033f9f)      ; dsb sy, so stores have drained
  (write-value-32 $d5033fdf)      ; isb
  (write-value-32 $d53be020)      ; mrs x0, cntpct_el0
  ldr x1 T-START:
  sub x0 x0 x1
  ldr x1 T-BEST:
  cmp x0 x1
  b.cs keep+                      ; unsigned >= best: not faster
  adr x2 T-BEST:
  str x0 [x2]
 :keep
  ldr x0 T-REPS:
  sub x0 x0 @1
  adr x2 T-REPS:
  str x0 [x2]
})

; x1 = bytes moved per run. label has already been sent by time-it
(subr timing-send () [x0] {
  mov x0 x1
  bl send-u64:
  ldr x0 T-BEST:
  bl send-u64:
  (write-value-32 $d53be000)      ; mrs x0, cntfrq_el0
  bl send-u64:
})

(subr send-u64 () [x0] {
  bl send-char:
  (for ([_ (in-range 7)])
    { lsr x0 x0 @8
      bl send-char: })
})

; pass message address in x8
(subr send-vcore-msg () [x0 x1 x2 x3 x8] {
     mov x0 x8
     (write-value-32 $d50b7e20)  ; dc civac, x0
     (write-value-32 $d5033f9f)  ; dsb sy
     ldr x0 VC_MBOX:
:wait
     ldr w1 (x0 @mbox-status)
     mov x2 @1
     lsl x2 x2 @31               ; full
     and x1 x1 x2
     cbnz x1 wait-
     mov x2 x8
     mov x3 @mbox-ch-prop
     orr x2 x2 x3
     str w2 (x0 @mbox-write)
:wait
     ldr w1 (x0 @mbox-status)
     mov x2 @1
     lsl x2 x2 @30               ; empty
     and x1 x1 x2
     cbnz x1 wait-
     ldr w1 (x0 @mbox-read)
     mov x2 @%1111
     and x1 x1 x2
     sub x1 x1 @mbox-ch-prop
     cbnz x1 wait-
})

  ; the following values are used for loading 64 bit addresses/values via LDR(literal)
  ; and they must be 64 bit aliged otherwise the CPU faults!
  /= 8
  (periph-addresses)
  :DELAY (write-value-64 $FFFF)
:START (write-value-64 $80000)

  :TABLE(write-value-64 $30000)
:VC_MBOX (write-value-64 (+ PERIPH-BASE $00B880))
:CONVERT (write-value-64 $3FFFFFFF)
:VMEM (write-value-64 0)
:BUF-BASE (write-value-64 $200000)       ; test buffer, 2MB..4MB, clear of image and tables
:T-START (write-value-64 0)
:T-BEST (write-value-64 0)
:T-REPS (write-value-64 0)

;; framebuffer request, 16 byte aligned (low 4 bits are the channel)
/= 16
:MBOX-MSG
    (write-value-32 (* 22 4))
    (write-value-32 0)
      (write-value-32 $48003)   ; physical size
      (write-value-32 8)
      (write-value-32 0)
      (write-value-32 320)
      (write-value-32 200)
      (write-value-32 $48004)   ; virtual size
      (write-value-32 8)
      (write-value-32 8)
      (write-value-32 320)
      (write-value-32 200)
      (write-value-32 $48005)   ; depth
      (write-value-32 4)
      (write-value-32 4)
      (write-value-32 32)
      (write-value-32 $40001)   ; allocate
      (write-value-32 8)
      (write-value-32 8)
:fb   (write-value-32 4096)
      (write-value-32 0)
    (write-value-32 0)          ; end tag
  
})
