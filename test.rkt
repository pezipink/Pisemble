
#lang pisemble
(require (for-syntax syntax/parse racket/stxparam))
(require racket/stxparam)
(require "periph.rkt" "stack.rkt")
(set-emulator-program! emu "kernel8.img")

(define s-frame-size 256) ; size of all saved registers
(define SYNC_INVALID_EL1t 0)
(define IRQ_INVALID_EL1t 1)
(define FIQ_INVALID_EL1t 2)
(define ERROR_INVALID_EL1t 3)

(define SYNC_INVALID_EL1h 4)
(define IRQ_INVALID_EL1h 5)
(define FIQ_INVALID_EL1h 6)
(define ERROR_INVALID_EL1h 7)

(define SYNC_INVALID_EL0_64 8)
(define IRQ_INVALID_EL0_64 9)
(define FIQ_INVALID_EL0_64 10)
(define ERROR_INVALID_EL0_64 11)

(define SYNC_INVALID_EL0_32 12)
(define IRQ_INVALID_EL0_32 13)
(define FIQ_INVALID_EL0_32 14)
(define ERROR_INVALID_EL0_32 15)

(define SYSTEM_TIMER_IRQ_0 1)
(define SYSTEM_TIMER_IRQ_1 %10)

(define SCTLR_RESERVED
  (bitwise-ior (arithmetic-shift 3 28)
               (arithmetic-shift 3 22)
               (arithmetic-shift 1 20)
               (arithmetic-shift 1 11)))
(define SCTLR_EE_LITTLE_ENDIAN          (arithmetic-shift 0 25))
(define SCTLR_EOE_LITTLE_ENDIAN         (arithmetic-shift 0 24))
(define SCTLR_I_CACHE_DISABLED          (arithmetic-shift 0 12))
(define SCTLR_D_CACHE_DISABLED          (arithmetic-shift 0 2))
(define SCTLR_I_CACHE_ENABLED           (arithmetic-shift 1 12))
(define SCTLR_D_CACHE_ENABLED           (arithmetic-shift 1 2))
(define SCTLR_MMU_DISABLED              (arithmetic-shift 0 0))
(define SCTLR_MMU_ENABLED               (arithmetic-shift 1 0))

(define SCTLR_VALUE_MMU_DISABLED  (bitwise-ior SCTLR_RESERVED SCTLR_EE_LITTLE_ENDIAN SCTLR_I_CACHE_DISABLED SCTLR_D_CACHE_DISABLED SCTLR_MMU_DISABLED))
(define SCTLR_VALUE_MMU_DISABLED2  (bitwise-ior SCTLR_RESERVED SCTLR_EE_LITTLE_ENDIAN SCTLR_I_CACHE_ENABLED SCTLR_D_CACHE_ENABLED SCTLR_MMU_DISABLED))

; ***************************************
; HCR_EL2, Hypervisor Configuration Register (EL2), Page 2487 of AArch64-Reference-Manual.
; ***************************************

(define HCR_RW            (arithmetic-shift 1 31))
(define HCR_VALUE     HCR_RW)

; ***************************************
; SCR_EL3, Secure Configuration Register (EL3), Page 2648 of AArch64-Reference-Manual.
; ***************************************

(define SCR_RESERVED          (arithmetic-shift 3 4))
(define SCR_RW        (arithmetic-shift 1 10))
(define SCR_NS        (arithmetic-shift 1 0))
(define SCR_VALUE             (bitwise-ior SCR_RESERVED SCR_RW SCR_NS))

; ***************************************
; SPSR_EL3, Saved Program Status Register (EL3) Page 389 of AArch64-Reference-Manual.
; ***************************************

(define SPSR_MASK_ALL       (arithmetic-shift 7 6))
(define SPSR_EL1h     (arithmetic-shift 5 0))
(define SPSR_VALUE      (bitwise-ior SPSR_MASK_ALL SPSR_EL1h))
(define-syntax (kernel-entry stx)
  (syntax-parse stx
    [(_)
     #'{
;        (PUSH x0 x1 x2 x3 x4 x5 x6 x7 x8)

        sub sp sp @s-frame-size
        stp x0 x1   [sp @(* 16 0)]
        stp x2 x3   [sp @(* 16 1)]
        stp x4 x5   [sp @(* 16 2)]
        stp x6 x7   [sp @(* 16 3)]
        stp x8 x9   [sp @(* 16 4)]
        stp x10 x11 [sp @(* 16 5)]
        stp x12 x13 [sp @(* 16 6)]
        stp x14 x15 [sp @(* 16 7)]
        stp x16 x17 [sp @(* 16 8)]
        stp x18 x19 [sp @(* 16 9)]
        stp x20 x21 [sp @(* 16 10)]
        stp x22 x23 [sp @(* 16 11)]
        stp x24 x25 [sp @(* 16 12)]
        stp x27 x27 [sp @(* 16 13)]
        stp x28 x29 [sp @(* 16 14)]
        str x30 [sp @(* 16 15)]

        }]))
(define-syntax (kernel-exit stx)
  (syntax-parse stx
    [(_)
     #'{
;        (POP x8 x7 x6 x5 x4 x3 x2 x1 x0)
        ldp x0 x1   [sp @(* 16 0)]
        ldp x2 x3   [sp @(* 16 1)]
        ldp x4 x5   [sp @(* 16 2)]
        ldp x6 x7   [sp @(* 16 3)]
        ldp x8 x9   [sp @(* 16 4)]
        ldp x10 x11 [sp @(* 16 5)]
        ldp x12 x13 [sp @(* 16 6)]
        ldp x14 x15 [sp @(* 16 7)]
        ldp x16 x17 [sp @(* 16 8)]
        ldp x18 x19 [sp @(* 16 9)]
        ldp x20 x21 [sp @(* 16 10)]
        ldp x22 x23 [sp @(* 16 11)]
        ldp x24 x25 [sp @(* 16 12)]
        ldp x26 x27 [sp @(* 16 13)]
        ldp x28 x29 [sp @(* 16 14)]
        ldr x30 [sp @(* 16 15)]
        add sp sp @s-frame-size
        eret
        }]))
(define-syntax (handle-invalid-entry stx)
  (syntax-parse stx
    [(_ type)
     #'{
        mov x0 @type
        mrs x1 esr_el1
        mrs x2 elr_el1
        bl show-invalid-entry-message:
        b err-hang:
        }
     ]))

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
  ; (debug-reg x0 w1 w2 x3)


(define pi 'pi3)
;(define pi 'pi4)

; the vector table is layed out 0x80 bytes
; apart (
(define-syntax (ventry stx)
  (syntax-parse stx
    [(_ vector:label-targ)
     #'{
        /= $80        
        b vector
        }]
    [(_ vector:label-targ more ...+)
     #'{
        /= $80        
        b vector
        (ventry more ...)
        }]))

(define VCORE-MBOX (+ PERIPH-BASE $00B880))

(aarch64 {
     width = 1024
     height = 768      
     set-offset = $1C
     clr-offset = $28
     udn-offset = $E4
     aux-enables = $4
     aux-io = $40
     aux-ier = $44
     aux-iir = $48
     aux-lcr = $4c
     aux-mcr = $50
     aux-lsr = $54
     aux-cntrl = $60
     aux-baud = $68

     ;mailbox offsets
     mbox-read = $0
     mbox-poll = $10
     mbox-sender = $14
     mbox-status = $18
     mbox-config = $1c
     mbox-write = $20
     mbox-response = $80000000
     mbox-full     = $80000000
     mbox-empty    = $40000000

     ;Mailbox channels
     mbox-ch-prop = $8  ; request to VC from ARM


     mrs x0 mpidr_el1
     mov x1 @$3
     and x0 x0 x1
     cbz x0 main:
     b hang:
 
:err-hang
     b err-hang-    

:hang
     wfe
     b hang-     

     :main

     ;; mov     x0, #3 << 20
;;     msr     cpacr_el1, x0	 // Enable FP/SIMD at EL1
   (write-value-32 $d2a00600)
   (write-value-32 $d5181040)
     
    ; get ready to switch from EL3 down to EL1
    ldr     x0 SCTLR-VALUE-MMU-DISABLED:
    msr	    sctlr_el1 x0		

    ldr     x0 HCR-VALUE:
    msr     hcr_el2 x0

    ldr     x0 SCR-VALUE:
    msr     scr_el3 x0

    ldr     x0 SPSR-VALUE:
    msr     spsr_el3 x0
    
    adr     x0 el1_entry:
    msr     elr_el3 x0

     eret			

:el1_entry

    ldr x1 START:
    mov sp x1
    (init-uart)
    (debug-str "init irq vector" #t)
    bl irq-vector-init:
;    (debug-str "init timer " #t)
    bl timer-init:
;    (debug-str "init ic" #t)
    bl enable-interrupt-controller:
;    (debug-str "enable irq" #t)
;    bl enable-irq:
  ldr x0 SMICS:
  mov x1 @0
  str w1 (x0 @0)
    ; bl dump-regs:     
;     (debug-str "HELLO WORLD!")
 ;    (debug-str "BAREMETAL RPI4 MEETS RACKETLANG!")
     ;(debug-str "heres a number" #t)
     mov x0 @$BAD
     lsl x0 x0 @16
     movk x0 @$F00D
     lsl x0 x0 @16
     movk x0 @$DEAD
     lsl x0 x0 @16
     movk x0 @$bEEf


     (debug-reg x0 w0)

     (debug-str "isqr 1" #t)
     mov x0 @1
     bl isqr:
     (debug-reg x0 w0)

     (debug-str "isqr 100" #t)
     mov x0 @100
     bl isqr:
     (debug-reg x0 w0)

     ldr x1 FAKE-ISR: ; SMI 
     (debug-reg x1)

     lsl x0 x0 @32

     (debug-reg x0 w0)
     
     ldr x0 TEST:

     (debug-reg x0 )
     adr x0 TEST:
     ldr x0 (x0 @0)
     (debug-reg x0 )
     lsr x0 x0 @8
     (debug-reg x0 )
     lsr x0 x0 @8
     (debug-reg x0 )
     lsr x0 x0 @8
     (debug-reg x0 )
     lsr x0 x0 @8
     (debug-reg x0 )
     
:here     adr x0 here-
     (debug-reg w0)


     (debug-str "value before:" #t)
     adr x0 MBOX-MSG:
     (debug-reg w0)
;    add x0 x0 @24
     ldrb w0 (x0 @0)
     (debug-reg w0)

     (debug-str "fb addr nefore" #t)
     adr x0 fb:
     ldr w0 (x0 @0)
     (debug-reg x0)
     adr x8 MBOX-MSG:
     bl send-vcore-msg:
; MSG START     
;;      ldr x0 VC_MBOX: 
;; :wait      
;;      ldr w1 (x0 @mbox-status)
;;      (debug-str "STATUS" #t)
;;      (debug-reg x0)


;;      ; and with 0x8000_0000
;;      mov x2 @1
;;      lsl x2 x2 @31
;;      and x1 x1 x2
;;      cbnz x1 wait-
;;      (debug-str "RTS" #t)
;;      ; attempt to call the mailbox interface
;;      ; upper 28 bits are the address of the message
;;      adr x2 MBOX-MSG:
;;      mov x0 x2
;; ;     (debug-w0)
;;      ldr x0 VC_MBOX: 
;;      ; lower four bits specify the mailbox channel,
;;      ; in this case mbox-ch-prop (8)
;;      mov x3 @mbox-ch-prop
;;      orr x2 x2 x3 ; we dont support orr reg-reg-imm yet
;;      ;now we wait until the FULL flag is not set so we can
;;      ; send our message
;; ;     (debug-reg w0)
;; ;     (debug-reg w2)

;;      str w2 (x0 @mbox-write)

;; ;     (debug-str "WFR" #t)
;;      ; now wait for a response 
;; :wait      
;;     ldr w1 (x0 @mbox-status)
;; ;    (debug-str "STATUS")
;; ;    (debug-reg x1)
;;      ; and with 0x4000_0000
;;      mov x2 @1
;;      lsl x2 x2 @30
;;      and x1 x1 x2
;;      cbnz x1 wait-
;; ;     (debug-str "DONE")
;;      ; check if mbox-read = channel


;;      ldr w1 (x0 @mbox-read)
;;      (debug-reg x1)
;;      mov x2 @%1111
;;      and x1 x1 x2
;;      sub x1 x1 @mbox-ch-prop
;;      cbnz x1 wait-

;MSG FINISH
     
     (debug-str "bpl" #t)

     adr x0 bpl:
;     add x0 x0 @20
     ldr w0 (x0 @0)
     (debug-reg x0)
     
     (debug-str "fb addr" #t)
     adr x0 fb:
;     add x0 x0 @20
     ldr w0 (x0 @0)
     (debug-reg x0)
     ;convert to gpu address
     ldr x1 CONVERT:
     and x0 x0 x1

; draw pixels!
     
     mov x8 x0  ; X8 holds base video memory
     adr x9 VMEM:
     str x8 (x9 @0)  ; store memory pointer in VMEM
     mov x1 @255
     lsl x1 x1 @32
     movk x1 @255
;     lsl x1 x1 @32
;     movk x1 @255
;     lsl x1 x1 @8
:draw
     mov x4 @height  ; rows
:row     
     mov x3 @(/ width 8)
:col
     str x1 (x0 @0)
     str x1 (x0 @8)
     str x1 (x0 @16)
     str x1 (x0 @24)
     add x0 x0 @32
;     add w1 w1 @1
     sub x3 x3 @1
     cbnz x3 col-

     sub x4 x4 @1
     cbnz x4 row-
; second page 

;     mov x1 @$FF00  ; comment this line to keep colour same (stop flashing page flip!)

:draw
     mov x4 @height  ; rows
:row     
     mov x3 @(/ width 8)
:col
     str x1 (x0 @0)
     str x1 (x0 @8)
     str x1 (x0 @16)
     str x1 (x0 @24)
     add x0 x0 @32
;     add w1 w1 @1
     sub x3 x3 @1
     cbnz x3 col-

     sub x4 x4 @1
     cbnz x4 row-
     mov x0 x8



     
     ; now we can scroll using the virtual offset message

     
;; :flip

;;      (debug-str "flip" #t)
;;   ldr x0 TIMER_CLO:
;;   ldr w0 (x0 @0)
;;   (debug-reg x0)

;;    mov x3 @0
;;    bl page-flip:

;;    (wait DELAY:)

;;    mov x3 @height
;;    bl page-flip:

;;    (wait DELAY:)

;; b flip-


     (debug-str "DONEDONE" #t)
     bl enable-irq:

     mov x0 @100
   (write-value-32 $9E220000) ;scvtf s0 x0
   (write-value-32 $1E21C000) ;fsqrt s0 s0
   (write-value-32 $9E250000) ; fcvtau x0 s0
     
     bl update-gfx:
 :loop
     b loop-

(create-send-char)


:dump-regs
  (PUSH x30)
  (dump-all-regs)
  (POP x30)
  ret x30

(subr page-flip ([ptr x0]) [x0 x1 x2] {
  ;pass y value in x3
  adr ptr MBOX-VOFFSET-MSG:
mov x2 @4
mov x1 @(* 8 4)
str w1 (ptr @0)  ; msg->size
add ptr ptr x2    ; +=4
mov x1 @0
str w1 (ptr @0) ; msg->request/response
add ptr ptr x2    ; +=4
mov x1 @4
lsl x1 x1 @16
movk x1 @$8009 
str w1 (ptr @0) ;msg->tag
add ptr ptr x2    ; +=4
mov x1 @8
str w1 (ptr @0) ; msg->value buffer size
add ptr ptr x2    ; +=4
mov x1 @0
str w1 (ptr @0) ; msg->tag request
add ptr ptr x2    ; +=4
str w1 (ptr @0) ; msg->x offset
add ptr ptr x2    ; +=4
mov x1 x3
str w1 (ptr @0) ; msg->y offset
add ptr ptr x2    ; +=4
mov x1 @0
str w1 (ptr @0) ; msg->end tag
adr x8 MBOX-VOFFSET-MSG:
bl send-vcore-msg:
})

; calculate integer square root with linear binary search
; X0 input; X0 output; no stack
(subr isqr ([y x0]
            [L x1]
            [M x2]
            [R x3]) [] {
    mov L @0     ; L = 0
    add R y @1   ; R = y + 1                         
:loop
    sub x4 R @1  ;r - 1 todo: need subs
    cmp L x4
    b.eq done+   ; while L != r -1 
    add M L R    ; M = L + R
    lsr M M @1   ; M /= 2
    mul x4 M M
    cmp x4 y
    b.le lte:
    mov R M 
    b loop-
:lte
    mov L M
    b loop-
:done
    mov X0 L
})
; pass message address in x8
(subr send-vcore-msg ()[x0 x1 x2 x3 x8]{
     ldr x0 VC_MBOX: 
:wait      
     ldr w1 (x0 @mbox-status)
     ; and with 0x8000_0000
     mov x2 @1
     lsl x2 x2 @31
     and x1 x1 x2
     cbnz x1 wait-
     ; attempt to call the mailbox interface
     ; upper 28 bits are the address of the message
     ; expected to be in x8
     ldr x0 VC_MBOX:
     mov x2 x8
     ; lower four bits specify the mailbox channel,
     ; in this case mbox-ch-prop (8)
     mov x3 @mbox-ch-prop
     orr x2 x2 x3 ; we dont support orr reg-reg-imm yet
     ; send our message
     str w2 (x0 @mbox-write)
     ; now wait for a response 
:wait      
    ldr w1 (x0 @mbox-status)
     ; and with 0x4000_0000
     mov x2 @1
     lsl x2 x2 @30
     and x1 x1 x2
     cbnz x1 wait-
     ; check if mbox-read = channel
     ldr w1 (x0 @mbox-read)
 ;    (debug-reg x1)
     mov x2 @%1111
     and x1 x1 x2
     sub x1 x1 @mbox-ch-prop
     cbnz x1 wait-


     })


:enable-irq
msr daifclr @2
ret x30

:disable-irq
msr daifset @2
ret x30

:show-invalid-entry-message
(debug-str "unhandled exception" #t)
(debug-reg x0 x1 x2)
ret x30

/= $800
:vectors
(ventry
sync_invalid_el1t:
irq_invalid_el1t:
fiq_invalid_el1t:
error_invalid_el1t:
sync_invalid_el1h:
el1_irq:
fiq_invalid_el1h:
error_invalid_el1h:
sync_invalid_el0_64:
irq_invalid_el0_64:
fiq_invalid_el0_64:
error_invalid_el0_64:
sync_invalid_el0_32:
irq_invalid_el0_32:
fiq_invalid_el0_32:
error_invalid_el0_32:
 )

:sync_invalid_el1t
  (handle-invalid-entry  SYNC_INVALID_EL1t)

:irq_invalid_el1t
  (handle-invalid-entry  IRQ_INVALID_EL1t)

:fiq_invalid_el1t
  (handle-invalid-entry  FIQ_INVALID_EL1t)

:error_invalid_el1t
  (handle-invalid-entry  ERROR_INVALID_EL1t)

:sync_invalid_el1h
  (handle-invalid-entry  SYNC_INVALID_EL1h)

:fiq_invalid_el1h
  (handle-invalid-entry  FIQ_INVALID_EL1h)

:error_invalid_el1h
  (handle-invalid-entry  ERROR_INVALID_EL1h)

:sync_invalid_el0_64
  (handle-invalid-entry  SYNC_INVALID_EL0_64)

:irq_invalid_el0_64
  (handle-invalid-entry  IRQ_INVALID_EL0_64)

:fiq_invalid_el0_64
  (handle-invalid-entry  FIQ_INVALID_EL0_64)

:error_invalid_el0_64
  (handle-invalid-entry  ERROR_INVALID_EL0_64)

:sync_invalid_el0_32
  (handle-invalid-entry  SYNC_INVALID_EL0_32)

:irq_invalid_el0_32
  (handle-invalid-entry  IRQ_INVALID_EL0_32)

:fiq_invalid_el0_32
  (handle-invalid-entry  FIQ_INVALID_EL0_32)

:error_invalid_el0_32
  (handle-invalid-entry  ERROR_INVALID_EL0_32)

:el1_irq
  (kernel-entry)
  bl  handle-irq:
  (kernel-exit)

:irq-vector-init
  adr x0 vectors:
  msr vbar_el1 x0
  ret x30

(subr enable-interrupt-controller () [x0 x1] {
  ;; ldr x0 ENABLE_IRQS_1:
  ;; mov x1 @SYSTEM_TIMER_IRQ_1      
  ;; str w1 (x0 @0)
  ldr x0 ENABLE_IRQS_2:
  ldr x1 FAKE-ISR: ; SMI 
  str w1 (x0 @0)
})


(subr timer-init () [x0 x1] {
  ldr x0 TIMER_CLO:
  ldr w0 (x0 @0)
  (debug-str "timer val" #t)                               
  (debug-reg x0)
  ldr x1 DELAY2:
  add w1 w0 w1
  (debug-reg w1)
  ldr x0 TIMER_C1:
  str w1 (x0 @0)
  
  
})

(subr get-back-buffer () [x1 x2] {
    ldr x0 VMEM:
    mov x1 x0
    ;x0 now has the base video memory pointer
    ; if CURRENT-PAGE is 0 then we want to send
    ; pointer + ((height / 2) * width)

    ldr x0 CURRENT-PAGE:
    cbz x0 done+
 ;   (debug-str "HERE" #t)
    ldr x2 FRAME-SIZE:
    add x1 x1 x2

:done ; return in x0
    mov x0 x1
})

(subr update-gfx (
                  [x x1]
                  [y x2]
                  [colour w3]
                  [temp x4]
                  [dx x5]
                  [dy x6]
                  [dx2 x7]
                  [dy2 x8]
                  [vptr x9]
                  [pAX x10]
                  [pAY x11]
                  [pBX x12]
                  [pBY x13]
                  [row-size x14]
                  [fc x15]
                  ) [x0 x1 x2 x3 x4]
                    {
 ;   ldr row-size ROW-SIZE:
:loop
 ldr x0 finished-rendering:
 cbnz x0 loop-
; (debug-str "render" #t)
 bl get-back-buffer:
 mov vptr x0  

 
   ldr fc frame-count:
;   lsr fc fc @1
   mov x0 @$3FFF
   and fc fc x0
   mov x0 @4
   mul fc fc x0
   
   adr x0 cx1lookup:
   add x0 x0 fc
   ldr w10 (X0 @0)

   adr x0 cy1lookup:
   add x0 x0 fc
   ldr w11 (X0 @0)

   adr x0 cx2lookup:
   add x0 x0 fc
   ldr w12 (X0 @0)

   adr x0 cy2lookup:
   add x0 x0 fc
   ldr w13 (X0 @0)

   ;; ldr pAX pointAX:
   ;; ldr pAY pointAY:
   ;; ldr pBX pointBX:
   ;; ldr pBY pointBY:

      
   mov y @(/ height 2)
;   mov colour @$FFFF
:y-loop
mov x @(/ width 2)
  ; calculate distance of this pixel to point a and b
   mov dy y
   mov temp pAY
   sub dy dy temp
   mul dy dy dy

   mov dy2 y
   mov temp pBY
   sub dy2 dy2 temp
   mul dy2 dy2 dy2


:x-loop
   mov dx x
   mov temp pAX
   sub dx dx temp
   mul dx dx dx

   mov dx2 x
   mov temp pBX
   sub dx2 dx2 temp
   mul dx2 dx2 dx2

   add x0 dx dy   ; sqrt dx + dy

   (write-value-32 $9E220000) ;scvtf s0 x0
   (write-value-32 $1E21C000) ;fsqrt s0 s0
   (write-value-32 $9E250000) ; fcvtau x0 s0

   mov temp x0
   add x0 dx2 dy2   ; sqrt dx2 + dy2

   (write-value-32 $9E220000) ;scvtf s0 x0
   (write-value-32 $1E21C000) ;fsqrt s0 s0
   (write-value-32 $9E250000) ; fcvtau x0 s0

   eor colour temp x0   ; xor sqrts 
   lsr colour colour @4
   mov temp @1
   and colour colour temp
   cbnz colour other+
   mov colour @$00FF
   lsl x3 x3  @16
   movk colour @$FFFF
   b store+
:other
   mov colour @$0000
:store   

   str colour (vptr @0)
   sub x x @1
   add vptr vptr @4
   cbnz x x-loop-
   sub y y @1
   sub vptr vptr @(* (/ width 2) 4)
   ldr temp ROW-SIZE:
   add vptr vptr temp

   cbnz y y-loop-


   mov temp @1
   adr x0 finished-rendering:
   str temp (w0 @0)

   ldr temp frame-count: ; frame-count++
   add temp temp @1
   adr x0 frame-count:
   str temp (x0 @0)
   

   b loop-
})                        

(subr handle-irq () [] {
;(debug-str "handled interrupt" #t)
;; ldr x0 TIMER_CS:
;; mov x1 @%10
;; str w1 (x0 @0)
  ldr x0 SMICS:  ; ack vsync
  mov x1 @0
  str w1 (x0 @0)

  ; only flip if render finished
  ldr x0 finished-rendering:
  cbz x0 quit:
;  (debug-str "flip" #t)
  ldr x1 CURRENT-PAGE:
  cbnz x1 f+
  mov x3 @height
  b done+
:f
  mov x3 @0
:done
  adr x1 CURRENT-PAGE:
  str x3 (x1 @0)

  bl page-flip:
  adr x1 finished-rendering:
  mov x2 @0
  str w2 (x1 @0)  
  ;bl update-gfx:

  
:quit})


  ; the following values are used for loading 64 bit addresses/values via LDR(literal)
  ; and they must be 64 bit aliged otherwise the CPU faults!
  /= 8
  (periph-addresses)
:DELAY (write-value-64 $FFFFF)
  :DELAY2 (write-value-64 200000)
  :VC_MBOX (write-value-64 VCORE-MBOX)
  :TEST (write-value-64 $123456789ABCDEF)
:START (write-value-64 $80000)
;:START (write-value-64 $400000)
:CONVERT (write-value-64 $3FFFFFFF)
:HCR-VALUE (write-value-64 HCR_VALUE)
:SCTLR-VALUE-MMU-DISABLED (write-value-64 SCTLR_VALUE_MMU_DISABLED2)
:SCR-VALUE (write-value-64 SCR_VALUE)
:SPSR-VALUE (write-value-64 SPSR_VALUE)
:FAKE-ISR (write-value-64 $10000)
:CURRENT-PAGE (write-value-64 0)
:FRAME-SIZE (write-value-64 (* width height 4))
:ROW-SIZE (write-value-64 (* width 4))
:VMEM (write-value-64 0)
:pointAX(write-value-64 10)
:pointAY(write-value-64 10)
:pointBX(write-value-64 100)
:pointBY(write-value-64 100)
:finished-rendering (write-value-64 0)
:frame-count (write-value-64 0)


; to communicate with the video core gpu we need an address that
  ; is 16-byte algined - the lower 4 bits are not set. these are then
  ; used to specify the channel
  /= 16
  :MBOX-MSG
    (write-value-32 (* 35 4)) ; total buffer size bytes including headers
    (write-value-32 0) ; request / response code (0 = request)
    ;begin tags
    ; set physical 
      (write-value-32 $48003)
      (write-value-32 8) ; value buffer size
      (write-value-32 0) ; 0 = request
      (write-value-32 width) ; width
      (write-value-32 height); height
    ; set virt
      (write-value-32 $48004)
      (write-value-32 8) ; value buffer size
      (write-value-32 8) ; 0 = request
      (write-value-32 width) ; width
      (write-value-32 (* 2 height)); height

    ; set virtoff
      (write-value-32 $48009)
      (write-value-32 8) ; value buffer size
      (write-value-32 0) ; 0 = request
      (write-value-32 0) ; x
      (write-value-32 (/ height 2)); y

   ; set depth 
      (write-value-32 $48005)
      (write-value-32 4) 
      (write-value-32 4) 
      (write-value-32 32) ; bpp

   ; set pxlorder
      (write-value-32 $48006)
      (write-value-32 4) 
      (write-value-32 4) 
      (write-value-32 1) ; RGB

   ; get fb
      (write-value-32 $40001)
      (write-value-32 8) 
      (write-value-32 8) 
:fb
      (write-value-32 4096) ; FramebufferInfo.pointer
      (write-value-32 0) ; FramebufferInfo.size

   ; get pitch
      (write-value-32 $40008)
      (write-value-32 4) 
      (write-value-32 4) 
:bpl      (write-value-32 0) ; bytes per line


    (write-value-32 0) ;end tag
  /= 16
  :MBOX-VOFFSET-MSG
    (write-value-32 (* 8 4)) ; total buffer size bytes including headers
    (write-value-32 0) ; request / response code (0 = request)
    ;begin tags
    ; set virtoff
      (write-value-32 $48009)
      (write-value-32 8) ; value buffer size
 :voffset-req
(write-value-32 0) ; 0 = request
      (write-value-32 0) ; x
   :voffset-y
      (write-value-32 0); y
    (write-value-32 0) ;end tag


/= 8
:cx1lookup
(for ([t (in-range 0 (* 1024 10))])
  (let* ([delta (/ 3.14 2)]
        [a (sin (+ delta (* 3 (/ t 100))))]
        )
    (write-value-32  (exact-round (+ (/ width 2 2) (* (/ width 2 2) a))))))
:cy1lookup
(for ([t (in-range 0 (* 1024 10))])
  (let* ([delta (/ 3.14 2)]
        [a (sin (+ delta (* 4 (/ t 100))))]
        )
    (write-value-32  (exact-round (+ (/ height 2 2) (* (/ height 2 2) a))))))

:cx2lookup
(for ([t (in-range 0 (* 1024 10))])
  (let* ([delta 3.14;(/ 3.14 2)
                ]
        [a (sin (+ delta (* 3 (/ t 100))))]
        )
    (write-value-32  (exact-round (+ (/ width 2 2) (* (/ width 2 2) a))))))
:cy2lookup
(for ([t (in-range 0 (* 1024 10))])
  (let* ([delta 3.14;(/ 3.14 2)
                ]
        [a (sin (+ delta (* 2 (/ t 100))))]
        )
    (write-value-32  (exact-round (+ (/ height 2 2) (* (/ height 2 2) a))))))


})





