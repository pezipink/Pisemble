#lang pisemble
(require (for-syntax syntax/parse racket/stxparam racket/syntax))
(require syntax/parse/define)

(require racket/stxparam)
(require racket/file)
(require racket/string)
(require racket/match)
(require threading)
(require "periph.rkt" "stack.rkt" "struct.rkt")

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

(define-syntax-parser inc [(_ reg:register) #'{ add reg reg @1 }])
(define-syntax-parser dec [(_ reg:register) #'{ sub reg reg @1 }])

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
        ; shift and send the remaining 3 or 7 bytes
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


; the vector table is layed out 0x80 bytes apart
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

(aarch64 "kernel8.img" ["2dgfx.obj" "textures.obj" "maps.obj"] {
     width = 320
     height = 200      

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

     ; if cpu zero then goto main:
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

    ;;     msr     cpacr_el1, x0	 // Enable FP/SIMD at EL1
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


    ; Core entry point at Exception level 1 

    ; Set stack pointer 
    ldr x1 START:
    mov sp x1

    ; setup the mini-uart for debug comms
    (init-uart)
    (debug-str "init irq vector" #t)
    ; setup IRQ vectors 
    bl irq-vector-init:
;    (debug-str"init ic" #t)
    ; switch on interrupt conteroller
    ; for wolf this is the SMI interrupt only at the moment
    ; which is triggered by the screen vsync signal
    bl enable-interrupt-controller:
;    (debug-str "enable irq" #t)
    ; clear the SMICS flag just in case.
    ; we won't enable the interrupts just yet
    ldr x0 SMICS:
    mov x1 @0
    str w1 [x0]

    ; now we are going to ask for a frame buffer
;    (debug-str "value before:" #t)
    adr x0 MBOX-MSG:
    (debug-reg w0)
    ;    add x0 x0 @24
    ldrb w0 (x0 @0)
    (debug-reg w0)
    
    ;; adr x0 tilemap:
    ;; mov x1 @256
    ;; bl memory-dump:
    (debug-str "loading level.." #t)
    bl setup-game-level:
    bl scan-info-plane:
    ;    adr x1 disassemble-dump:
    ;; mov x1 @0
    ;; mov x2 @512
    ;; bl disassemble-dump:
    
    adr x0 tilemap:
    mov x1 @256
    bl memory-dump:
    
    
    (debug-str "fb addr nefore" #t)
    adr x0 fb:
    ldr w0 [x0]
    (debug-reg x0)
    adr x8 MBOX-MSG:
    bl send-vcore-msg:
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
     ;; str x1 (x0 @0)
     ;; str x1 (x0 @8)
     ;; str x1 (x0 @16)
     ;; str x1 (x0 @24)
     add x0 x0 @32
;     add w1 w1 @1
     sub x3 x3 @1
     cbnz x3 col-

     sub x4 x4 @1
     cbnz x4 row-
; second page 

;    mov x1 @$FF00  ; comment this line to keep colour same (stop flashing page flip!)
    mov x1 @$FF00
    lsl x1 x1 @16
    
:draw
     mov x4 @height  ; rows
:row     
     mov x3 @(/ width 8)
:col
     ;; str x1 (x0 @0)
     ;; str x1 (x0 @8)
     ;; str x1 (x0 @16)
     ;; str x1 (x0 @24)
     add x0 x0 @32
;     add w1 w1 @1
     sub x3 x3 @1
     cbnz x3 col-

     sub x4 x4 @1
     cbnz x4 row-
     mov x0 x8
     ; now we can scroll using the virtual offset message


     (debug-str "DONEDONE" #t)

    
     bl enable-irq:

     mov x1 @0
     mov x28 @105  ; 130 for 2d pics, 105 for textures
     mov x27 @60

          mov x1 @84
;     bl render-pic:
;     bl dump-regs:
     bl update-gfx:
 :loop
     b loop-

(create-send-char)


:dump-regs
  (PUSH x30)
  (dump-all-regs)
  (POP x30)
  ret x30

(define-syntax-parser nibble->hex
  [(_ input:register )
   #'{ ; with an additional register we can remove this
      ; branch by using csel - debug code though so
      ; doesn't matter
      cmp input @10
      b.ge hex+
      add input input @48     ; add 48 (0-9)
      b done+
  :hex
      add input input @55     ; add 65 (A-F)
      :done}])

(define-syntax-parser bound-ascii
  [(_ input:register )
   #'{
     cmp input @$20
     b.ge done+
     mov input @$2E  ;; '.'
    :done}])

(define dump-template "0000000000000000 : 00 00 00 00 | 00 00 00 00 | 00 00 00 00 | 00 00 00 00 | 0000000000000000")
(subr memory-dump () [x0 x1 x2 x3 x4 x5 x6 x7 x8] {
  ; arguments 
  ; x0 will be the start address
  ; x1 will be the amount of rows to send (* 4 32 bits + ascii)                                 
  ; //////
  ; we are going to write                                            
  ; 00000000000012FF : FF FF FF FF | FF FF FF FF | FF FF FF FF | FF FF FF FF | asciiasciiascii.
  ; now we can skip ahead four in the output and read the first 32 bit value


  (define (write-byte) {
    lsr x4 x3 @4      ; top 4 bits first                    
    (nibble->hex x4)  ; convert to ascii hex
    strb w4 [x6] @1   ; modify string with result
    and x4 x3 x5      ; bottom 4 bits
    (nibble->hex x4)  ; convert to ascii hex
    strb w4 [x6] @1   ; modify string with result
    })

  (define (write-byte-raw) {
    (bound-ascii w3)
    strb w3 [x6] @1   ; modify string with result
  })
                                              
  template-len = (string-length dump-template)
  mov x8 x1 ; x8 holds row count down
  (whilenz x8 {
    ; first part is to overwrite the address with the requested address
    adr x6 memory-dump-template:
    mov x3 x0           ; address to write
    mov x2 @16          ; loop counter (+1)
    mov x5 @$F          ; because we don't support and x x imm
    (whilenz x2 {
      lsr x4 x3 @60     ; shift to byte
      and x4 x4 x5      ; mask the 4 bits
      (nibble->hex x4)  ; convert to ascii hex
      strb w4 [x6] @1   ; modify string with result
      lsl x3 x3 @4      ; shift in next nibble
      sub x2 x2 @1      ; 
    })             


    add x6 x6 @3 ; spacer

    mov x7 @4
    (whilenz x7 {
      ldrb w3 [x0] @1
      (write-byte)
      add x6 x6 @1
      
      ldrb w3 [x0] @1
      (write-byte)
      add x6 x6 @1
      
      ldrb w3 [x0] @1
      (write-byte)
      add x6 x6 @1
      
      ldrb w3 [x0] @1
      (write-byte)
      add x6 x6 @3

      sub x7 x7 @1
    })

    ; now for the ascii version
    sub x0 x0 @16
    mov x7 @16

    (whilenz x7 {
      ldrb w3 [x0] @1
      (write-byte-raw)    
      sub x7 x7 @1
    })
    
    adr x1 memory-dump-template:
    mov x2 @template-len
    bl send-string:
    sub x8 x8 @1
  })

})
:memory-dump-template (write-ascii dump-template)                  

/= 4
(subr disassemble-dump
      ([address x1]
       [instruction-count w2])
      [x0] {
    ; initiate disassemble send
    mov w0 @4
    bl send-char:
    ; send address         
    mov x0 x1
    bl send-char:
    (for ([_ (in-range 7)])
      { lsr x0 x0 @8
        bl send-char: })
    ; send count    
    mov x0 x2
    bl send-char:
    (for ([_ (in-range 3)])
      { lsr x0 x0 @8
        bl send-char: })
    lsr instruction-count instruction-count @1 ; we do 64 bits at a time
    ; send instructions
    (whilenz instruction-count {
      ldr x0 [x1] @8
      bl send-char:
      (for ([_ (in-range 7)])
        { lsr x0 x0 @8
          bl send-char: })
      
      sub instruction-count instruction-count @1
    })
})         


/= 4
(subr send-string
      ([address x1]
       [len x2]) [x0] {
  ; initiate string send
  mov w0 @1
  bl send-char:

  (whilenz len {
    ldrb x0 [address] @1
    bl send-char:
    sub len len @1
  })
  mov w0 @$A ; line feed
  bl  send-char:
   ;; mov w0 @$D ; cr
  ;; bl  send-char:

  ; signal string end
  mov w0 @0
  bl send-char:
})
  
; x0 = address
; x1 = count
; this trashes registers .. so push first!
;; (subr :dump-mem-64
;;       ([x1 address]
;;        [x2 remaining]) [] {
;;   ; first print the address like so  "$00000000 : "
;;   mov x0 @(#/'$)
;;   bl send-char                               
;; })                       



(define-syntax-parser zero-mem
  [(_ base:register size:expr)
   #'(letrec
         ([aux
           (λ (offset rem-size)
             (cond
               [(>= rem-size 8) (begin {str x0 (base @offset)} (aux (+ offset 8) (- rem-size 8)))]
               [(>= rem-size 4) (begin {str w0 (base @offset)} (aux (+ offset 4) (- rem-size 4)))]
               [(>= rem-size 2) (begin {strh w0 (base @offset)} (aux (+ offset 2) (- rem-size 2)))]
               [(>= rem-size 1) (begin {strb w0 (base @offset)} (aux (+ offset 1) (- rem-size 1)))]
               [(= rem-size 0) (void)]
               [else (error (format "FAIL ~a ~a" offset rem-size))]))])
       (begin
         {mov x0 @0}
         (aux 0 size)))])


(/struct _voffset_msg
  ([total-size 4]     ; total buffer size bytes including headers
   [request-code 4]   ; request / response code (0 = request)
   ;begin tags
   [tag-id 4]
   [request-size 4]
   [tag-request-code 4]
   [x-loc 4]         ; virtual x pos
   [y-loc 4]         ; virtual y pos
   [end-tag 4]))

(define-syntax-parser write-virtual-offset
  [(_ base:register y-loc:register)
   #'{ ;note this trashes x0
      (zero-mem base (_voffset_msg/sizeof))
      (_voffset_msg/total-size-set (* 8 4) base)
      (_voffset_msg/tag-id-set $48009 base)
      (_voffset_msg/request-size-set 8 base)
      (_voffset_msg/y-loc-set y-loc base)
     }])

; virtual "page flip" here we tell the gpu to
; display the off-screen portion of the video memory
; that has been drawn to
; pass y value in x3
(subr page-flip ([ptr x16]) [x0 x1 x2 x16] {
  ; reserve space on stack for v-offset message
  sub sp sp @(_voffset_msg/sizeof)                                          
  (write-virtual-offset sp x3)
  mov x8 sp
  bl send-vcore-msg:
  add sp sp @(_voffset_msg/sizeof)                                          
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
mov x1 x2
mov x2 @32
bl disassemble-dump:
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
  str w1 [x0]
})

; returns a pointer to the current "back buffer"
; which is the area of video memory not currently
; being displayed
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

; plots a single pixel 
(subr plot
      ([vptr x0] ; base video memory
       [x x1]
       [y x2]
       [colour w3]) [x0] {

    ; each pixel is 4 bytes
    lsl x x @2  ; x *= 4  
    ldr x4 ROW-SIZE:
    mul y y x4
    add vptr vptr x
    add vptr vptr y
    str colour [vptr]
    
})

(begin-for-syntax
  (define labels (make-hash))
  (define (format-label label)
    (string->symbol (format ":~a"  label)))
  (define (format-target label type)
    (case type
      ['+     (string->symbol (format "~a+" label))]
      ['-     (string->symbol (format "~a-" label))]
      [else (error (format "bad label type ~a" type))]))
             
  (define (gen-label name)
    (if (hash-has-key? labels name)
        (let ([val (hash-ref labels name)])
          (hash-set! labels name (+ val 1))
          (format "~a_~a" name (+ val 1)))
        (begin
          (hash-set! labels name 0)
          (format "~a_~a" name 0)))))

(define-syntax-parser whilenz
  [(_ test-register:register body)
   (let ([loop-start (gen-label "whilenz-start")])
     (with-syntax
       ([start-label (format-label loop-start)]
        [start-target (format-target loop-start '-)])
        #'{
           start-label body
           cbnz test-register start-target
           }))])


; test routine that displays each texture on the screen
; this relies on the delay and image number loop
; from the gfx-update
(subr render-tex
      ([vptr x0]
       [texnum x1][iptr x1]
       [offset x3]
       [x w4]
       [y w5]
       [data w6]
       [screen-rowsize x8]
       [temp x9])
      [] {

    ldr screen-rowsize ROW-SIZE:
    ldr temp textures-address:
    ; apply offset    
    mov offset @$4000      ; texture size
    mul iptr offset texnum ; size * index
    add iptr temp iptr     ; start+offset

    ; now we are ready to copy
    mov y @64
    ; equiv to whilenz
    (whilenz y {
      mov x @64
      (whilenz x {
        ; copy 4 bytes from source to dest
        ldr data [iptr] @4 
        str data [vptr] @4 
        sub x x @1
       })
      sub y y @1
      ; move down a row in video memory
      sub vptr vptr @(* 64 4)
      add vptr vptr screen-rowsize
    })
})

; test routine that displays each 2d picture on the screen
; this relies on the delay and image number loop
; from the gfx-update
(subr render-pic
      ([vptr x0]
       [picnum x1][offset x1][iptr x1]
       [header x2]
       [width w2]
       [height w3]
       [x w4]
       [y w5]
       [data w6]
       [rowsize x7]
       [screen-rowsize x8]
       [temp x9])
      [] {

    ldr screen-rowsize ROW-SIZE:
    ; lookup the pic header info
;    adr header DATA-2d-image-header:
   ldr header 2d-image-header-address:
    lsl picnum picnum @3        ; index * 8
    add header picnum header    ; offset
    ldr offset [header]      ; load struct
   ; width/height is in lower 16 bits
    mov width w1
    mov height w1
    mov temp @$FFFF
    and width width temp
    lsl rowsize width @2
    lsr height height @16
    ; data offset in top 32 bits, move it down
    lsr offset offset @32
    ; base image ptr
;    adr temp DATA-2d-images:
    ldr temp 2d-images-address:
    ; apply offset
    add iptr offset temp
    ; now we are ready to copy



    mov y height
:y-loop
    mov x width

;    bl dump-regs:
:x-loop
    ; copy 4 bytes from source to dest
    ldr data (iptr @0)

    str data (vptr @0)
    add iptr iptr @4

    add vptr vptr @4
    
    sub x x @1
    cbnz x x-loop-


    sub y y @1


    ; move down a row in video memory
    sub vptr vptr rowsize
    add vptr vptr screen-rowsize

    cbnz y y-loop-

          
    })



; core render loop
; clear screen then call other rendering functions
(subr update-gfx  (
                  [temp x4] ; temporary / intermediates
                  [tptr x8]
                  [vptr x9] ; video memory pointer
                  [row-size x14]
                  ) [x0 x1 x2 x3 x4]
                    {

:loop
    ldr w0 finished-rendering:
    cbnz x0 loop-
    bl get-back-buffer:
    mov vptr x0

    ; clear screen

    ; we do 8 pixels each iteration so
    ; we need 320*200/8 = $1F40 iterations
    mov temp @$1F40
    mov x3 @0

:next
    str x3 [vptr]
    str x3 (vptr @8)
    str x3 (vptr @16)
    str x3 (vptr @24)
    add vptr vptr @32

    sub temp temp @1
    cbnz temp next-
    mov vptr x0

    ; x27 holds a delay
    ; this should be in memory
    ; since we never even push these!
    sub x27 x27 @1
    cbnz x27 skip+
    mov x27 @10
    ; x28 holds next picture to display
    ; also can easily be trashed!
    sub x28 x28 @1

    cbnz x28 skip+
    mov x28 @105  ; 130 for 2d pics, 105 for textures
    
:skip    
    mov x0 vptr
    mov x1 x28

;    mov x1 @84
    
    ;bl render-pic:  ; render 2d pics
    bl render-tex:   ; render 2d textures
    ;signal render has finished
    mov temp @1
    adr x0 finished-rendering:
    str temp [w0]


    b loop-
   })


; interrupt handler
; at the moment we are expecting only the
; SMICS interrupt which is the VSYNC
; here we only flip the pge if the renderer
; has actualy finished
(subr handle-irq () [] {
;(debug-str "handled interrupt" #t)
;; ldr x0 TIMER_CS:
;; mov x1 @%10
;; str w1 (x0 @0)
  ldr x0 SMICS:  ; ack vsync
  mov x1 @0
  str w1 [x0]

  ; only flip if render finished
  ldr w0 finished-rendering:
  cbz w0 quit:
;  (debug-str "flip" #t)
  ldr x1 CURRENT-PAGE:
  cbnz x1 f+
  mov x3 @height
  b done+
:f
  mov x3 @0
:done
  adr x1 CURRENT-PAGE:
  str x3 [x1]
  
  bl page-flip:
  ;bl update-gfx:
  adr x1 finished-rendering:
  mov x2 @0
  str w2 [x1]

  
:quit})


(define-syntax (compile-condition stx)
  (syntax-parse stx #:datum-literals (/and /or)
  [(_ (lhs:register cond:condition rhs:register) branch-target:label-targ)
   #'{cmp lhs rhs
      cond.branch-opcode branch-target}]
  [(_ (lhs:register cond:condition #:immediate n) branch-target:label-targ)
   #'{cmp lhs @n
      cond.branch-opcode branch-target}]

  [(_ (/and cases ...) then-target:label-targ)
   ; this has come from an OR since we are in non-inverse.
   ; the target passed is the success case for the parent OR
   ; here then we want to go to that label if all of the ANDs
   ; mathc, or short circuit to a new label after these ANDs if any fail
   (let ([else-start (gen-label "else-start")])
     (with-syntax
       ([else-label (format-label else-start)]
        [else-target (format-target else-start '+)])
        #'{
           (compile-inverse-condition cases else-target) ...
           b then-target
           else-label 
           }))]
   ;  #'(/if-and (cases ...) then-target)]
  ))

(define-syntax (compile-inverse-condition stx)
  (syntax-parse stx #:datum-literals (/and /or)
  [(_ (lhs:register cond:condition rhs:register) branch-target:label-targ)
   #'{cmp lhs rhs
      cond.branch-inverse-opcode branch-target}]
  [(_ (lhs:register cond:condition #:immediate n) branch-target:label-targ)
   #'{cmp lhs @n
      cond.branch-inverse-opcode branch-target}]

  [(_ (/or cases ...) else-target:label-targ)
   ; this has come from an AND since we are in inverse.
   ; the target passed is the fail case for the parent AND
   ; here then we want to go to that label if none of the ORs
   ; mathc, or short circuit to a new label after these ORs if any match
   (let ([then-start (gen-label "then-start")])
     (with-syntax
       ([then-label (format-label then-start)]
        [then-target (format-target then-start '+)])
        #'{
           (compile-condition cases then-target) ...
           b else-target
           then-label 
           }))
   
   ;#'(/if-or (cases ...) else-target)

   ]

  ))
(define-syntax-parser /when-or
  [(_ (conditions ...) then-body) 
   (let ([then-start (gen-label "then-start")]
         [if-end (gen-label "if-end")]         )
     (with-syntax
       ([then-label (format-label then-start)]
        [then-target (format-target then-start '+)]
        [end-label (format-label if-end)]
        [end-target (format-target if-end '+)])
        #'{
           (compile-condition conditions then-target) ...
           b end-target
           then-label then-body
           end-label
           }))])

(define-syntax-parser /when-and
  [(_ (conditions ...) then-body) 
   (let (
         [if-end (gen-label "if-end")])
     (with-syntax
       (
        [end-label (format-label if-end)]
        [end-target (format-target if-end '+)])
        #'{
           (compile-inverse-condition conditions end-target) ...
           then-body
           end-label
           }))])

(define-syntax-parser /if-and
  [(_ (conditions ...) then-body else-body) ; parent is an and
   (let (
         [else-start (gen-label "else-start")]
         [if-end (gen-label "if-end")])
     (with-syntax
       (
        [else-label (format-label else-start)]
        [else-target (format-target else-start '+)]
        [end-label (format-label if-end)]
        [end-target (format-target if-end '+)])
        #'{
           (compile-inverse-condition conditions else-target) ...
           then-body
           b end-target
           else-label else-body
           end-label
           }))]

  )

(define-syntax-parser /if-or
  [(_ (conditions ...) then-body else-body) 
   (let ([then-start (gen-label "then-start")]
         [if-end (gen-label "if-end")]         )
     (with-syntax
       ([then-label (format-label then-start)]
        [then-target (format-target then-start '+)]
        [end-label (format-label if-end)]
        [end-target (format-target if-end '+)])
        #'{
           (compile-condition conditions then-target) ...
           else-body           
           b end-target
           then-label then-body
           end-label
           }))]
  )

(define-syntax-parser /if #:datum-literals (/and /or)
  [(_ (/and conditions ...) then-expr:expr else-expr:expr)
   #'(/if-and
      (conditions ...)
      then-expr
      else-expr
     )]

  [(_ (/or conditions ...) then-expr:expr else-expr:expr)
   #'(/if-or
      (conditions ...)
      then-expr
      else-expr
      )]
  [(_ condition:expr then-body:expr else-body:expr)
   (let ([then-start (gen-label "then-start")]
         [else-start (gen-label "else-start")]
         [if-end (gen-label "if-end")])
     (with-syntax
       ([then-label (string->symbol (format ":~a" then-start))]
        [then-target (string->symbol (format "~a+" then-start))]
        [else-label (format-label else-start)]
        [else-target (format-target else-start '+)]
        [end-label (string->symbol (format ":~a" if-end))]
        [end-target (string->symbol (format "~a+" if-end))])
        #'{
           (compile-condition condition then-target)
           else-label  else-body
           b end-target
           then-label then-body
           end-label
           }))])

(define-syntax-parser /when #:datum-literals (/and /or)
  [(_ (/and conditions ...) then-expr:expr)
   #'(/when-and
      (conditions ...)
      then-expr
     )]

  [(_ (/or conditions ...) then-expr:expr)
   #'(/when-or
      (conditions ...)
      then-expr
      )]
  [(_ condition:expr then-body:expr)
   (let ([then-start (gen-label "then-start")]
         [if-end (gen-label "if-end")])
     (with-syntax
       ([then-label (string->symbol (format ":~a" then-start))]
        [then-target (string->symbol (format "~a+" then-start))]
        [end-label (string->symbol (format ":~a" if-end))]
        [end-target (string->symbol (format "~a+" if-end))])
        #'{
           (compile-condition condition then-target)
           b end-target
           then-label then-body
           end-label
          }))])

  


   
(define-syntax-parser /for ; note this won't work for a loop that runs 0 times
  [(_ initializer:expr condition:expr update:expr body:expr)
   (let ([loop-start (gen-label "for-start")]
         [loop-end (gen-label "for-end")])
     (with-syntax
       ([start-label (format-label loop-start)]
        [start-target (format-target loop-start '-)]
        [end-label (format-label loop-end)]
        [end-target (format-target loop-end '+)]
        )
        #'{initializer
           start-label
           body
           update
           (/when condition {b start-target})
;           (compile-condition condition start-target end-target)
           end-label
           }))])

;; (define-syntax-parser /when
;;   [(_ condition:expr body:expr)
;;    (let ([true-start (gen-label "when-true")]
;;          [false-start (gen-label "when-false")])
;;      (with-syntax
;;        ([true-label (format-label true-start)]
;;         [true-target (format-target true-start '+)]
;;         [false-label (format-label false-start)]
;;         [false-target (format-target false-start '+)]
;;         )
;;        #'{
;;           ; jump away when the condition is false
;;            (compile-inverse-condition condition false-target true-target)
;;            true-label
;;              body
;;            false-label 
;;            }))])

;; (define-syntax-parser /if
;;   [(_ condition:expr then-body:expr else-body:expr)
;;    (let ([then-start (gen-label "then-start")]
;;          [else-start (gen-label "else-start")]
;;          [if-end (gen-label "if-end")])
;;      (with-syntax
;;        ([then-label (string->symbol (format ":~a" then-start))]
;;         [then-target (string->symbol (format "~a+" then-start))]
;;         [else-label (format-label else-start)]
;;         [else-target (format-target else-start '+)]
;;         [end-label (string->symbol (format ":~a" if-end))]
;;         [end-target (string->symbol (format "~a+" if-end))])
;;         #'{
;;            (compile-condition condition then-target)
;;            else-label  else-body
;;            b end-target
;;            then-label then-body
;;            end-label
;;           }))])

; //////////////////////////////////////////////
; WOLFENSTEIN CODE 
; //////////////////////////////////////////////
(define MAXACTORS 150)
(define MAP_HEIGHT 64)
(define MAP_WIDTH 64)
  
(/struct objstruct
 ([active          1] ;activetype enum
  [ticcount        2]
  [obclass         1] ;classtype enum
  [statetype       8] ; *state
  [flags           4] ; FL_SHOOTABLE, etc
  [distance        4] ; signed; if negative, wait for that door to open
  [dirtype         1] ; dirtype enum
  [x               2] ; fixed point
  [y               2] ; fixed point
  [tilex           2]
  [tiley           2]
  [areanumber      1]
  [viewx           2]
  [viewheight      2]
  [transx          2] ; fixed, in global coord
  [transy          2] ; fixed, in global coord
  [angle           2]
  [hitpoints       2]
  [speed           4] ; signed
  [temp1           2]
  [temp2           2]
  [hidden          2]
  [next            8] ; *next objstruct
  [prev            8] ; *prev objstruct
  [pad             4] ; I'm adding this so it is 8 byte aligned (72 bytes)
  ))


(subr setup-game-level
      ([raw-ptr x1]
       [lvl-ptr x2]
       [x x3]
       [y x4]
       [tile w5]
       [offset x6])

      [x1 x2 x3 x4 x5 x6] {

  ; setup the walls and actors
  ; x1 will point at the raw map data for level 1
  ; x2 will point at the 2d byte array tilemap          
  AREATILE = 107
  ldr raw-ptr maps-address: ; this is a linker resolved label
  ; each raw map section is a 16 bit word

  ; for some reason the map is stored sideways compared to the raw data
  ; so we have to calculate the index here;
  ; x is the row and y is the offset [x][y]

  nop
  ;; (/if2 (/and (tile eq @5) (/or (x gt @10) (y gt @10)) (offset ne @10))
  ;;      {mov x1 @1}
  ;;      {mov x1 @0})

  ;; (/if  (/or (x gt @10)
  ;;             (/and (tile eq @5) (offset eq @42))
  ;;             (y gt @10) )
  ;;      {mov x1 @1}
  ;;      {mov x1 @0})
  
  ;; (/when (/and (x gt @10) (y gt @10)) {
  ;;   adr lvl-ptr tilemap:
  ;; })    
  
  (/for { mov y @0 } (y lt @MAP_HEIGHT) (inc y) {
    (/for { mov x @0 } (x lt @MAP_WIDTH) (inc x) {
      ldrh tile [raw-ptr] @2
      (/when (tile lt @AREATILE) {                                                         
        ; lvlptr = tilemap: + (64 * x) + y
        adr lvl-ptr tilemap:
        lsl offset x @6 ; * 64
        add lvl-ptr lvl-ptr offset
        add lvl-ptr lvl-ptr y
        strb tile [lvl-ptr] 
      })
    })                                                     
  })
  
    
})


(subr scan-info-plane
      ([raw-ptr x1]
       [x x2]
       [y x3]
       [temp x4]
       [tile w5])
      [x1 x2 x3 x4] {

  ; scan second map data plane and spawn player, statics, guards etc
  ; for now we are only going to spawn the player        
  ldr raw-ptr maps-address: ; this is a linker resolved label          
  ; add 64 * 64 * 2 for start of second plane
  mov x4 @64
  lsl x4 x4 @7
  add raw-ptr raw-ptr x4

  (/for { mov y @0 } (y lt @MAP_HEIGHT) (inc y) {
    (/for { mov x @0 } (x lt @MAP_WIDTH) (inc x) {
      ldrh tile [raw-ptr] @2
      (/when (/and (tile le @22) (tile ge @19)) {
        (debug-str "FOUND PLAYER!" #t)
        (debug-reg x)
        (debug-reg y)
      })
    })                                                     
  })
          
})          
;; ///////////////////////////////////////////////////////
;; wolfenstein GAME STATE VARIABLES
;; ///////////////////////////////////////////////////////
/= 8
; wall locations (byte[64][64])
:tilemap (reserve (* 64 64))
/= 8
; actor locations (ptr[64][64]
:actorat (reserve (* 64 64 8))
/= 8
:objlist (reserve (* (objstruct/sizeof) MAXACTORS))






  ; the following values are used for loading 64 bit addresses/values via LDR(literal)
  ; and they must be 64 bit aliged otherwise the CPU faults!
/= 8
  ; resolve-global-label-address will tell the linker to write the absolute
  ; address of the exported labels here, post-link.  These might be quite far away and
  ; as such out of range of the adr instruction and some jumps.
  ; this lets us load them with ldr.
  :2d-image-header-address (resolve-global-label-address :DATA-2d-image-header)
  :2d-images-address (resolve-global-label-address :DATA-2d-images)
  :textures-address (resolve-global-label-address :DATA-textures)
  :maps-address (resolve-global-label-address :DATA-maps)
  ; end linker
  ; ///
  (periph-addresses)
  :dELAY (write-value-64 $FFFFF)
  :dELAY2 (write-value-64 200000)
  :VC_MBOX (write-value-64 VCORE-MBOX)
  :TEST (write-value-64 $123456789ABCDEF)
:START (write-value-64 $800000) ; stack location

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
;;   /= 16
;;   :MBOX-VOFFSET-MSG
;;     (write-value-32 (* 8 4)) ; total buffer size bytes including headers
;;     (write-value-32 0) ; request / response code (0 = request)
;;     ;begin tags
;;     ; set virtoff
;;       (write-value-32 $48009)
;;       (write-value-32 8) ; value buffer size
;;  :voffset-req
;; (write-value-32 0) ; 0 = request
;;       (write-value-32 0) ; x
;;    :voffset-y
;;       (write-value-32 0); y
;;     (write-value-32 0) ;end tag



})





