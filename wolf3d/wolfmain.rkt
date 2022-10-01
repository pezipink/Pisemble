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

(aarch64 "kernel8.img" ["2dgfx.obj"] {
     width = 320
     height = 200      
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
   movz x0 @$30 LSL @16 ; 3 << 20
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
    str w1 (x0 @0)


    ; now we are going to ask for a frame buffer
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
     mov x28 @130
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

(asm-struct _voffset_msg
  ([total-size 4]     ; total buffer size bytes including headers
   [request-code 4]  ; request / response code (0 = request)
   ;begin tags
   [tag-id 4]
   [request-size 4]
   [tag-request-code 4]
   [x-loc 4]
   [y-loc 4]
   [end-tag 4]))

(define-syntax-parser write-virtual-offset
  [(_ base:register y-loc:register)
   #'{
      (_voffset_msg/total-size-set (* 8 4) base) ;note this splats x0
      (_voffset_msg/request-code-set 0 base)
      (_voffset_msg/tag-id-set $48009 base)
      (_voffset_msg/request-size-set 8 base)
      (_voffset_msg/tag-request-code-set 0 base)
      (_voffset_msg/x-loc-set 0 base)
      (_voffset_msg/y-loc-set y-loc base)
      (_voffset_msg/end-tag-set 0 base)
     }])

(subr page-flip ([ptr x16]) [x0 x1 x2 x16] {
  ;pass y value in x3
  adr ptr MBOX-VOFFSET-MSG:
  (write-virtual-offset ptr x3)
  adr x8 MBOX-VOFFSET-MSG:
  bl send-vcore-msg:
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
    str colour (vptr @0)
    
    })

;; (begin-for-syntax
;;   (define labels (make-hash))
;;   (define (gen-label name)
;;     (if (hash-has-key? labels name)
;;         (let ([val (hash-ref labels name)])
;;           (hash-set! labels name (+ val 1))
;;           (format "~a_~a" name (+ val 1)))
;;         (begin
;;           (hash-set! labels name 0)
;;           (format "~a_~a" name 0)))))

;; (define-syntax-parser ~=
;;   [(_ x:register y:register target:label-targ)
;;    #'{
;;       ;todo: special case for when either case is x31 (zero reg)
;;       cmp x y
;;       b.eq target
;;       }])
;; (define-syntax-parser ~<>
;;   [(_ x:register y:register target:label-targ)
;;    #'{
;;       ;todo: special case for when either case is x31 (zero reg)
;;       cmp x y
;;       b.ne target
;;       }])
;; (define-syntax-parser ~and
;;   ;A will be 0 if all tests pass
;;   ;Success is always defined as 0
;;   [(_ test ...+ final)
;;    (let ([done-label (gen-label "and_done")])
;;    (with-syntax
;;      ([start-label  (gen-label "and_start")]
;;       [done-source (string->symbol (format ":~a" done-label))]
;;       [done-target (string->symbol (format "~a+" done-label))])
;;      #'{
;;      (set-jump-source-current start-label)        
;;      {test
;;       b.ne done-target
;;      } ...
;;      final
;;      done-source
;;      }))])

;; (define-syntax-parser ~or
;;   ;A will be 0 if any tests pass
;;   ;Success is always defined as 0
;;   [(_ test ...+ final)
;;    (let ([done-label (gen-label "or_done")])
;;    (with-syntax
;;      ([start-label  (gen-label "or_start")]
;;       [done-source (string->symbol (format ":~a" done-label))]
;;       [done-target (string->symbol (format "~a+" done-label))])
;;      #'{
;;      (set-jump-source-current start-label )        
;;      {test
;;       b.eq done-target      
;;       } ...
;;      final     
;;      done-source
;;      }))])

;; (define-syntax-parser ~if
;;   [(_ test true-code false-code)
;;    (let ([false-label (gen-label "if-false")]
;;          [end-label (gen-label "if-end")])         
;;    (with-syntax
;;      ([start-label  (gen-label "if_start")]
;;       [false-source (string->symbol (format ":~a" false-label))]
;;       [false-target (string->symbol (format "~a:" false-label))]
;;       [end-source (string->symbol (format ":~a" end-label))]
;;       [end-target (string->symbol (format "~a:" end-label))])
;;      #'{
;;         (set-jump-source-current  start-label)
;;         test
;;         b.ne false-target
;;         true-code
;;         b end-target
;;         false-source
;;         false-code
;;         end-source
;;       }))])

;; (define-syntax-parser ~while
;;   [(_ test body)
;;    (let ([loop-start (gen-label "while-start")])
;;      (with-syntax
;;        ([start-label (string->symbol (format ":~a" loop-start))]
;;         [start-target (string->symbol (format "~a-" loop-start))])
;;         #'{
;;            (set-jump-source-current  start-label)
;;            body
;;            test
;;            b.ne start-target
;;           }))])

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
    ldr offset (header @0)      ; load struct
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

; render
(subr update-gfx  (
                  [x x1]  ; current x and y pixels
                  [y x2]
                  [colour w3]
                  [temp x4] ; temporary / intermediates
                  [tptr x8]
                  [vptr x9] ; video memory pointer
                  [row-size x14]
                  [fc x15] ; frame-count
                  ) [x0 x1 x2 x3 x4]
                    {

:loop
    ldr w0 finished-rendering:
    cbnz x0 loop-
    bl get-back-buffer:
    mov vptr x0
;;     mov colour @0
;;     movk colour @$ff00 LSL @16
;;     mov x @0
;;     mov y @0
;;     bl plot:

;;     mov colour @$FF
;;     movk colour @$ff00 LSL @16
;;     mov x @319
;;     mov y @0
;;     bl plot:

;;     mov colour @$FF00
;;     movk colour @$ff00 LSL @16
;;     mov x @0
;;     mov y @199
;;     bl plot:

;;     ;; mov colour @0
;;     movz colour @$FFFF LSL @16
    
;; ;    movk colour @$ff LSL @24
;;     mov x @319
;;     mov y @199
;;     bl plot:

    ; copy the title screen bytes directly 
    ; there are 320*200*4 = 256,000 bytes
    ; we can move them in 8 byte chunks (64 bits)
    ; which is 32,000 operations
    ; we can use offseting and further reduce this to
    ; 8000 loops ($1F40)
;;     adr tptr 2d-images: 
;;     mov temp @$1F40
;; :next    
;;     ldr x3 (tptr @0)
;;     str x3 (vptr @0)
;;     ldr x3 (tptr @8)
;;     str x3 (vptr @8)
;;     ldr x3 (tptr @16)
;;     str x3 (vptr @16)
;;     ldr x3 (tptr @24)
;;     str x3 (vptr @24)

;;     add tptr tptr @32
;;     add vptr vptr @32

;;     sub temp temp @1
;;     cbnz temp next-

    ; clear screen
    
    mov temp @$1F40
    mov x3 @0

:next
    str x3 (vptr @0)
    str x3 (vptr @8)
    str x3 (vptr @16)
    str x3 (vptr @24)
    add vptr vptr @32

    sub temp temp @1
    cbnz temp next-
    mov vptr x0

    
    sub x27 x27 @1
    cbnz x27 skip+
    mov x27 @10
    sub x28 x28 @1
    cbnz x28 skip+
    mov x28 @130
    
:skip    
    mov x0 vptr
    mov x1 x28

;    mov x1 @84
    
    bl render-pic:
    ;signal render has finished
    mov temp @1
    adr x0 finished-rendering:
    str temp (w0 @0)


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
  str x3 (x1 @0)
  
  bl page-flip:
  ;bl update-gfx:
  adr x1 finished-rendering:
  mov x2 @0
  str w2 (x1 @0)  

  
:quit})


  ; the following values are used for loading 64 bit addresses/values via LDR(literal)
  ; and they must be 64 bit aliged otherwise the CPU faults!
/= 8
  :2d-image-header-address (resolve-global-label-address :DATA-2d-image-header)
  :2d-images-address (resolve-global-label-address :DATA-2d-images)
  (periph-addresses)
  :dELAY (write-value-64 $FFFFF)
  :dELAY2 (write-value-64 200000)
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




})





