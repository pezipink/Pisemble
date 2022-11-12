#lang pisemble
(require (for-syntax syntax/parse racket/stxparam racket/syntax))
(require syntax/parse/define)

(require racket/stxparam)
(require racket/file)
(require racket/string)
(require racket/match)
(require racket/math)
(require threading)
(require "periph.rkt" "stack.rkt" "struct.rkt" "expr.rkt")

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
(define SCTLR_VALUE_MMU_DISABLED2  (bitwise-ior SCTLR_RESERVED SCTLR_EE_LITTLE_ENDIAN SCTLR_I_CACHE_ENABLED SCTLR_D_CACHE_DISABLED SCTLR_MMU_DISABLED))
(define SCTLR_VALUE_MMU_ENABLED  (bitwise-ior SCTLR_RESERVED SCTLR_EE_LITTLE_ENDIAN SCTLR_I_CACHE_ENABLED SCTLR_D_CACHE_ENABLED SCTLR_MMU_ENABLED))

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

; architectural feature access control register
(define CPACR_EL1_FPEN    (bitwise-ior (arithmetic-shift 1 21) (arithmetic-shift 1 20))) ; // don't trap SIMD/FP registers
(define CPACR_EL1_ZEN     (bitwise-ior (arithmetic-shift 1 17) (arithmetic-shift 1 16))) ;  // don't trap SVE instructions
(define CPACR_EL1_VAL     (bitwise-ior CPACR_EL1_FPEN CPACR_EL1_ZEN))

(define TCR_TG1_4K     (arithmetic-shift 2 30))
(define TCR_T1SZ       (arithmetic-shift (- 64 48) 16))
(define TCR_TG0_4K     (arithmetic-shift 0 14))
(define TCR_T0SZ       (- 64 48))
(define TCR_EL1_VAL    (bitwise-ior TCR_TG1_4K TCR_T1SZ TCR_TG0_4K TCR_T0SZ))

; mmu bits
(define MT_DEVICE_nGnRnE 		$0)
(define MT_NORMAL_NC			$1)
(define MT_DEVICE_nGnRnE_FLAGS		$00)
(define MT_NORMAL_NC_FLAGS  		$44)
(define MAIR_VALUE			(bitwise-ior (arithmetic-shift MT_DEVICE_nGnRnE_FLAGS (* 8 MT_DEVICE_nGnRnE)) (arithmetic-shift MT_NORMAL_NC_FLAGS (* 8 MT_NORMAL_NC))))

(define TD_VALID                   (arithmetic-shift 1 0))
(define TD_BLOCK                   (arithmetic-shift 0 1))
(define TD_TABLE                   (arithmetic-shift 1 1))
(define TD_ACCESS                  (arithmetic-shift 1 10))
(define MATTR_NORMAL_NC            $42)
(define MATTR_DEVICE_nGnRnE_INDEX  0)
(define MATTR_NORMAL_NC_INDEX      1)
(define TD_KERNEL_PERMS            (arithmetic-shift 1 54))
(define TD_INNER_SHARABLE          (arithmetic-shift 3 8))

(define TD_KERNEL_TABLE_FLAGS      (bitwise-ior TD_TABLE  TD_VALID))
(define TD_KERNEL_BLOCK_FLAGS      (bitwise-ior TD_ACCESS TD_INNER_SHARABLE TD_KERNEL_PERMS (arithmetic-shift MATTR_NORMAL_NC_INDEX 2) TD_BLOCK TD_VALID))
(define TD_DEVICE_BLOCK_FLAGS      (bitwise-ior TD_ACCESS TD_INNER_SHARABLE TD_KERNEL_PERMS (arithmetic-shift MATTR_DEVICE_nGnRnE_INDEX 2) TD_BLOCK TD_VALID))


(define PAGE_SHIFT 12)
(define TABLE_SHIFT 9)
(define SECTION_SHIFT (+ PAGE_SHIFT  TABLE_SHIFT))
(define PAGE_SIZE (arithmetic-shift 1 PAGE_SHIFT))
(define SECTION_SIZE (arithmetic-shift 1 SECTION_SHIFT))

(define PGD_SHIFT              (+ PAGE_SHIFT (* 3 TABLE_SHIFT)))
(define PUD_SHIFT              (+ PAGE_SHIFT (* 2 TABLE_SHIFT)))
(define PMD_SHIFT              (+ PAGE_SHIFT  TABLE_SHIFT))
(define PUD_ENTRY_MAP_SIZE     (arithmetic-shift 1 PUD_SHIFT))
(define BLOCK_SIZE $40000000)
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

(define-syntax-parser debug-str-reg
  [(_ str:string reg:register)
   #'{
      (debug-str str #f)
      (debug-reg reg)
     }])

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

(define-syntax-parser flip-gpio0
  [(_) #'{
          (preserve [x0 x1] {
            ldr x1 flip0:                   
            ;invert value
            mov x0 @1
            eor x1 x1 x0
            ; store new value
            adr x0 flip0:
            str x1 [x0]
            ; set out or clear depending on value
            (/if (x1 eq @0)
              {
               mov x1 @1
               ldr x0 GPIOSET0:
               str w1 [x0]
              }
              {
                mov x1 @1
                ldr x0 GPIOCLEAR0:
                str w1 [x0]
              })
          })                             
          }])

(define-syntax-parser flip-gpio1
  [(_) #'{
          (preserve [x0 x1] {
            ldr x1 flip1:                   
            ;invert value
            mov x0 @1
            eor x1 x1 x0
            ; store new value
            adr x0 flip1:
            str x1 [x0]
            ; set out or clear depending on value
            (/if (x1 eq @0)
              {
               mov x1 @%10
               ldr x0 GPIOSET0:
               str w1 [x0]
              }
              {
                mov x1 @%10
                ldr x0 GPIOCLEAR0:
                str w1 [x0]
              })
          })                             
         }])

(aarch64 "kernel8.img" ["2dgfx.obj" "textures.obj" "maps.obj"] {
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

     ; wolf bits
     TILESHIFT = 16
     TILEGLOBAL = (arithmetic-shift 1 16)
     TILEGLOBAL/2 = (/ TILEGLOBAL 2)
     ANGLES = 360
     FINEANGLES = 3600
     ANGLEQUAD = (/ 360 4)
     VIEWHEIGHT = 160
     VIEWWIDTH = 320
     TEXTUREMASK = 4032
     TEXTUREFROMFIXEDSHIFT = 4
     MINDIST = $5800
     TEXTURESIZE = (arithmetic-shift 1 6)
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
    ;; ldr     x0 SCTLR-VALUE-MMU-DISABLED:
    ;; msr	    sctlr_el1 x0		

    ldr     x0 HCR-VALUE:
    msr     hcr_el2 x0

    ldr     x0 SCR-VALUE:
    msr     scr_el3 x0

    ldr     x0 SPSR-VALUE:
    msr     spsr_el3 x0

    ; ===============
    ; MMU

    ldr x0 CPACR_EL1_VAL:
    (write-value-32 $d5181040) ; msr cpacr_el1 x0

    ldr x0 TCR_EL1_VAL:
    (write-value-32 $d5182040) ; msr tcr_el1 x0

    ldr x0 MAIR_VALUE:
    (write-value-32 $d518a200) ; msr MAIR_EL1, x0
    (write-value-32 $d539f220)
    (write-value-32 $b27a0000)
    (write-value-32 $d519f220)


    
    ; end MMU
    ; ==================
    adr     x0 el1_entry:
    msr     elr_el3 x0

    eret			

:el1_entry


    ; Core entry point at Exception level 1 

    ; Set stack pointer 
    ldr x1 START:
    mov sp x1


    ; =========== enable mmu 
    bl init-mmu:        

    ; write table locatio
    adr x0 id_pgd:
    (write-value-32 $d5182000) ;  msr ttbr0_el1, x0


    ; flip mmu enable bit

    ldr     x0 SCTLR-VALUE-MMU-ENABLED:
;    msr	    sctlr_el1 x0		

    mrs x0 sctlr_el1
    mov x1 @SCTLR_MMU_ENABLED
    orr x0 x0 x1

    mov x1 @SCTLR_I_CACHE_ENABLED
    orr x0 x0 x1

    mov x1 @SCTLR_D_CACHE_ENABLED
    orr x0 x0 x1

    ;; msr sctlr_el1 x0

    (write-value-32 $d5033f9f) ;dsb sy
    (write-value-32 $d5033fdf) ; isb
    ; ============== end mmu
    
    ; setup the mini-uart for debug comms
    (init-uart)
    (debug-str "uart up" #t)

    
    ; set gpio 0 and 1 to output
    ldr x0 GPFSEL:
    mov x1 @%001001
    str w1 [x0]

    
    
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

    ; calculate costable pointer into sintable
    adr x1 sintable:
    (debug-str-reg "sintable address " x1)
    ; here we want to do sintable+90 (360/4)    
    ; but since they are 4 bytes each we can
    ; simply use 360 since we'd have to << 2 anway

    (load-immediate x0 ANGLES)
    add x0 x1 x0
    adr x1 costable: ; index 0 seems to be 65537 instead of 65536!? check this
    str x0 [x1]
    (debug-str-reg "costable address " x0)

    
    ;    adr x1 disassemble-dump:
    ;; mov x1 @0
    ;; mov x2 @512
    ;; bl disassemble-dump:
    
    ;; adr x0 tilemap:
    ;; mov x1 @256
    ;; bl memory-dump:
    
    
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


    ; set alpha
;    bl set-alpha-mode:
    
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

          mov x1 @15
          ;     bl render-pic:
;     bl dump-regs:
     bl update-gfx:
 :loop
     b loop-

  ENTRIES-PER-TABLE = 512
  ENTRIES-PER-TABLE-1 = 511

(define-syntax-parser create-table-entry
 [(_ tbl:register next-tbl:register va:register shift temp1:register temp2:register temp3:register)
  #'{
     lsr temp1 va @shift                ; table_index = va >> shift
     mov temp2 @ENTRIES-PER-TABLE-1    
     and temp1 temp1 temp2              ; table index &= entries-1
     (load-immediate temp3 TD_KERNEL_TABLE_FLAGS)
     orr temp2 next-tbl temp3           ; descriptor = next_tbl | flags
     lsl temp1 temp1 @3                 ; table_index <<= 3
     add temp1 temp1 tbl                ; table_index += tbl
     str temp2 [temp1]                  ; [table_index] = descriptor
     }])

(define-syntax-parser create-block-map
 [(_ pmd:register vstart:register vend:register pa:register temp1:register temp2:register temp3:register temp4:register temp5:register)
  #'{
     ; vstart >>= section shift
     lsr temp1 vstart @SECTION_SHIFT
     ; vstart &= entries-1     
     mov temp2 @ENTRIES-PER-TABLE-1    
     and temp1 temp1 temp2

     ; vend >>= section shfit
     lsr temp2 vend @SECTION_SHIFT
     ; vend--
     sub temp2 temp2 @1
     ;vend &= entries-1
     mov temp3 @ENTRIES-PER-TABLE-1    
     and temp2 temp2 temp3

     lsr temp3 pa @SECTION_SHIFT
     lsl temp3 temp3 @SECTION_SHIFT

 :do-loop
     mov temp4 temp3 ; _pa = pa
     (load-immediate temp5 $3B400000)
;     (debug-str-reg "3b is " temp5)
     (/if (temp3 ge temp5)
          {
           (load-immediate temp5 TD_DEVICE_BLOCK_FLAGS)
           orr temp4 temp4 temp5
           }
          {
           (load-immediate temp5 TD_KERNEL_BLOCK_FLAGS)
           orr temp4 temp4 temp5
           })
     lsl temp5 temp1 @$3
     add temp5 temp5 pmd
     str temp4 [temp5]
     (load-immediate temp4 SECTION_SIZE)
     add temp3 temp3 temp4 ; pa += section size
     add temp1 temp1 @1 ; vsize ++
     (/when (temp1 le temp2) { b do-loop- })
     
   }])
     
:init-mmu
nop
(regs
 ([map_base x1]
  [tbl x2]
  [next_tbl x3]
  [block_tbl x4]
  [i x5]
  [offset x6]
  [offset_block x7]) {
  mov map_base @0             ;map_base
  adr tbl id_pgd:        ;tbl
  (load-immediate x0 PAGE_SIZE)
  add next_tbl tbl x0  ;next_tbl
  ;  (load-immediate x4 PGD_SHIFT)
  ;; (debug-str "create-table-entry main" #t)
  ;; (debug-str-reg "tbl " tbl)
  ;; (debug-str-reg "next_tbl " next_tbl)
  ;; (debug-str-reg "map_base " map_base)
  (create-table-entry tbl next_tbl map_base PGD_SHIFT x10 x11 x12)
  (load-immediate x0 PAGE_SIZE)
  add tbl tbl x0  ; tbl += page size
  add next_tbl next_tbl x0  ; next_tbl += page size
  mov block_tbl tbl     ; block_tbl = tbl
  (/for {mov i @0} (i lt @4) {add i i @1} {
   ;; (debug-str "create-table-entry " #t)
   ;; (debug-str-reg "tbl " tbl)
   ;; (debug-str-reg "next_tbl " next_tbl)
   ;; (debug-str-reg "map_base " map_base)

   (create-table-entry tbl next_tbl map_base PUD_SHIFT x10 x11 x12)

   (load-immediate x0 PAGE_SIZE)
   add next_tbl next_tbl x0  ; next_tbl += page size
   add block_tbl block_tbl x0  ; block_tbl += page size
   (load-immediate x0 PUD_ENTRY_MAP_SIZE)
   add map_base map_base x0  ; map_base += entry map size

   (load-immediate x0 BLOCK_SIZE)
   mul offset x0 i    ; offset = blocksize * i

   add offset_block offset x0    ; offset_block = offset + blocksize

   ;; (debug-str "create-block-map " #t)
   ;; (debug-str-reg "block-tbl " block_tbl)
   ;; (debug-str-reg "offset " offset)
   ;; (debug-str-reg "offset_block " offset_block)

  (create-block-map block_tbl offset offset_block offset x10 x11 x12 x13 x14)
   
   
  })

  ;; adr x0 id_pgd:
  ;; mov x1 @(+ 4 (/ $6000 16))
  ;;  bl memory-dump:
})
  ret x30
     
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
  (/whilenz x8 {
    ; first part is to overwrite the address with the requested address
    adr x6 memory-dump-template:
    mov x3 x0           ; address to write
    mov x2 @16          ; loop counter (+1)
    mov x5 @$F          ; because we don't support and x x imm
    (/whilenz x2 {
      lsr x4 x3 @60     ; shift to byte
      and x4 x4 x5      ; mask the 4 bits
      (nibble->hex x4)  ; convert to ascii hex
      strb w4 [x6] @1   ; modify string with result
      lsl x3 x3 @4      ; shift in next nibble
      sub x2 x2 @1      ; 
    })             


    add x6 x6 @3 ; spacer

    mov x7 @4
    (/whilenz x7 {
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

    (/whilenz x7 {
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
    (/whilenz instruction-count {
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

  (/whilenz len {
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

;; (subr send-string-nullterm
;;       ([address x1]) [x0] {

;;   ; initiate string send
;;   mov w0 @1                       
;;   bl send-char:

;;   (/whilenz x0 {
;;     ldrb x0 [address] @1  ; x0 = *address++
;;     bl send-char:
;;   })
;;   mov w0 @$A ; line feed
;;   bl  send-char:
;;   ; signal string end
;;   mov w0 @0
;;   bl send-char:
  
;; })

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

(/struct _setalpha_msg
  ([total-size 4]     ; total buffer size bytes including headers
   [request-code 4]   ; request / response code (0 = request)
   ;begin tags
   [tag-id 4]
   [request-size 4]
   [tag-request-code 4]
   [alpha-mode 4]         ; alpha mode
   [end-tag 4]))

(define-syntax-parser write-virtual-offset
  [(_ base:register y-loc:register)
   #'{ ;note this trashes x0
      (zero-mem base (sizeof/_voffset_msg))
      (_voffset_msg/total-size-set (* 8 4) base)
      (_voffset_msg/tag-id-set $48009 base)
      (_voffset_msg/request-size-set 8 base)
      (_voffset_msg/y-loc-set y-loc base)
      }])

(define-syntax-parser write-alpha-mode
  [(_ base:register a-mode:register)
   #'{ ;note this trashes x0
      (zero-mem base (sizeof/_setalpha_msg))
      (_setalpha_msg/total-size-set (* 7 4) base)
      (_setalpha_msg/tag-id-set $48007 base)
      (_setalpha_msg/request-size-set 4 base)
      (_setalpha_msg/alpha-mode-set a-mode base)
     }])

; virtual "page flip" here we tell the gpu to
; display the off-screen portion of the video memory
; that has been drawn to
; pass y value in x3
(subr page-flip ([ptr x16]) [x0 x1 x2 x16] {
  ; reserve space on stack for v-offset message
  sub sp sp @(sizeof/_voffset_msg)                                          
  (write-virtual-offset sp x3)
  mov x8 sp
  bl send-vcore-msg:
  add sp sp @(sizeof/_voffset_msg)                                          
  })

(subr set-alpha-mode () [x0 x1 x2 x16] {
  ; reserve space on stack for v-offset message
  sub sp sp @(sizeof/_setalpha_msg)
  mov x10 @2                                              
  (write-alpha-mode sp w10)
  mov x8 sp
  bl send-vcore-msg:
  add sp sp @(sizeof/_setalpha_msg)                                          
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
    (/whilenz y {
      mov x @64
      (/whilenz x {
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
                    [wtemp w5]
                  [tptr x8]
                  [vptr x9] ; video memory pointer
                  [vptr-bak x10]
                  [row-size x14]
                  ) [x0 x1 x2 x3 x4]
                    {

:loop
    ldr w0 finished-rendering:
    cbnz x0 loop-
    (flip-gpio1)
    bl get-back-buffer:
    mov vptr x0
    mov vptr-bak x0
    ; ===================
    ; clear screen
    
    ; we do 8 pixels each iteration so
    ; we need 320*200/8 = $1F40 iterations
;;     mov temp @$1F40
;;     mov x3 @0
;; :next
;;     str x3 [vptr]
;;     str x3 (vptr @8)
;;     str x3 (vptr @16)
;;     str x3 (vptr @24)
;;     add vptr vptr @32

;;     sub temp temp @1
;;     cbnz temp next-
;;     mov vptr x0

    ; end clear screen
    ; ====================


    ; only once code 
    adr x0 debug-flag:
    ldr x0 [x0]
    cbz x0 done+
    mov x1 vptr-bak
;    (debug-str "debg " #t)
    (preserve [x4 x8 x9 x10 x14] {
    bl ThreeDRefresh:   ; raycaster
       })

    ldr tptr player:
    (_objstruct/angle-get wtemp tptr)    
    add wtemp wtemp @1
    (/when (wtemp ge @360) {mov wtemp @0})
    (_objstruct/angle-set wtemp tptr) 

    
    adr x0 debug-flag:
    ldr temp [x0]
    sub temp temp @1
    adr x0 debug-flag:
    str temp [x0]

    
    
    :done
    ;signal render has finished
    mov temp @1
    adr x0 finished-rendering:
    str temp [x0]

    (flip-gpio1)
    b loop-
})

(define-syntax-parser FixedMul
  ; these should be x registers only, cba to put a check in
  ; trashes x0
  ; DO NOT USE x0 AS TARGET!
  ; make sure you SIGN ENTEND into x and y first!
  ([_ target:register x:register y:register]
   #'{
      ; TODO: madd here when i implement it
      mul target x y
      (load-immediate x0 $8000)
      add target target x0
      asr target target @16 
     }))

(subr VgaClearScreen
      ([vptr x1]
       [temp x2]       
       [x x3]
       [y x4]
       [ptr x5]
       [colour w6])
      [x1] {

   adr ptr palette:
   mov temp @$1d    
   lsl temp temp @2
   add ptr ptr temp
   ldr colour [ptr]

   ; ceiling hardcoded to palette $1d for now
   (/for { mov y @0 } (y lt @(/ VIEWHEIGHT 2)) (inc y) {
     (/for { mov x @0 } (x lt @$50) (inc x) {                                                   
         str colour [vptr]
         str colour [vptr @4]
         str colour [vptr @8]
         str colour [vptr @12]
         add vptr vptr @16
     })

   })

   ; floor is always $19
   adr ptr palette:
   mov temp @$19    
   lsl temp temp @2
   add ptr ptr temp
   ldr colour [ptr]

   (/for { } (y lt @VIEWHEIGHT) (inc y) {
     (/for { mov x @0 } (x lt @$50) (inc x) {                                                       str colour [vptr]
         str colour [vptr @4]
         str colour [vptr @8]
         str colour [vptr @12]
         add vptr vptr @16
     })
   })

   })
(subr ScalePost
      ([vptr x1]

       [ywcount w2]
       [yoffs w3]
       [yw w4]
       [yd w5]
       [yendoffs w6]
       [col w7]
       [ptr x8]
       )
      [x1] {
  ; scales and draws a strip of pixels from a wall texture

;  (debug-str "enter scalepost " #t)

  ;ywcount = yd = wallheight[postx] >> 3;
  adr ptr wallheight:
  ldr w10 postx:
  lsl w10 w10 @2
  add ptr ptr w10
  ldr w10 [ptr]
;  (debug-str-reg "wallheight[postx] " w10)
  lsr w10 w10 @3
;  (debug-str-reg ">>3 " w10)
  mov ywcount w10
  mov yd w10
  (/when (yd le @0) { mov yd @100 })

  ;yoffs = (viewheight / 2 - ywcount) * vbufPitch;
  mov w10 @(/ VIEWHEIGHT 2)
  sub w10 w10 ywcount
  ldr w11 vbufPitch:
  mul yoffs w10 w11
;  (debug-str-reg "yoffs " yoffs)
  (/when (yoffs lt @0) { mov yoffs @0 })

  ; yoffs += postx
  ldr w10 postx:
  add yoffs yoffs w10
;  (debug-str-reg "yoffs " yoffs)

 ; yendoffs = viewheight / 2 + ywcount - 1;
  mov w10 @(/ VIEWHEIGHT 2)
  add w10 w10 ywcount
  sub yendoffs w10 @1
;  (debug-str-reg "yendoffs " yendoffs)
  
  ;yw=TEXTURESIZE-1;
  mov yw @(- TEXTURESIZE 1)
;  (debug-str-reg "yw " yw)
  
  ; this loops scales when the wall takes up more than the
  ; vertical size of the screen, don't need it for first
  ; render (TODO)
   ;; while(yendoffs >= viewheight)
   ;;  {
   ;;      ywcount -= TEXTURESIZE/2;
   ;;      while(ywcount <= 0)
   ;;      {
   ;;          ywcount += yd;
   ;;          yw--;
   ;;      }
   ;;      yendoffs--;
   ;;  }
   ;;  if(yw < 0) return;

  ;col = postsource[yw]
  ldr ptr postsource:
  lsl w10 yw @2
  add ptr ptr w10
  ldr col [ptr] ; this is the 32bit pixel data 
;  (debug-str-reg "first col is " col)
  
  ;yendoffs = yendoffs * vbufPitch + postx;
  ldr w10 vbufPitch:
  ldr w11 postx:
  mul yendoffs yendoffs w10
  add yendoffs yendoffs w11
;  (debug-str-reg "yendoffs before loop " yendoffs)

;  (debug-str "entering loop .." #t)
  (/while (yoffs le yendoffs) {
;    (debug-str-reg "loop yoffs " yoffs)
    
    mov x20 vptr
    lsl x10 yendoffs @2
    add x20 x20 x10
    str col [x20]  ; write pixel data to video memory

    ;ywcount -= TEXTURESIZE/2
    mov x10 @(/ TEXTURESIZE 2)
    sub ywcount ywcount x10
  ;  (debug-str-reg "iloop ywcount " ywcount)
    (/when (ywcount le @0) {
  :top-loop

      add ywcount ywcount yd
 ;     (debug-str-reg "ywcount " ywcount)
      sub yw yw @1
;      (debug-str-reg "yw " yw)
      (/when (ywcount le @0)
             {
  ;            (debug-str "jmp 1 " #t)
              b top-loop-
              })
      (/when (yw lt @0)
             {
 ;             (debug-str "jmp 2 " #t)
              b local-end+
             } )
      ;col = postsource[yw]
      ldr ptr postsource:
      lsl w10 yw @2
      add ptr ptr w10
      ldr col [ptr] ; this is the 32bit pixel data 
;      (debug-str-reg "col is " col)      
    })
    ldr w10 vbufPitch:
    sub yendoffs yendoffs w10
;    (debug-str-reg "yendoffs nojw " yendoffs)
  })
:local-end                               
;  (debug-str "exit scalepost " #t)            
})

(subr HitHorizWall
      ([vptr x1]

       [wallpic w2]
       [texture w3]
       [ptr x4]
       )
      [x1] {
;  (debug-str "in hithorizwall" #t)
  ;texture = ((xintercept+texdelta)>>TEXTUREFROMFIXEDSHIFT)&TEXTUREMASK;  
  ldr w10 xintercept:
  ldr w11 texdelta:
  add w10 w10 w11
  asr w10 w10 @TEXTUREFROMFIXEDSHIFT
  mov w12 @TEXTUREMASK
  and w10 w10 w12
  mov texture w10
;  (debug-str-reg "texture " w10)

  ;if ytilestep == -1
  ldr w10 ytilestep:
  mov w11 @0
  sub w11 w11 @1 ; -1
  (/if (w10 eq w11)
       {
         ;yintercept += TILEGLOBAL;
         ldr w10 yintercept:
         (load-immediate w11 TILEGLOBAL)
         add w10 w10 w11
         adr ptr yintercept:
         str w10 [ptr]
       }
       ;else 
       {
         ; texture = TEXTUREMASK-texture
         mov w11 @TEXTUREMASK
         sub texture w11 texture
       })
  
  ;if(lastside!=-1) ScalePost();
  ldr w10 lastside:
  mov w11 @0
  sub w11 w11 @1
  (/when (w10 ne w11) (preserve [x1 x2 x3 x4] { bl ScalePost: }))

  ;lastside = 0
  mov x10 @0
  adr ptr lastside:
  str w10 [ptr]

  ;lastintercept = ytile
  ldr w10 ytile:
  adr ptr lastintercept:
  str w10 [ptr]

  ;lasttilehit = tilehit
  ldr w10 tilehit:
  adr ptr lasttilehit:
  str w10 [ptr]
  
  ;lasttexture = texture
  adr ptr lasttexture:
  str texture [ptr]


  ;wallheight[pixx] = CalcHeight()
  (preserve [x1 x2 x3 x4] { bl CalcHeight: })
 ; (debug-str-reg "calcheight " x0)
  adr ptr wallheight:
  ldr w10 pixx:
  lsl w10 w10 @2
  add ptr ptr w10
  str w0 [ptr]
  
  ;postx = pixx
  ldr w10 pixx:
  adr ptr postx:
  str w10 [ptr]


  ;; ;wallpic = horizwall[tilehit]  ;
  ldr w10 tilehit:
  lsl w10 w10 @1
  adr ptr horizwall:
  add ptr ptr w10
  ldrh wallpic [ptr]
;  (debug-str-reg "wallpix " wallpic)

  ;postsource = PM_GetTexture(wallpic) + texture
  ; this is a bit different for us since we don't
  ; have a page manager and are not using palette indexes.

  ; we need to find the start of the image data for wallpic index
  ; then offset into it texture << 2

  ldr ptr textures-address:
  mov w10 @$4000      ; texture size
  mul w11 w10 wallpic ; size * index
  add ptr ptr w11     ; start+offset

  ; now we are pointing at the start of image
  ; move along texture * 4 bytes
  lsl w10 texture @2
  add ptr ptr w10

  mov x10 ptr
  adr ptr postsource:
  str x10 [ptr]

  
  })

(subr HitVertWall
      ([vptr x1]

       [wallpic w2]
       [texture w3]
       [ptr x4]
       )
      [x1] {
;  (debug-str "in hitvertwall" #t)
  ;texture = ((yintercept+texdelta)>>TEXTUREFROMFIXEDSHIFT)&TEXTUREMASK;  
  ldr w10 yintercept:
  ldr w11 texdelta:
  add w10 w10 w11
  asr w10 w10 @TEXTUREFROMFIXEDSHIFT
  mov w12 @TEXTUREMASK
  and w10 w10 w12
  mov texture w10
;  (debug-str-reg "texture " w10)

  ;if xtilestep == -1
  ldr w10 xtilestep:
  mov w11 @0
  sub w11 w11 @1 ; -1
  (/when (w10 eq w11)
       {
         ; texture = TEXTUREMASK-texture
         mov w11 @TEXTUREMASK
         sub texture w11 texture
         ;xintercept += TILEGLOBAL;
         ldr w10 xintercept:
         (load-immediate w11 TILEGLOBAL)
         add w10 w10 w11
         adr ptr xintercept:
         str w10 [ptr]             
       })
  
  ;if(lastside!=-1) ScalePost();
  ldr w10 lastside:
  mov w11 @0
  sub w11 w11 @1
  (/when (w10 ne w11) (preserve [x1 x2 x3 x4] { bl ScalePost: }))

  ;lastside = 1
  mov x10 @1
  adr ptr lastside:
  str w10 [ptr]

  ;lastintercept = xtile
  ldr w10 xtile:
  adr ptr lastintercept:
  str w10 [ptr]

  ;lasttilehit = tilehit
  ldr w10 tilehit:
  adr ptr lasttilehit:
  str w10 [ptr]
  
  ;lasttexture = texture
  adr ptr lasttexture:
  str texture [ptr]


  ;wallheight[pixx] = CalcHeight()
  (preserve [x1 x2 x3 x4] { bl CalcHeight: })
;  (debug-str-reg "calcheight " x0)
  adr ptr wallheight:
  ldr w10 pixx:
  lsl w10 w10 @2
  add ptr ptr w10
  str w0 [ptr]
  
  ;postx = pixx
  ldr w10 pixx:
  adr ptr postx:
  str w10 [ptr]


  ;; ;wallpic = vertwall[tilehit]  ;
  ldr w10 tilehit:
  lsl w10 w10 @1
  adr ptr vertwall:
  add ptr ptr w10
  ldrh wallpic [ptr]
;  (debug-str-reg "wallpix " wallpic)

  ;postsource = PM_GetTexture(wallpic) + texture
  ; this is a bit different for us since we don't
  ; have a page manager and are not using palette indexes.

  ; we need to find the start of the image data for wallpic index
  ; then offset into it texture << 2

  ldr ptr textures-address:
  mov w10 @$4000      ; texture size
  mul w11 w10 wallpic ; size * index
  add ptr ptr w11     ; start+offset

  ; now we are pointing at the start of image
  ; move along texture * 4 bytes
  lsl w10 texture @2
  add ptr ptr w10

  mov x10 ptr
  adr ptr postsource:
  str x10 [ptr]

  
})

(subr CalcHeight
      ([vptr x1]

       )
      [x1] {
;  (debug-str "in calcheight " #t)
  ;fixed z = FixedMul(xintercept - viewx, viewcos) - FixedMul(yintercept - viewy, viewsin);
  ldr w10 xintercept:
  ldr w11 viewx:                
  ldr w12 viewcos:
;  (debug-str-reg "xintercept " w10)
;  (debug-str-reg "viewx " w11)
;  (debug-str-reg "viewcos " w12)
  sub w10 w10 w11
  sxtw x10 w10
  sxtw x12 w12
  (FixedMul x20 x10 x12)
;  (debug-str-reg "fixed1 " x20)
  
  ldr w10 yintercept:
  ldr w11 viewy:                
  ldr w12 viewsin:
  sub w10 w10 w11
  sxtw x10 w10
  sxtw x12 w12
  (FixedMul x21 x10 x12)
;  (debug-str-reg "fixed2 " x21)
  ; x10 = z
  sub w10 x20 x21
 ; (debug-str-reg "z " w10)
  ; if (z < mindist) z = mindist
  mov x11 @MINDIST
  (/when (w10 lt x11) { 
    mov w10 @MINDIST
  })

  ; int height = heightnumberator / (z>>8)
  asr w10 w10 @8
  ldr w11 heightnumerator:
;  (debug-str-reg "asr z " w10)
;  (debug-str-reg "numerator " w11)
  udiv w10 w11 w10
;  (debug-str-reg "udiv " w10)
  
 ; pretty sure this code isn't used/*
 ;   if(height < min_wallheight)
 ;       min_wallheight = height;
  ;   */

  mov x0 x10 
})

(subr AsmRefresh
      ([vptr x1]

       [xstep w2]
       [ystep w3]
       [xpartial w4]
       [ypartial w5]
       [ptr x6]
       [xtemp x7]

       [local-pixx w8]
       [wtemp w9]

       )
      [x1] {
  ; NOTE: this function uses x10+ for all item temporaries, except the named ones above
  ; (and our old friend x0 of course)
 
  ; this is the actual raycaster

;  (debug-str "enter asm referesh" #t)          
  mov xstep @0
  mov ystep @0
  mov xpartial @0
  mov ypartial @0

  ; for each X column of the screen ..
  (/for {mov local-pixx @0} (local-pixx lt  @320) (inc local-pixx) {
;    (debug-str "pixx loop" #t)
;    (debug-str-reg "LOOPINDEX " local-pixx)
    ; store global pixx (keep in sync with loop counter)
    adr ptr pixx:
    str local-pixx [ptr]
    ; perform angle calcs
    
    ;w0 = midangle + pixelangle[pixx]
    adr ptr pixelangle:    
    mov wtemp local-pixx
    lsl wtemp wtemp @1
    add ptr ptr wtemp 
    ldrh w0 [ptr]
    sxth w0 w0
;    (debug-str-reg "pixelangle[pixx] = " w0)
    ldr wtemp midangle:
    add w10 w0 wtemp
;    (debug-str-reg "+midangle = " w0)

    (/when (w10 lt @0)  {
;      (debug-str "angl < 0" #t)
      mov w11 @FINEANGLES
      add w10 w10 w11
    })

    (/when (w10 ge @3600)  {
;      (debug-str "angl >= 3600" #t)                  
      mov w11 @FINEANGLES
      sub w10 w10 w11

      })

    (/cond
     ([(w10 lt @900)
       {
;         (debug-str "angl < 900" #t)
         mov w0 @1
         adr ptr xtilestep:
         str w0 [ptr]
         ; TODO: need movn / neg
         mov w0 @0
         sub w0 w0 @1  ; -1
         adr ptr ytilestep:
         str w0 [ptr]
         ; xstep = finetangent[900-1-angl]
         adr ptr finetangent:
         mov w11 @899 ; w11 index
         sub w11 w11 w10 ; index -= angl
;         (debug-str-reg "index " w11)
         lsl w11 w11 @2  ; 32 bit
         add ptr ptr w11
         ldr xstep [ptr]
;         (debug-str-reg "xstep " xstep)

         ;ystep = -finetangent[angl]
         mov w11 w10 ; w11 = angl
         adr ptr finetangent:
         lsl w11 w11 @2
         add ptr ptr w11
         ldr ystep [ptr]
         sub ystep wzr ystep
;         (debug-str-reg "ystep " ystep)
         ; xpartial = xpartialup
         ldr xpartial xpartialup:
;         (debug-str-reg "xpartial " xpartial)
         ; ypartial = ypartialdown
         ldr ypartial ypartialdown:
;         (debug-str-reg "ypartial " ypartial)
         
       }]
      [(w10 lt @1800)
       {

;         (debug-str "angl < 1800" #t)
         mov w0 @0
         sub w0 w0 @1  ; -1
         adr ptr xtilestep:
         str w0 [ptr]
         adr ptr ytilestep:
         str w0 [ptr]
         ; xstep = -finetangent[angl-900]
         adr ptr finetangent:
         mov w11 @900 ; w11 index
         sub w11 w10 w11 ; angle - 900
;         (debug-str-reg "index " w11)
         lsl w11 w11 @2  ; 32 bit
         add ptr ptr w11
         ldr xstep [ptr]
         sub xstep xzr xstep
;         (debug-str-reg "xstep " xstep)

         ;ystep = -finetangent[1800-1-angl]
         mov w11 @1799
         sub w11 w11 w10
         adr ptr finetangent:
         lsl w11 w11 @2
         add ptr ptr w11
         ldr ystep [ptr]
         sub ystep wzr ystep
;         (debug-str-reg "ystep " ystep)
         ldr xpartial xpartialdown:
;         (debug-str-reg "xpartial " xpartial)
         ldr ypartial ypartialdown:
;         (debug-str-reg "ypartial " ypartial)
         

       }]
      [(w10 lt @2700)
       {
;        (debug-str "angl < 2700" #t)
         mov w0 @0
         sub w0 w0 @1  ; -1
         adr ptr xtilestep:
         str w0 [ptr]
         mov w0 @1
         adr ptr ytilestep:
         str w0 [ptr]
         ; xstep = -finetangent[2700-1-angl]
         adr ptr finetangent:
         mov w11 @2699 ; w11 index
         sub w11 w11 w10 ; 
;         (debug-str-reg "index " w11)
         lsl w11 w11 @2  ; 32 bit
         add ptr ptr w11
         ldr xstep [ptr]
         sub xstep xzr xstep
;         (debug-str-reg "xstep " xstep)

         ;ystep = finetangent[angl-1800]
         mov w11 @1800
         sub w11 w10 w11
         adr ptr finetangent:
         lsl w11 w11 @2
         add ptr ptr w11
         ldr ystep [ptr]
;         (debug-str-reg "ystep " ystep)
         ldr xpartial xpartialdown:
;         (debug-str-reg "xpartial " xpartial)
         ldr ypartial ypartialup:
;         (debug-str-reg "ypartial " ypartial)
      
        }]
      [(w10 lt @3600)
       {
;        (debug-str "angl < 3600" #t)
   
         mov w0 @1
         adr ptr xtilestep:
         str w0 [ptr]
         adr ptr ytilestep:
         str w0 [ptr]
         ; xstep = finetangent[angl-2700]
         adr ptr finetangent:
         mov w11 @2700 ; w11 index
         sub w11 w10 w11 ; 
;         (debug-str-reg "index " w11)
         lsl w11 w11 @2  ; 32 bit
         add ptr ptr w11
         ldr xstep [ptr]
;         (debug-str-reg "xstep " xstep)

         ;ystep = finetangent[3600-1-angl]
         mov w11 @3599
         sub w11 w11 w10
         adr ptr finetangent:
         lsl w11 w11 @2
         add ptr ptr w11
         ldr ystep [ptr]
;         (debug-str-reg "ystep " ystep)
         ldr xpartial xpartialup:
;         (debug-str-reg "xpartial " xpartial)
         ldr ypartial ypartialup:
;         (debug-str-reg "ypartial " ypartial)
   
        }]))

    ;yintercept = FixedMul(ystep,xpartial) + viewy
    ; make sure these are in x registers first
    ; sign extend to 64 bits

    sxtw x10 ystep
    sxtw x11 xpartial
    (FixedMul x12 x10 x11)
    mov x0 x12
;    (debug-str-reg "fixed mul res " w0)
    ldr wtemp viewy:
    add w0 w0 wtemp
    adr ptr yintercept:
    str w0 [ptr]
;    (debug-str-reg "yintercept " w0)
    mov w12 w0 ; w12 = yintercept (used in a moment)
    
    ;xtile = focaltx + xtilestep
    ldr w10 focaltx:
    ldr w11 xtilestep:
    add w10 w10 w11
    adr ptr xtile:
    str w10 [ptr]
;    (debug-str-reg "xtile " w10)
    
    ; xspot = (xtile << 6) + (yintercept >> 16)
    lsl w10 w10 @6
    lsr w12 w12 @16
    add w0 w10 w12
    adr ptr xspot:
    str w0 [ptr]
;    (debug-str-reg "xspot " w0)

    ;xintercept = FixedMul(xstep,ypartial) + viewx
    sxtw x10 xstep
    sxtw x11 ypartial
    (FixedMul x12 x10 x11)
    mov x0 x12
;    (debug-str-reg "fixed mul res " w0)
    ldr wtemp viewx:
    add w0 w0 wtemp
    adr ptr xintercept:
    str w0 [ptr]
;    (debug-str-reg "xintercept " w0)
    mov w12 w0 ; w12 = xintercept (used in a moment)

    ;ytile = focalty + ytilestep
    ldr w10 focalty:
;        (debug-str-reg "focalty " w10)
    ldr w11 ytilestep:
;            (debug-str-reg "ytilestep " w11)
    add w10 w10 w11
    adr ptr ytile:
    str w10 [ptr]
;    (debug-str-reg "ytile " w10)
    
    ; yspot = ((xintercept >> 16) << 6) + ytile

    lsr w12 w12 @16
    lsl w12 w12 @6
    add w0 w10 w12
    adr ptr yspot:
    str w0 [ptr]
;    (debug-str-reg "yspot " w0)

    adr ptr texdelta:
    mov x0 @0
    str w0 [ptr]
    

    ;  special pushwall handling (TODO)
            
    ; begin raycasting loop
  :start-vert-loop

    ; if ytilestep = -1 && (yintercept>>16 <= ytile) goto horizentry
    ; if ytilestep = 1 && (yintercept>>16 >= ytile) goto horizentry
    ldr w10 ytilestep:
    ldr w11 yintercept:
    asr w11 w11 @16
;    (debug-str-reg "asr " w11)
    mov w12 @0
    sub w12 w12 @1 ; w12 = -1
    ldr w13 ytile:
    ; if ytilestep = -1 && (yintercept>>16 <= ytile) goto horizentry
    (/when (/and (w10 eq w12) (w11 le w13)) {
;      (debug-str "check 1 true" #t)
      b horizentry:                                             
      })
    ; if ytilestep = 1 && (yintercept>>16 >= ytile) goto horizentry
    (/when (/and (w10 eq @1) (w11 ge w13)) {
;      (debug-str "check 2 true" #t)
      b horizentry:                                             
    })
  :vertentry
    ; TODO: there's an OOB check we don't need right now
    ; if(xspot > maparea) break;
;    (debug-str "in vertentry" #t)
    ; tilehit = tilemap[xspot]
    adr ptr tilemap:
    ldr wtemp xspot:
    add ptr ptr wtemp
    ldrb w0 [ptr]
;    (debug-str-reg "tilehit " w0)
    adr ptr tilehit:
    str w0 [ptr]

    ;if tilehit
    (/when (w0 ne @0) {

      ;TODO: a bunch of door and pushwall stuff
;      (debug-str-reg "HIT V " w0)                 

      ;xintercept=xtile<<TILESHIFT;
      ldr w10 xtile:
      lsl w10 w10 @TILESHIFT
      adr ptr xintercept:
      str w10 [ptr]
;      (debug-str "1 " #t)
      ;ytile = (short) (yintercept >> TILESHIFT);
      ldr w10 yintercept:
      asr x10 x10 @TILESHIFT
      adr ptr ytile:
      str w10 [ptr]
;      (debug-str "2 " #t)

      (preserve [x1 x2 x3 x4 x5 x6 x7 x8 x9] {
        bl HitVertWall:
      })        
      b loop-next+
    })                   

  :passvert  

    ;; todo spotvis update when we take on sprites
    ;xtile += xtilestep
    ldr w10 xtile:
    ldr w11 xtilestep:
    add w10 w10 w11
    adr ptr xtile:
    str w10 [ptr]

    ;yintercept += ystep
    ldr w12 yintercept:
    ;ldr w11 ystep:
    add w10 w12 ystep
    adr ptr yintercept:
    str w10 [ptr]

    ; xspot = (xtile << 6) + (yintercept >> 16)
    mov w12 w10
    ldr w10 xtile:
    lsl w10 w10 @6
    lsr w12 w12 @16
    add w0 w10 w12
    adr ptr xspot:
    str w0 [ptr]
;    (debug-str-reg "xspot v " w0)

    b start-vert-loop:

  :start-horiz-loop
    ; if xtilestep = -1 && (xintercept>>16 <= xtile) goto vertentry
    ; if xtilestep = 1 && (xintercept>>16 >= xtile) goto vertentry
    ldr w10 xtilestep:
    ldr w11 xintercept:
    asr w11 w11 @16
    mov w12 @0
    sub w12 w12 @1 ; w12 = -1
    ldr w13 xtile:
    ; if xtilestep = -1 && (xintercept>>16 <= xtile) goto vertentry
    (/when (/and (w10 eq w12) (w11 le w13)) {
;      (debug-str "check 3 true" #t)
      b vertentry:                                             
      })
    ; if xtilestep = 1 && (xintercept>>16 >= xtile) goto vertentry
    (/when (/and (w10 eq @1) (w11 ge w13)) {
;      (debug-str "check 4 true" #t)
      b vertentry:                                             
    })
  :horizentry
;    (debug-str "in horizentry" #t)
    ; tilehit = tilemap[yspot]
    adr ptr tilemap:
    ldr wtemp yspot:
    add ptr ptr wtemp
    ldrb w0 [ptr]
;    (debug-str-reg "tilehit " w0)
    adr ptr tilehit:
    str w0 [ptr]

    ;if tilehit
    (/when (w0 ne @0) {

      ;TODO: a bunch of door and pushwall stuff
;      (debug-str-reg "HIT H " w0)                 
      ;yintercept=ytile<<TILESHIFT;
      ldr w10 ytile:
      lsl w10 w10 @TILESHIFT
      adr ptr yintercept:
      str w10 [ptr]
      
      ;xtile = (short) (xintercept >> TILESHIFT);
      ldr w10 xintercept:
      asr x10 x10 @TILESHIFT
      adr ptr xtile:
      str w10 [ptr]
      (preserve [x1 x2 x3 x4 x5 x6 x7 x8 x9] {
        bl HitHorizWall:
      })        
  
  
      
      ;break
      b loop-next+
    })                   

    
  :passhoriz
    ;; todo spotvis update when we take on sprites
    ;ytile += ytilestep
    ldr w10 ytile:
;    (debug-str-reg "ytile " w10)
    ldr w11 ytilestep:
;    (debug-str-reg "ytilestep " w11)
    add w10 w10 w11
    adr ptr ytile:
    str w10 [ptr]
;    (debug-str-reg "ph ytile " w10)
    ;xintercept += xstep
    ldr w12 xintercept:
;    (debug-str-reg "xintercept " w12)
    ;ldr w11 xstep:
;    (debug-str-reg "xstep " xstep)
    add w10 w12 xstep
    adr ptr xintercept:
    str w10 [ptr]
;    (debug-str-reg "ph xintercept " w10)
    ; yspot = ((xintercept >> 16) << 6) + ytile
    mov w12 w10
    asr w12 w12 @16
    lsl w12 w12 @6
    ldr w10 ytile:
    add w0 w10 w12
    adr ptr yspot:
    str w0 [ptr]
 ;   (debug-str-reg "yspot h " w0)

    
    b start-horiz-loop:

  :loop-next
    ldr local-pixx pixx:
  })

;  (debug-str "exit asm referesh" #t)          
})

(subr WallRefresh
      ([vptr x1]

       [ptr x2]
       [temp x2]
       )
      [x1] {
 ; xpartialdown = viewx & (TILEGLOBAL-1)
  (load-immediate temp (- TILEGLOBAL 1))
  ;; adr x0 viewx:
  ;; ldr w0 [x0]
  ldr w0 viewx:
  and x0 x0 temp
  adr ptr xpartialdown:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "xpartialdown is " x0)
  ;; ; end debug

  ; xpartialup = TILEGLOBAL - xpartialdown (x0)
  (load-immediate temp TILEGLOBAL)
  sub x0 temp x0
  adr ptr xpartialup:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "xpartialup is " x0)
  ;; ; end debug

  
  ; ypartialdown = viewy & (TILEGLOBAL-1)
  (load-immediate temp (- TILEGLOBAL 1))
  ldr w0 viewy:
;  (debug-str-reg "viewy is " w0)
  and x0 x0 temp
;  (debug-str-reg "result is " w0)
  adr ptr ypartialdown:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "ypartialdown is! " x0)
  ;; ; end debug

  
  ; ypartialup = TILEGLOBAL - ypartialdown (x0)
  (load-immediate temp TILEGLOBAL)
  sub x0 temp x0
  adr ptr ypartialup:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "ypartialup is " x0)
  ;; ; end debug

  ; lastside = -1
  mov x0 @0
  sub x0 x0 @1
  adr ptr lastside:
  str w0 [ptr]
  bl AsmRefresh:
  bl ScalePost:
})

(subr ThreeDRefresh
      ([vptr x1]
       
       [ply x2]
       [temp w3]
       [temp2 w4]
       [temp3 x5]
       [ptr x5]
       [vangle w6])
      [x1]
      {

  ; first we'd clear spotvis that will detect sprite hits
  ; but we are not doing this yet     

  ; ===============
  ; CalcViewVariables

  ; player: is a pointer!
  ldr ply player:

  ; viewangle = ply->angle    
  (_objstruct/angle-get temp ply)    
  adr x0 viewangle:
  str temp [x0]
  mov vangle temp
  ;; ; debug
  ;; ldr w0 [x0]
  ;; (debug-str-reg "viewangle is " x0)
  ;; ; end debug
  
  
  ;midangle = viewangle * (FINEANGLES/ANGLES)
  (load-immediate x0 (/ FINEANGLES ANGLES))
  mul temp2 temp x0
  adr x0 midangle:
  str temp2 [x0]

  ; debug
  ldr w0 [x0]
;  (debug-str-reg "midangle is " x0)
  ; end debug
  
  ;TODO: really need to add support for the ldr / str with
  ; offset registers optionally shifted
  
  ; viewsin = sintable[viewangle]
  ; temp = viewangle
  adr ptr sintable:
  lsl temp2 vangle @2  ; multiply by 4 since 32bits
  add ptr ptr temp2    ; todo: this could all be 1 instruction
  ldr w0 [ptr]
  adr ptr viewsin:
  str w0 [ptr]

  ; debug
  ldr w0 [ptr]
;  (debug-str-reg "viewsin is " x0)
  ; end debug

  
  ; viewy = player->x + FIxedMul(focallength,viewsin)  
  ldr temp2 focallength:
  sxtw x0 w0
  sxtw temp2 temp2
  (FixedMul temp3 x0 temp2)
;  (debug-str-reg "rresult " temp3)
  (_objstruct/y-get temp2 ply)
;  (debug-str-reg "player->y " temp2)
  sub x0 temp2 temp3
  adr ptr viewy:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "viewy is " x0)
  ;; ; end debug

  ; focalty = viewy >> 16
  lsr w0 w0 @16
  adr ptr focalty:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "focalty is " x0)
  ;; ; end debug

  ; viewcos = costable[viewangle]
  ; tmep = viewangle
  ; costable is a pointer
  ldr ptr costable:
  lsl temp2 vangle @2  ; multiply by 4 since 32bits
  add ptr ptr temp2
  ldr w0 [ptr]
  adr ptr viewcos:
  str w0 [ptr]

  ;;   ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "viewcos is " x0)
  ;; ; end debug

  
  ; viewx = player->x - FIxedMul(focallength,viewcos)  
  ldr temp2 focallength:
  ;  (debug-str-reg "focallength " temp2)
  sxtw x0 w0
  sxtw temp2 temp2
  (FixedMul temp3 x0 temp2)
;  (debug-str-reg "result " temp3)
  (_objstruct/x-get temp2 ply)
;  (debug-str-reg "player->x " temp2)
  sub x0 temp2 temp3
  adr ptr viewx:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "viewx is " x0)
  ;; ; end debug

  
  ; focaltx = viewx >> 16
  lsr w0 w0 @16
  adr ptr focaltx:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "focaltx is " x0)
  ;; ; end debug
  
  
  ; viewtx = player->x >> 16}
  (_objstruct/x-get temp2 ply)
  lsr x0 temp2 @16
  adr ptr viewtx:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "viewtx is " x0)
  ;; ; end debug

  
  ; viewty = player->y >> 16}
  (_objstruct/y-get temp2 ply)
  lsr x0 temp2 @16
  adr ptr viewty:
  str w0 [ptr]

  ;; ; debug
  ;; ldr w0 [ptr]
  ;; (debug-str-reg "viewty is " x0)
  ;; ; end debug

  
  ; =================

  bl VgaClearScreen:
  bl WallRefresh:
  ; ==================

;  (debug-str "finish 3d refersh" #t)
})

; old gfx code that does the slideshows
;; (subr update-gfx  (
;;                   [temp x4] ; temporary / intermediates
;;                   [tptr x8]
;;                   [vptr x9] ; video memory pointer
;;                   [row-size x14]
;;                   ) [x0 x1 x2 x3 x4]
;;                     {

;; :loop
;;     ldr w0 finished-rendering:
;;     cbnz x0 loop-
;;     bl get-back-buffer:
;;     mov vptr x0

;;     ; ===================
;;     ; clear screen
    
;;     ; we do 8 pixels each iteration so
;;     ; we need 320*200/8 = $1F40 iterations
;;     mov temp @$1F40
;;     mov x3 @0
;; :next
;;     str x3 [vptr]
;;     str x3 (vptr @8)
;;     str x3 (vptr @16)
;;     str x3 (vptr @24)
;;     add vptr vptr @32

;;     sub temp temp @1
;;     cbnz temp next-
;;     mov vptr x0

;;     ; end clear screen
;;     ; ====================
    
;;     ; x27 holds a delay
;;     ; this should be in memory
;;     ; since we never even push these!
;;     sub x27 x27 @1
;;     cbnz x27 skip+
;;     mov x27 @10
;;     ; x28 holds next picture to display
;;     ; also can easily be trashed!
;;     sub x28 x28 @1

;;     cbnz x28 skip+
;;     mov x28 @105  ; 130 for 2d pics, 105 for textures
    
;; :skip    
;;     mov x0 vptr
;;     mov x1 x28

;;     mov x1 @15
    
;;     ;bl render-pic:  ; render 2d pics
;;     bl render-tex:   ; render 2d textures
;;     ;signal render has finished
;;     mov temp @1
;;     adr x0 finished-rendering:
;;     str temp [w0]


;;     b loop-
;;    })


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
  (flip-gpio0)
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

  
  :quit
    (flip-gpio0)})


; //////////////////////////////////////////////
; WOLFENSTEIN CODE 
; //////////////////////////////////////////////
(define MAXACTORS 150)
(define MAP_HEIGHT 64)
(define MAP_WIDTH 64)
  
(/struct _objstruct
 ([statetype       8] ; *state
  [next            8] ; *next objstruct
  [prev            8] ; *prev objstruct

  [flags           4] ; FL_SHOOTABLE, etc
  [distance        4] ; signed; if negative, wait for that door to open
  [speed           4] ; signed  
  [x               4] ; fixed point
  [y               4] ; fixed point
  [transx          4] ; fixed, in global coord
  [transy          4] ; fixed, in global coord
  
  [ticcount        2]
  [tilex           2]
  [tiley           2]
  [viewx           2]
  [viewheight      2]
  [angle           2]
  [hitpoints       2]

  [temp1           2]
  [temp2           2]
  [hidden          2]
  
  [active          1] ;activetype enum
  [obclass         1] ;classtype enum
  [dirtype         1] ; dirtype enum
  [areanumber      1]

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

(subr spawn-player
      ([tilex x1]
       [tiley x2]
       [dir x3]

       [temp x4]
       [ptr x5])
      [x4] {

  ; for now we assign the player to actors[0]
  ; in the future this needs to be in InitActorList
  ; and it must update the freelist etc.         
  ; for now we only have a player and it is hardcoded         
  adr ptr objlist:
  adr temp player:
  str ptr [temp]  ; player = objlist[0]

  mov temp @1

  (_objstruct/obclass-set temp ptr) ; playerobj = 1
  (_objstruct/active-set temp ptr)  ; ac_yes = 1
  (_objstruct/tilex-set tilex ptr)  ; tilex = tilex
  (_objstruct/tiley-set tiley ptr)  ; tiley = tiley

  ; areanumber = map index (tiley << 6) + tilex  
  lsl temp tiley @6
  add temp temp tilex
  ; multiply by 2 here because we need words not bytes. the C code is silently doubling the index
  add temp temp temp
  ldr x0 maps-address: ; this is a linker resolved label
  add temp temp x0
  
  ldrb temp [temp]
  (_objstruct/areanumber-set temp ptr) 

  ; x = (tilex << TILESHIFT) + (TILEGLOBAL/2) (fixed point)
  (debug-str-reg "tilexx = " tilex)
  lsl temp tilex @TILESHIFT
  (load-immediate x0 TILEGLOBAL/2)
  add temp temp x0
  (debug-str-reg "player->x = " temp)
  (_objstruct/x-set temp ptr)

  (_objstruct/x-get temp ptr)
  (debug-str-reg "* player->x = " temp)

  ; y = (tiley << TILESHIFT) + (TILEGLOBAL/2) (fixed point)
  lsl temp tiley @TILESHIFT
  (load-immediate x0 TILEGLOBAL/2)  
  add temp temp x0
  (debug-str-reg "player->y = " temp)
  (_objstruct/y-set temp ptr)  


  ; the last bits are state, angle and flags.
  ; state we don't care about for now
  ; angle resolves to zero for the first map so we ignore that for now
  mov temp @180
  (_objstruct/angle-set temp ptr)
  
  mov temp @4
  (_objstruct/flags-set temp ptr)  ; FL_NEVERMARK = 4
  
  ;; (debug-str "obclass" #t)
  ;; (_objstruct/obclass-get temp ptr)
  ;; (debug-reg temp)

  ;; (debug-str "active" #t)
  ;; (_objstruct/active-get temp ptr)
  ;; (debug-reg temp)

  ;; (debug-str "tilex" #t)
  ;; (_objstruct/tilex-get temp ptr)
  ;; (debug-reg temp)

  ;; (debug-str "tiley" #t)
  ;; (_objstruct/tiley-get temp ptr)
  ;; (debug-reg temp)

  ;; (debug-str "areanumber" #t)
  ;; (_objstruct/areanumber-get temp ptr)
  ;; (debug-reg temp)

  ;; (debug-str "x" #t)
  ;; (_objstruct/x-get temp ptr)
  ;; (debug-reg temp)

  ;; (debug-str "y" #t)
  ;; (_objstruct/y-get temp ptr)
  ;; (debug-reg temp)

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
        ;; call spawn-player with x, y, (tile - 19)
        ;; calculate direction
        (preserve [x1 x2 x3] {
          mov x1 x
          mov x2 y
          sub x3 tile @19
          bl spawn-player:
        })
        (debug-str "PLAYER SPAWNED!" #t)
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
:objlist (reserve (* (sizeof/_objstruct) MAXACTORS))
/= 8
:player  (write-value-64 0)  ; objstruct*

/= 8
:horizwall
(write-value-16 0)
(for ([i (in-range 1 64)])
  (write-value-16 (* (- i 1) 2)))

/= 8
:vertwall
(write-value-16 0)
(for ([i (in-range 1 64)])
  (write-value-16 (+ (* (- i 1) 2) 1)))

/= 8
:wallheight
(for ([x (in-range 320)])
  (write-value-32 0))

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
  :flip0 (write-value-64 0)
  :flip1 (write-value-64 0)
  :dELAY (write-value-64 $FFFFF)
  :dELAY2 (write-value-64 200000)
  :VC_MBOX (write-value-64 VCORE-MBOX)
  :TEST (write-value-64 $123456789ABCDEF)
:START (write-value-64 $800000) ; stack location

:CONVERT (write-value-64 $3FFFFFFF)
:HCR-VALUE (write-value-64 HCR_VALUE)
:SCTLR-VALUE-MMU-DISABLED (write-value-64 SCTLR_VALUE_MMU_DISABLED2)
:SCTLR-VALUE-MMU-ENABLED (write-value-64 SCTLR_VALUE_MMU_ENABLED)
:SCR-VALUE (write-value-64 SCR_VALUE)
:SPSR-VALUE (write-value-64 SPSR_VALUE)
:CPACR_EL1_VAL (write-value-64 CPACR_EL1_VAL)
:TCR_EL1_VAL(write-value-64 TCR_EL1_VAL)
:MAIR_VALUE (write-value-64 MAIR_VALUE)
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
:execute-once (write-value-64 0)

(define-syntax-parser execute-once
  ([_ body]
   #'{
      ldr x0 execute-once:
      cbz execute-once-skip:
      mov x0 @0
      (preserve [x1] {
        adr x1 execute-once:
        str x0 [x1]                        
      })                
    :execute-once-skip  
     }))
  ; to communicate with the video core gpu we need an address that
  ; is 16-byte algined - the lower 4 bits are not set. these are then
  ; used to specify the channel
  /= 16
  :MBOX-MSG
    (write-value-32 (* 39 4)) ; total buffer size bytes including headers
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


   ; set alpha
      (write-value-32 $48007)
      (write-value-32 4) 
      (write-value-32 4) 
      (write-value-32 0) ; alpha off

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

; wolf lets you resize the viewport and thus it recalcualtes various projection bits
; our version is hardcoded at 320 * 160 (no border) so we can calculate the bits at compile time
/= 8
;wolf uses a lot of global state!
:postsource (write-value-64 0) ; pointer to texture column data
:debug-flag (write-value-64 (* 360 4))
:focallength (write-value-32 $5700)
:scale (write-value-32 128)
:heightnumerator (write-value-32 223232)
:viewx (write-value-32 0)
:viewy (write-value-32 0)
:viewsin (write-value-32 0)
:viewcos (write-value-32 0)
:vbufPitch (write-value-32 320)
:xpartialup (write-value-32 0)
:xpartialdown (write-value-32 0)
:ypartialup (write-value-32 0)
:ypartialdown (write-value-32 0)
:xintercept (write-value-32 0)
:yintercept (write-value-32 0)
:pixx (write-value-32 0)
:texdelta (write-value-32 0)
:lastside (write-value-32 0)
:lasttilehit (write-value-32 0)
:lasttexture (write-value-32 0)
:lastintercept (write-value-32 0)
:postx (write-value-32 0)
; these next ones are shorts but we'll store them as 4 bytes
:tilehit (write-value-32 0)
:viewangle (write-value-32 0)
:midangle (write-value-32 0)
:angle (write-value-32 0)
:xstep (write-value-32 0)
:ystep (write-value-32 0)
:xtile (write-value-32 0)
:ytile (write-value-32 0)
:xtilestep (write-value-32 0)
:ytilestep (write-value-32 0)
:viewtx (write-value-32 0)
:viewty (write-value-32 0)
:focaltx (write-value-32 0)
:focalty (write-value-32 0)
:xspot (write-value-32 0)
:yspot (write-value-32 0)
; end shorts

/= $1000
:id_pgd
(reserve (+ (* 6 (arithmetic-shift 1 12)))) ; $6000
(write-value-64 $BADF00D)
(write-value-64 $BADF00D)
(define viewwidth 320)
(define halfview (/ viewwidth 2))
(define facedist (+ 0.0 #x5700 MINDIST))
(define scale 128)
(define heightnumerator 223232)
(define radtoint 572.9578)
/= 8
:pixelangle
(let ([vec (make-vector viewwidth)]
      [calcangle (λ (i)
         (define tang (/(* i #x10000) viewwidth facedist))
         (define ang (atan tang))
         (define intang  (inexact->exact (floor (* ang radtoint))))
         intang)])
  (for ([i (in-range halfview)])
    (let* ([ang (calcangle i)]
           [nang (- ang)])
      (vector-set! vec (- halfview 1 i) ang)
      (vector-set! vec (+ halfview i) nang)))
  (for ([v vec])
;    (displayln v)
    (write-value-16 v)
    ))

(define GLOBAL1 (arithmetic-shift 1 16))
; next up is the tangent table
/= 8
:finetangent
(let ([vec (make-vector (/ FINEANGLES 4))])
  (for ([i (in-range (/ FINEANGLES 8))])
    (define tang (tan (/ (+ i 0.5) radtoint)))
    (vector-set! vec i (inexact->exact (floor (* tang GLOBAL1))))
    (vector-set! vec (- (/ FINEANGLES 4) 1 i) (inexact->exact (floor (* (/ 1 tang) GLOBAL1)))))
  (for ([v vec])
    (write-value-32 v)))

; now for sin / cos
; costable overlays sintable with a quarter phase shift
(define anglestep (/ pi 2 ANGLEQUAD))
/= 8
:costable (write-value-64 0)  ; pointer into sintable
:sintable  ; costable is :sintable + (ANGLEQUAD * 4 (since its 32bits)) (or in this case, just ANGLES!)
(let ([vec (make-vector (+ ANGLES ANGLEQUAD))])
  (define ang 0)
  (for ([i (in-range ANGLEQUAD)])
    (define value (inexact->exact (floor (* (sin ang) GLOBAL1))))
    (vector-set! vec i value)
    (vector-set! vec (+ i ANGLES) value)
    (vector-set! vec (- (/ ANGLES 2) i) value)

    (vector-set! vec (- ANGLES i) (- value))
    (vector-set! vec (+ (/ ANGLES 2) i) (- value))

    (set! ang (+ ang anglestep))
    
    )
  (vector-set! vec ANGLEQUAD 65536)
  (vector-set! vec (* 3 ANGLEQUAD) -65536)
  (for ([v vec])
    (write-value-32 v)))

/= 8
; palette (only used for backgroudn colours)
:palette
(let*
    ([lines  (file->lines "wolfpal.inc")]
     [splits  (map (λ (line) (string-split line "RGB")) lines)]
     [clean  (map (λ (line) (string-replace (string-replace line ")" "") "(" "")) (flatten splits))]
     [rgbs (map (λ (line) (string-split line ",")) clean)])
  (for ([rgb rgbs])
    (write-value (string->number (string-trim (list-ref rgb 2))))
    (write-value (string->number (string-trim (list-ref rgb 1))))
    (write-value (string->number (string-trim (list-ref rgb 0))))
    (write-value 255)))


})


