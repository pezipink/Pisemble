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
(define-syntax-parser setup-mmu
  [(_) #'{
;    nop
    (load-immediate x1 $3520)    ; 4GB space 4KB granularity
    ;  msr TCR-EL1 x1
    ; (write-value-32 $d5182041)

    ;msr TCR_EL3
    (write-value-32 $d51e2041)
    (debug-str "1" #t)
    (load-immediate x1 $ff440400) ; ; ATTR0 Device-nGnRnE ATTR1 Device. (non Gathering, non Re-ordering, non Early write acknowledgement)
    ; msr MAIR_EL1 x1
    ;(write-value-32 $d518a201)

    ; msr MAIR_EL3 x1
    (write-value-32 $d51ea201)
    (debug-str "2" #t)
    ;sub x0 sp $1000  (sub x0, sp, #1 LSL #12 ) dont support lsl here yet
    (write-value-32 $d14007e0)
    ; todo bic sp x0 @$0fff
    (write-value-32 $9274cc1f)
    mov x0 sp
    ;(write-value-32 $d5182000) ; msr ttbr0_el1,x0
    (write-value-32 $d51e2000) ; msr ttbr0_el3,x0
    (debug-str "3" #t)

    ; Set up translation table entries in memory with looped store instructions.
    ; Set the level 1 translation table.
    ; The first entry points to level2_pagetable.
    ;sub x1 sp $1000                 ; allocate 4096 bytes on stack, enough for 512 level 2 block entries
    (write-value-32 $d14007e0)    ; dont support sub with lsl yet
    ;(write-value-32 $d14007e1)
    ;bic sp x1 @$0fff
    (write-value-32 $9274cc3f)     ; level2_pagetable must be a 4KB aligned address.
    mov x1 sp                      ; NSTable=0 APTable=0 XNTable=0 PXNTable=0.
    mov x10 @3
    orr x1 x1 x10
    str x1 [x0] @8
    (debug-str "4" #t)
    
    ; AttrIdx=000 Device-nGnRnE.
    ; The second entry is 1GB block from 0x40000000 to 0x7FFFFFFF.
    (load-immediate x2 $40000741)
    str x2 [x0] @8

    ; The third entry is 1GB block from 0x80000000 to 0xBFFFFFFF.
    (load-immediate x2 $80000741)
    str x2 [x0] @8

    ; The fourth entry is 1GB block from 0xC0000000 to 0xFFFFFFFF
    (load-immediate x2 $C0000741)
    str x2 [x0] @8

    ;level 2 translation table
    (load-immediate x2 $74D)
    mov x0 sp

;    (debug-str "enter loop" #t)
    mov x4 @512
    (load-immediate x5 $200000)
 :loop
    str x2 [x0] @8
    add x2 x2 x5
    cmp x4 @480
    b.ne docache+
    mov x6 @$C
    eor x2 x2 x6
:docache
 ; todo subs x4 x4 @1
    (write-value-32 $f1000484)
    b.ne loop-
;    (debug-str "finished loop" #t)

    ;enable MMU
    (write-value-32 $d539f220)
    (write-value-32 $b27a0000)
    (write-value-32 $d519f220)
    ;(write-value-32 $d5381000) ; EL1 mrs x0, sctlr_el1
    (write-value-32 $d53e1000)  ; EL3 mrs x0, sctlr_el3
    (write-value-32 $b27e0000)
    (write-value-32 $b2740000)
    (write-value-32 $b2400000)
    ;(write-value-32 $d5181000)  ; msr sctlr_el1 x0
    (write-value-32 $d51e1000)  ; msr sctlr_el3 x0
    (write-value-32 $d5033f9f)
    (write-value-32 $d5033fdf)
         }])

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
    (setup-mmu)
    (debug-str "mnu up" #t)
    
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
    
 :loop
     b loop-

;; inline the code and label for the send-char subroutine     
(create-send-char)

:dump-regs
  (PUSH x30)
  (dump-all-regs)
  (POP x30)
  ret x30

  ; the following values are used for loading 64 bit addresses/values via LDR(literal)
  ; and they must be 64 bit aliged otherwise the CPU faults!
  /= 8
  (periph-addresses)
  :DELAY (write-value-64 $FFFF)
:START (write-value-64 $80000)

  :TABLE(write-value-64 $30000)
  
})
