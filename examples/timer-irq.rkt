#lang pisemble
;; =====================================================================
;; TIMER IRQ - system timer interrupt at EL2, standalone image
;; =====================================================================
;; Installs its own vector table, fires the BCM system timer compare 1
;; every 250 ms, and counts the interrupts in the handler. The main loop
;; sleeps in wfi and prints "tick" each time the count changes.
;; Does not use lib/platform.rkt, which sends every vector to a fault handler.
;;
;; Build:  racket timer-irq.rkt         -> timer-irq.img (beside this file)
;; Run:    qemu-system-aarch64 -M raspi3b -kernel timer-irq.img -display none -serial null -serial stdio
(require "../lib/periph.rkt")
(require "../lib/stack.rkt")

(define (el-op base) (+ base (arithmetic-shift 4 16)))

(define INTERVAL 250000) ; the system timer counts 1 MHz

(aarch64 "timer-irq.img" []{
  mrs x0 mpidr_el1
  mov x1 @$3
  and x0 x0 x1
  cbz x0 main:
:park
  wfe
  b park-

:main
  ldr x1 stack-top:
  mov sp x1
  (init-uart)

  adr x0 vectors:
  (write-value-32 (el-op $d518c000))  ; msr vbar_el2, x0

  ; IMO routes physical IRQs to EL2. Without it QEMU treats them as EL1 IRQs and masks them at EL2.
  mrs x0 hcr_el2
  orr x0 x0 @16
  msr hcr_el2 x0

  ; compare 1 fires at CLO + INTERVAL
  ldr x1 TIMER_CS:
  ldr w0 [x1 @4]
  ldr x2 interval:
  add w0 w0 w2
  str w0 [x1 @$10]
  mov w0 @2
  str w0 [x1]                         ; clear any stale match 1

  ; route system timer 1 (IRQ 1) through the BCM interrupt controller
  ldr x1 ENABLE_IRQS_1:
  mov w0 @2
  str w0 [x1]

  msr daifclr @2                      ; unmask IRQ

  (debug-str "timer irq demo" #t)

:wait-irq
  wfi
  ldr x1 ticks:
  adr x2 printed:
  ldr x3 [x2]
  cmp x1 x3
  b.eq wait-irq-
  str x1 [x2]
  (debug-str "tick" #t)
  b wait-irq-

; IRQ handler: save what it uses, acknowledge, rearm, count, return
:irq
  (PUSH x0)
  (PUSH x1)
  (PUSH x2)
  ldr x1 TIMER_CS:
  mov w0 @2
  str w0 [x1]                         ; write 1 to clear match 1
  ldr w0 [x1 @4]
  ldr x2 interval:
  add w0 w0 w2
  str w0 [x1 @$10]                    ; next match at CLO + INTERVAL
  ldr x0 ticks:
  add x0 x0 @1
  adr x1 ticks:
  str x0 [x1]
  (POP x2)
  (POP x1)
  (POP x0)
  eret

:bad
  (debug-str "unexpected exception" #t)
:halt
  b halt-

(create-send-char)

; 16 entries of $80 bytes. At EL2 the core runs on SP_EL2, so an IRQ is taken
; to entry 5 (current EL, SPx). Entry 1 (SP0) points at the same handler in case
; SPSel is ever cleared.
/= $800
:vectors
/= $80
b bad:      ; 0 current EL SP0, sync
/= $80
b irq:      ; 1 current EL SP0, IRQ
/= $80
b bad:      ; 2 current EL SP0, FIQ
/= $80
b bad:      ; 3 current EL SP0, SError
/= $80
b bad:      ; 4 current EL SPx, sync
/= $80
b irq:      ; 5 current EL SPx, IRQ
/= $80
b bad:      ; 6 current EL SPx, FIQ
/= $80
b bad:      ; 7 current EL SPx, SError
/= $80
b bad:      ; 8 lower EL AArch64, sync
/= $80
b bad:      ; 9 lower EL AArch64, IRQ
/= $80
b bad:      ; 10 lower EL AArch64, FIQ
/= $80
b bad:      ; 11 lower EL AArch64, SError
/= $80
b bad:      ; 12 lower EL AArch32, sync
/= $80
b bad:      ; 13 lower EL AArch32, IRQ
/= $80
b bad:      ; 14 lower EL AArch32, FIQ
/= $80
b bad:      ; 15 lower EL AArch32, SError

/= 8
(periph-addresses)
:stack-top (write-value-64 $80000)
:interval  (write-value-64 INTERVAL)
:ticks     (write-value-64 0)
:printed   (write-value-64 0)
})
