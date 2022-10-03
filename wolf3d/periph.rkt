#lang pisemble
(require (for-syntax syntax/parse))
(require "stack.rkt")
(define pi 'pi3)
;(define pi 'pi4)

(define PERIPH-BASE
  (cond
    [(equal? pi 'pi3) $3F000000]
    [(equal? pi 'pi4) $FE000000]
    [else (error "unsupported pi model")]))
(define SMICS (+ PERIPH-BASE $600000))
(define GPIOFSEL (+ PERIPH-BASE $200000))
(define GPIOFSEL1 (+ PERIPH-BASE $200004))
(define GPIOUD   (+ PERIPH-BASE $2000E4))
(define IRQ_BASIC_PENDING (+ PERIPH-BASE $0000B200))
(define IRQ_PENDING_1   (+ PERIPH-BASE $0000B204))
(define IRQ_PENDING_2   (+ PERIPH-BASE $0000B208))
(define FIQ_CONTROL   (+ PERIPH-BASE $0000B20C))
(define ENABLE_IRQS_1   (+ PERIPH-BASE $0000B210))
(define ENABLE_IRQS_2   (+ PERIPH-BASE $0000B214))
(define ENABLE_BASIC_IRQS (+ PERIPH-BASE $0000B218))
(define DISABLE_IRQS_1    (+ PERIPH-BASE $0000B21C))
(define DISABLE_IRQS_2    (+ PERIPH-BASE $0000B220))
(define DISABLE_BASIC_IRQS  (+ PERIPH-BASE $0000B224))
(define TIMER_CS        (+ PERIPH-BASE $00003000))
(define TIMER_CLO       (+ PERIPH-BASE $00003004))
(define TIMER_CHI       (+ PERIPH-BASE $00003008))
(define TIMER_C0        (+ PERIPH-BASE $0000300C))
(define TIMER_C1        (+ PERIPH-BASE $00003010))
(define TIMER_C2        (+ PERIPH-BASE $00003014))
(define TIMER_C3        (+ PERIPH-BASE $00003018))

(define AUX-BASE (+ PERIPH-BASE $215000))

(define set-offset $1C)
(define clr-offset $28)
(define udn-offset $E4)
(define aux-enables $4)
(define aux-io $40)
(define aux-ier $44)
(define aux-iir $48)
(define aux-lcr $4c)
(define aux-mcr $50)
(define aux-lsr $54)
(define aux-cntrl $60)
(define aux-baud $68)


(define (periph-addresses) {
  /= 8                            
:ALT5 (write-value-64 %010_010_000_000_000_000)
:GPFSEL (write-value-64 GPIOFSEL)
:GPFSEL1 (write-value-64 GPIOFSEL1)
:GPUD (write-value-64 GPIOUD)
:AUXB (write-value-64 AUX-BASE)
:IRQ_BASIC_PENDING (write-value-64 IRQ_BASIC_PENDING)
:IRQ_PENDING_1 (write-value-64 IRQ_PENDING_1)
:IRQ_PENDING_2 (write-value-64 IRQ_PENDING_2)
:FIQ_CONTROL (write-value-64 FIQ_CONTROL)
:ENABLE_IRQS_1 (write-value-64 ENABLE_IRQS_1)
:ENABLE_IRQS_2 (write-value-64 ENABLE_IRQS_2)
:ENABLE_BASIC_IRQS (write-value-64 ENABLE_BASIC_IRQS)
:DISABLE_IRQS_1 (write-value-64 DISABLE_IRQS_1)
:DISABLE_IRQS_2 (write-value-64 DISABLE_IRQS_2)
:DISABLE_BASIC_IRQS (write-value-64 DISABLE_BASIC_IRQS)
:TIMER_CS (write-value-64 TIMER_CS)
:TIMER_CLO (write-value-64 TIMER_CLO)
:TIMER_CHI (write-value-64 TIMER_CHI)
:TIMER_C0 (write-value-64 TIMER_C0)
:TIMER_C1 (write-value-64 TIMER_C1)
:TIMER_C2 (write-value-64 TIMER_C2)
:TIMER_C3 (write-value-64 TIMER_C3)
:SMICS (write-value-64 SMICS)
})

(define (init-uart) {
     ldr x1 ALT5:
     ldr x0 GPFSEL1:
     str w1 (x0 @0)
     ; now GPIO 14 and 15 is set to ALT5
     ; set 14 and 15 to have no pullup/pulldown resistors
     ; for this we need to set 00 in 2 places at CNTGRL_REG0
     ; but for simplicity we'll just splat the whole thing
     mov x1 @0
     strh w1 [x0 @udn-offset]

     ; now enable the mini-uart
     ldr  x0 AUXB:
     mov  x1 @1
     strb w1 [x0 @aux-enables]  ; enable mini uart
     mov  x1 @0
     strb w1 [x0 @aux-cntrl]    ; disable auto flow and rx/tx
     strb w1 [x0 @aux-ier]      ; disable interrupts
     mov  x1 @3
     strb w1 [x0 @aux-lcr]      ; 8 bit mode
     mov  x1 @0
     strb w1 [x0 @aux-mcr]      ; set rts line high
     (cond
       [(equal? pi 'pi3) {mov x1 @270}]
       [(equal? pi 'pi4) {mov x1 @541}]
       [else (error "unsupported pi model")])
     strh w1 [x0 @aux-baud]
     mov  x1 @3
     strb w1 [x0 @aux-cntrl]      ; enable tx rx


     })

; send string over the mini UART
; TODO we want proper string support in assembler and linker
; rather than hardcoding them like some sort of savage
(define (debug-str str newline?)
 {
  (PUSH x0)
  ; our protocol expects a byte of 1 to then
  ; receive a string
  mov w0 @1
  bl send-char:
  }
  (for ([c str]) ; loop over each character
    {
     mov w0 @(char->integer c)
     bl send-char:
     })
  (when newline?
    {
     mov w0 @$A ; line feed
     bl  send-char:
     mov w0 @$D ; cr
     bl  send-char:
    })
  {
   mov w0 @$0 ; null
   bl  send-char:
   (POP x0)
  })

(define-syntax (create-send-char stx)
  (syntax-parse stx
    [(_)
      #'(subr send-char ([addr x1]) [x0 x1 x2 x3] {
          ldr  addr AUXB:
          ; wait for ready bit
          mov  x2 @$20
       :wait     
          ldr  w3 (addr @aux-lsr)
          and  w3 w2 w3
          cbz  w3 wait-
          strb w0 [addr @aux-io]})]))

(provide init-uart debug-str create-send-char periph-addresses PERIPH-BASE)
                     
