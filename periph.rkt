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
(define GPIOFSEL (+ PERIPH-BASE $200000))
(define GPIOFSEL1 (+ PERIPH-BASE $200004))
(define GPIOUD   (+ PERIPH-BASE $2000E4))

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
     strh w1 (x0 @udn-offset)

     ; now enable the mini-uart
     ldr  x0 AUXB:
     mov  x1 @1
     strb w1 (x0 @aux-enables)  ; enable mini uart
     mov  x1 @0
     strb w1 (x0 @aux-cntrl)    ; disable auto flow and rx/tx
     strb w1 (x0 @aux-ier)      ; disable interrupts
     mov  x1 @3
     strb w1 (x0 @aux-lcr)      ; 8 bit mode
     mov  x1 @0
     strb w1 (x0 @aux-mcr)      ; set rts line high
     (cond
       [(equal? pi 'pi3) {mov x1 @270}]
       [(equal? pi 'pi4) {mov x1 @541}]
       [else (error "unsupported pi model")])
     strh w1 (x0 @aux-baud)    
     mov  x1 @3
     strb w1 (x0 @aux-cntrl)      ; enable tx rx


     })

; send string over the mini UART
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
          strb w0 (addr @aux-io)})]))

(provide init-uart debug-str create-send-char periph-addresses PERIPH-BASE)
                     
