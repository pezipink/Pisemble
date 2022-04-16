#lang pisemble
(require (for-syntax syntax/parse))
(set-emulator-program! emu "kernel8.img")

(define-syntax (wait stx)
  (syntax-parse stx
    [(_ value)
     #'{ldr x2 value
        :inner
        sub x2 x2 @1
        cbnz x2 inner- }]))


(define PERIPH-BASE $FE000000)
(define GPIOFSEL (+ PERIPH-BASE $200000))
(define GPIOFSEL1 (+ PERIPH-BASE $200004))
(define GPIOUD   (+ PERIPH-BASE $2000E4))
(define VCORE-MBOX (+ PERIPH-BASE $00B880))
(define AUX-BASE (+ PERIPH-BASE $215000))

(define-syntax (PUSH stx)
  (syntax-parse stx
    [(_ r:register)
     #'{ str r [sp @-16] !}]))

(define-syntax (PUSHH stx)
  (syntax-parse stx
    [(_ r:register)
     #'{ strh r [sp @-16] !}]))

(define-syntax (PUSHB stx)
  (syntax-parse stx
    [(_ r:register)
     #'{ strb r [sp @-16] !}]))

(define-syntax (POP stx)
  (syntax-parse stx
    [(_ r:register)
     #'{ ldr r [sp] @-16 }]))

(define-syntax (POPH stx)
  (syntax-parse stx
    [(_ r:register)
     #'{ ldrh r [sp] @-16 }]))

(define-syntax (POPB stx)
  (syntax-parse stx
    [(_ r:register)
     #'{ ldrb r [sp] @-16 }]))


(aarch64 {
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
     
     mrs x0 mpidr_el1
     mov x1 @$3
     and x0 x0 x1
     cbz x0 main:
 
:hang
     wfe
     b hang-     

:main
    ldr x1 START:
    mov sp x1
    ldr x1 ALT5:
    
    
    ldr x0 GPFSEL1:
;;      ;mov x1 @$12  ; set 
    str w1 (x0 @0)
;;      ; now GPIO 14 and 15 is set to ALT5
;;      ; set 14 and 15 to have no pullup/pulldown resistors
;;      ; for this we need to set 00 in 2 places at CNTGRL_REG0
;;      ; but for simplicity we'll just splat the whole thing
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
     mov  x1 @541
     strh w1 (x0 @aux-baud)    ; baud 115200
     mov  x1 @3
     strb w1 (x0 @aux-cntrl)      ; enable tx rx

     ; send string over the mini UART
     (define (debug-str str)
       {
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
       {
        mov w0 @$A ; line feed
        bl  send-char:
        mov w0 @$D ; cr
        bl  send-char:
        mov w0 @$0 ; null
        bl  send-char:
       })

     
     ; send string over the mini UART
     (define (debug-w0)
       {
        ; our protocol expects a byte of 2 to then
        ; receive a 32 bit number
        orr w8 w0 w0
        mov w0 @2
        bl send-char:
        orr w0 w8 w8
        bl  send-char:
        lsr w0 w0 @8
        bl  send-char:
        lsr w0 w0 @8    
        bl  send-char:
        lsr w0 w0 @8    
        bl  send-char:
       })

     
     
;     (debug-str "HELLO WORLD!")
 ;    (debug-str "BAREMETAL RPI4 MEETS RACKETLANG!")
     (debug-str "heres a number")
     mov w0 @$BAD
     lsl x0 x0 @16
     movk w0 @$F00D
     (debug-w0)
:here     adr x0 here-
     (debug-w0)

     mov w1 @42
     (PUSH x1)
     (POP x0)
     (debug-w0)
     
 :loop
     b loop:

:send-char ; put char in w0
  ldr  x1 AUXB:
  ; wait for ready bit
  mov  x2 @$20
  :wait     
  ldr  w3 (x1 @aux-lsr)
  and  w3 w2 w3
  cbz  w3 wait-
  strb w0 (x1 @aux-io)
  ret x30


  ; the following values are used for loading 64 bit addresses/values via LDR(literal)
  ; and they must be 64 bit aliged otherwise the CPU faults!
  /= 8
  :ALT5 (write-value-64 %010_010_000_000_000_000)
  :GPFSEL (write-value-64 GPIOFSEL)
  :GPFSEL1 (write-value-64 GPIOFSEL1)
  :GPUD (write-value-64 GPIOUD)
  :AUXB (write-value-64 AUX-BASE)
  :DELAY (write-value-64 $FFFF)

  :START (write-value-64 $80000)

})

;BLINKY 1!!
;; (aarch64 {
;;      set-offset = $1C
;;      clr-offset = $28

;;      ldr x0 GPFSEL:
;;      mov x1 @1
;;      strb w1 (x0 @0)
;;      ; now GPIO 0 is set to OUTPUT
;;  :loop
;;      ; blink thy LED
;;      strb x1 (x0 @set-offset)
;;      (wait DELAY:)
;;      strb x1 (x0 @clr-offset)
;;      (wait DELAY:)
;;      b loop:
     
;;   ;this is the base for gpio fsel on the RPI4
;;   ;; :GPFSEL (write-value-32 $FE200000)
;;   ;; (write-value-32 $0)
;;   :GPFSEL (write-value-64 GPIOFSEL)

  
;;   :DELAY (write-value-64 $FFFF)
  

;; })

