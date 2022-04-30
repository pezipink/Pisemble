#lang pisemble
(require (for-syntax syntax/parse racket/stxparam))
(require racket/stxparam)
(require "periph.rkt" "stack.rkt")
(set-emulator-program! emu "kernel8.img")

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

(define-syntax (debug-reg-no-stack stx)
  (syntax-parse stx
    [(_ r:register)
     #'{
        ; preserve x0, it is used for passing char to send-char
        ; first send code indicating whether a 32 or 64 bit number will be sent
        mov x1 r
        mov w0 @(if `r.is32 2 3)
        bl send-char:
        ; restore r into x0 and send first char
        mov x0 x1
        bl  send-char:
        ; shift and send the remainig 3 or 7 bytes
        (for ([_ (in-range (if `r.is32 3 7))])
          { lsr x0 x0 @8
            bl send-char: })
 }]
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
     ;; (define-rename-transformer-parameter test
     ;;   (make-rename-transformer #'x1))

     ;;     (arm-line mov r #:immediate 3)))
     msr daifclr @2
     msr daifset @2
     msr vbar_el1 x0
     msr esr_el1 x0

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

    (init-uart)

     
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
     (debug-reg-no-stack x0 )
     
     (debug-reg x0 w0)

     lsl x0 x0 @32

     (debug-reg x0 w0)
     
     ldr x0 TEST:
          (debug-reg-no-stack x0 )
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
;     mov x8 x0
     mov x1 @$FF00
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
     mov x0 x8



     
     ; now we can scroll using the virtual offset message

     
:flip

   mov x3 @0
   bl page-flip:

   (wait DELAY:)

   mov x3 @height
   bl page-flip:

   (wait DELAY:)

b flip-


     (debug-str "DONEDONE" #t)
 :loop
     b loop:

(create-send-char)

:enable-irq
msr daifclr @2
eret

:disble-irq
msr daifset @2
eret

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
      

  ; the following values are used for loading 64 bit addresses/values via LDR(literal)
  ; and they must be 64 bit aliged otherwise the CPU faults!
  /= 8
  (periph-addresses)
  :DELAY (write-value-64 $FFFF)
  :VC_MBOX (write-value-64 VCORE-MBOX)
  :TEST (write-value-64 $123456789ABCDEF)
  :START (write-value-64 $80000)
  :CONVERT (write-value-64 $3FFFFFFF)
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
      (write-value-32 384); y

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


  ;; :MBOX-MSG
  ;;   (write-value-32 (* 8 4)) ; total buffer size bytes including headers
  ;;   (write-value-32 0) ; request / response code (0 = request)
  ;;     ;begin tags
  ;;     (write-value-32 $10004) ; Get firmware revision
  ;;     (write-value-32 8) ; value buffer size
  ;;     (write-value-32 0) ; 0 = request
  ;;     (write-value-32 0)
  ;;     (write-value-32 0); response should be written here
  ;;   (write-value-32 0) ;end tag


  ; SWITCH ON ACT LED
;; :MBOX-MSG
;;     (write-value-32 (* 8 4)) ; total buffer size bytes including headers
;;     (write-value-32 0) ; request / response code (0 = request)
;;       ;begin tags
;;       (write-value-32 $38041) ;set led
;;       (write-value-32 8) ; req length
;;       (write-value-32 0) ; 0 = request
;;       (write-value-32 130)
;;       (write-value-32 1)
;;     (write-value-32 0) ;end tag
    
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

