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

(define-syntax (subr stx)
  ; define a subroutine. create a label for it and push/pop the supplied
  ; regs as a prologue/epilogue. Of course, with a bit more work they
  ; could be automatically detected
  (syntax-parse stx
    [(_ subroutine-name:id [used-reg:register ...] code )
     #:with (reg-rev ...)
     (datum->syntax this-syntax (reverse (syntax->list #'(used-reg ...))))
     #:with label
     (let* ([sym (syntax-e #'subroutine-name)]
            [str (symbol->string sym)]
            [label-str (format ":~a" str)]
            [label (string->symbol label-str)])
       (datum->syntax this-syntax label))
     #'(begin
         (try-set-jump-source `label set-jump-source-current)
         (PUSH x30) ; always preserve return address register
         (PUSH used-reg) ...
         code
         (POP reg-rev) ...
         (POP x30)
         { ret x30 })]))

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
     #'{ ldr r [sp] @16 }]))

(define-syntax (POPH stx)
  (syntax-parse stx
    [(_ r:register)
     #'{ ldrh r [sp] @16 }]))

(define-syntax (POPB stx)
  (syntax-parse stx
    [(_ r:register)
     #'{ ldrb r [sp] @16 }]))


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
     
     ; VideoCore message tags

     
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
     (cond
       [(equal? pi 'pi3) {mov x1 @270}]
       [(equal? pi 'pi4) {mov x1 @541}]
       [else (error "unsupported pi model")])
     strh w1 (x0 @aux-baud)    
     mov  x1 @3
     strb w1 (x0 @aux-cntrl)      ; enable tx rx

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


     
     ; send string over the mini UART
     ;; (define (debug-w0)
     ;;   {
     ;;    ; our protocol expects a byte of 2 to then
     ;;    ; receive a 32 bit number
     ;;    (PUSH x0)
     ;;    orr w8 w0 w0
     ;;    mov w0 @2
     ;;    bl send-char:
     ;;    orr w0 w8 w8
     ;;    bl  send-char:
     ;;    lsr w0 w0 @8
     ;;    bl  send-char:
     ;;    lsr w0 w0 @8    
     ;;    bl  send-char:
     ;;    lsr w0 w0 @8    
     ;;    bl  send-char:
     ;;    (POP x0)
     ;;   })

     
     bl dump-regs:     
;     (debug-str "HELLO WORLD!")
 ;    (debug-str "BAREMETAL RPI4 MEETS RACKETLANG!")
     (debug-str "heres a number" #t)
     mov w0 @$BAD
     lsl x0 x0 @16
     movk w0 @$F00D
     (debug-reg x0 w0)


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

     
     ldr x0 VC_MBOX: 
:wait      
     ldr w1 (x0 @mbox-status)
     (debug-str "STATUS" #t)
     (debug-reg x0)
     (debug-reg x1)

     ; and with 0x8000_0000
     mov x2 @1
     lsl x2 x2 @31
     and x1 x1 x2
     cbnz x1 wait-
     (debug-str "RTS" #t)
     ; attempt to call the mailbox interface
     ; upper 28 bits are the address of the message
     adr x2 MBOX-MSG:
     mov x0 x2
;     (debug-w0)
     ldr x0 VC_MBOX: 
     ; lower four bits specify the mailbox channel,
     ; in this case mbox-ch-prop (8)
     mov x3 @mbox-ch-prop
     orr x2 x2 x3 ; we dont support orr reg-reg-imm yet
     ;now we wait until the FULL flag is not set so we can
     ; send our message
     (debug-reg w0)
     (debug-reg w2)

     str w2 (x0 @mbox-write)

     (debug-str "WFR" #t)
     ; now wait for a response 
:wait      
    ldr w1 (x0 @mbox-status)
;    (debug-str "STATUS")
;    (debug-reg x1)
     ; and with 0x4000_0000
     mov x2 @1
     lsl x2 x2 @30
     and x1 x1 x2
     cbnz x1 wait-
;     (debug-str "DONE")
     ; check if mbox-read = channel


     ldr w1 (x0 @mbox-read)
     (debug-reg x1)
     mov x2 @%1111
     and x1 x1 x2
     sub x1 x1 @mbox-ch-prop
     cbnz x1 wait-

     ;; (debug-str "response code")
     ;; adr x0 MBOX-MSG:
     ;; add x0 x0 @4
     ;; ldr w0 (x0 @0)
     ;; (debug-reg x0)

     ;; (debug-str "inner response code")
     ;; adr x0 MBOX-MSG:
     ;; add x0 x0 @16
     ;; ldr w0 (x0 @0)
     ;; (debug-reg x0)

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
     mov x1 @255
     mov x4 @600  ; rows
:row     
     mov x3 @$800
:col
     str w1 (x0 @0)
     add w0 w0 @4
;     add w1 w1 @32    
     sub x3 x3 @1
     cbnz x3 col-

     sub x4 x4 @1
     cbnz x4 row-
     
     (debug-str "DONEDONE" #t)
 :loop
     b loop:

(subr send-char [x0 x1 x2 x3] {
  ldr  x1 AUXB:
  ; wait for ready bit
  mov  x2 @$20
  :wait     
  ldr  w3 (x1 @aux-lsr)
  and  w3 w2 w3
  cbz  w3 wait-
  strb w0 (x1 @aux-io)
})
     
;; :send-char ; put char in w0
;;   (PUSH x30)
;;   (PUSH x0)
;;   (PUSH x1)
;;   (PUSH x2)
;;   (PUSH x3)
;;   ldr  x1 AUXB:
;;   ; wait for ready bit
;;   mov  x2 @$20
;;   :wait     
;;   ldr  w3 (x1 @aux-lsr)
;;   and  w3 w2 w3
;;   cbz  w3 wait-
;;   strb w0 (x1 @aux-io)
;;   (POP x3)
;;   (POP x2)
;;   (POP x1)
;;   (POP x0)
;;   (PUSH x30)
;;   ret x30

:dump-regs
  (PUSH x30)
  (dump-all-regs)
  (POP x30)
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
  :VC_MBOX (write-value-64 VCORE-MBOX)
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
      (write-value-32 800) ; width
      (write-value-32 600); height
    ; set virt
      (write-value-32 $48004)
      (write-value-32 8) ; value buffer size
      (write-value-32 8) ; 0 = request
      (write-value-32 800) ; width
      (write-value-32 600); height

    ; set virtoff
      (write-value-32 $48009)
      (write-value-32 8) ; value buffer size
      (write-value-32 0) ; 0 = request
      (write-value-32 0) ; x
      (write-value-32 0); y

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

