#lang pisemble
(require (for-syntax syntax/parse racket/stxparam))
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
     b loop:

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
  
})
