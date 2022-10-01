;Pisemble
;Copyright Ross McKinlay, 2022

#lang racket

(require (for-syntax syntax/parse
                     racket/string
                     racket/base
                     racket/match
                     racket/syntax
                     racket/list))

(require syntax/parse/define)

(require "objfile.rkt")
(define is-debug #f)

(define-syntax (wdb stx)  
  (syntax-parse stx
    [(_ text)
     #'(when is-debug
         (writeln text))]
  [(_ text args ...)
   #'(when is-debug
       (writeln (format text args ...)))]))

(define (pbin n)
  (writeln (~r n #:base 2 #:min-width 32 #:pad-string "0")))

(define (phex n)
  (writeln (~r n #:base 16 #:min-width 32 #:pad-string "0")))

(define (to-bin n)
  (~r n #:base 2 #:min-width 32 #:pad-string "0"))

(define (lo-byte input)
  (bitwise-and input #xFF))

(define (hi-byte input)
  (arithmetic-shift
   (bitwise-and input #xFF00)
   -8))

(define (lo-byte2 input)
  (arithmetic-shift
   (bitwise-and input #xFF0000)
   -16))

(define (hi-byte2 input)
  (arithmetic-shift
   (bitwise-and input #xFF000000)
   -24))

(struct metadata (opcode           ; opcode symbol
                 addressing-mode  ; symbol
                 raw-value        ; binary value without register/value encoding
                 32bit-mask       ; if this opcode supports 32bit mode, then this mask will
                                  ; be AND'd with the result if  a 32bit register is used
                                  ; thus clearing any not-set bits
                 imm-mod          ; some instructions need their immediate value shifted or modified
                                  ; depending on 32 bit, this function takes (imm 32bit?) and returns the new imm
                 ) #:transparent)

(struct encoders (data-encoder address-encoder) #:transparent)
(define-syntax (bitwise-ior-n stx)
  (syntax-parse stx
    [(_ n ns ...+) #'(bitwise-ior n (bitwise-ior-n ns ...))]
    [(_ n) #'n]))
     

(define-syntax (create-opcode-encoders stx)
  (syntax-parse stx
    [(_ ([addressing-mode data-encoder address-encoder] ...))
    #'(make-hash
       (list
        (cons
         addressing-mode
         (encoders data-encoder address-encoder)) ...))]))

; data encoder is a function that takes the registers and other values known
; at syntax parsing time, except immediate values (with special cases for optional shifts and such)
; basically anything that can't be a label.

; address encoder is a function that encodes the literal value for an opcode.
; this might be at syntax-expand time or it might be later when the assembler resolves
; labels

; the order of operands here is always the order as they appear left to right in the
; reference manual - EXCEPT the immediate values are placed into their own function
; and so removed from the order 
(define opcode-encoders
  (create-opcode-encoders
   (['imm26
     (match-lambda [(list bin) bin])
     (match-lambda
       [(list bin imm26)
        ; divide by 4 here since our offset is in bytes the cpu is interested in
        ; words of 4 bytes. Splat the resulting top 6 bits as we are encoding
        ; a 26 bit immediate value
        (let ([shifted (bitwise-and #x3FFFFFF (arithmetic-shift imm26 -2))])
          (bitwise-ior bin shifted))])]
    ['imm19-rt
     (match-lambda
       [(list bin rt)
        ;rt goes in the bottom 5 bits
        (bitwise-ior bin rt)])
     (match-lambda
       [(list bin imm19)
        ; we need to divide by 4 but also shift up 5 bits so we can instead
        ; shift left by 3, then splat the top 19 bits and lower 5 bits
        (let ([shifted (bitwise-and #x00FFFFE0 (arithmetic-shift imm19 3))])
          (bitwise-ior bin shifted))])     ]
    ['imm19
     (match-lambda
       [(list bin) bin])
     (match-lambda
       [(list bin imm19)
        ; we need to divide by 4 but also shift up 5 bits so we can instead
        ; shift left by 3, then splat the top 19 bits and lower 5 bits
        (let ([shifted (bitwise-and #x00FFFFE0 (arithmetic-shift imm19 3))])
          (bitwise-ior bin shifted))])     ]
    ['hw-imm16-rd
     (match-lambda
       [(list bin rd shift)
        (when  (not (eq? (remainder shift 16) 0))
          (error (format "error: shift value ~x is not a multiple of 16 " shift)))
        (let ([shifted (arithmetic-shift (quotient shift 16) 21)])          
          ;rd goes in the bottom 5 bits
          (bitwise-ior-n rd bin shifted))])
     (match-lambda
       [(list bin imm16)
        (when (>= imm16 (arithmetic-shift 1 16))
          (error (format "error: immediate value ~x is larger than 16 bits" imm16)))
          ; shift up 5 bits, keep 16 bits
          (let ([shifted (arithmetic-shift (bitwise-and #xFFFF imm16) 5)])
          (bitwise-ior-n bin shifted))])
     ]
    ['imm16-rd
     (match-lambda
       [(list bin rd)        
        ;rd goes in the bottom 5 bits
        (bitwise-ior-n rd bin)])
     (match-lambda
       [(list bin imm16)
        (when (>= imm16 (arithmetic-shift 1 16))
          (error (format "error: immediate value ~x is larger than 16 bits" imm16)))
        ; shift up 5 bits, keep 16 bits
        (let ([shifted (arithmetic-shift (bitwise-and #xFFFF imm16 ) 5) ])
          (bitwise-ior-n bin shifted))])

     ]
    ['immlo-immhi-rd
     (match-lambda
       [(list bin rd)        
        ;rd goes in the bottom 5 bits
        (bitwise-ior-n rd bin)])
     (match-lambda
       [(list bin imm)
        ; bottom two bits go to 30-29, rest to 23-5
        (let* (
              [shifted-lo (arithmetic-shift (bitwise-and #x3 imm ) 29) ]
              [shifted (arithmetic-shift (bitwise-and #x1FFFFC imm ) 3) ])
          (bitwise-ior-n bin shifted-lo shifted))])

     ]
    ['immr-imms-rn-rd 
     (match-lambda
       [(list bin rd rn)
        (let ([rn-shifted (arithmetic-shift rn 5)])
          (bitwise-ior-n rn-shifted rd bin))])
     (match-lambda
       [(list bin imm6)
        ; this is encoded a bit strange. we need to negate the value and preserve
        ; zero all the extra twos complement bits for immr
        ; then do the same but negate and -1 for imms
        (let ([immr (arithmetic-shift (bitwise-and #b111111 (- imm6)) 16)]
              [imms (arithmetic-shift (bitwise-and #b111111 (- (+ 1 imm6))) 10)])
          (bitwise-ior-n immr imms bin))])

     ]
    ['immr6-rn-rd 
     (match-lambda
       [(list bin rd rn)
        (let ([rn-shifted (arithmetic-shift rn 5)])
          (bitwise-ior-n rn-shifted rd bin))])
     (match-lambda
       [(list bin imm6)
        (when (>= imm6 (arithmetic-shift 1 6))
          (error (format "error: immediate value ~x is larger than 6 bits" imm6)))
        (let ([immr (arithmetic-shift imm6 16)])
          (bitwise-ior-n immr bin))])
     ]
    ['imm7-rd2-rn-rd 
     (match-lambda
       [(list bin rd rd2 rn)
        (let ([rn-shifted (arithmetic-shift rn 5)]
              [rd2-shifted (arithmetic-shift rd2 10)])
          (bitwise-ior-n rn-shifted rd2-shifted rd bin))])
     (match-lambda
       [(list bin imm7)
        (when (>= imm7 (arithmetic-shift 1 7))
          (error (format "error: immediate value ~x is larger than 7 bits" imm7)))
        (let ([immr (arithmetic-shift imm7 15)])
          (bitwise-ior-n immr bin))])
     ]
    
    ['imm9-rn-rd
     (match-lambda
       [(list bin rd rn)
        (let ([rn-shifted (arithmetic-shift rn 5)])
          (bitwise-ior-n rn-shifted rd bin))])
     (match-lambda
       [(list bin imm9)
        (when (>= imm9 (arithmetic-shift 1 9))
          (error (format "error: immediate value ~x is larger than 9 bits" imm9)))
        (let ([imm (arithmetic-shift (bitwise-and #b111111111 imm9) 12)])
          (bitwise-ior-n imm bin))])
     ]

    ['imm12-rn-rd
     (match-lambda
       [(list bin rt rn)
        (let ([rn-shifted (arithmetic-shift rn 5)])
          (bitwise-ior-n rn-shifted rt bin))])
     (match-lambda
       [(list bin imm12)
        (when (>= imm12 (arithmetic-shift 1 12))
          (error (format "error: immediate value ~x is larger than 12 bits" imm12)))
        (let ([imm (arithmetic-shift (bitwise-and #b111111111111 imm12) 10)])
          (bitwise-ior-n imm bin))])
     ]
    ['imm9-rn-rd
     (match-lambda
       [(list bin rt rn)
        (let ([rn-shifted (arithmetic-shift rn 5)])
          (bitwise-ior-n rn-shifted rt bin))])
     (match-lambda
       [(list bin imm9)
        (when (>= imm9 (arithmetic-shift 1 9))
          (error (format "error: immediate value ~x is larger than 9 bits" imm9)))
        (let ([imm-shifted (arithmetic-shift (bitwise-and #b111111111 imm9) 12)])
          (bitwise-ior-n imm-shifted bin))])
     ]

    ; this needs more work
    ; the immeditadate value splits the register number
    ['sysreg-imm4
     (match-lambda
       [(list bin sr)
        (let ([sr-shifted (arithmetic-shift sr 5)])
          (bitwise-ior-n sr-shifted bin))])
     (match-lambda
       [(list bin imm)
        (let ([imm-shifted (arithmetic-shift imm 8)])
          (bitwise-ior-n imm-shifted bin))])
     ]
    ['rt-sysreg
     (match-lambda
       [(list bin rt sr)
        (let ([sr-shifted (arithmetic-shift sr 5)])
          (bitwise-ior-n sr-shifted rt bin))])
     (match-lambda
       [(list bin) bin])
     ]
    ['sysreg-rt
     (match-lambda
       [(list bin sr rt)
        (let ([sr-shifted (arithmetic-shift sr 5)])
          (bitwise-ior-n sr-shifted rt bin))])
     (match-lambda
       [(list bin) bin])
     ]
    ['rm-cond-rn-rd
     (match-lambda
       [(list bin rd rn rm con)
        (let ([rn-shifted (arithmetic-shift rn 5)]
              [rm-shifted (arithmetic-shift rm 16)]
              [cond-shifted (arithmetic-shift con 12)])
          (bitwise-ior-n rd rn-shifted rm-shifted cond-shifted bin))])
     (match-lambda
       [(list bin) bin])
     ]
    ['rm-rn-rd
     (match-lambda
       [(list bin rd rn rm)
        (let ([rn-shifted (arithmetic-shift rn 5)]
              [rm-shifted (arithmetic-shift rm 16)])
          (bitwise-ior-n rd rn-shifted rm-shifted bin))])
     (match-lambda
       [(list bin) bin])
     ]
    ['rm-rd
     (match-lambda
       [(list bin rd rm)
        (let ([rm-shifted (arithmetic-shift rm 16)])
          (bitwise-ior-n rd rm-shifted bin))])
     (match-lambda
       [(list bin) bin])
     ]
    ['rm-rn
     (match-lambda
       [(list bin rn rm)
        (let ([rm-shifted (arithmetic-shift rm 16)]
              [rn-shifted (arithmetic-shift rn 5)])
          (bitwise-ior-n rn-shifted rm-shifted bin))])
     (match-lambda
       [(list bin) bin])
     ]
    ['rn
     (match-lambda
       [(list bin rn)
        (let ([rn-shifted (arithmetic-shift rn 5)])
          (bitwise-ior-n rn-shifted bin))])
     (match-lambda
       [(list bin) bin])
     ]
    ['none
     (match-lambda
       [(list bin) bin])
     (match-lambda
       [(list bin) bin])
     ]
    )))


(define-syntax (create-opcode-metadata stx)
  (syntax-parse stx
    [(_ ([opcode addressing-mode-type addressing-mode raw-value 32bit imm-mod] ...))
     #'(make-hash
        (list
         (cons
          (cons opcode addressing-mode-type)
          (metadata opcode addressing-mode raw-value 32bit imm-mod)) ...))]))


(define opcode-metadata
  (let ([b31 #b01111111111111111111111111111111]
        [b30 #b10111111111111111111111111111111]
        [lsl-imm-special #b01111111101111111111111111111111];here we splat 'N' and sf
        [lsr-imm-special #b01111111101111110111111111111111];here we splat 'N', sf and the upper bit of imms
        [shift-2-or-3
         (λ (imm is32?)
           (if is32?
               (if (eq? 0 (bitwise-and #b11 imm))
                   (arithmetic-shift imm -2)
                   (error "not divisible by 4"))
               (if (eq? 0 (bitwise-and #b111 imm))
                   (arithmetic-shift imm -3)
                   (error "not divisible by 8"))))]
        [always-shift-1 (λ (imm is32?)
                          (if (eq? 0 (bitwise-and #b1 imm))
                              (arithmetic-shift imm -1)
                              (error "not divisible by 2")))]
        [is-byte (λ (imm is32?)
                   (if (> imm 255)
                       (error (format "only support +/128 not ~a" imm))
                       imm))])
  (create-opcode-metadata
   (['add 'reg-reg-imm   'imm12-rn-rd                        #b10010001000000000000000000000000 b31 #f]
    ['add 'reg-reg-reg   'rm-rn-rd                           #b10001011000000000000000000000000 b31 #f]
    ['adr 'reg-lbl       'immlo-immhi-rd                     #b00010000000000000000000000000000 #f #f]
    ['and 'reg-reg-reg   'rm-rn-rd                           #b10001010000000000000000000000000 b31 #f]    
    ['b   'lbl           'imm26                              #b00010100000000000000000000000000 #f #f]

    ['b.eq 'lbl          'imm19                              #b01010100000000000000000000000000 #f #f] ; equal Z = 1
    ['b.ne 'lbl          'imm19                              #b01010100000000000000000000000001 #f #f] ; not equal, Z = 0
    ['b.cs 'lbl          'imm19                              #b01010100000000000000000000000010 #f #f] ; Carry set, C = 1
    ['b.cc 'lbl          'imm19                              #b01010100000000000000000000000011 #f #f] ; Carry clear, C = 0    
    ['b.mi 'lbl          'imm19                              #b01010100000000000000000000000100 #f #f] ; Minus, neg ; N = 1
    ['b.pl 'lbl          'imm19                              #b01010100000000000000000000000101 #f #f] ; Positive or zero ; N = 0
    ['b.vs 'lbl          'imm19                              #b01010100000000000000000000000110 #f #f] ; Overflow  ; V = 1
    ['b.vc 'lbl          'imm19                              #b01010100000000000000000000000111 #f #f] ; No overfolow, V = 0
    ['b.hi 'lbl          'imm19                              #b01010100000000000000000000001000 #f #f] ; Unsigned higher, C = 1 && z = 0
    ['b.ls 'lbl          'imm19                              #b01010100000000000000000000001001 #f #f] ; Unsigned lower or same !(C=1&&z=0)
    ['b.ge 'lbl          'imm19                              #b01010100000000000000000000001010 #f #f] ; Signed greater eq, N = V
    ['b.lt 'lbl          'imm19                              #b01010100000000000000000000001011 #f #f] ; Signed le, N! = V   
    ['b.gt 'lbl          'imm19                              #b01010100000000000000000000001100 #f #f] ; Signed gt, Z = 0 && N = V
    ['b.le 'lbl          'imm19                              #b01010100000000000000000000001101 #f #f] ; Signed le or eq, !(Z=0 & N=V
    ['b.al 'lbl          'imm19                              #b01010100000000000000000000001110 #f #f] ; Always


    ['bl  'lbl           'imm26                              #b10010100000000000000000000000000 #f #f]
    ['cbz 'reg-lbl       'imm19-rt                           #b10110100000000000000000000000000 b31 #f]
    ['cbnz 'reg-lbl      'imm19-rt                           #b10110101000000000000000000000000 b31 #f]
    ['cmp  'reg-reg      'rm-rn                              #b11101011000000000000000000011111 b31 #f]
    ['csel 'reg-reg-reg-cond 'rm-cond-rn-rd                  #b10011010100000000000000000000000 b31 #f]
    ['eor 'reg-reg-reg   'rm-rn-rd                           #b11001010000000000000000000000000 b31 #f]
    ['eret 'none         'none                               #b11010110100111110000001111100000 #f #f ]

    ['ldp 'reg-reg_reg-imm_ 'imm7-rd2-rn-rd                  #b10101001010000000000000000000000 b31 shift-2-or-3]
    ['ldr 'reg_reg_imm  'imm9-rn-rd                          #b11111000010000000000010000000000 b30 #f]
    ['ldr 'reg_reg-imm_excla  'imm9-rn-rd                    #b11111000010000000000110000000000 b30 #f]

    ['ldr 'reg-lbl       'imm19-rt                           #b01011000000000000000000000000000 b30 #f]
    ['ldr  'reg_reg-imm_ 'imm12-rn-rd                        #b11111001010000000000000000000000 b30 shift-2-or-3]
    ['ldrh 'reg_reg_imm  'imm9-rn-rd                         #b01111000010000000000010000000000 #f #f]
    ['ldrh 'reg_reg-imm_excla  'imm9-rn-rd                   #b01111000010000000000110000000000 #f #f]
    
    ['ldrh 'reg_reg-imm_ 'imm12-rn-rd                        #b01111001010000000000000000000000 b31 #f]

    ['ldrb 'reg_reg_imm  'imm9-rn-rd                         #b00111000010000000000010000000000 b30 #f]
    ['ldrb 'reg_reg-imm_excla  'imm9-rn-rd                   #b00111000010000000000110000000000 #f #f]
    ['ldrb 'reg_reg-imm_ 'imm12-rn-rd                        #b00111001010000000000000000000000 #f  #f]
    ['lsl 'reg-reg-imm   'immr-imms-rn-rd                    #b11010011010000000000000000000000 lsl-imm-special #f]
    ['lsr 'reg-reg-imm   'immr6-rn-rd                        #b11010011010000001111110000000000 lsr-imm-special #f]

    ['mov 'reg-reg       'rm-rd                              #b10101010000000000000001111100000 b31 #f]
    ; todo: not sure N here works always set to one with 32 bhit as well?
    ['mov 'reg-imm       'imm16-rd                           #b11010010100000000000000000000000 b31 #f]
    ['movz 'reg-imm      'imm16-rd                           #b11010010100000000000000000000000 b31 #f]
    ['movz 'reg-imm-shift 'hw-imm16-rd                       #b11010010100000000000000000000000 b31 #f]
    ['movk 'reg-imm      'imm16-rd                           #b11110010100000000000000000000000 b31 #f]
    ['movk 'reg-imm-shift 'hw-imm16-rd                       #b11110010100000000000000000000000 b31 #f]
    ['mul 'reg-reg-reg   'rm-rn-rd                           #b10011011000000000111110000000000 b31 #f]
    ['mrs 'reg-sysreg    'rt-sysreg                          #b11010101001100000000000000000000 #f #f]
    ['msr 'sysreg-reg    'sysreg-rt                          #b11010101000100000000000000000000 #f #f]
    ['msr 'sysreg-imm4   'sysreg-imm4                        #b11010101000000000000000000011111 #f #f]
    ['neg 'reg-reg       'rm-rd                              #b11001011000000000000001111100000 b31 #f]
    ['nop 'none          'none                               #b11010101000000110010000000011111 #f #f]
    ['orr 'reg-reg-reg   'rm-rn-rd                           #b10101010000000000000000000000000 b31 #f]
    ['ret 'reg           'rn                                 #b11010110010111110000000000000000 #f #f]

    ['stp 'reg-reg_reg-imm_ 'imm7-rd2-rn-rd                  #b10101001000000000000000000000000 b31 shift-2-or-3]

    ['str 'reg_reg-imm_excla  'imm9-rn-rd                    #b11111000000000000000110000000000 b30 #f]
    ['str 'reg_reg_imm    'imm9-rn-rd                        #b11111000000000000000010000000000 b30 #f]

    ;unsigned offset mode str. divide imm12 by 4 or 8 first 
    ['str 'reg_reg-imm_  'imm12-rn-rd                        #b11111001000000000000000000000000 b30 shift-2-or-3]

    ['strh 'reg_reg-imm_excla 'imm9-rn-rd                    #b01111000000000000000110000000000 #f #f]
    ['strh 'reg_reg_imm   'imm9-rn-rd                        #b01111000000000000000010000000000 #f #f]
    
    ;unsigned offset mode, 32 bit only. always shift by 4
    ['strh 'reg_reg-imm_ 'imm12-rn-rd                        #b01111001000000000000000000000000 b31 always-shift-1]

    ['strb 'reg_reg-imm_excla 'imm9-rn-rd                    #b00111000000000000000110000000000 #f #f]
    ['strb 'reg_reg_imm   'imm9-rn-rd                        #b00111000000000000000010000000000 #f #f]
    ;unsigned offset mode, 32 bit only]
    ['strb 'reg_reg-imm_ 'imm12-rn-rd                        #b00111001000000000000000000000000 b31 #f]
    ['stur 'reg_reg-imm_ 'imm9-rn-rd                         #b11111000000000000000000000000000 b30 is-byte]

    ['sub 'reg-reg-imm   'imm12-rn-rd                        #b11010001000000000000000000000000 b31 #f]
    ['sub 'reg-reg-reg   'rm-rn-rd                           #b11001011000000000000000000000000 b31 #f]
    ['wfe 'none          'none                               #b11010101000000110010000001011111 #f #f]))))

(struct context (data location minl maxl jump-table branches-waiting linker-labels) #:mutable #:transparent)
(struct target-label (immediate-encoder relative location) #:transparent)
(define prog (context (make-vector 65536000 0) 0 0 0 (make-hash) (make-hash) (list)))

(define (update-min v)
  (cond [(< v (context-minl prog)) (set-context-minl! prog v)]))

(define (update-max v)
  (cond [(> v (context-maxl prog)) (set-context-maxl! prog v)]))

(define (update-min-max v)
  (update-min v)
  (update-max v))

(define (set-location v)
  (set-context-location! prog v)
  (update-min-max v))

(define (inc-location)
  (set-location (+ 1 (context-location prog))))

(define (is-global-label? key)
  (char-upper-case? (string-ref key 1)))

(define current-obj (create-blank-obj-file))
    
(define (set-current-obj new-obj)
  (set-obj-file-string-table! current-obj (obj-file-string-table new-obj))
  (set-obj-file-reverse-string-table! current-obj (obj-file-reverse-string-table new-obj))
  (set-obj-file-object-composition! current-obj (obj-file-object-composition new-obj))
  (set-obj-file-symbol-table! current-obj (obj-file-symbol-table new-obj))
  (set-obj-file-target-table! current-obj (obj-file-target-table new-obj))
  (set-obj-file-data! current-obj (obj-file-data new-obj)))


(define (set-jump-source label location)
  (when (is-global-label? label)
    (if (hash-has-key? (obj-file-reverse-string-table current-obj) label)
        (error (format "could not create the global label ~A since it already exists in a linked object" label))
        (set-current-obj 
         (match-let*
             ([obj current-obj]
              [(cons index obj) (add-or-get-string obj label)]
              )
           (begin
              (set-obj-file-symbol-table!
               obj
               (cons (cons index location) (obj-file-symbol-table obj)))
              obj)
           ))))

  (let* ([h (context-jump-table prog)]
         [v (hash-ref! h label '())])
    (hash-set! h label (cons location v))))
    
(define (set-jump-source-current label)
  (set-jump-source label (context-location prog)))

(define (set-jump-source-next label)
  (set-jump-source label (+ (context-location prog) 1)))

(define (add-branch-dest label imm-encoder relative location)
    (wdb "adding branch dest ~a ~a ~a ~a" label imm-encoder relative location)
  (let* ([h (context-branches-waiting prog)]
         [v (hash-ref! h label '())])
    (hash-set! h label (cons (target-label imm-encoder relative location) v))))

(define (set-current-value v)
  (vector-set! (context-data prog) (context-location prog) v))

(define (set-linker-resolved-label global-label location)
  (displayln (format "label ~a loc ~a" global-label location))
  ; append label * loction to the linker-labels list
  (set-context-linker-labels!
   prog
   (cons
    (cons global-label location)
    (context-linker-labels prog))))
         


(define (try-set-jump-source expr f)
  (wdb "in try set jump source with ~a" expr)
  (cond [(symbol? expr)
         (wdb "setting jump source ~a" expr)
         (f (symbol->string expr))]))

(define (write-transition-target expr imm-encoder)
  (wdb "write trans target ~a " expr)
  (let* ([s (symbol->string expr)]
         [relative (cond
                     [(string-suffix? s "+") '+]
                     [(string-suffix? s "-") '-]
                     [else #f])])
    (let ([symbol-name
           (match relative
             [(or '+ '-)
              (substring s 0 (- (string-length s) 1))]
             [_ (substring s 0 (- (string-length s) 1))]
             )])
      (wdb "transition target ~a ~a" symbol-name relative)
      (add-branch-dest (string-append ":" symbol-name) imm-encoder relative (context-location prog))
      
      )))

(define (here) (context-location prog))

(define (align n)
  (define (aux i)
    (if (eq? (remainder i n) 0)
        (set-location i)
        (aux (+ i 1))))
  (aux (context-location prog)))

(define (write-value expr)
;  (writeln (format "writing value ~a ~a" expr (to-bin expr)))
  (cond 
        [(number? expr)
         (begin
           (set-current-value expr)
           (inc-location)
           
           (update-min-max (context-location prog)))]))

(define (write-values exprs)
  (for ([e (flatten exprs)])
    (if (list? e)
        (write-values e)
        (write-value e))))
(define (write-value-16 e)
  (write-values
   (list
     (lo-byte e)
     (hi-byte e))))
(define (write-value-32 e)
  (write-values
   (list
     (lo-byte e)
     (hi-byte e)
     (lo-byte2 e)
     (hi-byte2 e))))
(define (write-value-64 e)
  (let ([msb (arithmetic-shift e -32)])
    (write-value-32 e)
    (write-value-32 msb)))

(begin-for-syntax
  (define-syntax-class label-targ
    (pattern x:id #:when
             (let ([s (symbol->string (syntax-e #'x))])
               (or (string-suffix? s ":")
                   (string-suffix? s "+")
                   (string-suffix? s "-"))))))

(begin-for-syntax
  (define-syntax-class label
    (pattern x:id #:when
             (let ([s (symbol->string (syntax-e #'x))])
               (or (string-prefix? s ":"))))))


(define-syntax (resolve-global-label-address stx)
  (syntax-parse stx
    [(_ global-label:label)
;     #:with label (symbol->string (syntax-e #'label))
     #'(begin
         (set-linker-resolved-label 'global-label (here))
         (set-location (+ 8 (context-location prog))))]))

(define-syntax (label-loc stx)
  (syntax-parse stx
    [(_ label:label-targ)
     (let ([s (symbol->string (syntax-e #'label))])
       (with-syntax ([new-symbol (string-append ":" (substring s 0 (- (string-length s) 1)))])
       (cond
         [(string-suffix? s ":")
            #'(find-closest-label 'new-symbol (here) #f)]
         [(string-suffix? s "+")
            #'(find-closest-label 'new-symbol (here) '+)]
         [(string-suffix? s "-")
            #'(find-closest-label 'new-symbol (here) '-)]
         )))]))

(define (write-instruction opcode syntax-mode is-32bit? data-args immediate-address-args transition-target)
  (begin
       ; (writeln (format "write-instruction ~a ~a ~a ~a ~a ~a" opcode syntax-mode is-32bit? data-args immediate-address-args transition-target))
  (let*([meta (hash-ref opcode-metadata (cons opcode syntax-mode))]
        [raw (metadata-raw-value meta)]
        [addressing-mode (metadata-addressing-mode meta)]
        [32bit-mask (metadata-32bit-mask meta)]
        ; main data encoder
        [data-encoder (encoders-data-encoder (hash-ref opcode-encoders addressing-mode))]
        ; encoder for immediate values and addresses - might get called again after label resolution
        [address-encoder (encoders-address-encoder (hash-ref opcode-encoders addressing-mode))]
        ; unique per-instruction additional immediate value encoder 
        [imm-mod-raw (metadata-imm-mod meta)]
        [imm-mod (if imm-mod-raw imm-mod-raw (λ (bin is32) bin))]
        ; define a new address encoder in terms of imm-mod
        [final-address-encoder
         (match-lambda
           [(list bin) bin]
           [(list bin imm) (address-encoder (list bin (imm-mod imm is-32bit?)))])]
        [with-data (data-encoder (cons raw data-args))]
        [with-addr (final-address-encoder (cons with-data immediate-address-args))]

        [final (if (and 32bit-mask is-32bit?) (bitwise-and with-addr 32bit-mask) with-addr)])
    ; for each instruction we encode it using the data and address encoder
    ; then we process the 32 bit mask if this is in 32 bit mode
    ; later the address encoder might get called again with a label target

    ; now we mark this as a target if applicable then write the bytes
    (when transition-target (write-transition-target transition-target final-address-encoder))
;    (pbin final)
;    (phex final)
;    (wdb "write-instruction ~a ~a ~a ~a ~a" opcode syntax-mode is-32bit? data-args immediate-address-args)
    (write-value-32 final))))

(begin-for-syntax
  (define-syntax-class immediate
    #:description "immediate value"
    (pattern #:immediate))
  (define-syntax-class system-register
    #:description "system register"
    #:datum-literals (mpidr_el1 daifset daifclr vbar_el1 esr_el1 elr_el1 sctlr_el1 hcr_el2 scr_el3 spsr_el3 elr_el3)
    ; these are not currently implemented properly.
    ; the immediate and reg modes of mrs encode these
    ; values differently. but you also can't set/get all regs
    ; with both modes. at the moment daif will only work
    ; with msr imm and mpidr with mrs reg and msr reg
    ; this needs work in general. but it will do for my current needs
    (pattern mpidr_el1 #:with regnum #x4005)
    (pattern vbar_el1 #:with regnum #x4600)
    (pattern esr_el1 #:with regnum #x4290)
    (pattern hcr_el2 #:with regnum #x6088)
    (pattern scr_el3 #:with regnum #x7088)
    (pattern spsr_el3 #:with regnum #x7200)
    (pattern elr_el3 #:with regnum #x7201)

    (pattern sctlr_el1 #:with regnum #x4080)
    (pattern elr_el1 #:with regnum #x4201)
    (pattern daifset #:with regnum #x1a06)
    (pattern daifclr #:with regnum #x1a07))

  (define-syntax-class condition
    #:description "condition"
    #:datum-literals (eq ne cs cc mi pl vs vc hi ls ge lt gt le al)
    (pattern eq #:with condnum #b0000) ; equal Z = 1
    (pattern ne #:with condnum #b0001) ; not equal, Z = 0
    (pattern cs #:with condnum #b0010) ; Carry set, C = 1
    (pattern cc #:with condnum #b0011) ; Carry clear, C = 0    
    (pattern mi #:with condnum #b0100) ; Minus, neg ; N = 1
    (pattern pl #:with condnum #b0101) ; Positive or zero ; N = 0
    (pattern vs #:with condnum #b0110) ; Overflow  ; V = 1
    (pattern vc #:with condnum #b0111) ; No overfolow, V = 0
    (pattern hi #:with condnum #b1000) ; Unsigned higher, C = 1 && z = 0
    (pattern ls #:with condnum #b1001) ; Unsigned lower or same !(C=1&&z=0)
    (pattern ge #:with condnum #b1010) ; Signed greater eq, N = V
    (pattern lt #:with condnum #b1011) ; Signed le, N! = V   
    (pattern gt #:with condnum #b1100) ; Signed gt, Z = 0 && N = V
    (pattern le #:with condnum #b1101) ; Signed le or eq, !(Z=0 & N=V
    (pattern al #:with condnum #b1110) ; Always
)
  (define-syntax-class opcode
    #:description "opcode"
    (pattern x:id
             #:do [(define sym (syntax-e (attribute x)))
                   (define ocs
                     (list 'add 'adr 'and 'b 'b.eq 'b.ne 'b.cs 'b.cc 'b.mi 'b.pl 'b.vs 'b.vc 'b.hi 'b.ls 'b.ge 'b.lt 'b.gt 'b.le 'b.al
                           'bl 'cbz 'cbnz 'csel 'cmp 'eor 'eret 'ldp 'ldr 'ldrh 'ldrb 'lsl 'lsr 'mov 'movk 'movz 'mrs 'msr 'mul 'neg 'nop
                           'orr 'ret 'scvtf 'stp 'str 'strb 'strh 'stur 'sub 'wfe))]
             #:when (ormap (λ (x) (eq? sym x)) ocs)))
  (define-syntax-class register
    #:description "register"
    (pattern x:id
      
             #:do [; her we expand the identifier so this class works with
                   ; rename-transformer allowing the user to rename registers
                   (define actual (local-expand #'x 'expression #f))
                   (define str (symbol->string (syntax-e  actual)))
                   (define isX (or (eq? (string-ref str 0) #\X)
                                   (eq? (string-ref str 0) #\x)))
                   (define isW (or (eq? (string-ref str 0) #\W)
                                   (eq? (string-ref str 0) #\w)))
                   (define isSp  (equal? str "sp" ))
                   (define isZ  (or (equal? str "xzr" ) (equal? str "wzr" )))
                   (define num
                     (if (or isSp isZ) #b11111
                         (string->number (substring str 1))))]
             #:when (or isSp (and (or isX isW) (and (<= num 31)(>= num 0))))
             #:with regnum num
             #:with is32 isW))
    ;; (define-syntax-class fpu-register
    ;; #:description "fpu-register"
    ;; (pattern x:id
      
    ;;          #:do [; here we expand the identifier so this class works with
    ;;                ; rename-transformer allowing the user to rename registers
    ;;                (define actual (local-expand #'x 'expression #f))
    ;;                (define str (symbol->string (syntax-e  actual)))
    ;;                (define isB (or (eq? (string-ref str 0) #\B)
    ;;                                (eq? (string-ref str 0) #\b)))
    ;;                (define isH (or (eq? (string-ref str 0) #\H)
    ;;                                (eq? (string-ref str 0) #\h)))
    ;;                (define isS (or (eq? (string-ref str 0) #\S)
    ;;                                (eq? (string-ref str 0) #\s)))
    ;;                (define isD (or (eq? (string-ref str 0) #\D)
    ;;                                (eq? (string-ref str 0) #\d)))
    ;;                (define num
    ;;                  (string->number (substring str 1)))

    ;;                ]
    ;;          #:when  (and (or isB isH isS isD) (and (<= num 31)(>= num 0)))
    ;;          #:with regnum num
             
    ;;          #:with is32 (or isB isH isS)))
  (define-syntax-class register-32
    (pattern x:register
             #:when (syntax-e #'x.is32)))
  (define-syntax-class register-64
    (pattern x:register
             #:when (not (syntax-e #'x.is32)))))
(define-syntax (arm-line stx)
;  (writeln stx)
  (syntax-parse stx #:datum-literals (= ! LSL )
   ; nop
   [(_ (~optional label:label) oc:opcode)
    #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'oc 'none #f '() '() #f))]
   ; b lbl:
   [ (_ (~optional label:label) oc:opcode targ:label-targ)
    #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'oc 'lbl #f '() '(0) 'targ))]   
   ; adr x0 lbl:
   [(_ (~optional label:label) oc:opcode rt:register targ:label-targ)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-lbl `rt.is32 (list `rt.regnum) '(0) 'targ))]
   ; ret x30
   [(_ (~optional label:label) oc:opcode rn:register)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg #f (list `rn.regnum) '() #f))]
;   msr daifset @2
   [(_ (~optional label:label) oc:opcode sr:system-register #:immediate n)
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'oc 'sysreg-imm4 #f (list `sr.regnum) (list n) #f))]
   ; mrs x0 sys
   [(_ (~optional label:label) oc:opcode rt:register sr:system-register)
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'oc 'reg-sysreg #f (list `rt.regnum `sr.regnum) '() #f))]
   ; msr vbar_el1 x1
   [(_ (~optional label:label) oc:opcode sr:system-register rt:register)
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'oc 'sysreg-reg #f (list `sr.regnum `rt.regnum) '() #f))]
   ; mov x0 @1 LSL @16
   [(_ (~optional label:label) oc:opcode rt:register #:immediate n LSL #:immediate shift)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-imm-shift `rt.is32 (list `rt.regnum shift) (list n) #f))]
   ; mov x0 @1
   [(_ (~optional label:label) oc:opcode rt:register #:immediate n)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-imm `rt.is32 (list `rt.regnum) (list n) #f))]
   ; mov sp x1
   [(_ (~optional label:label) oc:opcode rt:register rn:register)
    #:when (equal? (syntax-e #'rt) 'sp)
    #'(begin
      (~? (try-set-jump-source `label set-jump-source-current))
        ;rewrite this as ADD (mov sp x is an alias for add sp x 0 )
      (write-instruction 'add 'reg-reg-imm `rt.is32 (list `rt.regnum `rn.regnum) (list 0) #f))
    ]
   ; mov wzr x1
   [(_ (~optional label:label) oc:opcode rt:register rm:register)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg `rt.is32 (list `rt.regnum `rm.regnum) '() #f))]
   ; sub x1 x2 @12
   [(_ (~optional label:label) oc:opcode rt:register rn:register #:immediate n)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg-imm `rt.is32 (list `rt.regnum `rn.regnum) (list n) #f))]
   ; str x1 [sp @-16] !
   [(_ (~optional label:label) oc:opcode rt:register (rn:register #:immediate n) !)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg_reg-imm_excla `rt.is32 (list `rt.regnum `rn.regnum) (list n) #f))]
   ; stur x0 [x1 @10]
   [(_ (~optional label:label) oc:opcode rt:register (rn:register #:immediate n))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg_reg-imm_ `rt.is32 (list `rt.regnum `rn.regnum) (list n) #f))]
   ;str x1 [x2] @16
   [(_ (~optional label:label) oc:opcode rt:register (rn:register) #:immediate n)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg_reg_imm `rt.is32 (list `rt.regnum `rn.regnum) (list n) #f))]
   ;ldp x1 x2 [sp @4]
   [(_ (~optional label:label) oc:opcode rt:register rt2:register (rn:register #:immediate n))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg_reg-imm_ `rt.is32 (list `rt.regnum `rt2.regnum `rn.regnum) (list n) #f))]
   ; add x1 x2 x4
   [(_ (~optional label:label) oc:opcode rt:register rn:register rm:register)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg-reg `rt.is32 (list `rt.regnum `rn.regnum `rm.regnum) '() #f))]
   ; csel x0 x1 x2 gt
   [(_ (~optional label:label) oc:opcode rt:register rn:register rm:register cond:condition)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg-reg-cond `rt.is32 (list `rt.regnum `rn.regnum `rm.regnum `cond.condnum) '() #f))]
   ; scvtf s0 x0
   ;; [(_ (~optional label:label) oc:opcode rd:fpu-register rn:register)
   ;;  #'(begin
   ;;      (~? (try-set-jump-source `label set-jump-source-current))
   ;;      (write-instruction 'oc 'reg-reg `rd.is32 (list `rd.regnum `rd.ftype `rn.regnum) '() #f))]
   ; eret
   [(_ (~optional label:label) oc:opcode)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'none #f (list ) '() #f))]
   
   [(_ label:label)
     #'(try-set-jump-source `label set-jump-source-current)]

   [(_ label:label e:expr)
     #'(begin (try-set-jump-source `label set-jump-source-current) e) ]
   [(_ (~optional label:label) (~literal /=) t:nat )
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (align t))]

   
   [(_ v:identifier = e:expr)
     #'(define v e)]

   [(_ e:expr ... ) #'(begin e ...) ]
   [(_ e ) #'e] ))

(define-syntax (arm-block stx)
  (syntax-parse stx
    ([_ line ... ] #'(begin line ... ))))

(define-syntax (data stx)
  (syntax-parse stx
    [(_ v ... )
     #'(write-values (list v ...))]))

(define (find-closest-label key location relative)
  (define (aux input)
    (let-values ([(input f)
                  (if (equal? relative '+)
                      (values (sort input <) >=)
                      (values (sort input >) <=))])
      (match input
        [(list-rest a _) #:when (f a location) a]
        [(list-rest a tail) (aux tail)]
        [(list) (error (format "no relative label named ~a was found looking in direction ~a" key relative))])))
  
  (let ([labels (hash-ref (context-jump-table prog) key)])
    (wdb  "searching labels ~A for ~a from ~a ~a\n" labels key location relative)
    (cond
      [(and (= (length labels) 1) (equal? relative #f)) (car labels)]
      [(equal? relative #f)  
         (error (format "more than one label named ~a was found" key))]
      [else (aux labels)])
     
    ;; (if (= 1 (length labels))
    ;;     (car labels)
    ;;     (aux labels))
    ))

(define (reset-prog)
  (set-context-data! prog (make-vector 65536000 0))
  (set-context-location! prog 0)
  (set-context-minl! prog 0)
  (set-context-maxl! prog 0)
  (set-context-jump-table! prog (make-hash))
  (set-context-branches-waiting! prog (make-hash))
  )
(define-syntax (aarch64 stx)
  (syntax-parse stx
    [(_ filename:string [linkfiles:string ...] a ...)
     #:with link-list #'(list linkfiles ...)
     #'(begin
         (reset-prog)
         (define is-image? (string-suffix? filename ".img"))
         
         (set-current-obj
          ; add the current file name as an object
          (match-let*
              ([obj (create-blank-obj-file)]
               [(cons index obj) (add-or-get-string obj filename)])
            (begin
              (set-obj-file-object-composition!
               obj
               (cons index (obj-file-object-composition obj)))
              obj)))

         (displayln "begin")
         ; if this is an object file, only build if the source file is newer than the output file
         
         a ... 
         (displayln "DONE")
         (hash-for-each
          (context-branches-waiting prog)
          (λ (k dest)
            ; if this is a global label
            ; then this will attempt to be linked after the rest of the assembly,
            ; if this is producing an image file
            (if (is-global-label? k)                
             ; this is an external symbol that will be linked at the end
             (for [(current-target dest)]
               (let*
                   ([obj (add-external-target current-obj k (target-label-location current-target))])
                (set-current-obj obj)))
             ; otherwise resolve it
             (for [(current-target dest)]
                (let*
                    ; find the label
                    ([actual  
                      (find-closest-label
                       k
                       (target-label-location current-target)
                       (target-label-relative current-target))]
                     ; calculate offset in bytes
                     [amount (- actual (target-label-location current-target))]
                     [encoder (target-label-immediate-encoder current-target)])

                  (let* (
                         [current (target-label-location current-target)]
                         ; reconstruct the 32bit int from the 4 bytes
                         [hi2 (arithmetic-shift (vector-ref(context-data prog)(+ current 3 )) 24)]
                         [lo2 (arithmetic-shift (vector-ref(context-data prog)(+ current 2 )) 16)]
                         [hi1 (arithmetic-shift (vector-ref(context-data prog)(+ current 1 )) 8)]
                         [lo1 (vector-ref(context-data prog) current)]
                         [whole (bitwise-ior (bitwise-ior (bitwise-ior lo1 hi1) lo2) hi2)]
                         ; call the encoder that will place the offset in the correct place
                         ; depending on the type of opcode
                         [modified (encoder (list whole amount))])
                    
                    ; write the new result over the top of the old slots

                    (vector-set!
                     (context-data prog)
                     (target-label-location current-target)
                     (lo-byte modified))
                    (vector-set!
                     (context-data prog)
                     (+ 1 (target-label-location current-target))
                     (hi-byte modified))
                    (vector-set!
                     (context-data prog)
                     (+ 2 (target-label-location current-target))
                     (lo-byte2 modified))
                    (vector-set!
                     (context-data prog)
                     (+ 3 (target-label-location current-target))
                     (hi-byte2 modified))
                    )
                  )))))

         ; for both objects and images we begin by building the current object's data
         ; and then linking in other files.  after that, we either write the object file as-is
         ; or if we are building an image then we perform the final link relocations
         ; and write the resulting binary data to the image file.

         (begin ; write object file
           ; set the data
           (set-obj-file-data!
            current-obj              
            (vector-copy (context-data prog)(context-minl prog) (context-maxl prog)))
           ; now merge linked files together 
           (let
               ([merged
                 (for/fold ([merge (create-blank-obj-file)])
                           ([fn link-list])
                   ; only merge if this file has not already been linked
                   (let ([bad (for/first ([index (obj-file-object-composition merge)]
                                          #:when (equal? fn (hash-ref (obj-file-string-table merge) index)))
                                #t)])
                     (if bad
                         (begin
                           (displayln (format "file ~a already linked, skipping.." fn))
                           merge )
                         (begin
                           (displayln (format "linking file ~a.." fn))
                         (merge-objs merge (read-obj-file fn))))))])

             ; now we can merge with the current object.
             ; first, check that we don't have any global labels in the current object
             ; that are also in the merged object
             (displayln "resolving linked objects with image file..")
             (for ([id (map car (obj-file-symbol-table current-obj))])
               (let ([name (hash-ref (obj-file-string-table current-obj) id)])
                 (when (hash-has-key? (obj-file-reverse-string-table merged) name)
                   (error (format "could not link as the global symbol ~a already exists" name)))))
             
             (set-current-obj (merge-objs current-obj merged)))

           ;; (displayln "merged object files:")
           ;; (displayln current-obj)
           )
         ;write numbers to file!
         (if is-image?
             (begin
  
               
               ; now we must perform the actual relocations where we re-write the branch instructions.
               ; this is similar the the local branch code above, except here we do not have the
               ; correct address encoding function.  to get it we need to look at the instruction
               ; and map it back to the correct opcode/addressingmode/encoder.  There aren't many
               ; instructions that support labels as operands and many (b family) can be handled in
               ; one case.
               (let ([symbol-lookup (make-immutable-hash (obj-file-symbol-table current-obj))]
                     [write-global-address
                      (λ (offset modified)
                        (let ([msb (arithmetic-shift modified -32)])
                          (vector-set!
                           (obj-file-data current-obj)
                           offset
                           (lo-byte modified))
                          (vector-set!
                           (obj-file-data current-obj)
                           (+ 1 offset)
                           (hi-byte modified))
                          (vector-set!
                           (obj-file-data current-obj)
                           (+ 2 offset)
                           (lo-byte2 modified))
                          (vector-set!
                           (obj-file-data current-obj)
                           (+ 3 offset)
                           (hi-byte2 modified))
                          (vector-set!
                           (obj-file-data current-obj)
                           (+ 4 offset)
                           (lo-byte msb))
                          (vector-set!
                           (obj-file-data current-obj)
                           (+ 5 offset)
                           (hi-byte msb))
                          (vector-set!
                           (obj-file-data current-obj)
                           (+ 6 offset)
                           (lo-byte2 msb))
                          (vector-set!
                           (obj-file-data current-obj)
                           (+ 7 offset)
                           (hi-byte2 msb))
                          ))]
                     [rewrite
                      (λ (offset modified)
                        (begin 
                          (vector-set!
                           (obj-file-data current-obj)
                           offset
                           (lo-byte modified))
                          (vector-set!
                           (obj-file-data current-obj)
                           (+ 1 offset)
                           (hi-byte modified))
                          (vector-set!
                           (obj-file-data current-obj)
                           (+ 2 offset)
                           (lo-byte2 modified))
                          (vector-set!
                           (obj-file-data current-obj)
                           (+ 3 offset)
                           (hi-byte2 modified))

                          ))])
                 ; first we can write fully resolved label addresses where requested from the
                 ; special resolve-global-label function
                 (for* ([x (context-linker-labels prog)])
                   (match-let*([(cons label location) x]
                               [string-index (hash-ref (obj-file-reverse-string-table current-obj) (symbol->string label))]
                               [actual-location (hash-ref symbol-lookup string-index)])
                     (begin
                       (displayln (format "rewriting label ~a from ~a to ~a" label location actual-location))
                       (rewrite location actual-location)
                       )))
                 

                 (for ([pair (obj-file-target-table current-obj)])
                   (match-let*([(cons string-index offset) pair]
                               [actual-location (hash-ref symbol-lookup string-index)]
                               [amount (- actual-location offset)]
                               [top-byte (vector-ref(obj-file-data current-obj)(+ offset 3 ))]
                               [hi2 (arithmetic-shift (vector-ref(obj-file-data current-obj)(+ offset 3 )) 24)]
                               [lo2 (arithmetic-shift (vector-ref(obj-file-data current-obj)(+ offset 2 )) 16)]
                               [hi1 (arithmetic-shift (vector-ref(obj-file-data current-obj)(+ offset 1 )) 8)]
                               [lo1 (vector-ref(obj-file-data current-obj) offset)]
                               [whole (bitwise-ior (bitwise-ior (bitwise-ior lo1 hi1) lo2) hi2)]
                       
                                   
                               )
                     (cond
                       [(equal? top-byte #b0101010)
                        ; this covers all the b.cond instructions
                        (let* ([encoder (encoders-address-encoder (hash-ref opcode-encoders 'imm19))]
                               [modified (encoder (list whole amount))])
                          (rewrite offset modified))]

                       [(or (equal? top-byte #b10010100)
                            (equal? top-byte #b00010100))
                       ; B, bl imm26
                        (let* ([encoder (encoders-address-encoder (hash-ref opcode-encoders 'imm26))]
                               [modified (encoder (list whole amount))])
                          (rewrite offset modified))]
                       [(equal? top-byte #b00010000)
                       ; adr 'immlo-immhi-rd
                        (let* ([encoder (encoders-address-encoder (hash-ref opcode-encoders 'immlo-immhi-rd))]
                               [modified (encoder (list whole amount))])
                          (rewrite offset modified))]
                       [(or (equal? top-byte #b10110100)
                            (equal? top-byte #b10110101)
                            (equal? top-byte #b01011000))
                       ; cbz, cbnz
                        (let* ([encoder (encoders-address-encoder (hash-ref opcode-encoders 'imm19-rt))]
                               [modified (encoder (list whole amount))])
                          (rewrite offset modified))]

                       [else (error (format "could not relocate instruction ~A" whole))])

                     )))

               
               (let ([out (open-output-file filename #:exists 'replace #:mode 'binary)])
                 (for ([i  (obj-file-data current-obj)])
                   (write-byte i out)
                   )
                 (wdb "closing")
                 (displayln (format "wrote image file ~a" filename))
                 (close-output-port out))
               )
             (begin ; write object file
               (displayln (format "writing object file ~a" filename))
               (write-obj-file current-obj filename)
               
               )
             )
           
         
         ;; (if (or (not (file-exists? 'filename))
         ;;           (> 
         ;;            (file-or-directory-modify-seconds  (resolved-module-path-name (module-path-index-resolve (syntax-source-module #'filename))))
         ;;            (file-or-directory-modify-seconds 'filename)))
         ;;     (assemble)

         ;; (displayln (format "skipping ~a - already up to date" filename))
         ;; )
     )]))
  
(provide (all-defined-out))
(provide (for-syntax immediate system-register opcode register register-32 register-64 label label-targ))


;#xFFFF0000

