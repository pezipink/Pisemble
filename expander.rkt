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

(require "objfile.rkt" "dwarf.rkt")
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
    ['tbz-rt-bit
     (match-lambda
       [(list bin rt bit is32)
        (when (or (< bit 0) (> bit 63))
          (error (format "error: bit number ~a is out of range 0-63" bit)))
        (when (and is32 (>= bit 32))
          (error (format "error: bit number ~a is out of range for a w register" bit)))
        ; bit 5 of the bit number goes to bit 31, bits 4-0 go to 23-19
        (bitwise-ior-n bin rt (arithmetic-shift (bitwise-and bit #x1F) 19)
                       (if (>= bit 32) #x80000000 0))])
     (match-lambda
       [(list bin imm)
        ; imm14 is a word offset, so it covers -32768..32764 bytes
        (when (or (< imm -32768) (> imm 32764) (not (zero? (bitwise-and imm 3))))
          (error (format "error: branch offset ~a is out of range for tbz/tbnz" imm)))
        (bitwise-ior-n bin (arithmetic-shift (bitwise-and #x3FFF (arithmetic-shift imm -2)) 5))])]
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
        (let ([immr (arithmetic-shift (bitwise-and #b1111111 imm7) 15)])
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

    ['imm13-rn-rd
     (match-lambda
       [(list bin rd rn)
        (bitwise-ior-n (arithmetic-shift rn 5) rd bin)])
     (match-lambda
       [(list bin imm13)
        (when (>= imm13 (arithmetic-shift 1 13))
          (error (format "error: encoded logical immediate ~x is larger than 13 bits" imm13)))
        (bitwise-ior bin (arithmetic-shift imm13 10))])
     ]

    ['imm12-rn
     (match-lambda
       [(list bin rn)
        (let ([rn-shifted (arithmetic-shift rn 5)])
          (bitwise-ior-n rn-shifted bin))])
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
    ['imm16-only
     (match-lambda [(list bin) bin])
     (match-lambda
       [(list bin imm16)
        (when (>= imm16 (arithmetic-shift 1 16))
          (error (format "error: immediate value ~x is larger than 16 bits" imm16)))
        (bitwise-ior bin (arithmetic-shift imm16 5))])]
    ['barrier
     (match-lambda [(list bin) bin])
     (match-lambda
       [(list bin crm)
        (when (>= crm 16)
          (error (format "error: barrier option ~x is larger than 4 bits" crm)))
        (bitwise-ior (bitwise-and bin (bitwise-not #xF00)) (arithmetic-shift crm 8))])]
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
    ['rn-rd
      (match-lambda
        [(list bin rd rn)
         (bitwise-ior-n rd (arithmetic-shift rn 5) bin)])
      (match-lambda
        [(list bin) bin])]
    ['rm-rn-rd-imms
      (match-lambda
        [(list bin rd rn rm)
         (bitwise-ior-n rd (arithmetic-shift rn 5) (arithmetic-shift rm 16) bin)])
      (match-lambda
        [(list bin imms)
         (bitwise-ior-n bin (arithmetic-shift imms 10))])]
    ['rn-rd-immr-imms
     (match-lambda
       [(list bin rd rn)
        (bitwise-ior-n (arithmetic-shift rn 5) rd bin)])
     (match-lambda
       [(list bin immr imms)
        (bitwise-ior-n bin (arithmetic-shift immr 16) (arithmetic-shift imms 10))])
     ]
    ['rm-ra-rn-rd
     (match-lambda
       [(list bin rd rn rm ra)
        (let ([rn-shifted (arithmetic-shift rn 5)]
              [rm-shifted (arithmetic-shift rm 16)]
              [ra-shifted (arithmetic-shift ra 10)])
          (bitwise-ior-n rd rn-shifted rm-shifted ra-shifted bin))])
     (match-lambda
       [(list bin) bin])
     ]
    ['rm-option-s-rn-rd
     (match-lambda
       [(list bin rd rn rm s)
        (bitwise-ior-n (arithmetic-shift rm 16) (arithmetic-shift s 12) (arithmetic-shift rn 5) rd bin)])
     (match-lambda
       [(list bin) bin])
     ]
    ['rm-shift-rn-rd
     (match-lambda
       [(list bin rd rn rm shift-bits)
        (bitwise-ior-n (arithmetic-shift rm 16) (arithmetic-shift rn 5) rd shift-bits bin)])
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
    ['rn-rd
     (match-lambda
       [(list bin rd rn)
        (let ([rn-shifted (arithmetic-shift rn 5)])
          (bitwise-ior-n rd rn-shifted bin))])
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
    ['rd-imm8
     (match-lambda
       [(list bin rd)
        (bitwise-ior-n rd bin)])
     (match-lambda
       [(list bin imm8)
        (bitwise-ior-n (arithmetic-shift imm8 13) bin)])
     ]

    ['vec-rm-rn-rd-q-size
     (match-lambda
       [(list bin rd rn rm q size)
        (bitwise-ior-n bin rd (arithmetic-shift rn 5) (arithmetic-shift rm 16) (arithmetic-shift q 30) (arithmetic-shift size 22))])
     (match-lambda
       [(list bin) bin])
     ]
    ['vec-rn-rd-q-imm5
     (match-lambda
       [(list bin rd rn q imm5)
        (bitwise-ior-n bin rd (arithmetic-shift rn 5) (arithmetic-shift q 30) (arithmetic-shift imm5 16))])
     (match-lambda
       [(list bin) bin])
     ]
    ['vec-rd-q-imm8
     (match-lambda
       [(list bin rd q)
        (bitwise-ior-n bin rd (arithmetic-shift q 30))])
     (match-lambda
       [(list bin imm8)
        (bitwise-ior-n bin (arithmetic-shift (arithmetic-shift imm8 -5) 16) (arithmetic-shift (bitwise-and imm8 31) 5))])
     ]
    ['vec-rt-rn-q-size
     (match-lambda
       [(list bin rt rn q size)
        (bitwise-ior-n bin rt (arithmetic-shift rn 5) (arithmetic-shift q 30) (arithmetic-shift size 10))])
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


; the 8 bit immediate of FMOV (scalar, immediate) is abcdefgh, giving
; (-1)^a * 2^(E-bias) * (1 + efgh/16), where E = NOT(b):Replicate(b,5):cd (single, bias 127)
; or NOT(b):Replicate(b,8):cd (double, bias 1023)
(define (fp-imm8-value i double?)
  (let* ([a (bitwise-bit-field i 7 8)]
         [b (bitwise-bit-field i 6 7)]
         [cd (bitwise-bit-field i 4 6)]
         [efgh (bitwise-bit-field i 0 4)]
         [e (if double?
                (+ (* (- 1 b) 1024) (* b 255 4) cd)
                (+ (* (- 1 b) 128) (* b 31 4) cd))]
         [bias (if double? 1023 127)])
    (* (if (= a 1) -1 1) (expt 2 (- e bias)) (/ (+ 16 efgh) 16))))

(define (fp-imm8-encode imm double?)
  (let ([target (inexact->exact imm)])
    (or (for/first ([i (in-range 256)] #:when (= (fp-imm8-value i double?) target)) i)
        (error (format "fmov immediate ~a cannot be encoded as a ~a precision 8 bit float"
                       imm (if double? "double" "single"))))))

; movi 8 bit vector immediate: a single byte
(define (movi-byte-imm8 imm is32?)
  (unless (and (exact-integer? imm) (<= 0 imm 255))
    (error (format "movi immediate must be between 0 and 255, not ~a" imm)))
  imm)

; movi 64 bit vector immediate: each byte must be 00 or FF, and imm8 has one bit per byte
(define (movi-d-imm8 imm is32?)
  (unless (and (exact-integer? imm) (<= 0 imm (sub1 (expt 2 64))))
    (error (format "movi 2d immediate must be between 0 and #xFFFFFFFFFFFFFFFF, not ~a" imm)))
  (for/fold ([acc 0]) ([i (in-range 8)])
    (define byte (bitwise-bit-field imm (* 8 i) (* 8 (add1 i))))
    (cond
      [(= byte #xFF) (bitwise-ior acc (arithmetic-shift 1 i))]
      [(= byte 0) acc]
      [else (error (format "movi 2d immediate ~x: each byte must be 00 or FF" imm))])))

(define (fp-imm8 imm is32?) (fp-imm8-encode imm #f))
(define (fp-imm8-double imm is32?) (fp-imm8-encode imm #t))

; scalar FP syntax modes for double precision (d) registers use a "-d" suffix
(define (fp-mode mode dbl?)
  (if dbl? (string->symbol (format "~a-d" mode)) mode))

; returns the 13 bit N:immr:imms field for a bitmask immediate (and/orr/eor/ands)
; a value is encodable when it is a rotated run of ones repeated across the register
(define (encode-logical-immediate imm is32?)
  (define w (if is32? 32 64))
  (define mask (sub1 (arithmetic-shift 1 w)))
  (when (or (< imm (- (arithmetic-shift 1 (sub1 w)))) (> imm mask))
    (error (format "logical immediate ~a does not fit in ~a bits" imm w)))
  (define v (bitwise-and imm mask))
  (when (or (= v 0) (= v mask))
    (error (format "logical immediate ~x can not be encoded, all zeros and all ones are not allowed" v)))
  (define (replicates? e)
    (define elem (bitwise-and v (sub1 (arithmetic-shift 1 e))))
    (= v (for/fold ([acc 0]) ([k (in-range 0 w e)])
           (bitwise-ior acc (arithmetic-shift elem k)))))
  (define (ror x r e)
    (bitwise-and (sub1 (arithmetic-shift 1 e))
                 (bitwise-ior (arithmetic-shift x (- r))
                              (arithmetic-shift x (- e r)))))
  (define (candidate e)
    (and (replicates? e)
         (let* ([elem (bitwise-and v (sub1 (arithmetic-shift 1 e)))]
                [s (for/sum ([k (in-range e)]) (if (bitwise-bit-set? elem k) 1 0))]
                [immr (for/first ([r (in-range e)]
                                  #:when (= (ror (sub1 (arithmetic-shift 1 s)) r e) elem))
                        r)])
           (and immr (list e s immr)))))
  (define found (for/or ([e (in-list '(2 4 8 16 32 64))] #:when (<= e w)) (candidate e)))
  (unless found
    (error (format "logical immediate ~x can not be encoded in ~a bits" v w)))
  (match-let ([(list e s immr) found])
    (define n (if (= e 64) 1 0))
    (define imms (bitwise-ior (bitwise-and (bitwise-not (sub1 (* 2 e))) #x3F) (sub1 s)))
    (bitwise-ior (arithmetic-shift n 12) (arithmetic-shift immr 6) imms)))

(define (bitfield-in-range imm is32?)
  (define max-val (if is32? 31 63))
  (if (or (< imm 0) (> imm max-val))
      (error (format "bitfield immediate ~a must be between 0 and ~a" imm max-val))
      imm))

; ccmp operands: nzcv is 0-15 and the immediate form takes a 5-bit value
(define (ccmp-operand-in-range what v max-val)
  (if (and (exact-integer? v) (<= 0 v max-val))
      v
      (error (format "ccmp ~a must be between 0 and ~a, not ~a" what max-val v))))

; S bit of a register offset load/store: LSL #0 or LSL #log2(access size)
(define (register-offset-s-bit oc is32? amount)
  (define log2-size (case oc [(ldrb strb) 0] [(ldrh strh) 1] [else (if is32? 2 3)]))
  (cond
    [(not amount) 0]
    [(eqv? amount 0) 0]
    [(eqv? amount log2-size) 1]
    [else (error (format "~a register offset shift must be 0 or ~a, not ~a" oc log2-size amount))]))

; S bit of a 128 bit register offset access: LSL #0 or LSL #4
(define (q-offset-s-bit amount)
  (cond
    [(not amount) 0]
    [(eqv? amount 0) 0]
    [(eqv? amount 4) 1]
    [else (error (format "q register offset shift must be 0 or 4, not ~a" amount))]))

; shift type (bits 23:22) and amount (bits 15:10) of a shifted register operand
(define (shifted-operand-bits kind is32? amount)
  (define type (case kind [(LSL) 0] [(LSR) 1] [(ASR) 2] [(ROR) 3]))
  (define max-amount (if is32? 31 63))
  (unless (and (exact-nonnegative-integer? amount) (<= amount max-amount))
    (error (format "~a shift amount must be between 0 and ~a, not ~a" kind max-amount amount)))
  (bitwise-ior (arithmetic-shift type 22) (arithmetic-shift amount 10)))

; option (bits 15:13) and left shift amount (bits 12:10) of an extended register operand
(define (extended-operand-bits kind is32? rm-is32? amount)
  (define option (case kind [(UXTB) 0] [(UXTH) 1] [(UXTW) 2] [(UXTX) 3] [(SXTB) 4] [(SXTH) 5] [(SXTW) 6] [(SXTX) 7]))
  (define x-register? (and (not is32?) (memv option '(3 7)) #t))
  (unless (and (exact-nonnegative-integer? amount) (<= amount 4))
    (error (format "~a extend amount must be between 0 and 4, not ~a" kind amount)))
  (unless (eq? rm-is32? (not x-register?))
    (error (format "~a requires a ~a register for Rm" kind (if x-register? "x" "w"))))
  (bitwise-ior (arithmetic-shift option 13) (arithmetic-shift amount 10)))

; ldrsw only loads into x registers; ldrsb and ldrsh have a separate opcode for w destinations
(define (sign-extending-load-mode oc is32?)
  (cond
    [(and (memq oc '(ldrsw ldursw)) is32?) (error (format "~a destination must be an x register" oc))]
    [(and is32? (not (memq oc '(ldrsw ldursw)))) 'reg_reg-imm_w]
    [else 'reg_reg-imm_]))

(define opcode-metadata
  (let ([b31             #b01111111111111111111111111111111]
        [b30             #b10111111111111111111111111111111]
        [sxt             #b01111111101111111111111111111111] ; N and 31 for SXTB SXTH
        [lsl-imm-special #b01111111100111110111111111111111];here we splat 'N', sf, and the upper bits of immr and imms
        [lsl-in-range
         (λ (imm is32?)
           (cond
            [(and is32? (or (< imm 1) (> imm 31))) (error (format "immediate arguments to 32bit lsl must be between 1 and 31, not ~a" imm))]
            [(and (not is32?) (or (< imm 1) (> imm 63))) (error (format "immediate arguments to 64bit lsl must be between 1 and 63, not ~a" imm))]
            [else imm]))]
            
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
        [shift-3 (λ (imm is32?)
                   (if (eq? 0 (bitwise-and #b111 imm))
                       (arithmetic-shift imm -3)
                       (error "not divisible by 8")))]
        [shift-4 (λ (imm is32?)
                   (if (eq? 0 (bitwise-and #b1111 imm))
                       (arithmetic-shift imm -4)
                       (error "not divisible by 16")))]
        [shift-2 (λ (imm is32?)
                   (if (eq? 0 (bitwise-and #b11 imm))
                       (arithmetic-shift imm -2)
                       (error "not divisible by 4")))]
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
    ['and 'reg-reg-reg   'rm-rn-rd                           #b10001010000000000000000000000000 b31 #f ]
    ['and 'reg-reg-imm   'imm13-rn-rd                        #x92000000 b31 encode-logical-immediate]
    ['ands 'reg-reg-imm  'imm13-rn-rd                        #xF2000000 b31 encode-logical-immediate]
    ['ands 'reg-reg-reg  'rm-rn-rd                           #xEA000000 b31 #f]
    ['orr 'reg-reg-imm   'imm13-rn-rd                        #xB2000000 b31 encode-logical-immediate]
    ['eor 'reg-reg-imm   'imm13-rn-rd                        #xD2000000 b31 encode-logical-immediate]
    ['asr 'reg-reg-imm   'immr6-rn-rd                        #b10010011010000001111110000000000 lsr-imm-special #f]
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
    ['tbz  'reg-bit-lbl  'tbz-rt-bit                         #x36000000 #f #f]
    ['tbnz 'reg-bit-lbl  'tbz-rt-bit                         #x37000000 #f #f]
    ['cmp  'reg-reg      'rm-rn                              #b11101011000000000000000000011111 b31 #f]
    ['cmp  'reg-imm      'imm12-rn                           #b11110001000000000000000000011111 b31 #f]
    ['csel 'reg-reg-reg-cond 'rm-cond-rn-rd                  #b10011010100000000000000000000000 b31 #f]
    ['eor 'reg-reg-reg   'rm-rn-rd                           #b11001010000000000000000000000000 b31 #f]
    ['eret 'none         'none                               #b11010110100111110000001111100000 #f #f ]

    ['ldp 'reg-reg_reg-imm_ 'imm7-rd2-rn-rd                  #b10101001010000000000000000000000 b31 shift-2-or-3]
     ['ldp 'reg-reg_reg_imm                                   'imm7-rd2-rn-rd #xA8C00000 b31 shift-2-or-3]
     ['ldp 'reg-reg_reg-imm_excla                             'imm7-rd2-rn-rd #xA9C00000 b31 shift-2-or-3]
     ['stp 'reg-reg_reg_imm                                   'imm7-rd2-rn-rd #xA8800000 b31 shift-2-or-3]
     ['stp 'reg-reg_reg-imm_excla                             'imm7-rd2-rn-rd #xA9800000 b31 shift-2-or-3]
     ['ldr  'reg_reg-reg-shift  'rm-option-s-rn-rd #xF8606800 b30 #f]
     ['str  'reg_reg-reg-shift  'rm-option-s-rn-rd #xF8206800 b30 #f]
     ['ldrb 'reg_reg-reg-shift  'rm-option-s-rn-rd #x38606800 #f #f]
     ['strb 'reg_reg-reg-shift  'rm-option-s-rn-rd #x38206800 #f #f]
     ['ldrh 'reg_reg-reg-shift  'rm-option-s-rn-rd #x78606800 #f #f]
     ['strh 'reg_reg-reg-shift  'rm-option-s-rn-rd #x78206800 #f #f]
     ['add  'reg-reg-reg-shift  'rm-shift-rn-rd #x8B000000 b31 #f]
     ['adds 'reg-reg-reg-shift  'rm-shift-rn-rd #xAB000000 b31 #f]
     ['sub  'reg-reg-reg-shift  'rm-shift-rn-rd #xCB000000 b31 #f]
     ['subs 'reg-reg-reg-shift  'rm-shift-rn-rd #xEB000000 b31 #f]
     ['add  'reg-reg-reg-ext    'rm-shift-rn-rd #x8B200000 b31 #f]
     ['adds 'reg-reg-reg-ext    'rm-shift-rn-rd #xAB200000 b31 #f]
     ['sub  'reg-reg-reg-ext    'rm-shift-rn-rd #xCB200000 b31 #f]
     ['subs 'reg-reg-reg-ext    'rm-shift-rn-rd #xEB200000 b31 #f]
     ['and  'reg-reg-reg-shift  'rm-shift-rn-rd #x8A000000 b31 #f]
     ['ands 'reg-reg-reg-shift  'rm-shift-rn-rd #xEA000000 b31 #f]
     ['orr  'reg-reg-reg-shift  'rm-shift-rn-rd #xAA000000 b31 #f]
     ['eor  'reg-reg-reg-shift  'rm-shift-rn-rd #xCA000000 b31 #f]
     ['bic  'reg-reg-reg-shift  'rm-shift-rn-rd #x8A200000 b31 #f]
     ['bics 'reg-reg-reg-shift  'rm-shift-rn-rd #xEA200000 b31 #f]
     ['orn  'reg-reg-reg-shift  'rm-shift-rn-rd #xAA200000 b31 #f]
     ['bic  'reg-reg-reg        'rm-rn-rd                     #x8A200000 b31 #f]
     ['bics 'reg-reg-reg        'rm-rn-rd                     #xEA200000 b31 #f]
     ['orn  'reg-reg-reg        'rm-rn-rd                     #xAA200000 b31 #f]
     ['adc  'reg-reg-reg        'rm-rn-rd                     #x9A000000 b31 #f]
     ['sbc  'reg-reg-reg        'rm-rn-rd                     #xDA000000 b31 #f]
     ['ldrsw 'reg_reg-imm_   'imm12-rn-rd #xB9800000 #f shift-2]
     ['ldrsb 'reg_reg-imm_   'imm12-rn-rd #x39800000 #f #f]
     ['ldrsb 'reg_reg-imm_w  'imm12-rn-rd #x39C00000 #f #f]
     ['ldrsh 'reg_reg-imm_   'imm12-rn-rd #x79800000 #f always-shift-1]
     ['ldrsh 'reg_reg-imm_w  'imm12-rn-rd #x79C00000 #f always-shift-1]
     ['prfm 'reg_reg-imm_    'imm12-rn-rd #xF9800000 #f shift-3]
     ['ccmp 'ccmp-reg        'rm-cond-rn-rd #xFA400000 b31 #f]
     ['ccmp 'ccmp-imm        'rm-cond-rn-rd #xFA400800 b31 #f]
    ['ldr 'reg_reg_imm  'imm9-rn-rd                          #b11111000010000000000010000000000 b30 #f]
    ['ldr 'reg_reg-imm_excla  'imm9-rn-rd                    #b11111000010000000000110000000000 b30 #f]

    ['ldr 'reg-lbl       'imm19-rt                           #b01011000000000000000000000000000 b30 #f]
    ['ldr  'reg_reg-imm_ 'imm12-rn-rd                        #b11111001010000000000000000000000 b30 shift-2-or-3]
    ['ldrh 'reg_reg_imm  'imm9-rn-rd                         #b01111000010000000000010000000000 #f #f]
    ['ldrh 'reg_reg-imm_excla  'imm9-rn-rd                   #b01111000010000000000110000000000 #f #f]
    
    ['ldrh 'reg_reg-imm_ 'imm12-rn-rd                        #b01111001010000000000000000000000 b31 always-shift-1]

    ['ldrb 'reg_reg_imm  'imm9-rn-rd                         #b00111000010000000000010000000000 b30 #f]
    ['ldrb 'reg_reg-imm_excla  'imm9-rn-rd                   #b00111000010000000000110000000000 #f #f]
    ['ldrb 'reg_reg-imm_ 'imm12-rn-rd                        #b00111001010000000000000000000000 #f  #f]
    ['lsl 'reg-reg-imm   'immr-imms-rn-rd                    #b11010011010000000000000000000000 lsl-imm-special lsl-in-range]
    ['lsr 'reg-reg-imm   'immr6-rn-rd                        #b11010011010000001111110000000000 lsr-imm-special #f]
    ['lsl 'reg-reg-reg   'rm-rn-rd                           #x9AC02000 b31 #f]
    ['lsr 'reg-reg-reg   'rm-rn-rd                           #x9AC02400 b31 #f]
    ['asr 'reg-reg-reg   'rm-rn-rd                           #x9AC02800 b31 #f]
    ['ror 'reg-reg-reg   'rm-rn-rd                           #x9AC02C00 b31 #f]
    ['madd 'reg-reg-reg-reg 'rm-ra-rn-rd                     #x9B000000 b31 #f]
    ['msub 'reg-reg-reg-reg 'rm-ra-rn-rd                     #x9B008000 b31 #f]
    ; 64-bit only: the 32-bit forms are rejected by the arm-line clauses
    ['smulh 'reg-reg-reg 'rm-rn-rd                           #x9B407C00 #f #f]
    ['umulh 'reg-reg-reg 'rm-rn-rd                           #x9BC07C00 #f #f]
     ['smull 'reg-reg-reg 'rm-rn-rd                           #x9B207C00 #f #f]
     ['umull 'reg-reg-reg 'rm-rn-rd                           #x9BA07C00 #f #f]
     ['ldxr  'reg-addr        'rn-rd                          #xC85F7C00 #xBFFFFFFF #f]
     ['ldaxr 'reg-addr        'rn-rd                          #xC85FFC00 #xBFFFFFFF #f]
     ['ldar  'reg-addr        'rn-rd                          #xC8DFFC00 #xBFFFFFFF #f]
     ['stlr  'reg-addr        'rn-rd                          #xC89FFC00 #xBFFFFFFF #f]
     ['stxr  'reg-addr-status 'rm-rn-rd                       #xC8007C00 #xBFFFFFFF #f]
     ['stlxr 'reg-addr-status 'rm-rn-rd                       #xC800FC00 #xBFFFFFFF #f]
    ['ubfm 'reg-reg-imm-imm 'rn-rd-immr-imms                 #xD3400000 #x7FBFFFFF bitfield-in-range]
    ['sbfm 'reg-reg-imm-imm 'rn-rd-immr-imms                 #x93400000 #x7FBFFFFF bitfield-in-range]
     ['bfm 'reg-reg-imm-imm 'rn-rd-immr-imms                 #xB3400000 #x7FBFFFFF bitfield-in-range]
     ['extr 'reg-reg-reg-imm 'rm-rn-rd-imms                  #x93C00000 #x7FBFFFFF bitfield-in-range]

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
    
    ;unsigned offset mode, 32 bit only. always shift by 1
    ['strh 'reg_reg-imm_ 'imm12-rn-rd                        #b01111001000000000000000000000000 b31 always-shift-1]

    ['strb 'reg_reg-imm_excla 'imm9-rn-rd                    #b00111000000000000000110000000000 #f #f]
    ['strb 'reg_reg_imm   'imm9-rn-rd                        #b00111000000000000000010000000000 #f #f]
    ;unsigned offset mode, 32 bit only]
    ['strb 'reg_reg-imm_ 'imm12-rn-rd                        #b00111001000000000000000000000000 b31 #f]
    ['stur 'reg_reg-imm_ 'imm9-rn-rd                         #b11111000000000000000000000000000 b30 is-byte]

    ['sub 'reg-reg-imm   'imm12-rn-rd                        #b11010001000000000000000000000000 b31 #f]
    ['sub 'reg-reg-reg   'rm-rn-rd                           #b11001011000000000000000000000000 b31 #f]
    ['sxtb 'reg-reg      'rn-rd                              #b10010011010000000001110000000000 sxt #f]
    ['sxth 'reg-reg      'rn-rd                              #b10010011010000000011110000000000 sxt #f]
    ['sxtw 'reg-reg      'rn-rd                              #b10010011010000000111110000000000 #f  #f]
    

    ; single precision FPU and FP load/store
    ['fabs   'fp-fp          'rn-rd           #x1E20C000 #f #f]
    ['fadd   'fp-fp-fp       'rm-rn-rd        #x1E202800 #f #f]
    ['fcmp   'fp-fp          'rm-rn           #x1E202000 #f #f]
    ['fcmp   'fp-zero        'rn              #x1E202008 #f #f]
    ['fcvtzs 'gp-fp          'rn-rd           #x9E380000 b31 #f]
    ['fcvtzu 'gp-fp          'rn-rd           #x9E390000 b31 #f]
    ['fdiv   'fp-fp-fp       'rm-rn-rd        #x1E201800 #f #f]
    ['fmov   'fp-gp          'rn-rd           #x1E270000 #f #f]
    ['fmov   'gp-fp          'rn-rd           #x1E260000 #f #f]
    ['fmov   'fp-fp          'rn-rd           #x1E204000 #f #f]
    ['fmov   'fp-imm         'rd-imm8         #x1E201000 #f fp-imm8]
    ['fmul   'fp-fp-fp       'rm-rn-rd        #x1E200800 #f #f]
    ['fneg   'fp-fp          'rn-rd           #x1E214000 #f #f]
    ['fsqrt  'fp-fp          'rn-rd           #x1E21C000 #f #f]
    ['fsub   'fp-fp-fp       'rm-rn-rd        #x1E203800 #f #f]
    ['fmax   'fp-fp-fp       'rm-rn-rd        #x1E204800 #f #f]
    ['fmin   'fp-fp-fp       'rm-rn-rd        #x1E205800 #f #f]
    ['fcsel  'fp-fp-fp-cond  'rm-cond-rn-rd   #x1E200C00 #f #f]
    ['fmadd  'fp-fp-fp-fp    'rm-ra-rn-rd     #x1F000000 #f #f]
    ['fmsub  'fp-fp-fp-fp    'rm-ra-rn-rd     #x1F008000 #f #f]
    ['fnmadd 'fp-fp-fp-fp    'rm-ra-rn-rd     #x1F200000 #f #f]
    ['fnmsub 'fp-fp-fp-fp    'rm-ra-rn-rd     #x1F208000 #f #f]
    ['ldr    'fp-reg_reg-imm_ 'imm12-rn-rd    #xBD400000 #f shift-2]
    ['scvtf  'fp-gp          'rn-rd           #x9E220000 b31 #f]
    ['ucvtf  'fp-gp          'rn-rd           #x9E230000 b31 #f]
    ['str    'fp-reg_reg-imm_ 'imm12-rn-rd    #xBD000000 #f shift-2]
    ['ldr    'fp-reg_reg-reg-shift 'rm-option-s-rn-rd #xBC606800 #f #f]
    ['str    'fp-reg_reg-reg-shift 'rm-option-s-rn-rd #xBC206800 #f #f]
    ; double precision (d) variants of the scalar FP rows
    ['fabs   'fp-fp-d        'rn-rd           #x1E60C000 #f #f]
    ['fadd   'fp-fp-fp-d     'rm-rn-rd        #x1E602800 #f #f]
    ['fcmp   'fp-fp-d        'rm-rn           #x1E602000 #f #f]
    ['fcmp   'fp-zero-d      'rn              #x1E602008 #f #f]
    ['fcvtzs 'gp-fp-d        'rn-rd           #x9E780000 b31 #f]
    ['fcvtzu 'gp-fp-d        'rn-rd           #x9E790000 b31 #f]
    ['fdiv   'fp-fp-fp-d     'rm-rn-rd        #x1E601800 #f #f]
    ['fmov   'fp-gp-d        'rn-rd           #x9E670000 b31 #f]
    ['fmov   'gp-fp-d        'rn-rd           #x9E660000 b31 #f]
    ['fmov   'fp-fp-d        'rn-rd           #x1E604000 #f #f]
    ['fmov   'fp-imm-d       'rd-imm8         #x1E601000 #f fp-imm8-double]
    ['fmul   'fp-fp-fp-d     'rm-rn-rd        #x1E600800 #f #f]
    ['fneg   'fp-fp-d        'rn-rd           #x1E614000 #f #f]
    ['fsqrt  'fp-fp-d        'rn-rd           #x1E61C000 #f #f]
    ['fsub   'fp-fp-fp-d     'rm-rn-rd        #x1E603800 #f #f]
    ['fmax   'fp-fp-fp-d     'rm-rn-rd        #x1E604800 #f #f]
    ['fmin   'fp-fp-fp-d     'rm-rn-rd        #x1E605800 #f #f]
    ['fcsel  'fp-fp-fp-cond-d 'rm-cond-rn-rd  #x1E600C00 #f #f]
    ['fmadd  'fp-fp-fp-fp-d  'rm-ra-rn-rd     #x1F400000 #f #f]
    ['fmsub  'fp-fp-fp-fp-d  'rm-ra-rn-rd     #x1F408000 #f #f]
    ['fnmadd 'fp-fp-fp-fp-d  'rm-ra-rn-rd     #x1F600000 #f #f]
    ['fnmsub 'fp-fp-fp-fp-d  'rm-ra-rn-rd     #x1F608000 #f #f]
    ['scvtf  'fp-gp-d        'rn-rd           #x9E620000 b31 #f]
    ['ucvtf  'fp-gp-d        'rn-rd           #x9E630000 b31 #f]
    ['ldr    'fp-reg_reg-imm_-d 'imm12-rn-rd  #xFD400000 #f shift-3]
    ['str    'fp-reg_reg-imm_-d 'imm12-rn-rd  #xFD000000 #f shift-3]
    ['ldr    'fp-reg_reg-reg-shift-d 'rm-option-s-rn-rd #xFC606800 #f #f]
    ['str    'fp-reg_reg-reg-shift-d 'rm-option-s-rn-rd #xFC206800 #f #f]

    ['ldr    'q-reg_reg-imm_        'imm12-rn-rd              #x3DC00000 #f shift-4]
    ['str    'q-reg_reg-imm_        'imm12-rn-rd              #x3D800000 #f shift-4]
    ['ldr    'q-reg_reg-reg-shift   'rm-option-s-rn-rd        #x3CE06800 #f #f]
    ['str    'q-reg_reg-reg-shift   'rm-option-s-rn-rd        #x3CA06800 #f #f]
    ['add    'vec-reg-reg-reg       'vec-rm-rn-rd-q-size      #x0E208400 #f #f]
    ['sub    'vec-reg-reg-reg       'vec-rm-rn-rd-q-size      #x2E208400 #f #f]
    ['fadd   'vec-reg-reg-reg-fp    'vec-rm-rn-rd-q-size      #x0E20D400 #f #f]
    ['fmul   'vec-reg-reg-reg-fp    'vec-rm-rn-rd-q-size      #x2E20DC00 #f #f]
    ['mov    'vec-reg-reg           'vec-rm-rn-rd-q-size      #x0EA01C00 #f #f]
    ['dup    'vec-reg-gp            'vec-rn-rd-q-imm5         #x0E000C00 #f #f]
    ['movi   'vec-reg-imm8          'vec-rd-q-imm8            #x0F00E400 #f movi-byte-imm8]
    ['movi   'vec-reg-imm8-d        'vec-rd-q-imm8            #x2F00E400 #f movi-d-imm8]
    ['ld1    'vec-reg-addr          'vec-rt-rn-q-size         #x0C407000 #f #f]
    ['st1    'vec-reg-addr          'vec-rt-rn-q-size         #x0C007000 #f #f]

    ['udiv 'reg-reg-reg  'rm-rn-rd                           #b10011010110000000000100000000000 b31 #f]

    ['adds 'reg-reg-imm  'imm12-rn-rd                        #xB1000000 b31 #f]
    ['adds 'reg-reg-reg  'rm-rn-rd                           #xAB000000 b31 #f]
    ['subs 'reg-reg-imm  'imm12-rn-rd                        #xF1000000 b31 #f]
    ['subs 'reg-reg-reg  'rm-rn-rd                           #xEB000000 b31 #f]
    ['cmn  'reg-reg      'rm-rn                              #xAB00001F b31 #f]
    ['cmn  'reg-imm      'imm12-rn                           #xB100001F b31 #f]
    ['tst  'reg-reg      'rm-rn                              #xEA00001F b31 #f]
    ['sdiv 'reg-reg-reg  'rm-rn-rd                           #x9AC00C00 b31 #f]
    ['movn 'reg-imm      'imm16-rd                           #x92800000 b31 #f]
    ['movn 'reg-imm-shift 'hw-imm16-rd                       #x92800000 b31 #f]
    ['csinc 'reg-reg-reg-cond 'rm-cond-rn-rd                 #x9A800400 b31 #f]
     ['csinv 'reg-reg-reg-cond 'rm-cond-rn-rd                 #xDA800000 b31 #f]
     ['csneg 'reg-reg-reg-cond 'rm-cond-rn-rd                 #xDA800400 b31 #f]
    ['br   'reg          'rn                                 #xD61F0000 #f #f]
    ['blr  'reg          'rn                                 #xD63F0000 #f #f]
    ['ret  'none         'none                               #xD65F03C0 #f #f]
    ['clz  'reg-reg      'rn-rd                              #xDAC01000 b31 #f]
    ['rbit 'reg-reg      'rn-rd                              #xDAC00000 b31 #f]
    ['rev16 'reg-reg     'rn-rd                              #xDAC00400 b31 #f]
    ['rev32 'reg-reg     'rn-rd                              #xDAC00800 #f #f]
    ['ldur  'reg_reg-imm_ 'imm9-rn-rd                        #xF8400000 b30 #f]
    ['ldurb 'reg_reg-imm_ 'imm9-rn-rd                        #x38400000 #f #f]
    ['ldurh 'reg_reg-imm_ 'imm9-rn-rd                        #x78400000 #f #f]
    ['ldursb 'reg_reg-imm_  'imm9-rn-rd                      #x38800000 #f #f]
    ['ldursb 'reg_reg-imm_w 'imm9-rn-rd                      #x38C00000 #f #f]
    ['ldursh 'reg_reg-imm_  'imm9-rn-rd                      #x78800000 #f #f]
    ['ldursh 'reg_reg-imm_w 'imm9-rn-rd                      #x78C00000 #f #f]
    ['ldursw 'reg_reg-imm_  'imm9-rn-rd                      #xB8800000 #f #f]
    ['svc  'imm16-only   'imm16-only                         #xD4000001 #f #f]
    ['hvc  'imm16-only   'imm16-only                         #xD4000002 #f #f]
    ['smc  'imm16-only   'imm16-only                         #xD4000003 #f #f]
    ['brk  'imm16-only   'imm16-only                         #xD4200000 #f #f]
    ['hlt  'imm16-only   'imm16-only                         #xD4400000 #f #f]
    ['dmb  'barrier      'barrier                            #xD50330BF #f #f]
    ['dsb  'barrier      'barrier                            #xD503309F #f #f]
    ['isb  'barrier      'barrier                            #xD50330DF #f #f]
    ['wfi 'none          'none                               #xD503207F #f #f]
    ['sev 'none          'none                               #xD503209F #f #f]
    ['wfe 'none          'none                               #b11010101000000110010000001011111 #f #f]))))

(struct context (data location minl maxl jump-table branches-waiting linker-labels) #:mutable #:transparent)
(struct target-label (immediate-encoder relative location) #:transparent)
(define prog (context (make-vector (* 1024 1024) 0) 0 0 0 (make-hash) (make-hash) (list)))

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
  (when (>= (context-location prog) (vector-length (context-data prog)))
    (displayln "program exceeded memory, extending by 1mb ...")
    (let ([new-vector (make-vector (* 1024 1024))])
      (set-context-data! prog (vector-append (context-data prog) new-vector))))

  (vector-set! (context-data prog) (context-location prog) v))

(define (set-linker-resolved-label global-label location)
;  (displayln (format "label ~a loc ~a" global-label location))
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

(define (write-ascii str)
  (for ([c str])
    (write-value (lo-byte (char->integer c)))))

(define (write-ascii-nullterm str)
  (for ([c str])
    (write-value (lo-byte (char->integer c))))
  (write-value 0))

(define (reserve byte-count)
  (for ([x (in-range byte-count)])
    (write-value (lo-byte 0))))

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
           [(list bin imm) (address-encoder (list bin (imm-mod imm is-32bit?)))]
           [(list bin imm1 imm2) (address-encoder (list bin (imm-mod imm1 is-32bit?) (imm-mod imm2 is-32bit?)))])]
        [with-data (data-encoder (cons raw data-args))]
        [with-addr (final-address-encoder (cons with-data immediate-address-args))]

        [final
         (if (and 32bit-mask is-32bit?) (bitwise-and with-addr 32bit-mask) with-addr)])
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
    (pattern daifclr #:with regnum #x1a07)
    ; generic form s<op0>_<op1>_c<crn>_c<crm>_<op2>, e.g. s3_0_c15_c2_0
    (pattern x:id
             #:do [(define m (regexp-match #rx"^s([0-9]+)_([0-9]+)_c([0-9]+)_c([0-9]+)_([0-9]+)$"
                                           (symbol->string (syntax-e #'x))))
                   (define packed
                     (and m
                          (let-values ([(op0 op1 crn crm op2) (apply values (map string->number (cdr m)))])
                            (and (<= 2 op0 3) (<= op1 7) (<= crn 15) (<= crm 15) (<= op2 7)
                                 (+ (arithmetic-shift (- op0 2) 14)
                                    (arithmetic-shift op1 11)
                                    (arithmetic-shift crn 7)
                                    (arithmetic-shift crm 3)
                                    op2)))))]
             #:when packed
             #:with regnum packed))

  (define-syntax-class condition
    #:description "condition"
    #:datum-literals (eq ne cs cc mi pl vs vc hi ls ge lt gt le al)
    (pattern eq
             #:with branch-opcode #'b.eq
             #:with branch-inverse-opcode #'b.ne
             #:with condnum #b0000) ; equal Z = 1
    (pattern ne
             #:with branch-opcode #'b.ne
             #:with branch-inverse-opcode #'b.eq
             #:with condnum #b0001) ; not equal, Z = 0
    (pattern cs
             #:with branch-opcode #'b.cs
             #:with branch-inverse-opcode #'b.cc
             #:with condnum #b0010) ; Carry set, C = 1
    (pattern cc
             #:with branch-opcode #'b.cc
             #:with branch-inverse-opcode #'b.cs
             #:with condnum #b0011) ; Carry clear, C = 0    
    (pattern mi
             #:with branch-opcode #'b.mi
             #:with branch-inverse-opcode #'b.pl
             #:with condnum #b0100) ; Minus, neg ; N = 1
    (pattern pl
             #:with branch-opcode #'b.pl
             #:with branch-inverse-opcode #'b.mi
             #:with condnum #b0101) ; Positive or zero ; N = 0
    (pattern vs
             #:with branch-opcode #'b.vs
             #:with branch-inverse-opcode #'b.vc
             #:with condnum #b0110) ; Overflow  ; V = 1
    (pattern vc
             #:with branch-opcode #'b.vc
             #:with branch-inverse-opcode #'b.vs
             #:with condnum #b0111) ; No overfolow, V = 0
    (pattern hi
             #:with branch-opcode #'b.hi
             #:with branch-inverse-opcode #'b.ls
             #:with condnum #b1000) ; Unsigned higher, C = 1 && z = 0
    (pattern ls
             #:with branch-opcode #'b.ls
             #:with branch-inverse-opcode #'b.hi
             #:with condnum #b1001) ; Unsigned lower or same !(C=1&&z=0)
    (pattern ge
             #:with branch-opcode #'b.ge
             #:with branch-inverse-opcode #'b.lt
             #:with condnum #b1010) ; Signed greater eq, N = V
    (pattern lt
             #:with branch-opcode #'b.lt
             #:with branch-inverse-opcode #'b.ge
             #:with condnum #b1011
             ) ; Signed le, N! = V   
    (pattern gt
             #:with branch-opcode #'b.gt
             #:with branch-inverse-opcode #'b.le
             #:with condnum #b1100) ; Signed gt, Z = 0 && N = V
    (pattern le
             #:with branch-opcode #'b.le
             #:with branch-inverse-opcode #'b.gt
             #:with condnum #b1101) ; Signed le or eq, !(Z=0 & N=V
    (pattern al
             #:with branch-opcode #'b.al
             #:with branch-inverse-opcode #'b.al
             #:with condnum #b1110) ; Always
)
  (define-syntax-class opcode
    #:description "opcode"
    (pattern x:id
             #:do [(define sym (syntax-e (attribute x)))
                   (define ocs
                     (list 'add 'adr 'and 'asr 'b 'b.eq 'b.ne 'b.cs 'b.cc 'b.mi 'b.pl 'b.vs 'b.vc 'b.hi 'b.ls 'b.ge 'b.lt 'b.gt 'b.le 'b.al
                           'bl 'cbz 'cbnz 'tbz 'tbnz 'csel 'cmp 'eor 'eret 'ldp 'ldr 'ldrh 'ldrb 'lsl 'lsr 'mov 'movk 'movz 'mrs 'msr 'mul 'neg 'nop
                           'orr 'ret 'scvtf 'stp 'str 'strb 'strh 'stur 'sub 'sxtb 'sxth 'sxtw 'udiv 'wfe
                           'fabs 'fadd 'fcmp 'fcvtzs 'fcvtzu 'fdiv 'fmov 'fmul 'fneg 'fsqrt 'fsub 'fmax 'fmin 'fmadd 'fmsub 'fnmadd 'fnmsub 'ucvtf 'fcsel
                           'adds 'subs 'cmn 'tst 'ands 'sdiv 'movn 'csinc 'cset 'br 'blr 'clz 'rbit 'rev16
                           'ldur 'ldurb 'ldurh 'svc 'hvc 'smc 'brk 'hlt 'dmb 'dsb 'isb 'wfi 'sev
                           'madd 'msub 'ror 'ubfm 'sbfm 'ldrsw 'ldrsb 'ldrsh 'csinv 'csneg 'bic 'bics 'orn 'adc 'sbc 'dup 'movi 'ld1 'st1))]
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
                   (define isWsp (equal? str "wsp"))
                   (define isZ  (or (equal? str "xzr" ) (equal? str "wzr" )))
                   (define isFp (equal? str "fp"))
                   (define isLr (equal? str "lr"))
                   (define num
                     (cond [(or isSp isWsp isZ) #b11111]
                           [isFp 29]
                           [isLr 30]
                           [else (string->number (substring str 1))]))]
             #:when (or isSp isWsp isFp isLr (and (or isX isW) (and (<= num 31)(>= num 0))))
             #:with regnum num
             #:with is32 isW))
  (define-syntax-class fpu-register
    #:description "single or double precision fpu register"
    (pattern x:id
             #:do [; here we expand the identifier so this class works with
                   ; rename-transformer allowing the user to rename registers
                   (define actual (local-expand #'x 'expression #f))
                   (define str (symbol->string (syntax-e  actual)))
                   (define isS (or (eq? (string-ref str 0) #\S)
                                   (eq? (string-ref str 0) #\s)))
                   (define isD (or (eq? (string-ref str 0) #\D)
                                   (eq? (string-ref str 0) #\d)))
                   (define num
                     (and (or isS isD) (string->number (substring str 1))))]
             #:when (and (exact-nonnegative-integer? num) (<= num 31))
             #:with regnum num
             #:with dbl isD))
  ; vector register with arrangement, e.g. v0.4s. The name is read directly rather
  ; than local-expanded, because v0.4s is not a bound identifier.
  (define-syntax-class vec-register
    #:description "vector register with arrangement (e.g. v0.4s)"
    (pattern x:id
             #:do [(define m (regexp-match #px"^v([0-9]+)[.](8b|16b|4h|8h|2s|4s|2d)$"
                                           (string-downcase (symbol->string (syntax-e #'x)))))]
             #:when (and m (<= (string->number (cadr m)) 31))
             #:with regnum (string->number (cadr m))
             #:with arr (string->symbol (caddr m))
             #:with esize (case (string-ref (caddr m) (sub1 (string-length (caddr m))))
                            [(#\b) 0] [(#\h) 1] [(#\s) 2] [else 3])
             #:with q (if (member (caddr m) '("16b" "8h" "4s" "2d")) 1 0)))
  (define-syntax-class q-register
    #:description "128 bit simd register (e.g. q0)"
    (pattern x:id
             #:do [(define m (regexp-match #px"^q([0-9]+)$"
                                           (string-downcase (symbol->string (syntax-e #'x)))))]
             #:when (and m (<= (string->number (cadr m)) 31))
             #:with regnum (string->number (cadr m))))
  (define-syntax-class register-32
    (pattern x:register
             #:when (syntax-e #'x.is32)))
  (define-syntax-class register-64
    (pattern x:register
             #:when (not (syntax-e #'x.is32)))))
(begin-for-syntax
  (define assembler-mnemonics
    '(adc adcs add adds adr and ands asr b b.eq b.ne b.cs b.cc b.mi b.pl b.vs b.vc
      b.hi b.ls b.ge b.lt b.gt b.le b.al bfi bic bics bl blr br brk cbz cbnz ccmp
      cinc clz cmn cmp cneg csel csinc csinv csneg cset dmb dsb eor eret extr hlt
      hvc isb ldar ldaxr ldp ldr ldrh ldrb ldrsb ldrsh ldrsw ldur ldurb ldurh ldursb ldursh ldursw ldxr
      lsl lsr madd mneg mov movk movn movz mrs msr msub mul mvn neg nop orn orr prfm
      rbit rev16 rev32 ret ror sbc sbfm sbfx scvtf sdiv sev smc smull smulh stlr stlxr
      stp str strb strh stur stxr sub subs svc sxtb sxth sxtw tbnz tbz tst ubfm ubfx udiv
      umull umulh uxtb uxth wfe wfi fabs fadd fcmp fcvtzs fdiv fmov fmul fneg
      fsqrt fsub fcvtzu fmax fmin fmadd fmsub fnmadd fnmsub ucvtf fcsel dup movi ld1 st1)))

;; source position of each assembled line: (offset file line col), newest first
(define source-records '())
;; function entries from subr: (offset . name), newest first
(define func-records '())
(define (reset-source-records!)
  (set! source-records '())
  (set! func-records '()))
(define (note-source! file line col offset)
  (set! source-records (cons (list offset file line col) source-records)))
(define (note-func! label)
  (define name (symbol->string label))
  (set! func-records (cons (cons (context-location prog) (substring name 1)) func-records)))

;; records of each object written in this run, keyed by file name. aarch64 resets
;; source-records for every pass, so the image pass uses this store to relocate
;; the records of the objects it links.
(define obj-source-store (make-hash))
(define (symbol-offsets obj)
  (for/list ([entry (obj-file-symbol-table obj)])
    (cons (hash-ref (obj-file-string-table obj) (car entry)) (cdr entry))))
(define (store-obj-source! filename)
  (hash-set! obj-source-store filename
             (list (symbol-offsets current-obj) source-records func-records)))
(define (relocate-linked-source! link-names)
  (define merged (symbol-offsets current-obj))
  (for ([fn link-names])
    (define stored (hash-ref obj-source-store fn #f))
    (when stored
      ; a global symbol present in both gives the object's base in the image
      (define base
        (for/first ([sym (car stored)] #:when (assoc (car sym) merged))
          (- (cdr (assoc (car sym) merged)) (cdr sym))))
      (when base
        (set! source-records
              (append (for/list ([r (cadr stored)])
                        (cons (+ base (car r)) (cdr r)))
                      source-records))
        (set! func-records
              (append (for/list ([f (caddr stored)])
                        (cons (+ base (car f)) (cdr f)))
                      func-records))))))

(begin-for-syntax
  (define (first-srcloc x)
    (cond
      [(and (syntax? x) (syntax-line x)) x]
      [(syntax? x) (first-srcloc (syntax-e x))]
      [(pair? x) (or (first-srcloc (car x)) (first-srcloc (cdr x)))]
      [else #f]))
  ;; macro-generated lines point at the line that invoked the macro. origin
  ;; identifiers are searched through the subterms; the last one with a line is the outermost use
  (define (origin-loc x)
    (cond
      [(syntax? x)
       (define ids (filter syntax-line (flatten-origin (syntax-property x 'origin))))
       (if (pair? ids) (list-ref ids (sub1 (length ids))) (origin-loc (syntax-e x)))]
      [(pair? x) (or (origin-loc (car x)) (origin-loc (cdr x)))]
      [else #f]))
  (define (flatten-origin o)
    (cond [(syntax? o) (list o)]
          [(pair? o) (append (flatten-origin (car o)) (flatten-origin (cdr o)))]
          [else '()]))
  ;; the own line is kept when written in the same file as its use site (e.g. inside a subr body)
  (define (line-loc x use)
    (define own (first-srcloc x))
    (define u (if (syntax? use) use (origin-loc x)))
    (cond
      [(and own u (not (equal? (format "~a" (syntax-source own)) (format "~a" (syntax-source u))))) u]
      [else (or own u)])))

(define-syntax (arm-line stx)
  (define loc (line-loc (cdr (syntax-e stx)) (or (syntax-property stx 'use-site) (origin-loc stx))))
  (with-syntax ([args (datum->syntax stx (cdr (syntax-e stx)))]
                [file (if loc (format "~a" (syntax-source loc)) "")]
                [line (if loc (syntax-line loc) 0)]
                [col (if loc (syntax-column loc) 0)])
    #'(begin
        (note-source! file line col (context-location prog))
        (arm-line-forms . args))))

(define-syntax (arm-line-forms stx)
;  (writeln stx)
  (syntax-parse stx #:datum-literals (= ! LSL )
   ; ldr q0 [x1 @16] and ldr q0 [x1] (str likewise): unsigned offset, a multiple of 16
   [(_ (~optional label:label) oc:opcode rt:q-register (rn:register #:immediate n))
    #:when (and (memq (syntax-e #'oc) '(ldr str)) (not (syntax-e #'rn.is32)))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'q-reg_reg-imm_ #f (list `rt.regnum `rn.regnum) (list n) #f))]
   [(_ (~optional label:label) oc:opcode rt:q-register (rn:register))
    #:when (and (memq (syntax-e #'oc) '(ldr str)) (not (syntax-e #'rn.is32)))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'q-reg_reg-imm_ #f (list `rt.regnum `rn.regnum) (list 0) #f))]
   ; ldr q0 [x1 x2] and ldr q0 [x1 x2 LSL @4] (str likewise): Rm must be an x register
   [(_ (~optional label:label) oc:opcode rt:q-register (rn:register rm:register))
    #:when (and (memq (syntax-e #'oc) '(ldr str)) (not (syntax-e #'rn.is32)) (not (syntax-e #'rm.is32)))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'q-reg_reg-reg-shift #f (list `rt.regnum `rn.regnum `rm.regnum (q-offset-s-bit #f)) '() #f))]
   [(_ (~optional label:label) oc:opcode rt:q-register (rn:register rm:register (~datum LSL) #:immediate s))
    #:when (and (memq (syntax-e #'oc) '(ldr str)) (not (syntax-e #'rn.is32)) (not (syntax-e #'rm.is32)))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'q-reg_reg-reg-shift #f (list `rt.regnum `rn.regnum `rm.regnum (q-offset-s-bit s)) '() #f))]
   ; add v0.4s v1.4s v2.4s (sub likewise): all three operands share an arrangement
   [(_ (~optional label:label) oc:opcode rd:vec-register rn:vec-register rm:vec-register)
    #:when (and (memq (syntax-e #'oc) '(add sub))
                (eq? (syntax-e #'rd.arr) (syntax-e #'rn.arr))
                (eq? (syntax-e #'rn.arr) (syntax-e #'rm.arr)))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'vec-reg-reg-reg #f (list `rd.regnum `rn.regnum `rm.regnum `rd.q `rd.esize) '() #f))]
   ; fadd v0.2s v1.2s v2.2s (fmul likewise): arrangements 2s, 4s and 2d only
   [(_ (~optional label:label) oc:opcode rd:vec-register rn:vec-register rm:vec-register)
    #:when (and (memq (syntax-e #'oc) '(fadd fmul))
                (memq (syntax-e #'rd.arr) '(2s 4s 2d))
                (eq? (syntax-e #'rd.arr) (syntax-e #'rn.arr))
                (eq? (syntax-e #'rn.arr) (syntax-e #'rm.arr)))
    #:with sz (if (eq? (syntax-e #'rd.arr) '2d) 1 0)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'vec-reg-reg-reg-fp #f (list `rd.regnum `rn.regnum `rm.regnum `rd.q `sz) '() #f))]
   ; mov v0.16b v1.16b (an alias of orr, with the source register as both operands)
   [(_ (~optional label:label) (~datum mov) rd:vec-register rn:vec-register)
    #:when (eq? (syntax-e #'rd.arr) (syntax-e #'rn.arr))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'mov 'vec-reg-reg #f (list `rd.regnum `rn.regnum `rn.regnum `rd.q 0) '() #f))]
   ; dup v0.4s w1 (b, h and s elements) or dup v0.2d x1 (d element)
   [(_ (~optional label:label) (~datum dup) rd:vec-register rn:register)
    #:when (if (= (syntax-e #'rd.esize) 3) (not (syntax-e #'rn.is32)) (syntax-e #'rn.is32))
    #:with imm5 (arithmetic-shift 1 (syntax-e #'rd.esize))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'dup 'vec-reg-gp #f (list `rd.regnum `rn.regnum `rd.q `imm5) '() #f))]
   ; movi v0.16b @5 (or 8b): one byte repeated in every lane
   [(_ (~optional label:label) (~datum movi) rd:vec-register #:immediate n)
    #:when (memq (syntax-e #'rd.arr) '(8b 16b))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'movi 'vec-reg-imm8 #f (list `rd.regnum `rd.q) (list n) #f))]
   ; movi v0.2d @16711935: each byte of the 64 bit value must be 00 or FF
   [(_ (~optional label:label) (~datum movi) rd:vec-register #:immediate n)
    #:when (eq? (syntax-e #'rd.arr) '2d)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'movi 'vec-reg-imm8-d #f (list `rd.regnum `rd.q) (list n) #f))]
   ; ld1 {v0.16b} [x1] and st1 {v0.4s} [x1]: the braces arrive as a one line arm-block
   [(_ (~optional label:label) oc:opcode ((~datum arm-block) ((~datum arm-line) rt:vec-register)) (rn:register))
    #:when (and (memq (syntax-e #'oc) '(ld1 st1)) (not (syntax-e #'rn.is32)))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'vec-reg-addr #f (list `rt.regnum `rn.regnum `rt.q `rt.esize) '() #f))]
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
   ; tbz x0 @3 lbl: (bit number via @n, the bit field is encoded from it)
   [(_ (~optional label:label) (~and oc (~or (~datum tbz) (~datum tbnz))) rt:register #:immediate bit targ:label-targ)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-bit-lbl #f (list `rt.regnum bit `rt.is32) '(0) 'targ))]
   ; ret x30
   [(_ (~optional label:label) oc:opcode rn:register)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg #f (list `rn.regnum) '() #f))]
   ; dmb @15, dsb @15, isb @15 (@n sets the barrier option)
   [(_ (~optional label:label) oc:opcode #:immediate n)
    #:when (memq (syntax-e #'oc) '(dmb dsb isb))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'barrier #f '() (list n) #f))]
   ; svc @0, hvc @0, smc @0, brk @0, hlt @0
   [(_ (~optional label:label) oc:opcode #:immediate n)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'imm16-only #f '() (list n) #f))]
;   msr daifset @2
   [(_ (~optional label:label) oc:opcode sr:system-register #:immediate n)
     #:when (not (regexp-match? #rx"^s[0-9]+_" (symbol->string (syntax-e #'sr))))
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
   ; rev32 x0 x1 (64-bit only)
   [(_ (~optional label:label) (~datum rev32) rt:register rn:register)
    #:when (not (or (syntax-e #'rt.is32) (syntax-e #'rn.is32)))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'rev32 'reg-reg #f (list `rt.regnum `rn.regnum) '() #f))]
   ; mov sp x1
   [(_ (~optional label:label) oc:opcode rt:register rn:register)
    #:when (equal? (syntax-e #'rt) 'sp)
    #'(begin
      (~? (try-set-jump-source `label set-jump-source-current))
        ;rewrite this as ADD (mov sp x is an alias for add sp x 0 )
      (write-instruction 'add 'reg-reg-imm `rt.is32 (list `rt.regnum `rn.regnum) (list 0) #f))
    ]
   ;mov x0 sp
   [(_ (~optional label:label) oc:opcode rt:register rn:register)
    #:when (equal? (syntax-e #'rn) 'sp)
    #'(begin
      (~? (try-set-jump-source `label set-jump-source-current))
        ;rewrite this as ADD (mov x sp is an alias for add x sp 0 )
      (write-instruction 'add 'reg-reg-imm `rt.is32 (list `rt.regnum `rn.regnum) (list 0) #f))
    ]
   ; mov wzr x1
   [(_ (~optional label:label) oc:opcode rt:register rm:register)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg `rt.is32 (list `rt.regnum `rm.regnum) '() #f))]
   ; ror x0 x1 #n is extr x0 x1 x1 #n. This must come before the generic immediate clause below.
   [(_ (~optional label:label) (~datum ror) rt:register rn:register #:immediate n)
     #:when (eq? (syntax-e #'rt.is32) (syntax-e #'rn.is32))
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'extr 'reg-reg-reg-imm `rt.is32 (list `rt.regnum `rn.regnum `rn.regnum) (list n) #f))]
   ; sub x1 x2 @12
   [(_ (~optional label:label) oc:opcode rt:register rn:register #:immediate n)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg-imm `rt.is32 (list `rt.regnum `rn.regnum) (list n) #f))]
   ; str x1 [sp @-16] !   PRE INDEX
   [(_ (~optional label:label) oc:opcode rt:register (rn:register #:immediate n) !)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg_reg-imm_excla `rt.is32 (list `rt.regnum `rn.regnum) (list n) #f))]
   ; ldrsw x0 [x1 @8], ldrsb x0 [x1 @3], ldrsh x0 [x1 @4]; w destinations select the _w opcode
   [(_ (~optional label:label) (~and oc (~or (~datum ldrsw) (~datum ldrsb) (~datum ldrsh) (~datum ldursw) (~datum ldursb) (~datum ldursh))) rt:register (rn:register #:immediate n))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc (sign-extending-load-mode 'oc `rt.is32) `rt.is32 (list `rt.regnum `rn.regnum) (list n) #f))]
   [(_ (~optional label:label) (~and oc (~or (~datum ldrsw) (~datum ldrsb) (~datum ldrsh) (~datum ldursw) (~datum ldursb) (~datum ldursh))) rt:register (rn:register))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc (sign-extending-load-mode 'oc `rt.is32) `rt.is32 (list `rt.regnum `rn.regnum) (list 0) #f))]
   ; stur x0 [x1 @10]
   [(_ (~optional label:label) oc:opcode rt:register (rn:register #:immediate n))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg_reg-imm_ `rt.is32 (list `rt.regnum `rn.regnum) (list n) #f))]
   ; specical case for str [x1] rewrites to str [x1] @0
   [(_ (~optional label:label) oc:opcode rt:register (rn:register))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg_reg-imm_ `rt.is32 (list `rt.regnum `rn.regnum) (list 0) #f))]
   ;str x1 [x2] @16 POST INDEX
   [(_ (~optional label:label) oc:opcode rt:register (rn:register) #:immediate n)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg_reg_imm `rt.is32 (list `rt.regnum `rn.regnum) (list n) #f))]
   ;ldp x1 x2 [sp @4]
   [(_ (~optional label:label) oc:opcode rt:register rt2:register (rn:register #:immediate n))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg_reg-imm_ `rt.is32 (list `rt.regnum `rt2.regnum `rn.regnum) (list n) #f))]
   ; ldp x1 x2 [sp @-16] !   PRE INDEX PAIR
   [(_ (~optional label:label) oc:opcode rt:register rt2:register (rn:register #:immediate n) !)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg_reg-imm_excla `rt.is32 (list `rt.regnum `rt2.regnum `rn.regnum) (list n) #f))]
   ; ldp x1 x2 [sp] @16 POST INDEX PAIR
   [(_ (~optional label:label) oc:opcode rt:register rt2:register (rn:register) #:immediate n)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg_reg_imm `rt.is32 (list `rt.regnum `rt2.regnum `rn.regnum) (list n) #f))]
   ; ldr x0 [x1 x2] register offset, S bit 0
   [(_ (~optional label:label) oc:opcode rt:register (rn:register rm:register))
    #:when (not (syntax-e #'rm.is32))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg_reg-reg-shift `rt.is32 (list `rt.regnum `rn.regnum `rm.regnum (register-offset-s-bit 'oc `rt.is32 #f)) '() #f))]
   ; ldr x0 [x1 x2 LSL @3] register offset, S bit set when the amount is the access size
   [(_ (~optional label:label) oc:opcode rt:register (rn:register rm:register (~datum LSL) #:immediate s))
    #:when (not (syntax-e #'rm.is32))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg_reg-reg-shift `rt.is32 (list `rt.regnum `rn.regnum `rm.regnum (register-offset-s-bit 'oc `rt.is32 s)) '() #f))]
   ; madd x0 x1 x2 x3
   [(_ (~optional label:label) oc:opcode rt:register rn:register rm:register ra:register)
    #:when (memq (syntax-e #'oc) '(madd msub))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg-reg-reg `rt.is32 (list `rt.regnum `rn.regnum `rm.regnum `ra.regnum) '() #f))]
   ; ldxr x0 [x1], ldaxr, ldar and stlr: exclusive and ordered access with no offset
    [(_ (~optional label:label) oc:id rt:register (rn:register))
     #:when (and (memq (syntax-e #'oc) '(ldxr ldaxr ldar stlr)) (not (syntax-e #'rn.is32)))
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'oc 'reg-addr `rt.is32 (list `rt.regnum `rn.regnum) '() #f))]
    ; stxr w0 x1 [x2] and stlxr: the status register is a 32-bit Rs
    [(_ (~optional label:label) oc:id rs:register rt:register (rn:register))
     #:when (and (memq (syntax-e #'oc) '(stxr stlxr)) (syntax-e #'rs.is32) (not (syntax-e #'rn.is32)))
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'oc 'reg-addr-status `rt.is32 (list `rt.regnum `rn.regnum `rs.regnum) '() #f))]
    ; ccmp x0 @5 @nzcv cond: compare with a 5-bit immediate
    [(_ (~optional label:label) (~datum ccmp) rn:register #:immediate imm #:immediate nzcv cond:condition)
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'ccmp 'ccmp-imm `rn.is32
                            (list (ccmp-operand-in-range "nzcv" nzcv 15) `rn.regnum (ccmp-operand-in-range "immediate" imm 31) `cond.condnum)
                            '() #f))]
    ; ccmp x0 x1 @nzcv cond: compare with a register
    [(_ (~optional label:label) (~datum ccmp) rn:register rm:register #:immediate nzcv cond:condition)
     #:when (eq? (syntax-e #'rn.is32) (syntax-e #'rm.is32))
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'ccmp 'ccmp-reg `rn.is32
                            (list (ccmp-operand-in-range "nzcv" nzcv 15) `rn.regnum `rm.regnum `cond.condnum)
                            '() #f))]
    ; prfm with a prefetch operation in Rt and an unsigned, scaled offset
    [(_ (~optional label:label) oc:id #:immediate op (rn:register))
     #:when (eq? (syntax-e #'oc) 'prfm)
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'oc 'reg_reg-imm_ #f (list op `rn.regnum) (list 0) #f))]
    [(_ (~optional label:label) oc:id #:immediate op (rn:register #:immediate n))
     #:when (eq? (syntax-e #'oc) 'prfm)
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'oc 'reg_reg-imm_ #f (list op `rn.regnum) (list n) #f))]
    ; smull x0 w1 w2 and umull x0 w1 w2: 64-bit destination, 32-bit sources
    [(_ (~optional label:label) (~datum smull) rt:register rn:register rm:register)
     #:when (and (not (syntax-e #'rt.is32)) (syntax-e #'rn.is32) (syntax-e #'rm.is32))
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'smull 'reg-reg-reg #f (list `rt.regnum `rn.regnum `rm.regnum) '() #f))]
    [(_ (~optional label:label) (~datum umull) rt:register rn:register rm:register)
     #:when (and (not (syntax-e #'rt.is32)) (syntax-e #'rn.is32) (syntax-e #'rm.is32))
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'umull 'reg-reg-reg #f (list `rt.regnum `rn.regnum `rm.regnum) '() #f))]
    ; mneg x0 x1 x2 is msub x0 x1 x2 xzr
   [(_ (~optional label:label) (~datum mneg) rt:register rn:register rm:register)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'msub 'reg-reg-reg-reg `rt.is32 (list `rt.regnum `rn.regnum `rm.regnum 31) '() #f))]
   ; smulh x0 x1 x2 (64-bit only, 32-bit operands fail to match)
   [(_ (~optional label:label) (~and oc (~or (~datum smulh) (~datum umulh))) rt:register rn:register rm:register)
    #:when (not (or (syntax-e #'rt.is32) (syntax-e #'rn.is32) (syntax-e #'rm.is32)))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg-reg #f (list `rt.regnum `rn.regnum `rm.regnum) '() #f))]
   ; ubfm x0 x1 #immr #imms and sbfm x0 x1 #immr #imms
   [(_ (~optional label:label) oc:opcode rt:register rn:register #:immediate immr #:immediate imms)
    #:when (memq (syntax-e #'oc) '(ubfm sbfm))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg-imm-imm `rt.is32 (list `rt.regnum `rn.regnum) (list immr imms) #f))]
   ; uxtb w0 w1 is ubfm w0 w1 #0 #7
   [(_ (~optional label:label) (~datum uxtb) rt:register rn:register)
    #:when (and (syntax-e #'rt.is32) (syntax-e #'rn.is32))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'ubfm 'reg-reg-imm-imm #t (list `rt.regnum `rn.regnum) (list 0 7) #f))]
   ; uxth w0 w1 is ubfm w0 w1 #0 #15
   [(_ (~optional label:label) (~datum uxth) rt:register rn:register)
    #:when (and (syntax-e #'rt.is32) (syntax-e #'rn.is32))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'ubfm 'reg-reg-imm-imm #t (list `rt.regnum `rn.regnum) (list 0 15) #f))]
   ; bfi x0 x1 #lsb #width is bfm x0 x1 #(-lsb mod size) #(width-1)
    [(_ (~optional label:label) (~datum bfi) rt:register rn:register #:immediate lsb #:immediate width)
     #:when (eq? (syntax-e #'rt.is32) (syntax-e #'rn.is32))
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (let* ([size (if `rt.is32 32 64)]
                [lsb-val lsb] [width-val width])
           (unless (and (<= 0 lsb-val (sub1 size)) (<= 1 width-val (- size lsb-val)))
             (error (format "bfi lsb ~a and width ~a do not fit in a ~a-bit register" lsb-val width-val size)))
           (write-instruction 'bfm 'reg-reg-imm-imm `rt.is32 (list `rt.regnum `rn.regnum) (list (modulo (- lsb-val) size) (sub1 width-val)) #f)))]
    ; extr x0 x1 x2 #lsb
    [(_ (~optional label:label) (~datum extr) rt:register rn:register rm:register #:immediate lsb)
     #:when (and (eq? (syntax-e #'rt.is32) (syntax-e #'rn.is32)) (eq? (syntax-e #'rt.is32) (syntax-e #'rm.is32)))
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'extr 'reg-reg-reg-imm `rt.is32 (list `rt.regnum `rn.regnum `rm.regnum) (list lsb) #f))]
   [(_ (~optional label:label) (~datum ubfx) rt:register rn:register #:immediate lsb #:immediate width)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (let ([lsb-val lsb] [width-val width])
          (write-instruction 'ubfm 'reg-reg-imm-imm `rt.is32 (list `rt.regnum `rn.regnum) (list lsb-val (+ lsb-val width-val -1)) #f)))]
   ; sbfx x0 x1 #lsb #width is sbfm x0 x1 #lsb #(lsb+width-1)
   [(_ (~optional label:label) (~datum sbfx) rt:register rn:register #:immediate lsb #:immediate width)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (let ([lsb-val lsb] [width-val width])
          (write-instruction 'sbfm 'reg-reg-imm-imm `rt.is32 (list `rt.regnum `rn.regnum) (list lsb-val (+ lsb-val width-val -1)) #f)))]
   ; add x0 x1 x2 LSL @4 (also LSR and ASR; ROR only for the logical forms)
   [(_ (~optional label:label) oc:opcode rt:register rn:register rm:register shift:id #:immediate n)
    #:when (or (memq (syntax-e #'shift) '(LSL LSR ASR))
               (and (eq? (syntax-e #'shift) 'ROR)
                    (memq (syntax-e #'oc) '(and ands orr eor bic bics orn))))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg-reg-shift `rt.is32 (list `rt.regnum `rn.regnum `rm.regnum (shifted-operand-bits 'shift `rt.is32 n)) '() #f))]
   ; cinc x0 x1 eq is csinc x0 x1 x1 with the inverted condition
   [(_ (~optional label:label) (~datum cinc) rt:register rn:register cond:condition)
    #:when (not (eq? (syntax-e #'cond) 'al))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'csinc 'reg-reg-reg-cond `rt.is32 (list `rt.regnum `rn.regnum `rn.regnum (bitwise-xor `cond.condnum 1)) '() #f))]
   ; cneg x0 x1 eq is csneg x0 x1 x1 with the inverted condition
   [(_ (~optional label:label) (~datum cneg) rt:register rn:register cond:condition)
    #:when (not (eq? (syntax-e #'cond) 'al))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'csneg 'reg-reg-reg-cond `rt.is32 (list `rt.regnum `rn.regnum `rn.regnum (bitwise-xor `cond.condnum 1)) '() #f))]
   ; add x0 x1 w2 UXTW #2 (also adds, sub and subs); the amount is optional
   [(_ (~optional label:label) oc:opcode rt:register rn:register rm:register ext:id #:immediate n)
    #:when (and (memq (syntax-e #'oc) '(add adds sub subs))
                (memq (syntax-e #'ext) '(UXTB UXTH UXTW UXTX SXTB SXTH SXTW SXTX))
                (eq? (syntax-e #'rt.is32) (syntax-e #'rn.is32)))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg-reg-ext `rt.is32 (list `rt.regnum `rn.regnum `rm.regnum (extended-operand-bits 'ext `rt.is32 `rm.is32 n)) '() #f))]
   [(_ (~optional label:label) oc:opcode rt:register rn:register rm:register ext:id)
    #:when (and (memq (syntax-e #'oc) '(add adds sub subs))
                (memq (syntax-e #'ext) '(UXTB UXTH UXTW UXTX SXTB SXTH SXTW SXTX))
                (eq? (syntax-e #'rt.is32) (syntax-e #'rn.is32)))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg-reg-ext `rt.is32 (list `rt.regnum `rn.regnum `rm.regnum (extended-operand-bits 'ext `rt.is32 `rm.is32 0)) '() #f))]
   ; mvn x0 x1 is orn x0 xzr x1
   [(_ (~optional label:label) (~datum mvn) rt:register rm:register)
    #:when (eq? (syntax-e #'rt.is32) (syntax-e #'rm.is32))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'orn 'reg-reg-reg `rt.is32 (list `rt.regnum 31 `rm.regnum) '() #f))]
   ; mvn x0 x1 LSL @2 is orn x0 xzr x1 LSL @2
   [(_ (~optional label:label) (~datum mvn) rt:register rm:register shift:id #:immediate n)
    #:when (and (memq (syntax-e #'shift) '(LSL LSR ASR ROR))
                (eq? (syntax-e #'rt.is32) (syntax-e #'rm.is32)))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'orn 'reg-reg-reg-shift `rt.is32 (list `rt.regnum 31 `rm.regnum (shifted-operand-bits 'shift `rt.is32 n)) '() #f))]
   ; add x1 x2 x4
   [(_ (~optional label:label) oc:opcode rt:register rn:register rm:register)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg-reg `rt.is32 (list `rt.regnum `rn.regnum `rm.regnum) '() #f))]
   ; cset x0 eq is csinc x0 xzr xzr with the inverted condition; al is not a valid alias condition
   [(_ (~optional label:label) oc:opcode rt:register cond:condition)
    #:when (not (eq? (syntax-e #'cond) 'al))
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'csinc 'reg-reg-reg-cond `rt.is32 (list `rt.regnum 31 31 (bitwise-xor `cond.condnum 1)) '() #f))]
   ; csel x0 x1 x2 gt
   [(_ (~optional label:label) oc:opcode rt:register rn:register rm:register cond:condition)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg-reg-cond `rt.is32 (list `rt.regnum `rn.regnum `rm.regnum `cond.condnum) '() #f))]
   ; fmov s0 w0 (or d0 x0 for double precision)
   [(_ (~optional label:label) (~datum fmov) rd:fpu-register rn:register)
    #:when (eq? (syntax-e #'rn.is32) (not (syntax-e #'rd.dbl)))
    #:with fpmode (if (syntax-e #'rd.dbl) #'fp-gp-d #'fp-gp)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'fmov 'fpmode #f (list `rd.regnum `rn.regnum) '() #f))]
   ; fmov w0 s0 (or x0 d0 for double precision)
   [(_ (~optional label:label) (~datum fmov) rd:register rn:fpu-register)
    #:when (eq? (syntax-e #'rd.is32) (not (syntax-e #'rn.dbl)))
    #:with fpmode (if (syntax-e #'rn.dbl) #'gp-fp-d #'gp-fp)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'fmov 'fpmode #f (list `rd.regnum `rn.regnum) '() #f))]
   ; fmov s0 @1.5 (or d0 @1.5 for double precision)
   [(_ (~optional label:label) (~datum fmov) rd:fpu-register #:immediate n)
    #:with fpmode (if (syntax-e #'rd.dbl) #'fp-imm-d #'fp-imm)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'fmov 'fpmode #f (list `rd.regnum) (list n) #f))]
   ; fcmp s0 @0 (or d0 @0 for double precision)
   [(_ (~optional label:label) (~datum fcmp) rn:fpu-register #:immediate n)
    #:when (eqv? (syntax-e #'n) 0)
    #:with fpmode (if (syntax-e #'rn.dbl) #'fp-zero-d #'fp-zero)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'fcmp 'fpmode #f (list `rn.regnum) '() #f))]
   ; scvtf s0 x0 (any integer width is accepted as the source)
   [(_ (~optional label:label) (~datum scvtf) rd:fpu-register rn:register)
    #:with fpmode (if (syntax-e #'rd.dbl) #'fp-gp-d #'fp-gp)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'scvtf 'fpmode `rn.is32 (list `rd.regnum `rn.regnum) '() #f))]
   ; ucvtf s0 w1 (any integer width is accepted as the source)
   [(_ (~optional label:label) (~datum ucvtf) rd:fpu-register rn:register)
    #:with fpmode (if (syntax-e #'rd.dbl) #'fp-gp-d #'fp-gp)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'ucvtf 'fpmode `rn.is32 (list `rd.regnum `rn.regnum) '() #f))]
   ; fcvtzs w0 s0 (any integer width is accepted as the destination)
   [(_ (~optional label:label) (~datum fcvtzs) rd:register rn:fpu-register)
    #:with fpmode (if (syntax-e #'rn.dbl) #'gp-fp-d #'gp-fp)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'fcvtzs 'fpmode `rd.is32 (list `rd.regnum `rn.regnum) '() #f))]
   ; fcvtzu w0 s0 (any integer width is accepted as the destination)
   [(_ (~optional label:label) (~datum fcvtzu) rd:register rn:fpu-register)
    #:with fpmode (if (syntax-e #'rn.dbl) #'gp-fp-d #'gp-fp)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'fcvtzu 'fpmode `rd.is32 (list `rd.regnum `rn.regnum) '() #f))]
   ; ldr s0 [x1 @8] (or ldr d0 [x1 @8])
   [(_ (~optional label:label) oc:opcode rt:fpu-register (rn:register #:immediate n))
    #:with fpmode (if (syntax-e #'rt.dbl) #'fp-reg_reg-imm_-d #'fp-reg_reg-imm_)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'fpmode #f (list `rt.regnum `rn.regnum) (list n) #f))]
   ; ldr s0 [x1]
   [(_ (~optional label:label) oc:opcode rt:fpu-register (rn:register))
    #:with fpmode (if (syntax-e #'rt.dbl) #'fp-reg_reg-imm_-d #'fp-reg_reg-imm_)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'fpmode #f (list `rt.regnum `rn.regnum) (list 0) #f))]
   ; ldr s0 [x1 x2] and ldr s0 [x1 x2 LSL @2]: register offset, Rm must be an x register
   [(_ (~optional label:label) oc:opcode rt:fpu-register (rn:register rm:register))
    #:when (and (memq (syntax-e #'oc) '(ldr str)) (not (syntax-e #'rm.is32)))
    #:with fpmode (if (syntax-e #'rt.dbl) #'fp-reg_reg-reg-shift-d #'fp-reg_reg-reg-shift)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'fpmode #f (list `rt.regnum `rn.regnum `rm.regnum (register-offset-s-bit 'oc (not `rt.dbl) #f)) '() #f))]
   [(_ (~optional label:label) oc:opcode rt:fpu-register (rn:register rm:register (~datum LSL) #:immediate s))
    #:when (and (memq (syntax-e #'oc) '(ldr str)) (not (syntax-e #'rm.is32)))
    #:with fpmode (if (syntax-e #'rt.dbl) #'fp-reg_reg-reg-shift-d #'fp-reg_reg-reg-shift)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'fpmode #f (list `rt.regnum `rn.regnum `rm.regnum (register-offset-s-bit 'oc (not `rt.dbl) s)) '() #f))]
   ; fadd s0 s1 s2 (or fadd d0 d1 d2): all three operands must have the same width
   [(_ (~optional label:label) oc:opcode rd:fpu-register rn:fpu-register rm:fpu-register)
    #:when (and (eq? (syntax-e #'rd.dbl) (syntax-e #'rn.dbl)) (eq? (syntax-e #'rn.dbl) (syntax-e #'rm.dbl)))
    #:with fpmode (if (syntax-e #'rd.dbl) #'fp-fp-fp-d #'fp-fp-fp)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'fpmode #f (list `rd.regnum `rn.regnum `rm.regnum) '() #f))]
   ; fcsel s0 s1 s2 eq (or fcsel d0 d1 d2 eq)
   [(_ (~optional label:label) (~datum fcsel) rd:fpu-register rn:fpu-register rm:fpu-register cond:condition)
    #:when (and (eq? (syntax-e #'rd.dbl) (syntax-e #'rn.dbl)) (eq? (syntax-e #'rn.dbl) (syntax-e #'rm.dbl)))
    #:with fpmode (if (syntax-e #'rd.dbl) #'fp-fp-fp-cond-d #'fp-fp-fp-cond)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'fcsel 'fpmode #f (list `rd.regnum `rn.regnum `rm.regnum `cond.condnum) '() #f))]
   ; fmadd s0 s1 s2 s3 (fmsub, fnmadd and fnmsub take the same operands)
   [(_ (~optional label:label) oc:opcode rd:fpu-register rn:fpu-register rm:fpu-register ra:fpu-register)
    #:when (and (memq (syntax-e #'oc) '(fmadd fmsub fnmadd fnmsub))
                (eq? (syntax-e #'rd.dbl) (syntax-e #'rn.dbl))
                (eq? (syntax-e #'rn.dbl) (syntax-e #'rm.dbl))
                (eq? (syntax-e #'rm.dbl) (syntax-e #'ra.dbl)))
    #:with fpmode (if (syntax-e #'rd.dbl) #'fp-fp-fp-fp-d #'fp-fp-fp-fp)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'fpmode #f (list `rd.regnum `rn.regnum `rm.regnum `ra.regnum) '() #f))]
   ; fabs s0 s1 (or fabs d0 d1)
   [(_ (~optional label:label) oc:opcode rd:fpu-register rn:fpu-register)
    #:when (eq? (syntax-e #'rd.dbl) (syntax-e #'rn.dbl))
    #:with fpmode (if (syntax-e #'rd.dbl) #'fp-fp-d #'fp-fp)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'fpmode #f (list `rd.regnum `rn.regnum) '() #f))]
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

   ; an assembler mnemonic that matched no instruction form is an error,
   ; not a call to an unbound Racket identifier
   [(_ head:id . args)
    #:when (and (memq (syntax-e #'head) assembler-mnemonics)
                (not (identifier-binding #'head)))
    (raise-syntax-error #f (format "invalid operands for instruction '~a'" (syntax-e #'head)) stx)]

   [(_ e:expr ... ) #'(begin e ...) ]
   [(_ e ) #'e] ))

(define-syntax (arm-block stx)
  (syntax-parse stx
    ([_ line ... ]
     ; a block produced by a macro passes its use site to its lines
     (define use (let ([ids (filter syntax-line (flatten-origin (syntax-property stx 'origin)))])
                   (and (pair? ids) (list-ref ids (sub1 (length ids))))))
     (with-syntax ([(tagged ...)
                    (for/list ([l (syntax->list #'(line ...))])
                      (if use (syntax-property l 'use-site (or (syntax-property l 'use-site) use)) l))])
       #'(begin tagged ...)))))

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
  (set-context-data! prog (make-vector (* 1024 1024) 0)) ; start at 1 meg
  (set-context-location! prog 0)
  (set-context-minl! prog 0)
  (set-context-maxl! prog 0)
  (set-context-jump-table! prog (make-hash))
  (set-context-branches-waiting! prog (make-hash))
  (reset-source-records!)
  )
;; global labels named :Bp-... (from the (breakpoint) macro) become gdb breakpoints.
;; $image_base is the QEMU -kernel load address; edit it in the file for other layouts.
(define (write-gdb-breakpoints image-filename)
  (define gdb-filename (string-append (regexp-replace #rx"[.]img$" image-filename "") ".gdb"))
  (call-with-output-file gdb-filename #:exists 'replace
    (λ (out)
      (fprintf out "# breakpoints for ~a, load with: source ~a\n" image-filename gdb-filename)
      (fprintf out "set $image_base = 0x80000\n")
      (for ([entry (obj-file-symbol-table current-obj)])
        (let ([name (hash-ref (obj-file-string-table current-obj) (car entry))])
          (when (string-prefix? name ":Bp-")
            (fprintf out "# ~a\n" (substring name 1))
            (fprintf out "break *($image_base + 0x~a)\n"
                     (number->string (cdr entry) 16))))))))

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
             
             (set-current-obj (merge-objs current-obj merged))
             (relocate-linked-source! link-list))

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
                       (displayln (format "rewriting label ~a from\n $~a to $~a" label
                                          (~r location #:base 16 #:min-width 16 #:pad-string "0")
                                          (~r actual-location #:base 16 #:min-width 16 #:pad-string "0")))
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
                       [(memv top-byte '(#x36 #x37 #xB6 #xB7))
                       ; tbz, tbnz (b5 is bit 31, so the top byte can be 0xB6/0xB7)
                        (let* ([encoder (encoders-address-encoder (hash-ref opcode-encoders 'tbz-rt-bit))]
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
                 (close-output-port out)
                 (write-debug-elf (string-append (regexp-replace #rx"[.]img$" filename "") ".elf")
                                  (apply bytes (for/list ([i (obj-file-data current-obj)]) i))
                                  source-records
                                  func-records
                                  filename)
                 (write-gdb-breakpoints filename))
               )
             (begin ; write object file
               (displayln (format "writing object file ~a" filename))
               (store-obj-source! filename)
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
(provide (for-syntax condition immediate system-register opcode register register-32 register-64 fpu-register label label-targ))


;#xFFFF0000

