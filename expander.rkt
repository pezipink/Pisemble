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
    ['imm16-rd
     (match-lambda
       [(list bin rd)        
        ;rd goes in the bottom 5 bits
        (bitwise-ior-n rd bin)])
     (match-lambda
       [(list bin imm16)
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
        (let ([shifted-lo (arithmetic-shift (bitwise-and #x3 imm ) 30) ]
              [shifted (arithmetic-shift (bitwise-and #x1FFFFC imm ) 5) ])
          (bitwise-ior-n bin shifted))])

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
        (let ([immr (arithmetic-shift imm6 16)])
          (bitwise-ior-n immr bin))])
     ]
    ['imm9-rn-rd
     (match-lambda
       [(list bin rd rn)
        (let ([rn-shifted (arithmetic-shift rn 5)])
          (bitwise-ior-n rn-shifted rd bin))])
     (match-lambda
       [(list bin imm9)
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
        (let ([imm-shifted (arithmetic-shift (bitwise-and #b111111111 imm9) 12)])
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
    ['adr 'reg-lbl       'immlo-immhi-rd                     #b00010000000000000000000000000000 #f #f]
    ['and 'reg-reg-reg   'rm-rn-rd                           #b10001010000000000000000000000000 b31 #f]    
    ['b   'lbl           'imm26                              #b00010100000000000000000000000000 #f #f]
    ['bl  'lbl           'imm26                              #b10010100000000000000000000000000 #f #f]
    ['cbz 'reg-lbl       'imm19-rt                           #b10110100000000000000000000000000 b31 #f]
    ['cbnz 'reg-lbl      'imm19-rt                           #b10110101000000000000000000000000 b31 #f]
    ; post index
    ['ldr 'reg_reg_imm  'imm9-rn-rd                          #b11111000010000000000010000000000 b30 #f]
    ; pre index
    ['ldr 'reg_reg-imm_excla  'imm9-rn-rd                    #b11111000010000000000110000000000 b30 #f]

    ['ldr 'reg-lbl       'imm19-rt                           #b01011000000000000000000000000000 b30 #f]
    ['ldr  'reg_reg-imm_ 'imm12-rn-rd                        #b10111001010000000000000000000000 b31 #f]
    ; post index
    ['ldrh 'reg_reg_imm  'imm9-rn-rd                         #b01111000010000000000010000000000 #f #f]
    ; pre index
    ['ldrh 'reg_reg-imm_excla  'imm9-rn-rd                   #b01111000010000000000110000000000 #f #f]
    
    ['ldrh 'reg_reg-imm_ 'imm12-rn-rd                        #b01111001010000000000000000000000 b31 #f]

    ; post index
    ['ldrb 'reg_reg_imm  'imm9-rn-rd                         #b00111000010000000000010000000000 b30 #f]
    ; pre index
    ['ldrb 'reg_reg-imm_excla  'imm9-rn-rd                   #b00111000010000000000110000000000 #f #f]
    ['ldrb 'reg_reg-imm_ 'imm12-rn-rd                        #b00111001010000000000000000000000 #f  #f]
    ['lsl 'reg-reg-imm   'immr-imms-rn-rd                    #b11010011010000000000000000000000 lsl-imm-special #f]
    ['lsr 'reg-reg-imm   'immr6-rn-rd                        #b11010011010000001111110000000000 lsr-imm-special #f]

    ['mov 'reg-reg       'rm-rd                              #b10101010000000000000001111100000 b31 #f]
    ; todo: not sure N here works always set to one with 32 bhit as well?
    ['mov 'reg-imm       'imm16-rd                           #b11010010100000000000000000000000 b31 #f]
    ['movk 'reg-imm      'imm16-rd                           #b11110010100000000000000000000000 b31 #f]
    ['mrs 'reg-sysreg    'rt-sysreg                          #b11010101001100000000000000000000 #f #f]
    ['orr 'reg-reg-reg   'rm-rn-rd                           #b10101010000000000000000000000000 b31 #f]
    ['ret 'reg           'rn                                 #b11010110010111110000000000000000 #f #f]
    ; pre-index
    ['str 'reg_reg-imm_excla  'imm9-rn-rd                    #b11111000000000000000110000000000 b30 #f]
    ; post index
    ['str 'reg_reg_imm    'imm9-rn-rd                        #b11111000000000000000010000000000 b30 #f]

    ;unsigned offset mode str. divide imm12 by 4 or 8 first 
    ['str 'reg_reg-imm_  'imm12-rn-rd                        #b11111001000000000000000000000000 b30 shift-2-or-3]

    ; pre-index
    ['strh 'reg_reg-imm_excla 'imm9-rn-rd                    #b01111000000000000000110000000000 #f #f]
    ; post-index
    ['strh 'reg_reg_imm   'imm9-rn-rd                        #b01111000000000000000010000000000 #f #f]
    
    ;unsigned offset mode, 32 bit only. always shift by 4
    ['strh 'reg_reg-imm_ 'imm12-rn-rd                        #b01111001000000000000000000000000 b31 always-shift-1]

    ; pre-index
    ['strb 'reg_reg-imm_excla 'imm9-rn-rd                    #b00111000000000000000110000000000 #f #f]
    ; post-index
    ['strb 'reg_reg_imm   'imm9-rn-rd                        #b00111000000000000000010000000000 #f #f]
    ;unsigned offset mode, 32 bit only]
    ['strb 'reg_reg-imm_ 'imm12-rn-rd                        #b00111001000000000000000000000000 b31 #f]
    ['stur 'reg_reg-imm_ 'imm9-rn-rd                         #b11111000000000000000000000000000 b30 is-byte]

    ['sub 'reg-reg-imm   'imm12-rn-rd                        #b11010001000000000000000000000000 b31 #f]
    ['wfe 'none          'none                               #b11010101000000110010000001011111 #f #f]))))

(struct context (data location minl maxl jump-table branches-waiting breakpoints) #:mutable #:transparent)
(struct emulator (path program breakpoints? labels? execute?) #:mutable #:transparent)
(struct target-label (immediate-encoder relative location) #:transparent)
(define prog (context (make-vector 65536000 0) 0 0 0 (make-hash) (make-hash) (mutable-set)))
(define emu (emulator "" "" true true false))

(define (configure-emu emu-path program-path execute-emu? enable-breakpoints?)
  (set-emulator-program! emu program-path)
  (set-emulator-execute?! emu execute-emu?)
  (set-emulator-breakpoints?! emu enable-breakpoints?)
  (set-emulator-path! emu emu-path))

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

(define (set-jump-source label location)
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

(define (write-value-32 e)
  ;used for writing ARM 32 bit instructions
  (write-values
   (list
     (lo-byte e)
     (hi-byte e)
     (lo-byte2 e)
     (hi-byte2 e))))
(define (write-value-64 e)
  ;used for writing ARM 32 bit instructions
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
        (writeln (format "write-instruction ~a ~a ~a ~a ~a ~a" opcode syntax-mode is-32bit? data-args immediate-address-args transition-target))
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
    (pbin final)
    (phex final)
;    (wdb "write-instruction ~a ~a ~a ~a ~a" opcode syntax-mode is-32bit? data-args immediate-address-args)
    (write-value-32 final))))

(begin-for-syntax
  (define-syntax-class register
    (pattern x:id
             #:do [(define str (symbol->string (syntax-e (attribute x))))
                   (define isX (or (eq? (string-ref str 0) #\X)
                                   (eq? (string-ref str 0) #\x)))
                   (define isW (or (eq? (string-ref str 0) #\W)
                                   (eq? (string-ref str 0) #\w)))
                   (define isSp  (equal? str "sp" ))
                   (define num
                     (if isSp #b11111 (string->number (substring str 1))))]
             #:when (or isSp (and (or isX isW) (and (<= num 31)(>= num 0))))
             #:with regnum num
             #:with is32 isW)))
(define-syntax (6502-line stx)
  (writeln stx)
  (define-syntax-class immediate
    (pattern #:immediate))



  (define-syntax-class system-register
    #:datum-literals (mpidr_el1)
    (pattern mpidr_el1 #:with regnum #x4005))

  (define-syntax-class opcode
    (pattern x:id
             #:do [(define sym (syntax-e (attribute x)))
                   (define ocs
                     (list 'adr 'and 'b 'bl 'cbz 'cbnz 'ldr 'ldrh 'ldrb 'lsl 'lsr 'mov 'movk 'mrs 'orr 'ret
                           'str 'strb 'strh 'stur 'sub 'wfe))]
             #:when (ormap (λ (x) (eq? sym x)) ocs)))
 
  (syntax-parse stx #:datum-literals (= !)
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
   ; mrs x0 sys
   [(_ (~optional label:label) oc:opcode rt:register sr:system-register)
     #'(begin
         (~? (try-set-jump-source `label set-jump-source-current))
         (write-instruction 'oc 'reg-sysreg #f (list `rt.regnum `sr.regnum) '() #f))]
   ; mov x0 @1
   [(_ (~optional label:label) oc:opcode rt:register #:immediate n)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-imm `rt.is32 (list `rt.regnum) (list n) #f))]
   ; mov wzr x1
   [(_ (~optional label:label) oc:opcode rt:register rn:register)
    #:when (equal? (syntax-e #'rt) 'sp)
    #'(begin
      (~? (try-set-jump-source `label set-jump-source-current))
        ;rewrite this as ADD
      (write-instruction 'add 'reg-reg-imm `rt.is32 (list `rt.regnum `rn.regnum) (list 0) #f))
    ]

   ; mov wzr x1
   ;; [(_ (~optional label:label) oc:opcode rt:register rm:register)
   ;;  #'(begin
   ;;      (~? (try-set-jump-source `label set-jump-source-current))
   ;;      (write-instruction 'oc 'reg-reg `rt.is32 (list `rt.regnum `rm.regnum) '() #f))]
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
   ; add x1 x2 x4
   [(_ (~optional label:label) oc:opcode rt:register rn:register rm:register)
    #'(begin
        (~? (try-set-jump-source `label set-jump-source-current))
        (write-instruction 'oc 'reg-reg-reg `rt.is32 (list `rt.regnum `rn.regnum `rm.regnum) '() #f))]
   
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

(define-syntax (6502-block stx)
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

(define-syntax (aarch64 stx)
  (syntax-parse stx
    [(_ a ...)
     #'(begin
         a ...
         (hash-for-each
          (context-branches-waiting prog)
          (λ (k dest)
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
                
                (when (or (> amount 127) (< amount -127))
                  (writeln
                   (format "warning: attempted to branch over +/-127 (~a) bytes to label ~a from location $~x"
                           amount k (target-label-location current-target))))
                   (writeln
                    (format "label ~A offset ~A "
                            (target-label-location current-target)
                            (- actual (target-label-location current-target))))

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
                ))))
                           
         ;write numbers to file!
         (define out (open-output-file (emulator-program emu) #:exists 'replace #:mode 'binary))
         ;; (write-byte (lo-byte (context-minl prog)) out)
         ;; (write-byte (hi-byte (context-minl prog)) out)
         (writeln (context-minl prog))
         (writeln (context-maxl prog))
         (for ([i  (vector-copy (context-data prog)(context-minl prog) (context-maxl prog) )])
           (write-byte i out)
           )
         (wdb "closing")
         (close-output-port out)

         )]))
  
(provide (all-defined-out))
(provide (for-syntax register))
