#lang pisemble
(require (for-syntax syntax/parse))

(define-syntax (regs stx)
  (syntax-parse stx
    [(_ ([new-reg old-reg:register]...) expr ...)
     #'(let-syntax ([new-reg (make-rename-transformer #'old-reg)] ...)
         expr ...)]))

(define-syntax (PUSH-REG-INNER stx)
  (writeln stx)
  (syntax-parse stx
    [(_ mode rn:register ... )
     #'{(Push-REG-INNER 0 mode rn ...)}]
    [(_ used-bytes:nat mode rn:register ... )
     #:when (= (syntax-e #'used-bytes) 16) ; finished slot
     #'{(Push-REG-INNER 0 mode rn ...)}]
    [(_ used-bytes:nat mode r:register-32 rn:register ... )
     #:when (<= (syntax-e #'used-bytes) 12) ; enough room for only 32bit reg
     (let ([ num (syntax-e #'used-bytes)])       
     #`{ str r [sp @-4] !
         (PUSH-REG-INNER #,(+ num 4) mode rn ...)})]
    [(_ used-bytes:nat mode r:register rn:register ... )
     #:when (<= (syntax-e #'used-bytes) 8) ; enough room for any reg
     (let ([ num (syntax-e #'used-bytes)])       
       (syntax-parse #'r
         [r:register-64 
          #`{ str r [sp @-8] !
             (PUSH-REG-INNER #,(+ num 8) mode rn ...) }]
         [r:register-32
          #`{ str r [sp @-4] !
              (PUSH-REG-INNER #,(+ num 4) mode rn ...)}]))]
    [(_ used-bytes:nat mode rn:register ...+ )
     ;otherwise pad the sp with remaning bytes
     #:with pad (datum->syntax this-syntax (- 16 (syntax-e #'used-bytes))) 
     #' { sub sp sp @pad
          (PUSH-REG-INNER 0 mode rn ...)    
        }    
     ]
    [(_ used-bytes:nat mode )
     ;pad the sp with remaning bytes
     #:with pad (datum->syntax this-syntax (- 16 (syntax-e #'used-bytes)))
     #' {sub sp sp @pad }    ]))

(define-syntax (PUSH stx)
  (syntax-parse stx
    [(_ r:register rn ...+)
     #'{ str r [sp @-16] !
         (PUSH rn ...)    }]
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
    [(_ r:register rn ...+)
     #'{ ldr r [sp] @16
         (POP rn ...)    }]
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

(define-syntax (subr stx)
  ; define a subroutine. create a label for it and push/pop the supplied
  ; regs as a prologue/epilogue. rename regs to user-friendlty names.
  ;Of course, with a bit more work they could be automatically detected
  (syntax-parse stx
    [(_ subroutine-name:id
        ([new-reg old-reg:register] ...)
        [used-reg:register ...]        
        code )
     #:with (reg-rev ...)
     (datum->syntax this-syntax (reverse (syntax->list #'(used-reg ...))))
     #:with label
     (let* ([sym (syntax-e #'subroutine-name)]
            [str (symbol->string sym)]
            [label-str (format ":~a" str)]
            [label (string->symbol label-str)])
       (datum->syntax this-syntax label))
     #'(regs ([new-reg old-reg] ...)
         (try-set-jump-source `label set-jump-source-current)
         (PUSH x30) ; always preserve return address register
         (PUSH used-reg) ...
         code
         (POP reg-rev) ...
         (POP x30)
         { ret x30 })]))

(provide (all-defined-out))
