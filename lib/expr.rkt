#lang pisemble
(require (for-syntax syntax/parse racket/stxparam racket/syntax))
(require syntax/parse/define)
(provide  /cond /if /when /for /whilenz /whilez /while
          /do-whilenz /do-whilez /do-while)

; Every macro in this file expands to assembler text: a {} block of
; instructions and labels, which the assembler then encodes. Control flow
; uses generated labels. A label is written :name_N, and a branch names it
; as name_N+ (the nearest matching label below) or name_N- (the nearest
; matching label above). Labels are never written by hand, so they cannot
; clash with the user's own labels.
(begin-for-syntax
  (define labels (make-hash))
  (define (format-label label)
    (string->symbol (format ":~a"  label)))
  (define (format-target label type)
    (case type
      ['+     (string->symbol (format "~a+" label))]
      ['-     (string->symbol (format "~a-" label))]
      [else (error (format "bad label type ~a" type))]))
             
  ; Each base name has its own counter, so nested uses get names such as
  ; for-start_0 and for-start_1. The counter is compile-time state, so names
  ; are unique within one compile.
  (define (gen-label name)
    (if (hash-has-key? labels name)
        (let ([val (hash-ref labels name)])
          (hash-set! labels name (+ val 1))
          (format "~a_~a" name (+ val 1)))
        (begin
          (hash-set! labels name 0)
          (format "~a_~a" name 0)))))

; Emits a compare and a conditional branch to branch-target. The branch is
; taken when the condition holds, and otherwise execution falls through.
; (lhs cond rhs) compares two registers. (lhs cond #:immediate n) compares
; with a literal, written (x1 ne @0).
; (/and c ...) branches only when every c holds. Each c is compiled with
; compile-inverse-condition, so any failure jumps to a local else label
; placed after the final branch.
(define-syntax-parser compile-condition #:datum-literals (/and)
  [(_ (lhs:register cond:condition rhs:register) branch-target:label-targ)
   #'{cmp lhs rhs
      cond.branch-opcode branch-target}]
  [(_ (lhs:register cond:condition #:immediate n) branch-target:label-targ)
   #'{cmp lhs @n
      cond.branch-opcode branch-target}]

  [(_ (/and cases ...) then-target:label-targ)
   ; this has come from an OR since we are in non-inverse.
   ; the target passed is the success case for the parent OR
   ; here then we want to go to that label if all of the ANDs
   ; mathc, or short circuit to a new label after these ANDs if any fail
   (let ([else-start (gen-label "else-start")])
     (with-syntax
       ([else-label (format-label else-start)]
        [else-target (format-target else-start '+)])
        #'{
           (compile-inverse-condition cases else-target) ...
           b then-target
         else-label 
           }))])

; The inverse of compile-condition. It branches to branch-target when the
; condition does not hold, and falls through when it does. It uses the
; opposite condition code.
; (/or c ...) branches only when no c holds. Each c is compiled with
; compile-condition, which jumps to a local then label on a match. If none
; match, the final b skips to branch-target.
(define-syntax-parser compile-inverse-condition #:datum-literals (/or)
  [(_ (lhs:register cond:condition rhs:register) branch-target:label-targ)
   #'{cmp lhs rhs
      cond.branch-inverse-opcode branch-target}]

  [(_ (lhs:register cond:condition #:immediate n) branch-target:label-targ)
   #'{cmp lhs @n
      cond.branch-inverse-opcode branch-target}]

  [(_ (/or cases ...) else-target:label-targ)
   ; this has come from an AND since we are in inverse.
   ; the target passed is the fail case for the parent AND
   ; here then we want to go to that label if none of the ORs
   ; mathc, or short circuit to a new label after these ORs if any match
   (let ([then-start (gen-label "then-start")])
     (with-syntax
       ([then-label (format-label then-start)]
        [then-target (format-target then-start '+)])
        #'{
           (compile-condition cases then-target) ...
           b else-target
         then-label 
           }))])

; (/when (/or c ...) body): runs body if any c holds. A matching c branches
; over the skip, and otherwise b end skips the body.
(define-syntax-parser /when-or
  [(_ (conditions ...) then-body) 
   (let ([then-start (gen-label "then-start")]
         [if-end (gen-label "if-end")]         )
     (with-syntax
       ([then-label (format-label then-start)]
        [then-target (format-target then-start '+)]
        [end-label (format-label if-end)]
        [end-target (format-target if-end '+)])
        #'{
           (compile-condition conditions then-target) ...
           b end-target
         then-label
           then-body
         end-label
           }))])

; (/when (/and c ...) body): runs body only if every c holds. Each c is
; inverted, so a failed c jumps past the body.
(define-syntax-parser /when-and
  [(_ (conditions ...) then-body) 
   (let ([if-end (gen-label "if-end")])
     (with-syntax
       ([end-label (format-label if-end)]
        [end-target (format-target if-end '+)])
        #'{
           (compile-inverse-condition conditions end-target) ...
           then-body
         end-label
           }))])

; (/if (/and c ...) then else): runs then if every c holds, otherwise else.
; Any failed c jumps to the else block.
(define-syntax-parser /if-and
  [(_ (conditions ...) then-body else-body) 
   (let (
         [else-start (gen-label "else-start")]
         [if-end (gen-label "if-end")])
     (with-syntax
       (
        [else-label (format-label else-start)]
        [else-target (format-target else-start '+)]
        [end-label (format-label if-end)]
        [end-target (format-target if-end '+)])
        #'{
           (compile-inverse-condition conditions else-target) ...
           then-body
           b end-target
         else-label
           else-body
         end-label
           }))])

; (/if (/or c ...) then else): runs then if any c holds, otherwise else.
; The first matching c jumps to the then block.
(define-syntax-parser /if-or
  [(_ (conditions ...) then-body else-body) 
   (let ([then-start (gen-label "then-start")]
         [if-end (gen-label "if-end")]         )
     (with-syntax
       ([then-label (format-label then-start)]
        [then-target (format-target then-start '+)]
        [end-label (format-label if-end)]
        [end-target (format-target if-end '+)])
        #'{
           (compile-condition conditions then-target) ...
           else-body           
           b end-target
         then-label
           then-body
         end-label
           }))])

; (/if cond then else) and (/if (/and ...) ...) / (/if (/or ...) ...).
; Layout for a single condition:
;     cond branches to then
;     else-body
;     b end
;   then: then-body
;   end:
; The /and and /or forms dispatch to /if-and and /if-or.
(define-syntax-parser /if #:datum-literals (/and /or)
  [(_ (/and conditions ...) then-expr:expr else-expr:expr)
   #'(/if-and
      (conditions ...)
      then-expr
      else-expr
     )]

  [(_ (/or conditions ...) then-expr:expr else-expr:expr)
   #'(/if-or
      (conditions ...)
      then-expr
      else-expr
      )]
  ; single condition
  [(_ condition:expr then-body:expr else-body:expr)
   (let ([then-start (gen-label "then-start")]
         [else-start (gen-label "else-start")]
         [if-end (gen-label "if-end")])
     (with-syntax
       ([then-label (format-label then-start)]
        [then-target (format-target then-start '+)]
        [else-label (format-label else-start)]
        [else-target (format-target else-start '+)]
        [end-label (format-label if-end)]
        [end-target (format-target if-end '+)])
        #'{
           (compile-condition condition then-target)
         else-label
           else-body
           b end-target
         then-label
           then-body
         end-label
           }))])

; (/when cond body): runs body when cond holds. Otherwise b end skips it.
; (/when (/and ...) body) and (/when (/or ...) body) dispatch to
; /when-and and /when-or.
(define-syntax-parser /when #:datum-literals (/and /or)
  [(_ (/and conditions ...) then-expr:expr)
   #'(/when-and
      (conditions ...)
      then-expr
     )]

  [(_ (/or conditions ...) then-expr:expr)
   #'(/when-or
      (conditions ...)
      then-expr
      )]

  ;single condition
  [(_ condition:expr then-body:expr)
   (let ([then-start (gen-label "then-start")]
         [if-end (gen-label "if-end")])
     (with-syntax
       ([then-label (format-label then-start)]
        [then-target (format-target then-start '+)]
        [end-label (format-label if-end)]
        [end-target (format-target if-end '+)])
        #'{
           (compile-condition condition then-target)
           b end-target
           then-label then-body
           end-label
          }))])
  
; (/cond ([test expr] ... [else expr])) is a chain of /if. Each test is any
; /if condition, so /and and /or work. The else clause must come last. With
; two clauses and no else, the second clause becomes a /when. A single
; clause with no else matches no pattern, so it is a syntax error. Use /if
; or /when for that.
(define-syntax-parser /cond #:datum-literals (else)
  [(_ ([test-expr eval-expr] [else else-expr]))
   #'(/if test-expr eval-expr else-expr)]

  [(_ ([test-expr eval-expr] [test2-expr eval2-expr]))
   #'(/if test-expr eval-expr (/when test2-expr eval2-expr))]

  [(_ ([test-expr eval-expr] cases ...+))
   #'(/if test-expr
          eval-expr
          (/cond (cases ...)))])


; Loops. The top-tested forms (/for, /while, /whilenz, /whilez) test before
; the body, so the body may run zero times. The /do- forms test after the
; body, so it always runs at least once. There is no break or continue.
; Branching to a label placed after the loop works as a manual break.
; //////


; (/for init cond update body): runs init, then jumps to the test. Each pass
; runs body and then update, and the test branches back to the body while
; cond holds. Layout:
;     init
;     b test
;   start: body
;     update
;   test: cond branches to start
(define-syntax-parser /for
  [(_ initializer:expr condition:expr update:expr body:expr)
   (let ([loop-start (gen-label "for-start")]
         [test-start (gen-label "for-test")])
     (with-syntax
       ([start-label (format-label loop-start)]
        [start-target (format-target loop-start '-)]
        [test-label (format-label test-start)]
        [test-target (format-target test-start '+)])
        #'{initializer
           b test-target
         start-label
           body
           update
         test-label
           (compile-condition condition start-target)
           }))])

; (/whilenz reg body): repeats body while reg is non-zero. The test runs
; first, so a zero reg on entry skips the body. Layout:
;   start: cbz reg end
;     body
;     b start
;   end:
(define-syntax-parser /whilenz
  [(_ test-register:register body)
   (let ([loop-start (gen-label "whilenz-start")]
         [loop-end (gen-label "whilenz-end")])
     (with-syntax
       ([start-label (format-label loop-start)]
        [start-target (format-target loop-start '-)]
        [end-label (format-label loop-end)]
        [end-target (format-target loop-end '+)])
        #'{
         start-label
           cbz test-register end-target
           body
           b start-target
         end-label
           }))])

; (/do-whilenz reg body): runs body, then repeats while reg is non-zero.
; The body runs at least once. If the body counts reg down and reg starts
; at 0, it wraps around and the loop runs for about 2^64 times. Layout:
;   start: body
;     cbnz reg start
(define-syntax-parser /do-whilenz
  [(_ test-register:register body)
   (let ([loop-start (gen-label "whilenz-start")])
     (with-syntax
       ([start-label (format-label loop-start)]
        [start-target (format-target loop-start '-)])
        #'{
           start-label body
           cbnz test-register start-target
           }))])

; (/whilez reg body): repeats body while reg is zero. The test runs first,
; so a non-zero reg on entry skips the body. Layout:
;   start: cbnz reg end
;     body
;     b start
;   end:
(define-syntax-parser /whilez
  [(_ test-register:register body)
   (let ([loop-start (gen-label "whilez-start")]
         [loop-end (gen-label "whilez-end")])
     (with-syntax
       ([start-label (format-label loop-start)]
        [start-target (format-target loop-start '-)]
        [end-label (format-label loop-end)]
        [end-target (format-target loop-end '+)])
        #'{
         start-label
           cbnz test-register end-target
           body
           b start-target
         end-label
           }))])

; (/do-whilez reg body): runs body, then repeats while reg is zero. The body
; runs at least once. Layout:
;   start: body
;     cbz reg start
(define-syntax-parser /do-whilez
  [(_ test-register:register body)
   (let ([loop-start (gen-label "whilez-start")])
     (with-syntax
       ([start-label (format-label loop-start)]
        [start-target (format-target loop-start '-)])
        #'{
         start-label
           body
           cbz test-register start-target
           }))])

; (/while cond body): repeats body while cond holds. The test runs first, so
; the body may run zero times. Layout:
;     b test
;   start: body
;   test: cond branches to start
(define-syntax-parser /while
  [(_ condition body)
   (let ([loop-start (gen-label "while-start")]
         [test-start (gen-label "while-test")])
     (with-syntax
       ([start-label (format-label loop-start)]
        [start-target (format-target loop-start '-)]
        [test-label (format-label test-start)]
        [test-target (format-target test-start '+)])
        #'{
           b test-target
         start-label
           body
         test-label
           (compile-condition condition start-target)
          }))])

; (/do-while cond body): runs body, then repeats while cond holds. The body
; runs at least once. Layout:
;   start: body
;     cond branches to start
(define-syntax-parser /do-while
  [(_ condition body)
   (let ([loop-start (gen-label "while-start")])
     (with-syntax
       ([start-label (format-label loop-start)]
        [start-target (format-target loop-start '-)])
        #'{
         start-label
           body
           (compile-condition condition start-target)
          }))])
