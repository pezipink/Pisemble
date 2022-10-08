#lang pisemble
(require (for-syntax syntax/parse racket/stxparam racket/syntax))
(require syntax/parse/define)
(provide  /cond /if /when /for /whilenz /whilez /while)

(begin-for-syntax
  (define labels (make-hash))
  (define (format-label label)
    (string->symbol (format ":~a"  label)))
  (define (format-target label type)
    (case type
      ['+     (string->symbol (format "~a+" label))]
      ['-     (string->symbol (format "~a-" label))]
      [else (error (format "bad label type ~a" type))]))
             
  (define (gen-label name)
    (if (hash-has-key? labels name)
        (let ([val (hash-ref labels name)])
          (hash-set! labels name (+ val 1))
          (format "~a_~a" name (+ val 1)))
        (begin
          (hash-set! labels name 0)
          (format "~a_~a" name 0)))))

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
        [end-target (format-label if-end '+)])
        #'{
           (compile-condition condition then-target)
         else-label
           else-body
           b end-target
         then-label
           then-body
         end-label
           }))])

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
  
(define-syntax-parser /cond #:datum-literals (else)
  [(_ ([test-expr eval-expr] [else else-expr]))
   #'(/if test-expr eval-expr else-expr)]

  [(_ ([test-expr eval-expr] [test2-expr eval2-expr]))
   #'(/if test-expr eval-expr (/when test2-expr eval2-expr))]

  [(_ ([test-expr eval-expr] cases ...+))
   #'(/if test-expr
          eval-expr
          (/cond (cases ...)))])


; loops
; //////


(define-syntax-parser /for ; note this won't work for a loop that runs 0 times
  [(_ initializer:expr condition:expr update:expr body:expr)
   (let ([loop-start (gen-label "for-start")]
         [loop-end (gen-label "for-end")])
     (with-syntax
       ([start-label (format-label loop-start)]
        [start-target (format-target loop-start '-)]
        [end-label (format-label loop-end)]
        [end-target (format-target loop-end '+)]
        )
        #'{initializer
         start-label
           body
           update
           (/when condition {b start-target})
         end-label
           }))])

(define-syntax-parser /whilenz
  [(_ test-register:register body)
   (let ([loop-start (gen-label "whilenz-start")])
     (with-syntax
       ([start-label (format-label loop-start)]
        [start-target (format-target loop-start '-)])
        #'{
           start-label body
           cbnz test-register start-target
           }))])

(define-syntax-parser /whilez
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

(define-syntax-parser /while
  [(_ condition body)
   (let ([loop-start (gen-label "while-start")])
     (with-syntax
       ([start-label (format-label loop-start)]
        [start-target (format-target loop-start '-)])
        #'{
         start-label
           body
           (/when condition {b start-label})
          }))])
