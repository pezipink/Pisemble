#lang pisemble
(require (for-syntax syntax/parse racket/stxparam racket/syntax))
(require syntax/parse/define)
(provide load-immediate /struct)

(define-syntax-parser load-immediate ;todo: probably want this as a core macro in pisemble itself?
  [(_ target:register value:expr)
   #'(cond
       [(< value (expt 2 16))
        { mov target @value  }]
       [(< value (expt 2 32))
        {
         mov target @(bitwise-and value $FFFF)
         movk target @(arithmetic-shift value -16) LSL @16
         }]
       [else (error (format "load-immediate: value too large, not currently supported ~a" value))])])

(define-syntax-parser asm-get-field
  [(_ ctx field-name size offset)
   #:when (equal? (eval (syntax-e #'size)) 8)
   #:with ident (format-id #'ctx "~a-get" #'field-name)
   #'(define-syntax (ident stx)
       (syntax-parse stx
         [(_ target:register base:register)
          #:with actual-target (format-id this-syntax "X~a" (syntax-e #'target.regnum))
          #'{ldr actual-target (base @offset)}]))]

  [(_ ctx field-name size offset)
   #:when (equal? (eval (syntax-e #'size)) 4)
   #:with ident (format-id #'ctx "~a-get" #'field-name)
   #'(define-syntax (ident stx)
       (syntax-parse stx
         [(_ target:register base:register)
          #:with actual-target (format-id this-syntax "W~a" (syntax-e #'target.regnum))
          #'{ldr actual-target (base @offset)}]))]

  [(_  ctx field-name size offset)
   #:when (equal? (eval (syntax-e #'size)) 2)
   #:with ident (format-id #'ctx "~a-get" #'field-name)
   #'(define-syntax (ident stx)
       (syntax-parse stx
         [(_ target:register base:register)
          #:with actual-target (format-id this-syntax "W~a" (syntax-e #'target.regnum))
          #'{ldrh actual-target (base @offset)}]))]

  [(_  ctx field-name size offset)
   #:when (equal? (eval (syntax-e #'size)) 1)
   #:with ident (format-id #'ctx "~a-get" #'field-name)
   #'(define-syntax (ident stx)
       (syntax-parse stx
         [(_ target:register base:register)
          #:with actual-target (format-id this-syntax "W~a" (syntax-e #'target.regnum))
          #'{ldrb actual-target (base @offset)}]))]

  [(_ ctx field-name size offset)
   #:with ident (format-id #'ctx "~a-get" #'field-name)
   #'(define-syntax (ident stx)
       (syntax-parse stx
         [(_ target:register base:register)
          #'(error "cannot get this field, it is too large to fit in a register!")]))])

(define-syntax-parser asm-set-field
  [(_ ctx field-name size offset)
   #:when (equal? (eval (syntax-e #'size)) 8)
   #:with ident (format-id #'ctx "~a-set" #'field-name)
   #'(define-syntax (ident stx)
       (syntax-parse stx
         [(_ target:register base:register)
          #:with actual-target (format-id this-syntax "X~a" (syntax-e #'target.regnum))
          #'{str actual-target (base @offset)}]))]

  [(_ ctx field-name size offset)
   #:when (equal? (eval (syntax-e #'size)) 4)
   #:with ident (format-id #'ctx "~a-set" #'field-name)
   #'(define-syntax (ident stx)
       (syntax-parse stx
         [(_ target:register base:register)
          #:with actual-target (format-id this-syntax "W~a" (syntax-e #'target.regnum))
          #'{str actual-target (base @offset)}]
         [(_ target:expr base:register)
          #'{(load-immediate w0 target)
             str w0 (base @offset)}]))]

  [(_  ctx field-name size offset)
   #:when (equal? (eval (syntax-e #'size)) 2)
   #:with ident (format-id #'ctx "~a-set" #'field-name)
   #'(define-syntax (ident stx)
       (syntax-parse stx
         [(_ target:register base:register)
          #:with actual-target (format-id this-syntax "W~a" (syntax-e #'target.regnum))
          #'{strh actual-target (base @offset)}]))]

  [(_  ctx field-name size offset)
   #:when (equal? (eval (syntax-e #'size)) 1)
   #:with ident (format-id #'ctx "~a-set" #'field-name)
   #'(define-syntax (ident stx)
       (syntax-parse stx
         [(_ target:register base:register)
          #:with actual-target (format-id this-syntax "W~a" (syntax-e #'target.regnum))
          #'{strb actual-target (base @offset)}]))]

  [(_ ctx field-name size offset)
   #:with ident (format-id #'ctx "~a-set" #'field-name)
   #'(define-syntax (ident stx)
       (syntax-parse stx
         [(_ target:register base:register)
          #'(error "cannot set this field, it is too large to fit in a register!")]))]
  )


(define-syntax-parser /struct
  [(_ struct-name:id ([member-name:id size:expr]...))
   #:with size-name (format-id this-syntax "~a/sizeof" #'struct-name)
   #:with ([field-name field-size offset] ... )
   (let-values
       ([(res off)
         (for/fold
             ([result '()][offsets '(0)])
             ([name (syntax->list #'(member-name ...))]
              [size (syntax->list #'(size ...))])
           (values
            (cons
             (list
              ; syntax object for the function identifier
              (format-id this-syntax "~a/~a" #'struct-name (syntax-e name))
              size
              ; syntax object represeting the addition of the accumulated offsets
              (datum->syntax this-syntax (cons '+ offsets)))
             result)
            ; accumulate field offsets
            (cons size offsets)))])
     res) 
   
   #'(begin
       ; generate sizeof struct
       (define (size-name) (+ size ...))
       (define-for-syntax (size-name) (+ size ...))
       ; generate each field name offset
       (define (field-name) offset) ...
       (define-for-syntax (field-name) offset) ...
       (asm-get-field struct-name field-name field-size offset) ...
       (asm-set-field struct-name field-name field-size offset) ...
       )
   ])


