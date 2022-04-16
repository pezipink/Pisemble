
;Pisemble
;Copyright Ross McKinlay, 2022
#lang racket
(require syntax/readerr)

(provide wrapper1
         make-pis-readtable)

(define (make-pis-readtable)
  (make-readtable (current-readtable)
                  #\{ 'terminating-macro read-lbrace
                  #\@ 'terminating-macro read-@
                  #\% 'terminating-macro read-%
                  #\£ 'terminating-macro read-£
                  #\$ 'terminating-macro read-$
                  ))

(define (wrapper1 thk)
  (parameterize ([current-readtable (make-pis-readtable)])
    (thk)))
 
(define read-lbrace
  (case-lambda
   [(ch in)
    (parse-arm-block in in (object-name in))]
   [(ch in src line col pos)
    (parse-arm-block in in src)]))

(define read-@
  (case-lambda
    [(ch in)
     #'#:immediate ]
    [(ch in src line col pos)
     #'#:immediate]))

(define read-£
  (case-lambda
    [(ch in)
     #'#:indirect ]
    [(ch in src line col pos)
     #'#:indirect]))

(define read-%
  (case-lambda
    [(ch in)
     (parse-number #\% (read in))]
    [(ch in src line col pos)
     (parse-number #\% (read in))]))

(define (read-hex-string in)
  (define (aux acc)
    (let ([c (peek-char in)])
      (cond
        [(string-contains? "0123456789abcdefABCDEF" (make-string 1 c))
         (aux (string-append acc (make-string 1 (read-char in))))]
        [else acc])))
  (aux ""))

(define read-$
  (case-lambda
    [(ch in)
     (parse-number #\$ (read-hex-string in))]
    [(ch in src line col pos)
      (parse-number #\$ (read-hex-string in))]))

(define (parse-number pre input)
  (let ([radix 
         (case pre
           [(#\$) 16]
           [(#\%) 2])]
         [str (cond [(number? input) (number->string input)]
                    [(symbol? input) (symbol->string input)]
                    [else input])])
    (string->number (string-replace str "_" "") radix)))

(define (parse-arm-block val in src)  
  (define (parse-arm-line acc paren-count)
    ; skip nested expressions, we assume a line ends at the first newline char
    ; outside of parens.
    (let ([c (peek-char in)])
      (cond
        [(and (equal? c #\;) (equal? paren-count 0))
         ;treat a comment as an end-of-line
         ;(writeln "comment")
         (begin
           (read-line in)
           
           `(arm-line ,@(reverse acc)))]
        
        [(and (equal? c #\newline) (equal? paren-count 0))
         ;(writeln "new line")
         (begin
           (read-char in)
           `(arm-line ,@(reverse acc)))]

        [(char-whitespace? c)
         (begin
           (read-char in)
           (parse-arm-line acc paren-count))]

        [(equal? c #\})
         `(arm-line ,@(reverse acc))]
        
        [else
         (parse-arm-line (cons [read-syntax "" in] acc) paren-count)])))

  (define (aux acc)
    (let ([c (peek-char in)])
      (cond
        [(equal? c #\})
         (begin
           (read-char in)
           `(arm-block ,@(reverse acc)))]
        [else
         (let ([c (parse-arm-line '() 0)])
           (if (equal? c '(arm-line))
               (aux acc)
               (aux (cons c acc))))])))

  (aux '()))
 
