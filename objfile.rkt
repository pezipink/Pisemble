#lang racket
(require racket/match)
; 'pisemobj' 

(define magic-number #x6a626f6d65736970)

(struct obj-file (
  ; a string table is a hash of int * string (symbol strings - not supporting runtime strings yet)
  string-table
  ; runtime only reverse string lookup (not serialized)
  reverse-string-table
  ; list of string indexes representing which .obj files are already linked
  object-composition
  ; list of (string index * offset) offset into data block for global symbol name
  symbol-table
  ; places in the code that need their jump locations updated to a symbol location
  ; a list of (string index * offset)
  ; this only ever happens when creating an image file, not obj files
  target-table
  ; actual bytes
  data )#:transparent #:mutable)



(define (create-blank-obj-file)
  (obj-file
   (make-immutable-hash)
   (make-immutable-hash)
   '()
   '()
   '()
   (make-vector 0)))

(define (read-8 port)
  (read-byte port))
(define (read-16 port)
  (bitwise-ior (read-8 port) (arithmetic-shift (read-8 port) 8)))
(define (read-32 port)
  (bitwise-ior (read-16 port) (arithmetic-shift (read-16 port) 16)))
(define (read-64 port)
  (bitwise-ior (read-32 port) (arithmetic-shift (read-32 port) 32)))

(define (write-8 number port) (write-byte (bitwise-and number #xFF) port))

(define (write-16 number port)
  (write-8 number port)
  (write-8 (arithmetic-shift number -8) port))

(define (write-32 number port)
  (write-16 number port)
  (write-16 (arithmetic-shift number -16) port))

(define (write-64 number port)
  (write-32 number port)
  (write-32 (arithmetic-shift number -32) port))

(define (write-obj-file obj-file filename)
  (define out (open-output-file filename #:exists 'replace #:mode 'binary))
  (write-64 magic-number out)
  ; write string table
  ; first the count
  (write-32 (hash-count (obj-file-string-table obj-file)) out)
  ; now each index followed by each string
  (for ([(x y) (obj-file-string-table obj-file)])
    (write-32 x out)
    (write-32 (string-length y) out)
    (write-string y out))

  ; write obj assoc list
  (write-32 (length (obj-file-object-composition obj-file)) out)
  (for ([x (obj-file-object-composition obj-file)])
    (write-32 x out))

  (write-32 (length (obj-file-symbol-table obj-file)) out)
  ; write global symbol list
  (for ([x (obj-file-symbol-table obj-file)])
    (write-32 (car x) out)
    (write-64 (cdr x) out))

  (write-32 (length (obj-file-target-table obj-file)) out)
  ; write target list
  (for ([x (obj-file-target-table obj-file)])
    (write-32 (car x) out)
    (write-64 (cdr x) out))

  ; output byte count  
  (write-64 (vector-length (obj-file-data obj-file)) out)
  (for ([n (in-range (vector-length (obj-file-data obj-file)))])
    (write-8 (vector-ref (obj-file-data obj-file) n) out))
  (close-output-port out))

(define (read-obj-file filename)
  (define in (open-input-file filename #:mode 'binary))
  (define magic (read-64 in))
  (when (not (equal? magic magic-number))
    (error (format "the file ~A is not a valid pisemble object file" filename)))
  (let* (
         [str-count (read-32 in)]
         [str-table
          (make-immutable-hash
           (for/list ([n (in-range str-count)])
             (let ([index (read-32 in)]
                   [str-len (read-32 in)])
               (cons index (read-string str-len in)))))]
         [oc-list          
          (for/list ([n (read-32 in)])
            (read-32 in))]
         [gsa-list          
          (for/list ([n (read-32 in)])
            (cons (read-32 in) (read-64 in)))]
         [targ-list          
          (for/list ([n (read-32 in)])
            (cons (read-32 in) (read-64 in)))]
         [data (let ([vec (make-vector (read-64 in))])
                 (for ([n (in-range (vector-length vec))])
                   (vector-set! vec n (read-8 in)))
                 vec)]
         )
                     
    (close-input-port in)
    (obj-file
     str-table
     (string-table-reverse str-table)
     oc-list
     gsa-list
     targ-list
     data)))

(define (create-dummy-obj)
  (let ([st
         (make-immutable-hash
          (list
           (cons 0 ":Hello")
           (cons 1 "test.obj")
           (cons 2 ":World")
))]
        [oa (list 1)]
        [gsa (list (cons 0 100)  ( cons 2 200))]
        [tg (list (cons 1 900)  )])
  (obj-file
   st
   (string-table-reverse st)
   oa
   gsa
   tg
   (list->vector '(42 90)))))
(define (create-dummy-obj-2)
  (let ([st
         (make-immutable-hash
          (list
           (cons 0 ":Function")
           (cons 1 ":Label")
           (cons 2 "test.obj")
           (cons 3 "test2.obj")
           (cons 4 ":Hello")))]
        [oa (list 2 3)]
        [gsa (list (cons 0 1000)  (cons 1 20000))]
        [tg (list (cons 4 23900)  )])
  (obj-file
   st
   (string-table-reverse st)
   oa
   gsa
   tg
   (list->vector '(42)))))

;; (write-obj-file (create-dummy-obj) "c:\\temp\\obj.obj")

;; (read-obj-file "c:\\temp\\obj.obj")

(define (string-table-reverse string-table)
  (for/hash ([(k v) string-table])
    (values v k)))

(define (string-table-max-index string-table)
  (if (equal? (hash-count string-table) 0)
      -1
      (apply max (hash-keys string-table))))

(define (produce-string-mapping
         core-string-table
         reverse-string-table
         merge-string-table)
  ; when we merge/link object files, they might have some of the same strings.
  ; for new strings, the indexes are probably already used.
  ; we need to produce a new string index table, and a mapping from
  ; mergee index -> new index.
  (let-values
      ([(x new-map translation)
        (let* ([core-rev reverse-string-table]
               [core-max-index (string-table-max-index core-string-table)])
          (for/fold
              ([new-index (+ 1 core-max-index)]
               [result-map core-string-table]
               [translation (make-immutable-hash)])
              ([(k v) merge-string-table])
            (if (hash-has-key? core-rev v)
                ; this string already exists, keep the existing table
                ; and generate the translation
                (values new-index result-map (hash-set translation k (hash-ref core-rev v)))
                ; otherwise add this string with a new index                
                (values
                 (+ 1 new-index)
                 (hash-set result-map new-index v)
                 (hash-set translation k new-index)))))])
    (cons new-map translation)))


                   
(define (update-obj-with-string-translations merge-obj translations)
  ; this will fix up all string indexes in the obj, except for the
  ; string table itself; since its purpose is to prepare the data
  ; for merging into the core obj file
  (obj-file
   (make-immutable-hash) ; blank string table
   (make-immutable-hash) ; blank reverse string table
   ; obj composition
   (map (λ (x) (hash-ref translations x)) (obj-file-object-composition merge-obj))
   ; sym table
   (map (match-lambda [(cons x y) (cons (hash-ref translations x) y)]) (obj-file-symbol-table merge-obj))
   ;targets
   (map (match-lambda [(cons x y) (cons (hash-ref translations x) y)]) (obj-file-target-table merge-obj))
 
   (obj-file-data merge-obj)))

(define (append-data-aligned existing-data new-data)
  (define current-count (vector-length existing-data))
  (define (aux i)
    (if (eq? (remainder i 16) 0)
        i
        (aux (+ i 1))))
  ;we only append new blocks 16 byte aligned
  (define new-start (aux current-count))
  (define total-length (+ new-start (vector-length new-data)))
  (define vec (make-vector total-length))
  (vector-copy! vec 0 existing-data)
  (vector-copy! vec new-start new-data)
  (cons new-start vec))



(define (merge-objs core-obj merge-obj)
  ; merge the objects together, extending the data instruction-aligned
  (match-let*
      (
       ; first produce a new string table and translation mapping
       [(cons new-strings string-translations)
        (produce-string-mapping
         (obj-file-string-table core-obj)
         (obj-file-reverse-string-table core-obj)
         (obj-file-string-table merge-obj))]
       ; update the merge object with the new string indexes
       [merge-obj (update-obj-with-string-translations merge-obj string-translations)]
       ; recreate reverse lookup

       ; update composition table; we can ignore anything that exists in core 
       [object-comp
        (let*([indexes (for/hash ([x (obj-file-object-composition core-obj)]) (values x #t))]
              [new-items
               (for/fold
                   ([new-items '()])
                   ([x (obj-file-object-composition merge-obj)])
                 (if (hash-has-key? indexes x)
                     new-items
                     (cons x new-items)))]
              [new-items (append new-items (obj-file-object-composition core-obj))])
          (sort new-items <))]
              

       ; align-merge the data blocks and determine new offset for merged data
       [(cons merge-data-offset data)
        (append-data-aligned
         (obj-file-data core-obj)
         (obj-file-data merge-obj))]


       ; update the symbol table - skipping any symbols already defined
       ; in the core-obj. we can use the string index since they were updated 
       [symbol-table
        (let*
            ([indexes (for/hash ([x (obj-file-symbol-table core-obj)]) (values x #t))]
             [new-items
              (for/fold
                  ([new-items '()])
                  ([x (obj-file-symbol-table merge-obj)])
                (match-let ([(cons k v) x])
                  (if (hash-has-key? indexes k)
                      new-items
                      ; apply the new offset to the symbol location
                      (cons  (cons k (+ merge-data-offset v)) new-items))))]
             [new-items
              (append (obj-file-symbol-table core-obj)
                      new-items)])
          (sort new-items < #:key car)
          )]

       [target-table
        (let*
            ([indexes (for/hash ([x (obj-file-target-table core-obj)]) (values x #t))]
             [new-items
              (for/fold
                  ([new-items '()])
                  ([x (obj-file-target-table merge-obj)])
                (match-let ([(cons k v) x])
                  (if (hash-has-key? indexes k)
                      new-items
                      ; apply the new offset to the symbol location
                      (cons  (cons k (+ merge-data-offset v)) new-items))))]
             [new-items
              (append (obj-file-target-table core-obj)
                      new-items)])
          (sort new-items < #:key car)
          )]
          
       
       )
    (obj-file
     new-strings
     (string-table-reverse new-strings)
     object-comp
     symbol-table
     target-table
     data)))

 
(define (add-or-get-string obj str)
  (define table (obj-file-string-table obj))
  (define new-index (+ 1 (string-table-max-index table)))
  (define rev-index (obj-file-reverse-string-table obj))
  (if (hash-has-key? rev-index str)
      (cons (hash-ref rev-index str) obj)
      (cons new-index
            (struct-copy obj-file obj
                         [string-table (hash-set table new-index str)]
                         [reverse-string-table (hash-set rev-index str new-index)]))))

(define (add-external-target obj str location)
  (match-define (cons index new-obj) (add-or-get-string obj str))
  (struct-copy obj-file new-obj
               [target-table (cons (cons index location) (obj-file-target-table new-obj))]))



;; (write-obj-file 
;; (let ([a (create-dummy-obj)]
;;       [b (create-dummy-obj-2)])
;;   (writeln a)
;;   (merge-objs a b))

;; "c:\\testz.obj")



;(add-or-get-string (read-obj-file "object1.obj") "test")
;(read-obj-file "object1.obj")
;(read-obj-file "object2.obj")
;(merge-objs (read-obj-file "object1.obj")(read-obj-file "object2.obj"))
(provide
 create-blank-obj-file
 add-or-get-string
 read-obj-file
 write-obj-file
 merge-objs
 add-external-target
 append-data-aligned
 
 obj-file-string-table
 set-obj-file-string-table!
 obj-file-reverse-string-table
 set-obj-file-reverse-string-table!
 obj-file-object-composition
 set-obj-file-object-composition!
 obj-file-symbol-table
 set-obj-file-symbol-table!
 obj-file-target-table
 set-obj-file-target-table!
 obj-file-data
 set-obj-file-data!

         )


