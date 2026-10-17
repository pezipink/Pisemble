#lang racket/base
;; Wraps a raw image in an AArch64 ELF with DWARF 4 line info, so gdb can map
;; addresses back to source lines. QEMU still loads the raw image.
(require racket/list racket/string)
(provide write-debug-elf)

(define base-address #x80000)
(define text-file-offset #x1000)

(define (le n k)
  (apply bytes (for/list ([i k]) (bitwise-and (arithmetic-shift n (* -8 i)) 255))))

(define (uleb n)
  (let loop ([n n] [acc '()])
    (define b (bitwise-and n #x7f))
    (define rest (arithmetic-shift n -7))
    (if (zero? rest)
        (apply bytes (reverse (cons b acc)))
        (loop rest (cons (bitwise-ior b #x80) acc)))))

(define (sleb n)
  (let loop ([n n] [acc '()])
    (define b (bitwise-and n #x7f))
    (define rest (arithmetic-shift n -7))
    (define sign-set? (not (zero? (bitwise-and b #x40))))
    (if (or (and (zero? rest) (not sign-set?))
            (and (= rest -1) sign-set?))
        (apply bytes (reverse (cons b acc)))
        (loop rest (cons (bitwise-ior b #x80) acc)))))

(define (cstr s) (bytes-append (string->bytes/utf-8 s) #"\0"))
(define (align n a) (* a (quotient (+ n a -1) a)))

;; abbrevs: 1 compile unit, 2 base type, 3 subprogram, 4 lexical block, 5 variable
(define debug-abbrev
  (bytes-append
   (uleb 1) (uleb #x11) (bytes 1)  ; compile_unit, has children
   (uleb #x25) (uleb #x08)   ; producer, string
   (uleb #x13) (uleb #x05)   ; language, data2
   (uleb #x03) (uleb #x08)   ; name, string
   (uleb #x1b) (uleb #x08)   ; comp_dir, string
   (uleb #x11) (uleb #x01)   ; low_pc, addr
   (uleb #x12) (uleb #x07)   ; high_pc, data8 (length)
   (uleb #x10) (uleb #x17)   ; stmt_list, sec_offset
   (bytes 0 0)
   (uleb 2) (uleb #x24) (bytes 0)  ; base_type, no children
   (uleb #x0b) (uleb #x0b)   ; byte_size, data1
   (uleb #x3e) (uleb #x0b)   ; encoding, data1
   (uleb #x03) (uleb #x08)   ; name, string
   (bytes 0 0)
   (uleb 3) (uleb #x2e) (bytes 1)  ; subprogram, has children
   (uleb #x03) (uleb #x08)   ; name, string
   (uleb #x11) (uleb #x01)   ; low_pc, addr
   (uleb #x12) (uleb #x07)   ; high_pc, data8 (length)
   (uleb #x40) (uleb #x18)   ; frame_base, exprloc
   (bytes 0 0)
   (uleb 4) (uleb #x0b) (bytes 1)  ; lexical_block, has children
   (uleb #x11) (uleb #x01)   ; low_pc, addr
   (uleb #x12) (uleb #x07)   ; high_pc, data8 (length)
   (bytes 0 0)
   (uleb 5) (uleb #x34) (bytes 0)  ; variable, no children
   (uleb #x03) (uleb #x08)   ; name, string
   (uleb #x49) (uleb #x13)   ; type, ref4
   (uleb #x02) (uleb #x18)   ; location, exprloc
   (bytes 0 0)
   (uleb 6) (uleb #x17) (bytes 1)  ; union_type, has children
   (uleb #x03) (uleb #x08)   ; name, string
   (uleb #x0b) (uleb #x0b)   ; byte_size, data1
   (bytes 0 0)
   (uleb 7) (uleb #x0d) (bytes 0)  ; member, no children
   (uleb #x03) (uleb #x08)   ; name, string
   (uleb #x49) (uleb #x13)   ; type, ref4
   (uleb #x38) (uleb #x0b)   ; data_member_location, data1
   (bytes 0 0)
   (bytes 0)))


;; rows: (offset file line col) from the assembler, offsets relative to the image.
;; Returns (addr file line) sorted by address. At one address the lowest line is
;; last, because gdb resolves an address to the last row that starts there.
(define (usable-rows image-length records)
  (sort
   (for/list ([r (in-list records)]
              #:when (and (<= 0 (car r) image-length)
                          (not (equal? (cadr r) ""))
                          (> (caddr r) 0)))
     (list (car r) (string-replace (cadr r) "\\" "/") (caddr r)))
   (lambda (a b) (or (< (car a) (car b))
                     (and (= (car a) (car b)) (> (caddr a) (caddr b)))))))

(define (line-program rows image-length files)
  (define-values (parts addr line file)
    (for/fold ([parts (list (bytes 0 9 2) (le base-address 8))]
               [addr base-address] [line 1] [file 1])
              ([r (in-list rows)])
      (define target (+ base-address (car r)))
      (define fi (add1 (index-of files (cadr r))))
      (define new-parts
        (append
         (if (> target addr) (list (bytes 2) (uleb (- target addr))) '())
         (if (= fi file) '() (list (bytes 4) (uleb fi)))
         (if (= (caddr r) line) '() (list (bytes 3) (sleb (- (caddr r) line))))
         (list (bytes 1))))
      (values (append parts new-parts) target (caddr r) fi)))
  (apply bytes-append
         (append parts
                 (list (bytes 2) (uleb (- (+ base-address image-length) addr))
                       (bytes 0 1 1)))))

(define (debug-line files program)
  (define header-rest
    (bytes-append (bytes 1 1 1 #xfb 14 13)               ; min_inst, max_ops, default_is_stmt, line_base -5, line_range, opcode_base
                  (bytes 0 1 1 1 1 0 0 0 1 0 0 1)        ; standard opcode lengths
                  (bytes 0)                              ; no include directories
                  (apply bytes-append
                         (append (for/list ([f (in-list files)])
                                   (bytes-append (cstr f) (bytes 0 0 0)))
                                 (list (bytes 0))))))
  (define body
    (bytes-append (le 4 2) (le (bytes-length header-rest) 4) header-rest program))
  (bytes-append (le (bytes-length body) 4) body))

(define section-names '(".text" ".debug_abbrev" ".debug_info" ".debug_line" ".shstrtab" ".symtab" ".strtab" ".debug_frame"))

;; sh_link and sh_info are only used by .symtab
(define (section-header name type flags addr off size #:link [link 0] #:info [info 0]
                        #:align [al 1] #:entsize [entsize 0])
  (bytes-append (le name 4) (le type 4) (le flags 8) (le addr 8) (le off 8) (le size 8)
                (le link 4) (le info 4) (le al 8) (le entsize 8)))

(define (pad-to contents-len target)
  (make-bytes (- target contents-len) 0))

;; funcs: (addr . name) with addresses relative to the image. Returns (start name end)
;; sorted by start. Each function runs to the next function's start, or to the end
;; of the image.
(define (function-ranges funcs image-length)
  (define starts
    (remove-duplicates
     (sort (filter (λ (f) (<= 0 (car f) image-length)) funcs) < #:key car)
     #:key car))
  (define ends (append (map car (if (null? starts) '() (cdr starts))) (list image-length)))
  (for/list ([f (in-list starts)] [end (in-list ends)])
    (list (car f) (cdr f) end)))

;; .debug_info: one compile unit holding the interpretation types and one subprogram
;; per function. Each rename scope inside a function becomes a lexical block whose
;; variables live in the scope's register. A rename is a union of its register's
;; interpretations (u64/i64/f64, or u32/i32/f32), all at offset 0.
(define base-types '((u64 8 7) (i64 8 5) (f64 8 4) (u32 4 7) (i32 4 5) (f32 4 4)))
(define union-types '((reg64 8 (u64 i64 f64)) (reg32 4 (u32 i32 f32))))

(define (debug-info name comp-dir image-length funcs scopes)
  (define cu-attrs
    (bytes-append (uleb 1)
                  (cstr "Pisemble") (le #x8001 2)   ; DW_LANG_Mips_Assembler
                  (cstr name) (cstr comp-dir)
                  (le base-address 8) (le image-length 8) (le 0 4)))
  ; the unit header is 11 bytes, and type refs are relative to it
  (define first-type (+ 11 (bytes-length cu-attrs)))
  (define base-dies
    (for/list ([b (in-list base-types)])
      (bytes-append (uleb 2) (bytes (cadr b) (caddr b)) (cstr (symbol->string (car b))))))
  (define base-offs (chain-offsets first-type (map bytes-length base-dies)))
  (define base-off-of
    (for/hasheq ([b (in-list base-types)] [off (in-list base-offs)]) (values (car b) off)))
  (define union-dies
    (for/list ([u (in-list union-types)])
      (bytes-append
       (uleb 6) (cstr (symbol->string (car u))) (bytes (cadr u))
       (apply bytes-append
              (for/list ([m (in-list (caddr u))])
                (bytes-append (uleb 7) (cstr (symbol->string m))
                              (le (hash-ref base-off-of m) 4) (bytes 0))))
       (bytes 0))))
  (define union-offs
    (chain-offsets (+ first-type (apply + (map bytes-length base-dies)))
                   (map bytes-length union-dies)))
  (define type-off
    (for/fold ([h base-off-of]) ([u (in-list union-types)] [off (in-list union-offs)])
      (hash-set h (car u) off)))
  (define subprograms
    (for/list ([r (in-list (function-ranges funcs image-length))])
      (subprogram r scopes type-off)))
  (define body
    (bytes-append (le 4 2) (le 0 4) (bytes 8)
                  cu-attrs
                  (apply bytes-append base-dies)
                  (apply bytes-append union-dies)
                  (apply bytes-append subprograms)
                  (bytes 0)))
  (bytes-append (le (bytes-length body) 4) body))

;; offsets of consecutive chunks that start at start
(define (chain-offsets start sizes)
  (let loop ([off start] [sizes sizes] [acc '()])
    (if (null? sizes)
        (reverse acc)
        (loop (+ off (car sizes)) (cdr sizes) (cons off acc)))))

(define (subprogram r scopes type-off)
  (define start (car r))
  (define end (caddr r))
  (define inside
    (sort (filter (λ (s) (and (<= start (car s)) (< (car s) end) (<= (cadr s) end)
                              (pair? (caddr s))))
                  scopes)
          < #:key car))
  (bytes-append
   (uleb 3) (cstr (cadr r)) (le (+ base-address start) 8) (le (- end start) 8)
   (uleb 1) (bytes #x9c)    ; frame base: DW_OP_call_frame_cfa
   (apply bytes-append
          (for/list ([s (in-list inside)])
            (lexical-block s type-off)))
   (bytes 0)))

(define (lexical-block s type-off)
  (define start (car s))
  (define end (cadr s))
  (bytes-append
   (uleb 4) (le (+ base-address start) 8) (le (- end start) 8)
   (apply bytes-append
          (for/list ([v (in-list (caddr s))])
            (variable v type-off)))
   (bytes 0)))

;; v is (name regnum is32). The location is the register itself. The type is the
;; union of the register's interpretations.
(define (variable v type-off)
  (define name (car v))
  (define regnum (cadr v))
  (define is32 (caddr v))
  (bytes-append
   (uleb 5) (cstr (symbol->string name))
   (le (hash-ref type-off (if is32 'reg32 'reg64)) 4)
   (uleb 1) (bytes (+ #x50 regnum))))   ; DW_OP_reg0 is 0x50

(define (function-symbols funcs image-length)
  (define ranges (function-ranges funcs image-length))
  (define name-offsets
    (let loop ([ns (map cadr ranges)] [o 1] [acc '()])
      (if (null? ns)
          (reverse acc)
          (loop (cdr ns) (+ o (bytes-length (cstr (car ns)))) (cons o acc)))))
  (define strtab (apply bytes-append (bytes 0) (map (λ (r) (cstr (cadr r))) ranges)))
  (define entries
    (for/list ([r (in-list ranges)] [name-off (in-list name-offsets)])
      (define start (+ base-address (car r)))
      (define end (+ base-address (caddr r)))
      ; STB_GLOBAL, STT_FUNC; st_shndx 1 is .text
      (bytes-append (le name-off 4) (bytes #x12 0) (le 1 2) (le start 8) (le (- end start) 8))))
  (values (apply bytes-append (make-bytes 24 0) entries) strtab))

;; .debug_frame: one CIE, then one FDE per function. The CFA is sp plus a depth
;; that follows the sp changes recorded by the assembler. x30 is saved at its
;; slot relative to the CFA, so gdb can unwind through calls.
(define (frame-entry content)
  (define len (align (+ 4 (bytes-length content)) 8))
  (bytes-append (le (- len 4) 4) content (make-bytes (- len 4 (bytes-length content)) 0)))

(define (frame-instructions start end frames)
  (define in-func
    (sort (filter (λ (r) (and (<= start (car r)) (< (car r) end))) frames) < #:key car))
  (define-values (ops depth at)
    (for/fold ([ops '()] [depth 0] [at start]) ([r (in-list in-func)])
      (define off (car r))
      (define new-depth (max 0 (- depth (cadr r))))
      (define lr (caddr r))
      (define slot (and lr (- new-depth lr)))
      ; DW_CFA_advance_loc4, DW_CFA_def_cfa_offset, DW_CFA_offset x30 (data alignment -8)
      (define advance (if (> off at) (list (bytes 4) (le (- off at) 4)) '()))
      (define depth-op (if (= new-depth depth) '() (list (bytes #x0e) (uleb new-depth))))
      (define lr-op (if (and slot (> slot 0) (zero? (remainder slot 8)))
                        (list (bytes (bitwise-ior #x80 30)) (uleb (quotient slot 8)))
                        '()))
      (values (append ops advance depth-op lr-op) new-depth (max at off))))
  (apply bytes-append ops))

(define (debug-frame funcs frames image-length)
  ; CIE: version 4, no augmentation, address size 8, code align 1, data align -8,
  ; return address x30, initial CFA = sp + 0
  (define cie
    (frame-entry (bytes-append (le #xffffffff 4) (bytes 4 0 8 0 1) (sleb -8)
                               (bytes 30 #x0c 31 0))))
  (apply bytes-append
         cie
         (for/list ([r (in-list (function-ranges funcs image-length))])
           (define start (car r))
           (define end (caddr r))
           (frame-entry (bytes-append (le 0 4)                      ; CIE pointer
                                      (le (+ base-address start) 8)
                                      (le (- end start) 8)
                                      (frame-instructions start end frames))))))

(define (write-debug-elf elf-path image records funcs frames scopes fallback-name)
  (define image-length (bytes-length image))
  (define rows (usable-rows image-length records))
  (define files (let ([names (remove-duplicates (map cadr rows))])
                  (if (null? names)
                      (list (string-replace fallback-name "\\" "/"))
                      names)))
  (define comp-name (car files))
  (define comp-dir (let ([m (regexp-match #rx"^(.*)/[^/]*$" comp-name)])
                     (if m (cadr m) ".")))

  (define abbrev debug-abbrev)
  (define info (debug-info comp-name comp-dir image-length funcs scopes))
  (define line (debug-line files (line-program rows image-length files)))
  (define frame (debug-frame funcs frames image-length))
  (define-values (symtab strtab) (function-symbols funcs image-length))

  (define shstr (apply bytes-append (bytes 0) (map cstr section-names)))
  (define name-offsets
    (let loop ([ns section-names] [o 1] [acc '()])
      (if (null? ns)
          (reverse acc)
          (loop (cdr ns) (+ o (string-length (car ns)) 1) (cons o acc)))))
  (define (name-of i) (list-ref name-offsets i))

  (define abbrev-off (+ text-file-offset image-length))
  (define info-off (+ abbrev-off (bytes-length abbrev)))
  (define line-off (+ info-off (bytes-length info)))
  (define frame-off (+ line-off (bytes-length line)))
  (define shstr-off (+ frame-off (bytes-length frame)))
  (define symtab-off (align (+ shstr-off (bytes-length shstr)) 8))
  (define strtab-off (+ symtab-off (bytes-length symtab)))
  (define shoff (align (+ strtab-off (bytes-length strtab)) 8))
  ; section indices: 0 null, 1 .text, ..., 5 .shstrtab, 6 .symtab, 7 .strtab, 8 .debug_frame
  (define shdrs
    (bytes-append
     (make-bytes 64 0)
     (section-header (name-of 0) 1 6 base-address text-file-offset image-length #:align 4)
     (section-header (name-of 1) 1 0 0 abbrev-off (bytes-length abbrev))
     (section-header (name-of 2) 1 0 0 info-off (bytes-length info))
     (section-header (name-of 3) 1 0 0 line-off (bytes-length line))
     (section-header (name-of 4) 3 0 0 shstr-off (bytes-length shstr))
     (section-header (name-of 5) 2 0 0 symtab-off (bytes-length symtab)
                     #:link 7 #:info 1 #:align 8 #:entsize 24)
     (section-header (name-of 6) 3 0 0 strtab-off (bytes-length strtab))
     (section-header (name-of 7) 1 0 0 frame-off (bytes-length frame) #:align 8)))

  (define ehdr
    (bytes-append #"\177ELF" (bytes 2 1 1 0 0) (make-bytes 7 0)
                  (le 2 2) (le 183 2) (le 1 4) (le base-address 8)
                  (le 64 8) (le shoff 8) (le 0 4)
                  (le 64 2) (le 56 2) (le 1 2) (le 64 2) (le 9 2) (le 5 2)))
  (define phdr
    (bytes-append (le 1 4) (le 7 4) (le text-file-offset 8) (le base-address 8)
                  (le base-address 8) (le image-length 8) (le image-length 8) (le #x1000 8)))

  (define head (bytes-append ehdr phdr))
  (define contents
    (bytes-append head
                  (make-bytes (- text-file-offset (bytes-length head)) 0)
                  image
                  abbrev info line frame shstr
                  (pad-to (+ shstr-off (bytes-length shstr)) symtab-off)
                  symtab strtab
                  (pad-to (+ strtab-off (bytes-length strtab)) shoff)
                  shdrs))
  (call-with-output-file elf-path #:exists 'replace
    (lambda (out) (write-bytes contents out))))

