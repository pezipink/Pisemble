#lang racket/base
;; Checks the words emitted by the loop macros in lib/expr.rkt.
;; The top-tested forms must branch past the body when the count is 0.
(require rackunit racket/runtime-path racket/file)

(define-runtime-path loop-program "loop-program.rkt")

(define (image-words)
  (define dir (make-temporary-file "pisemble-test~a" 'directory))
  (parameterize ([current-directory dir])
    (dynamic-require loop-program #f))
  (define bs (file->bytes (build-path dir "loop-program.img")))
  (delete-directory/files dir)
  (for/list ([i (in-range 0 (bytes-length bs) 4)])
    (integer-bytes->integer bs #f #f i (+ i 4))))

(define expected
  (list
   ; /whilenz x1: cbz past the loop, body, b back
   #xB4000061 #x91000442 #x17FFFFFE
   ; /do-whilenz x1: body, cbnz back
   #x91000442 #xB5FFFFE1
   ; /whilez x1: cbnz past the loop, body, b back
   #xB5000061 #x91000442 #x17FFFFFE
   ; /do-whilez x1: body, cbz back
   #x91000442 #xB4FFFFE1
   ; /while x1 ne 0: b to the test, body, cmp, b.ne back
   #x14000002 #x91000442 #xF100003F #x54FFFFC1
   ; /do-while x1 ne 0: body, cmp, b.ne back
   #x91000442 #xF100003F #x54FFFFC1
   ; /for: init, b to the test, body, update, cmp, b.lt back
   #xD2800003 #x14000003 #x91000442 #x91000463 #xF100087F #x54FFFFAB
   ; b.hs, b.lo, b.cs and b.cc all branch back to :here, so the offsets are 0, -1, -2, -3
   #x54000002 #x54FFFFE3 #x54FFFFC2 #x54FFFFA3
   ; /do-while (x1 hs x2) and (x1 lo x2): body, cmp, back-branch
   #x91000463 #xEB02003F #x54FFFFC2
   #x91000463 #xEB02003F #x54FFFFC3))

(test-case "loop macros emit the expected branch structure"
  (define words (image-words))
  (check-true (>= (length words) (length expected)))
  (check-equal? (for/list ([w (in-list words)] [_ (in-list expected)]) w)
                expected))
