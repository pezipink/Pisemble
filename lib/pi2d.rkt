#lang pisemble
;; =====================================================================
;; pi2d - a 2D program on the Raspberry Pi 3 platform, without the low-level setup
;; =====================================================================
;; Usage, in your program file:
;;
;;     #lang pisemble
;;     (require pisemble/lib/pi2d)
;;     (pi2d #:width 1024 #:height 768 #:bpp 32
;;           #:init   () [x4] { ...runs once... }
;;           #:update () [x4] { ...runs once per frame, before render... }
;;           #:render ([page x0] [frame x1]) [x4 x5] { ...draws one frame... }
;;           ...any helper subrs and data, e.g. (write-values ...)...)
;;
;; Every hook is written like a subr: ([new old] ...) gives names to the
;; incoming registers, [used ...] lists the registers the hook clobbers
;; (they are saved and restored for you), and the last form is the code.
;; All hooks and the platform use x0-x28 freely; keep state in memory.
;;
;; #:width, #:height and #:bpp are optional. The defaults are 320, 200 and 32.
;; #:bpp 8 gives an 8 bit palette mode with one byte per pixel.
;; A hook you leave out does nothing.
;;
;; The render hook gets x0 = the back page to draw into and x1 = the frame
;; number. The platform flips the page after render returns.
;;
;; The program is assembled into <name>.obj, and <name>.img is built from
;; the platform and that object. Both go in the folder of the source file.
;; =====================================================================
(require "platform.rkt")
(provide pi2d (all-from-out "platform.rkt"))

;; Keyword arguments are read in order. #:width, #:height and #:bpp take one
;; value. Each hook takes three terms: ([new old] ...) [used ...] code.
;; Anything else is part of the body.
(define-syntax (pi2d stx)
  (define-values (w h b hooks body)
    (let loop ([as (cdr (syntax->list stx))] [w #'320] [h #'200] [b #'32]
               [hooks (hash)] [body '()])
      (define kw (and (pair? as) (keyword? (syntax-e (car as))) (syntax-e (car as))))
      (case kw
        [(#f) (if (null? as)
                  (values w h b hooks (reverse body))
                  (loop (cdr as) w h b hooks (cons (car as) body)))]
        [(#:width) (loop (cddr as) (cadr as) h b hooks body)]
        [(#:height) (loop (cddr as) w (cadr as) b hooks body)]
        [(#:bpp) (loop (cddr as) w h (cadr as) hooks body)]
        [(#:init #:update #:render)
         (unless (>= (length as) 4)
           (raise-syntax-error 'pi2d "hook needs args, used registers and code" stx (car as)))
         (loop (list-tail as 4) w h b
               (hash-set hooks kw (list (cadr as) (caddr as) (cadddr as)))
               body)]
        [else (raise-syntax-error 'pi2d "unknown keyword" stx (car as))])))
  (define (hook-subr label-sym kw)
    (define spec (hash-ref hooks kw #f))
    (define label (datum->syntax stx label-sym))
    (if spec
        (with-syntax ([label label] [args (car spec)] [used (cadr spec)] [code (caddr spec)])
          #'(subr label args used code))
        (with-syntax ([label label])
          #'(subr label () [] {}))))
  (define src (syntax-source stx))
  (define-values (dir base-name)
    (if (path-string? src)
        (let-values ([(dir name must-be-dir?) (split-path (path->complete-path src))])
          (values dir (regexp-replace #rx"[.][^.]*$" (path->string name) "")))
        (values (current-directory) "program")))
  (define obj-file (path->string (build-path dir (string-append base-name ".obj"))))
  (define img-file (path->string (build-path dir (string-append base-name ".img"))))
  (with-syntax ([obj obj-file] [img img-file]
                [w w] [h h] [b b]
                [init-form (hook-subr 'Pi2d-init '#:init)]
                [update-form (hook-subr 'Pi2d-update '#:update)]
                [render-form (hook-subr 'Pi2d-render '#:render)]
                [(body-form ...) (datum->syntax stx body)])
    #'(begin
        (set-size! w h)
        (set-bpp! b)
        (aarch64 obj []
          init-form
          update-form
          render-form
          body-form ...
          { (gfx-library) })
        (platform-image img obj))))
