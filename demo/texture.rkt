#lang racket/base
;; Loads an image and returns a list of rgb texel words, row by row.
;; Plain Racket module: racket/draw does not read correctly inside #lang pisemble.
(require racket/draw racket/class)
(provide load-texture)

(define (load-texture path tex-size)
  (define src (read-bitmap path))
  (define w (send src get-width))
  (define h (send src get-height))
  (define side (max w h))
  (define dst (make-bitmap tex-size tex-size #t))
  (define dc (new bitmap-dc% [bitmap dst]))
  (send dc set-smoothing 'smoothed)
  (send dc set-scale (/ tex-size side) (/ tex-size side))
  (send dc draw-bitmap src (/ (- side w) 2) (/ (- side h) 2))
  (define argb (make-bytes (* 4 tex-size tex-size) 0))
  (send dst get-argb-pixels 0 0 tex-size tex-size argb)
  (for/list ([i (* tex-size tex-size)])
    (define a (bytes-ref argb (* 4 i)))
    (define (channel k) (quotient (* (bytes-ref argb (+ (* 4 i) k)) a) 255))
    (+ #xFF000000 (* 65536 (channel 3)) (* 256 (channel 2)) (channel 1))))
