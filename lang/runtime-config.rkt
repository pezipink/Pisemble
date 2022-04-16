;Pisemble
;Copyright Ross McKinlay, 2022

#lang racket/base

(provide configure)

(require (only-in pisemble/reader make-pis-readtable))

(define (configure data)
  (current-readtable (make-pis-readtable)))