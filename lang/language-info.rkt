;Pisemble
;Copyright Ross McKinlay, 2022

#lang racket/base

(provide get-language-info)

(define (get-language-info data)
  (lambda (key default)
    (case key
      [(configure-runtime)
       '(#[pisemble/lang/runtime-config configure #f])]
      [else default])))