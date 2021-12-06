#lang racket
(require "lib/common.rkt"
         rackunit)

;; pt 1                    
(define dayNa
  (λ (ls)
    (void)))

;; pt 2
(define dayNb
  (λ (ls)
    (void)))

;; main
(module+ main
  (call-with-input-file "data/dayN.txt"
    (lambda (prt)
      (define lines (port->lines prt))
      (answer N 1 (dayNa lines))
      (answer N 2 (dayNb lines)))))
