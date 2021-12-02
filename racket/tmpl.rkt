#lang racket
(require "lib/common.rkt")

(define (dayNa lst)
  (void))

(define (dayNb lst)
  (void))

(module+ main
  (call-with-input-file "data/dayN.txt"
    (lambda (prt)
      (define lines (port->lines prt))
      (answer N 1 (dayNa lines))
      (answer N 2 (dayNb lines)))))
