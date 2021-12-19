#lang racket
(require "lib/common.rkt"
         rackunit)

;; these are from day 9 (back when everything was a vector):
(define vectorize
  (λ (ls)
    (for/vector ([row (in-list ls)])
      (for/vector ([col (in-list row)])
        (string->number col)))))

;; one alteration: this now includes the diagonally adjacents
(define adjacents
  (λ (row col)
    (define sanitize (λ (r c) (if (and (< -1 r 10) (< -1 c 10)) (cons r c) #f)))
    (filter (λ (t) t) (vector (sanitize (sub1 row) col)
                                     (sanitize row (add1 col))
                                     (sanitize row (sub1 col))
                                     (sanitize (add1 row) col)))))


;; pt 1                    
(define day11a
  (λ (ls)
    (void)))

;; pt 2
(define day11b
  (λ (ls)
    (void)))

;; main
#;(module+ main
  (call-with-input-file "data/day11.txt"
    (lambda (prt)
      (define lines (port->lines prt))
      (answer 11 1 (day11a lines))
      (answer 11 2 (day11b lines)))))

;; test
(module+ test
  (call-with-input-file "data/day11.test.txt"
    (lambda (prt)
      (define lines (port->lines prt))
      (displayln (day11a lines))
      (displayln (day11b lines)))))
