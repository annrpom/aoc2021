#lang racket
(require "lib/common.rkt"
         rackunit)

(define test '(16 1 2 0 4 2 7 1 2 14))

;; [ListOf Number] Number -> Number
;; returns the amount of fuel needed to align everything in list to number
(define crab-distance
  (λ (ls align)
    (for/fold ([sum 0])
              ([crab (in-list ls)])
      (+ sum (abs (- crab align))))))

;; [ListOf Number] -> Number
;; returns the minimum amount of crab fuel
(define min-fuel
  (λ (f ls)
    (define maxn (for/max ([i (in-list ls)]) i))
    (for/min ([i (in-range maxn)])
      (f ls (add1 i)))))

;; pt 1                    
(define day7a
  (λ (ls)
    (min-fuel crab-distance ls)))

;; pt 2

;; Number Number -> Number
;; returns the increase between each step from k to n
(define summation
  (λ (k n)
    (for/fold ([sum 0])
              ([i (in-range (abs (- k n)))]
               [j (in-naturals)])
      (+ sum (add1 j)))))

(check-equal? (summation 16 5) 66)

;; [ListOf Number] Number -> Number
;; returns the amount of fuel needed to align everything in list to number
(define crab-distance-summ
  (λ (ls align)
    (foldl + 0 (map (λ (k) (summation k align)) ls))))

(define day7b
  (λ (ls)
    (min-fuel crab-distance-summ ls)))

;; main
(module+ main
  (call-with-input-file "data/day7.txt"
    (lambda (prt)
      (define lines (map string->number (string-split (car (port->lines prt)) #rx",")))
      (answer 7 1 (day7a lines))
      (answer 7 2 (day7b lines)))))
