#lang racket
(require "lib/common.rkt")
(require rackunit)

;; pt 1
;; Depth-Step -> Number
;; returns the proper int represtation of a step relative to depth
(define depth-calc
  (λ (d)
    (let ([n (string->number (cadr d))])
      (if (string=? "down" (car d))
          n
          (* -1 n)))))

(check-equal? (depth-calc (list "down" "2")) 2)
(check-equal? (depth-calc (list "up" "2")) -2)

;; abstraction
(define take-s
  (λ (f ls h d aim)
    (cond
      [(null? ls) (* h d)]
      [(string=? "forward" (caar ls))
       (let ([x (string->number (cadar ls))])
         (take-s f (cdr ls) (+ x h) (if (number? aim) (+ (* x aim) d) d) aim))]
      [else (let ([x (depth-calc (car ls))])
              (take-s f (cdr ls) h (f d x) (if (number? aim) (+ aim x) aim)))])))

;; [ListOf Step] Number Number -> Number
;; returns horizontal * depth after steps are processed
(define take-steps
  (λ (ls)
    (take-s (λ (d x) (+ x d)) ls 0 0 #f)))

(check-equal? (take-steps null) 0)
(check-equal? (take-steps (list (list "forward" "2"))) 0)
(check-equal? (take-steps (list (list "forward" "2") (list "up" "1"))) -2)
(check-equal? (take-steps (list (list "forward" "2") (list "down" "1"))) 2)

;; shhhhh these signatures arent 100% true, but what is these days?
;; [ListOf Step] -> Number
;; returns the result of multiplying final horizontal pos by final depth
(define day2a
  (λ (ls)
    (take-steps ls)))

;; pt 2
;; [ListOf Step] Number Number Number -> Number
;; returns horizontal * depth after steps are processed
(define take-steps-mod
  (λ (ls)
    (take-s (λ (d x) d) ls 0 0 0)))

(define sanity-check (list (list "forward" "5")
                           (list "down" "5")
                           (list "forward" "8")
                           (list "up" "3")
                           (list "down" "8")
                           (list "forward" "2")))
(check-equal? (take-steps-mod null) 0)
(check-equal? (take-steps-mod sanity-check) 900)

(define day2b
  (λ (ls)
    (take-steps-mod ls)))

;; main
(module+ main
    (call-with-input-file "data/day2.txt"
      (lambda (prt)
        (define lines
          (map (λ (instr) (string-split instr #rx" ")) (port->lines prt)))
        (answer 2 1 (day2a lines))
        (answer 2 2 (day2b lines)))))
