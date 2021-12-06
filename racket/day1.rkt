#lang racket
(require "lib/common.rkt")
(require rackunit)


;; pt 1
;; [ListOfNumber] -> [ListOf [PairOf Number]]
;; returns a list of pairs of (prior . current) if there is a prior and current that exists
(define pair-off
  (λ (ls)
    (cond
      [(or (null? ls) (null? (cdr ls))) null]
      [else (cons (cons (car ls) (cadr ls))
                  (pair-off (cdr ls)))])))

(check-equal? (pair-off '(1 2 3)) '((1 . 2) (2 . 3)))
(check-equal? (pair-off '(1)) null)
(check-equal? (pair-off null) null)

;; [ListOf [PairOf Number]] -> Number
;; returns a count of all pairs of (prior . current) where proir < current
(define count-inc-pairs
  (λ (ls acc)
    (cond
      [(null? ls) acc]
      [else (let ([pair (car ls)])
              (if (< (car pair) (cdr pair))
                  (count-inc-pairs (cdr ls) (add1 acc))
                  (count-inc-pairs (cdr ls) acc)))])))

(check-equal? (count-inc-pairs (pair-off '(1 0 3 4 5)) 0) 3)
(check-equal? (count-inc-pairs (pair-off '(5 4 3 2 1)) 0) 0)
(check-equal? (count-inc-pairs (pair-off null) 0) 0)

;; [ListOf Number] -> Number
;; returns the amount of numbers that have increased from its prior
(define day1a
  (λ (lst)
    (count-inc-pairs (pair-off lst) 0)))


;; pt 2
;; [ListOfNumber] -> [ListOf [TripleOf Number]]
;; returns a list of triples of (prior . current . next) if such exist
(define triple-off
  (λ (ls)
    (cond
      [(or (null? ls) (> 2 (length ls))) null]
      [(eqv? 3 (length ls)) (list ls)]
      [else (cons (list (car ls) (cadr ls) (caddr ls))
                  (triple-off (cdr ls)))])))

(check-equal? (triple-off '(1 2 3)) '((1 2 3)))
(check-equal? (triple-off '(1 2 3 4 5)) '((1 2 3) (2 3 4) (3 4 5)))
(check-equal? (triple-off '(1)) null)
(check-equal? (triple-off null) null)

;; [ListOf [TripleOf Number]] -> [ListOf Number]
;; returns a flattened list where each number represents the sum of its respective triple
(define flat-triples
  (λ (ls)
    (map (λ (trip) (foldr + 0 trip)) ls)))

(check-equal? (flat-triples (triple-off '(1 2 3 4 5))) '(6 9 12))

;;[ListOf Number] -> Number
;; returns the amount of triples that have increased in from its prior triple
(define day1b
  (λ (lst)
    (day1a (flat-triples (triple-off lst)))))

;; main
(module+ main
    (call-with-input-file "data/day1.txt"
      (lambda (prt)
        (define lines (map string->number (port->lines prt)))
        (answer 1 1 (day1a lines))
        (answer 1 2 (day1b lines)))))
