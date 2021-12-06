#lang racket
(require "lib/common.rkt")
(require rackunit)

;; pt 1
(define sanity-check
  (list (list 0 0 1 0 0)
        (list 1 1 1 1 0)
        (list 1 0 1 1 0)
        (list 1 0 1 1 1)
        (list 1 0 1 0 1)
        (list 0 1 1 1 1)
        (list 0 0 1 1 1)
        (list 1 1 1 0 0)
        (list 1 0 0 0 0)
        (list 1 1 0 0 1)
        (list 0 0 0 1 0)
        (list 0 1 0 1 0)))

;; [ListOf Bin] BitInd -> [ListOf Bit]
;; returns the list of bits at the bth position
(define get-at-bit
  (λ (ls b)
    (foldl (λ (dob ansd) (cons (list-ref dob b) ansd)) null ls)))

(check-equal? (get-at-bit null 0) null)
(check-equal? (get-at-bit (list (list 0 1 1) (list 1 0 1)) 0) (list 1 0))

;; [ListOf Bit] -> Bit
;; returns the *st occuring bit
;; > would give most, < would give least
(define *st-bit
  (λ (f ls zeros ones)
    (cond
      [(null? ls) (if (f zeros ones) 0 1)]
      [(zero? (car ls)) (*st-bit f (cdr ls) (add1 zeros) ones)]
      [else (*st-bit f (cdr ls) zeros (add1 ones))])))

(check-equal? (*st-bit > (list 0 0 1 0 0) 0 0) 0)
(check-equal? (*st-bit < (list 0 0 1 0 0) 0 0) 1)

;; [ListOf Bin] -> Bin
;; returns a list of the *st occuring bit, given a list of bins
(define rate
  (λ (f ls)
    (for/list ([i (in-range 12)])
      (*st-bit f (get-at-bit ls i) 0 0))))

;; (: binary->natural (-> (Listof (Union 0 1)) Natural))
;; returns the decimal representation of a list of binaries
;; this was from a hmwk assignment of mine lmfaoo
(define binary->natural
  (λ (ls)
    (cond
      [(null? ls) 0]
      [else (+ (car ls) (* 2 (binary->natural (cdr ls))))])))

;; [ListOf Bin] -> Number
(define day3a
  (λ (ls)
    (let* ([gamma-rep (rate > ls)]
           [epsilon-rep (rate < ls)]
           [gamma (binary->natural (reverse gamma-rep))]
           [epsilon (binary->natural (reverse epsilon-rep))])
      (* gamma epsilon))))


;; pt 2
;; [ListOf Bin] -> Bin
;; return the bin representation of oxygen calc
(define element
  (λ (f ls iter)
    (cond
      [(null? (cdr ls)) ls]
      [else (element f (filter (λ (bin)
                                 (define ind-bit (get-at-bit ls iter))
                                 (eqv? (*st-bit f ind-bit 0 0) (list-ref bin iter))) ls)
                     (add1 iter))])))

(check-equal? (element > sanity-check 0) '((1 0 1 1 1))) ;; oxygen
(check-equal? (element <= sanity-check 0) '((0 1 0 1 0))) ;; co2


(define day3b
  (λ (ls)
    (let* ([bin-oxygen (car (element > ls 0))]
           [bin-co2 (car (element <= ls 0))]
           [oxygen (binary->natural (reverse bin-oxygen))]
           [co2 (binary->natural (reverse bin-co2))])
      (* oxygen co2))))


;; main                         
(module+ main
  (call-with-input-file "data/day3.txt"
    (lambda (prt)
      (define lines
        (map (λ (db) (map string->number db))
             (map (λ (bin) (string-split bin #rx"(?<=.)(?=.)")) (port->lines prt))))
      (answer 3 1 (day3a lines))
      (answer 3 2 (day3b lines)))))
