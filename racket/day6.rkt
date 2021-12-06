#lang racket
(require "lib/common.rkt"
         rackunit)

;; pt 1

;; [HashTable [Number Number]] -> [HashTable [Number Number]]
;; returns a inital population of our fish table
(define init-ht
  (λ (ht)
    (for/fold ([fishtable ht])
              ([i (in-range 9)])
      (hash-set fishtable i 0))))

;; [ListOf Number] -> [HashTable [Number Number]]
;; returns a mapping from fish-expiry to amount of fish with that expiry
(define gen-fish-map
  (λ (ls)
    (for/fold ([ht (init-ht (hash))])
              ([key (in-list ls)])
      (hash-set ht key (add1 (hash-ref ht key))))))

;; [HashTable [Number Number]] -> [HashTable [Number Number]]
;; returns what the fish will look like on the next day
(define next-day
  (λ (ht)
    (for/fold ([fishes ht])
              ([key (in-list (hash-keys ht))])
      (define new-amt-k (if (eqv? 8 key) 0 (add1 key)))
      (cond
        [(= 6 key) (hash-set fishes key (+ (hash-ref ht 0) (hash-ref ht new-amt-k)))]
        [else (hash-set fishes key (hash-ref ht new-amt-k))]))))

;; [HashTable [Number Number]] Number -> [HashTable [Number Number]]
;; invokes next-day n times on input table
(define n-days
  (λ (ht n)
    (for/fold ([fishes ht])
              ([day (in-range n)])
      (next-day fishes))))


(define day6a
  (λ (ls)
    (foldr + 0 (hash-values (n-days (gen-fish-map ls) 80)))))

;; pt 2
(define day6b
  (λ (ls)
    (foldr + 0 (hash-values (n-days (gen-fish-map ls) 256)))))

;; main
(module+ main
  (call-with-input-file "data/day6.txt"
    (lambda (prt)
      (define lines (map string->number (string-split (car (port->lines prt)) #rx",")))
      (void)
      (answer 6 1 (day6a lines))
      (answer 6 2 (day6b lines)))))

;; testing
#;(module+ test
  (define ht (init-ht (hash)))
  (define fishmap (gen-fish-map (list 3 4 3 1 2)))
  (foldr + 0 (hash-values (n-days fishmap 256))))
