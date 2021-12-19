#lang racket
(require "lib/common.rkt"
         rackunit)

;; pt 1

;; returns list matrix as vector matrix
(define vectorize
  (λ (ls)
    (for/vector ([row (in-list ls)])
      (for/vector ([col (in-list row)])
        (string->number col)))))

;;(define nrow (λ (ls) (vector-length ls)))
;;(define ncol (λ (ls) (vector-length (vector-ref ls 0))))
(define nrow (λ (ls) (length ls)))
(define ncol (λ (ls) (length (list-ref ls 0))))

;; gets the valid NESW adjacents
(define adjacents
  (λ (row col nrow ncol)
    (define sanitize (λ (r c) (if (and (< -1 r nrow) (< -1 c ncol)) (cons r c) #f)))
    (filter (λ (t) t) (list (sanitize (sub1 row) col)
                                     (sanitize row (add1 col))
                                     (sanitize row (sub1 col))
                                     (sanitize (add1 row) col)))))

#;(check-equal? (adjacents 0 0 5 5) (vector (vector 0 1) (vector 1 0)))

;; returns the value at r, c
(define get-at
  (λ (row col matrix)
    (list-ref (list-ref matrix row) col)))

#;(check-equal? (get-at 1 1 (vector (vector 1 2 3)
                                  (vector 0 8 5))) 8)

;; returns whether a given r,c is the lowest out of its neighbors
(define low?
  (λ (row col matrix)
    (define adj (adjacents row col (nrow matrix) (ncol matrix)))
    (define val-adj (map (λ (pair) (get-at (car pair) (cdr pair) matrix)) adj))
    (define curr (get-at row col matrix))
    (define min (argmin (λ (x) x) (append (list curr) val-adj)))
    (and (= min curr) (= 0 (length (filter (λ (n) (= min n)) val-adj))))))

;; returns a list of the values, plus 1, that are low
(define low-values
  (λ (matrix)
    (for/list ([row (range (nrow matrix))])
      (for/list ([col (range (ncol matrix))]
                 #:when (low? row col matrix))
        (get-at row col matrix)))))

(define day9a
  (λ (ls)
    (define vals (flatten (low-values ls)))
    (foldl (λ (v s) (+ (add1 v) s)) 0 vals)))

;; pt 2

;; need to fix - somehow got the answer but this was so bad i dont even want to
;; commit this

;; main
(module+ main
  (call-with-input-file "data/day9.txt"
    (lambda (prt)
      (define lines (map (λ (row) (filter (λ (s) (not (string=? "" s))) (string-split row ""))) (port->lines prt)))
      (define vector (vectorize lines))
      (define unvec (vector->list (vector-map (λ (row) (vector->list row)) vector)))
      #;(answer 9 1 (day9a vector))
      (answer 9 2 (day9b unvec)))))
