#lang racket
(require "lib/common.rkt"
         rackunit)

(define-struct coord [x y] #:transparent)
(define-struct segment [start end] #:transparent)

(define s1 (make-segment (make-coord 1 2) (make-coord 3 2)))
(define s2 (make-segment (make-coord 1 2) (make-coord 1 5)))
(define s3 (make-segment (make-coord 8 0) (make-coord 0 8)))

;; pt 1
;; Coord Coord -> [VectorOf Coord]
;; returns a line given a set of coords that will represent a diagonal line
(define build
  (λ (c1 c2)
    (let ([xdiff (- (coord-x c1) (coord-x c2))]
          [ydiff (- (coord-y c1) (coord-y c2))])
      (for/fold ([vec (vector)])
                ([i (in-range (add1 (abs xdiff)))])
        (let ([deltx (if (negative? xdiff) 1 -1)]
              [delty (if (negative? ydiff) 1 -1)])
          (vector-append vec (vector (coord (+ (coord-x c1) (* i deltx))
                                            (+ (coord-y c1) (* i delty))))))))))

(check-equal? (build (make-coord 1 1) (make-coord 3 3)) (vector (coord 1 1) (coord 2 2) (coord 3 3)))

;; Segment Boolean -> [VectorOf Coord]
;; returns a vector of generated coords that connects the segment
(define gen-line
  (λ (seg diagonal?)
    (match seg
      [(segment (coord x y1) (coord x y2)) 
       (build-vector (add1 (abs (- y1 y2))) (λ (n) (make-coord x (+ (min y1 y2) n))))]
      [(segment (coord x1 y) (coord x2 y))
       (build-vector (add1 (abs (- x1 x2))) (λ (n) (make-coord (+ (min x1 x2) n) y)))]
      [(segment (coord x1 y1) (coord x2 y2)) #:when (not diagonal?) (vector)]
      [(segment (coord x1 y1) (coord x2 y2)) #:when diagonal?
       (let ([xdiff (abs (- x1 x2))]
             [ydiff (abs (- y1 y2))])
                 (if (eqv? xdiff ydiff)
                     (build (coord x1 y1) (coord x2 y2))
                     (vector)))])))

(check-equal? (gen-line s1 #f) (vector (coord 1 2) (coord 2 2) (coord 3 2)))
(check-equal? (gen-line s2 #f) (vector (coord 1 2) (coord 1 3) (coord 1 4) (coord 1 5)))
(check-equal? (gen-line s3 #f) (vector))
(check-equal? (gen-line s3 #t)
              (vector (coord 8 0) (coord 7 1) (coord 6 2) (coord 5 3) (coord 4 4) (coord 3 5) (coord 2 6) (coord 1 7) (coord 0 8)))

;; [ListOf Segment] -> [HashTable [Coord Number]]
;; returns a mapping from segment to how many distinct points are on it
(define table
  (λ (ls diagonal?)
    (for*/fold ([ht (hash)])
               ([seg (in-list ls)]
                [coord (gen-line seg diagonal?)])
      (if (hash-has-key? ht coord)
          (hash-set ht coord (add1 (hash-ref ht coord)))
          (hash-set ht coord 1)))))


(define day5a
  (λ (ls)
    (length (filter (λ (n) (> n 1)) (hash-values (table ls #f))))))

;; pt 2!
(define day5b
  (λ (ls)
    (length (filter (λ (n) (> n 1)) (hash-values (table ls #t))))))


;; main
(define val-of-input
  (λ (exp)
    (match exp
      [`,x #:when (string? x)
           (let ([pair (string-split x #rx",")])
             (make-coord (string->number (string-normalize-spaces (car pair)))
                         (string->number (string-normalize-spaces (cadr pair)))))]
      [`(,c1 ,c2) (make-segment (val-of-input c1) (val-of-input c2))])))

(check-equal? (val-of-input (list "1,2" "3,4")) (make-segment (make-coord 1 2) (make-coord 3 4)))

(module+ main
  (call-with-input-file "data/day5.txt"
    (lambda (prt)
      (define lines (map val-of-input (map (λ (in) (string-split in #rx"->")) (port->lines prt))))
      (void)
      (answer 5 1 (day5a lines))
      (answer 5 2 (day5b lines)))))
