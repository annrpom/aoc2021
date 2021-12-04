#lang racket
(require "lib/common.rkt")
(require rackunit)

;                    
;                    
;          ;     ;;  
;          ;    ; ;  
;   ;;;;  ;;;;    ;  
;   ;   ;  ;      ;  
;   ;   ;  ;      ;  
;   ;   ;  ;      ;  
;   ;;;;   ;;;  ;;;;;
;   ;                
;   ;                
;

(define-struct marked [n])

;; from day 3, renaming from get-at-bit to list-at-index
(define list-at-index
  (λ (ls b)
    (foldl (λ (dob ansd) (cons (list-ref dob b) ansd)) null ls)))

;; Board -> Board
;; returns a list of columns in a board (instead of row) - this is also upsidedown
;; due to my usage of foldl in list-at-index. this is fine (for the problem statement). 
(define transpose-ish
  (λ (b)
    (for/list ([i (in-range (length (car b)))])
      (list-at-index b i))))

(check-equal? (transpose-ish '((1 2 3) (4 5 6))) '((4 1) (5 2) (6 3)))

;; Board -> Boolean
;; returns whether a given board has a marked row or column
(define winner?
  (λ (b)
    (define rowwin (ormap (λ (row) (not (findf number? row))) b))  ;; marked? did not work in findf (why)
    (define colwin (ormap (λ (row) (not (findf number? row))) (transpose-ish b)))
    (or rowwin colwin)))

(check-equal? (winner? '((4 2 5) ((make-marked 3) (make-marked 2) (make-marked 1)) (5 4 3))) #t)
(check-equal? (winner? '((4 2 5) ((make-marked 3) 2 (make-marked 1)) (5 4 3))) #f)
(check-equal? (winner? '((2 (make-marked 1) 4) (7 (make-marked 3) 6) (9 (make-marked 6) 1))) #t)
(check-equal? (winner? '((2 (make-marked 1) 4) (7 (make-marked 3) 6) (9 6 1))) #f)

;; Number Board -> Board
;; returns a board where n (if exists) is marked (marked means make-marked num)
(define mark-board
  (λ (n b)
    (map (λ (row) (map (λ (tile) (if (eqv? n tile) (make-marked tile) tile)) row)) b)))

;; this check-equal doesnt work ??? (why)
;; (check-equal? (mark-board 4 '((4 2 5) (-3 -2 -1) (5 4 3))) '(((make-marked 4) 2 5) (-3 -2 -1) (5 (make-marked 4) 3)))
(check-equal? (mark-board 0 '((4 2 5) (-3 -2 -1) (5 4 3))) '((4 2 5) (-3 -2 -1) (5 4 3)))

;; [ListOf Number] [ListOf Board] -> Board
;; returns the winning board given a list of drawn numbers
(define bingo
  (λ (nums boards ind)
    (cond
      [(> (length (filter winner? boards)) 0) (values (car (filter winner? boards)) ind)]
      [else (bingo (cdr nums) (map (λ (b) (mark-board (car nums) b)) boards) (add1 ind))])))


;; returns the sum of unmarked answers on a winning board
(define day4a
  (λ (nums boards)
    (match-define-values (board ind) (bingo nums boards 0))
    (define unmarked (flatten (map (λ (row) (filter number? row)) board)))
    (* (foldl + 0 unmarked) (list-ref nums (sub1 ind)))))


;                    
;                    
;          ;     ;;; 
;          ;    ;   ;
;   ;;;;  ;;;;      ;
;   ;   ;  ;       ;;
;   ;   ;  ;      ;; 
;   ;   ;  ;     ;   
;   ;;;;   ;;;  ;;;;;
;   ;                
;   ;                
;                    

(define day4b
  (λ (ls)
    (void)))


;                          
;                 ;        
;                          
;                ;;        
;   ;;;;; ;;;;    ;   ; ;; 
;   ; ; ;     ;   ;   ;;  ;
;   ; ; ;  ;;;;   ;   ;   ;
;   ; ; ; ;   ;   ;   ;   ;
;   ; ; ; ;;;;; ;;;;; ;   ;
;                          
;                          
;

;; [ListOf X] -> [ListOf [ListOf X]]
;; returns a nested list of lists given how many elements are supposed to be in the sublist
(define sublists
  (λ (ls n)
    (for/foldr ([acc '()]
                [ls-acc ls])
      ([i (in-range (/ (length ls) n))])
      (values (cons (take ls-acc n) acc) (drop ls-acc n)))))


(module+ main
    (call-with-input-file "data/day4.txt"
      (lambda (prt)
        (let* ([lines (map (λ (in) (string-split in #rx"\n\n")) (port->lines prt))]
               [nums (map string->number (string-split (caar lines) #rx","))]
               [boards (map (λ (row) (map string->number (flatten (map string-split row)))) (filter cons? (cdr lines)))])
          (match-define-values (b e) (sublists boards 5))
          (answer 4 1 (day4a nums b))
          #;(answer 4 2 (day4b nums boards))))))

(module+ test
  (define am-not-sane '(7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1))
  (define sanity-check '(((22 13 17 11 0)
                          (8 2 23 4 24)
                          (21 9 14 16 7)
                          (6 10 3 18 5)
                          (1 12 20 15 19))
                         ((3 15 0 2 22)
                          (9 18 13 17 5)
                          (19 8 7 25 23)
                          (20 11 10 24 4)
                          (14 21 16 12 6))
                         ((14 21 17 24 4)
                          (10 16 15 9 19)
                          (18 8 23 26 20)
                          (22 11 13 6 5)
                          (2 0 12 3 7))))
  #|(check-equal? (bingo am-not-sane sanity-check) '(((#<marked> #<marked> #<marked> #<marked> #<marked>)
                                                    (10 16 15 #<marked> 19)
                                                    (18 8 #<marked> 26 20)
                                                    (22 #<marked> 13 6 #<marked>)
                                                    (#<marked> #<marked> 12 3 #<marked>)))|#)
