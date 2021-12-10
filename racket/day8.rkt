#lang racket
(require "lib/common.rkt"
         rackunit)

(define pieces (list "acedgfb" "cdfbe" "gcdfa" "fbcad" "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab"))
(define puzzle (list "cdfeb" "fcadb" "cdfeb" "cdbaf"))
(define pieces-of-a-puzzle (cons pieces puzzle))

;; pt 1
(define unique-refs (make-hash '((1 . 2) (4 . 4) (7 . 3) (8 . 7))))

;; [ListOf String] -> Number
;; returns how many times 1, 4, 7, or 8 appear
(define count-n-seg
  (λ (ls)
    (length (filter (λ (s) (memv (string-length s) (hash-values unique-refs))) ls))))

(check-equal? (count-n-seg (list "ab" "agc" "aaaaaaaaa")) 2)

(define day8a
  (λ (ls)
    (count-n-seg ls)))

;; pt 2
;; Pred [ListOf String] -> String
;; returns the first string to meet the pred in the given list, along with updated list
(define (take-first p? ls ans-so-far)
  (cond
    [(p? (string->list (car ls)) ans-so-far) (car ls)]
    [else (take-first p? (cdr ls) ans-so-far)]))


(define m1 (λ (cs ls) (= (length cs) 2)))
(define m4 (λ (cs ls) (= (length cs) 4)))
(define m7 (λ (cs ls) (= (length cs) 3)))
(define m8 (λ (cs ls) (= (length cs) 7)))
;; base cases
(define m9 (λ (cs ls) (and (= (length cs) 6) (subset? (string->list (list-ref ls 1)) cs))))
(define m0 (λ (cs ls) (and (= (length cs) 6) (subset? (string->list (list-ref ls 0)) cs))))
(define m6 (λ (cs ls) (= (length cs) 6)))
(define m3 (λ (cs ls) (and (= (length cs) 5) (subset? (string->list (list-ref ls 0)) cs))))
(define m5 (λ (cs ls) (and (= (length cs) 5) (subset? cs (string->list (list-ref ls 4))))))
(define m2 (λ (cs ls) cs))
(define find-mapping (list m1 m4 m7 m8 m9 m0 m6 m3 m5 m2))

;;
(define apply-deductions
  (λ (ls fls)
    (for/fold ([mapping empty]
               [searchls ls])
              ([fn (in-list fls)])
      (displayln mapping)
      (displayln fn)
      (define s (take-first fn searchls (reverse mapping)))
      (values (cons s mapping) (remove s searchls)))))

(match-define-values (ans _) (apply-deductions pieces find-mapping))

;; String String -> Boolean
;; returns whether a given string is a srabble of another
(define scrabble?
  (λ (s1 s2)
    (define ls1 (string->list s1))
    (define ls2 (string->list s2))
    (= (length (set-union ls1 ls2)) (length ls1))))

(check-equal? (scrabble? "yes" "eys") #t)
(check-equal? (scrabble? "yes" "eysp") #f)

;; [ListOf String] String -> Number
;; returns index of element in list
(define scrabble-index-of
  (λ (x ls)
    (letrec ([helper (λ (ls x acc)
                       (cond
                         [(null? ls) acc]
                         [(scrabble? (car ls) x) acc]
                         [else (helper (cdr ls) x (add1 acc))]))])
      (helper ls x 0))))

(check-equal? (scrabble-index-of "no" (list "p" "on")) 1)

;; [ListOf String] -> [ListOf String]
;; returns a reordered list i hate this solution so incredibly much
(define reorder-ans
  (λ (ls)
    (define wrapper (λ (n) (list-ref ls n)))
    (list (wrapper 5) (wrapper 0) (wrapper 9) (wrapper 7)
          (wrapper 1) (wrapper 8) (wrapper 6) (wrapper 2)
          (wrapper 3) (wrapper 4))))

;; [PairOf [ListOf String] [ListOf String]] -> Number
;; returns the numerical value of the output
(define find-ans
  (λ (vals map)
    (define result (for/fold ([str ""])
                             ([i (in-list (cdr vals))])
                     (string-append str (number->string (scrabble-index-of i map)))))
    (string->number result)))

(check-equal? (find-ans pieces-of-a-puzzle (reorder-ans (reverse ans))) 5353)


(define day8b
  (λ (ls)
    (foldl + 0 (map (λ (config) (match-define-values (ans _) (apply-deductions (car config) find-mapping))
           (find-ans config (reorder-ans (reverse ans))))
         ls))))

;; main
(module+ main
  (call-with-input-file "data/day8.txt"
    (lambda (prt)
      (define lines (map (λ (s) (string-split s " | ")) (port->lines prt)))
      (define outputs (flatten (map (λ (s) (string-split s " ")) (map cadr lines))))
      (define vals (map (λ (line) (cons (string-split (car line) " ") (string-split (cadr line) " "))) lines))
      (answer 8 1 (day8a outputs))
      (answer 8 2 (day8b vals))))) ;; vals are '(("abe" "ge") . ("ge ))
