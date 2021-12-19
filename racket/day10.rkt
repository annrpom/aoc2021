#lang racket
(require "lib/common.rkt"
         rackunit)

;; pt 1
(define pairs (make-hash '(("[" . "]") ("(" . ")") ("{" . "}") ("<" . ">"))))
(define syntax-points (make-hash '(("]" . 57) (")" . 3) ("}" . 1197) (">" . 25137))))

;; Syntax -> Boolean
;; returns whether the given syntax is an opening syntax
(define opening-syntax?
  (λ (s)
    (member s (hash-keys pairs))))

(check-equal? (opening-syntax? "]") #f)
(check-equal? (opening-syntax? "[") '("["))

;; [ListOf Syntax] -> [ListOf Syntax]
;; returns a list of all syntax errors in a given line
(define find-first-error
  (λ (ls)
    (for/fold ([stack '("$")] ;; we initially push down a bottom stack marker
               [errors '()])
              ([s (in-list ls)])
      (cond
        [(opening-syntax? s) (values (cons s stack) errors)]
        [else (define top (car stack))
              (if (equal? (hash-ref pairs top) s)
                  (values (cdr stack) errors)
                  (values (cdr stack) (cons s errors)))]))))

;; wrapper
(define unwrap-find-first
  (λ (line)
    (match-define-values (st res) (find-first-error line))
    (reverse res)))

;; [ListOf Lines] -> [ListOf Syntax]
;; returns a list of all the first syntax errors in each line
(define get-syntax-lines
  (λ (ls)
    (flatten (map (λ (l)
                    (define res (unwrap-find-first l))
                    (if (empty? res) res (car res)))
                  ls))))

(define day10a
  (λ (ls)
    (for/sum ([s (in-list (get-syntax-lines ls))])
      (hash-ref syntax-points s))))

;; pt 2

;; wrapper
(define unwrap-complete-rest
  (λ (line)
    (match-define-values (st res) (find-first-error line))
    st))

(define completion-points (make-hash '(("[" . 2) ("(" . 1) ("{" . 3) ("<" . 4))))

;; [ListOf Syntax] -> Number
;; returns a score of a complete list of syntax for the incomplete lines
(define complete-rest
  (λ (ls)
    (define incomplete (unwrap-complete-rest ls))
    (for/fold ([sum 0])
              ([s (in-list incomplete)]
               #:when (hash-has-key? completion-points s))
      (+ (* 5 sum) (hash-ref completion-points s)))))


;; [ListOf Lines] -> [ListOf Number]
;; returns a list of all the scores of completing the incomplete lines
(define get-score-incomplete
  (λ (ls)
    (flatten (map (λ (l)
                    (define res (unwrap-find-first l))
                    (if (empty? res) (complete-rest l) '()))
                  ls))))

(define day10b
  (λ (ls)
    (define res (get-score-incomplete ls))
    (define mid (floor (/ (length res) 2)))
    (list-ref (sort res <) mid)))

;; main
(module+ main
  (call-with-input-file "data/day10.txt"
    (lambda (prt)
      (define lines (map (λ (l) (map string (string->list l))) (port->lines prt)))
      (void)
      (answer 10 1 (day10a lines))
      (answer 10 2 (day10b lines)))))

;; test
#;(module+ test
  (call-with-input-file "data/day10.test.txt"
    (lambda (prt)
      (define lines (map (λ (l) (map string (string->list l))) (port->lines prt)))
      (displayln (day10a lines))
      (displayln (day10b lines)))))
