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

;; [ListOf [DozenOf Bin]] BitInd -> [ListOf Bits]
;; returns the list of bits at the bth position
(define get-at-bit
  (λ (ls b)
    (foldl (λ (dob ansd) (cons (list-ref dob b) ansd)) null ls)))

(check-equal? (get-at-bit null 0) null)
(check-equal? (get-at-bit (list (list 0 1 1) (list 1 0 1)) 0) (list 1 0))

;; [ListOf Bit] -> Bit
;; returns the most occuring bit
(define *st-bit
  (λ (f ls zeros ones)
    (cond
      [(null? ls) (if (f zeros ones) 0 1)]
      [(zero? (car ls)) (*st-bit f (cdr ls) (add1 zeros) ones)]
      [else (*st-bit f (cdr ls) zeros (add1 ones))])))

;; [ListOf [DozenOf Bin]] -> Bin
(define rate
  (λ (f ls)
    (for/list ([i (in-range 12)])
      (list (*st-bit f (get-at-bit ls i) 0 0)))))


;; (: binary->natural (-> (Listof (Union 0 1)) Natural))
;; returns the decimal representation of a list of binaries
;; this was from a hmwk assignment of mine lmfaoo
(define binary->natural
  (λ (ls)
    (cond
      [(null? ls) 0]
      [else (+ (car ls) (* 2 (binary->natural (cdr ls))))])))

(define day3a
  (λ (lst)
    (let* ([gamma-rep (flatten (rate > lst))]
           [epsilon-rep (flatten (rate < lst))]
           [gamma (binary->natural (reverse gamma-rep))]
           [epsilon (binary->natural (reverse epsilon-rep))])
      (* gamma epsilon))))


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

(define day3b
  (λ (lst)
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
(module+ main
  (call-with-input-file "data/day3.txt"
    (lambda (prt)
      (define lines
        (map (λ (db) (map string->number db))
             (map (λ (bin) (string-split bin #rx"(?<=.)(?=.)")) (port->lines prt))))
      (void)
      (answer 3 1 (day3a lines))
      #;(answer 3 2 (day3b lines)))))
