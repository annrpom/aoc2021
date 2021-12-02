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

(define dayNa
  (λ (lst)
    (void)))


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

(define dayNb
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
  (call-with-input-file "data/dayN.txt"
    (lambda (prt)
      (define lines (port->lines prt))
      (answer N 1 (dayNa lines))
      (answer N 2 (dayNb lines)))))
