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
  (λ (ls)
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

(module+ main
  (call-with-input-file "data/dayN.txt"
    (lambda (prt)
      (define lines (port->lines prt))
      (answer N 1 (dayNa lines))
      (answer N 2 (dayNb lines)))))
