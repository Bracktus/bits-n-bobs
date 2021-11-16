#lang racket

(define dims-str-lst
  ;a list of strings
  (for/list ([line (file->lines "day-2.input")])
             line))
       
(define (dim-str->dim-lst str)
  ;converts a string into a list of values
  (map string->number (string-split str "x")))

(define (dim-lst->surface-lst dim-lst)
  (let ([l (car dim-lst)]
        [w (cadr dim-lst)]
        [h (caddr dim-lst)])
    
    (list (* 2 l w)
          (* 2 w h)
          (* 2 h l))))

(define (minim lst)
  
 (define (min-of-2 x y)
   (if (< x y) x y))
  
 (foldl min-of-2 (first lst) (rest lst)))

(define (min-2 lst)
  (let* ([1st-min (minim lst)]
        [2nd-min (minim (remove 1st-min lst))])
    
    (list 1st-min 2nd-min)))

(define (sum lst)
  (apply + lst))

(define (prod lst)
  (apply * lst))
  
(define (slack surface-lst)
  (define (half n)
    (/ n 2))
  (minim (map half surface-lst)))

(define (face-ribbon dim)
  (define (double n)
    (* 2 n))
    
  (sum (map double (min-2 dim))))

(define (vol-ribbon dim-lst)
  (prod dim-lst))

(define total-tape
  (let* ([str-lst dims-str-lst]
         [dim-lst (map dim-str->dim-lst str-lst)]
         [surface-lst (map dim-lst->surface-lst dim-lst)])
    
    (sum (map (λ (l) (+ (slack l) (sum l))) surface-lst))))

(define total-ribbon
  (let* ([str-lst dims-str-lst]
         [dim-lst (map dim-str->dim-lst str-lst)])
    
    (sum (map (λ (l) (+ (face-ribbon l) (vol-ribbon l)))
              dim-lst))))

  

  
     
 

