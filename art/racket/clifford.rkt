#lang sketching

(define a -1.3)
(define b 1)
(define c 1.7)
(define d 1.7)
(define r 130)
(define iters 500000)

(define (setup)
  (size 1000 1000))

(define (draw)
  (translate (/ width 2)
             (/ height 2))
  
  (draw-clifford a b c d 0 0 iters)
  (save "pics/blue.png")
  (no-loop))
 
(define turq (color 9 243 247 1))
(define dark-blue (color 31 79 183 1))
(define purp (color 84 34 247 1))
(define pink (color 247 34 183 1))
(define orange (color 244 99 14 1))
(define yellow (color 252 229 83 1))
(define black (color 30 30 30))
(define off-white (color 247 213 210))

(define (draw-clifford a b c d init-x init-y count)
  (no-fill)
  (background black)
  
  (let loop ([x init-x]
             [y init-y]
             [count count])
    
    (define xy-pair
      (clifford a b c d x y))

    (define dist
      (sqrt (+ (sq (+ (car xy-pair) (cdr xy-pair)))
               (sq (+ x y)))))
    
    (define lerp-val
      (remap dist 0 4 0 1))

    (stroke (lerp-color turq dark-blue lerp-val))
    
    (cond [(= count 0)]

          [else (point (* r x) (* r y))
                (loop (car xy-pair)
                      (cdr xy-pair)
                      (sub1 count))])))

(define (clifford a b c d x y)
  (let ([x1 (sin (* a y))]
        [x2 (* c (cos (* a x)))]
        [y1 (sin (* b x))]
        [y2 (* d (cos (* b y)))])
    
    (cons (+ x1 x2)
          (+ y1 y2))))
