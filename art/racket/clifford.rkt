#lang sketching

(define a 2)
(define b 2)
(define c 1)
(define d -1)

(define (setup)
  (size 600 600)
  (background 0))

(define (draw)

  (translate (/ width 2)
             (/ height 2))
  
  (draw-clifford a b c d 0 0.1 200000)
  (no-loop))

(define (draw-clifford a b c d init-x init-y count)
  
  (no-fill)
  (define colour (color 238 225 240 10))
  (stroke colour)
  (let loop ([x init-x]
             [y init-y]
             [count count])

    (define xy-pair
      (clifford a b c d x y))

    (cond [(= count 0) (println "done!")]

          [else (point (* 100 x) (* 100 y))
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
