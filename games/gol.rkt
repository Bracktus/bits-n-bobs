#lang sketching
(require math/matrix)
(require racket/list)
(require racket/vector)

(define side-sqrs 50)
(define total-squares (* side-sqrs side-sqrs));must be a square number
(define side-len 640)

(define (square-size side-len num-squares)
  (/ (* side-len side-len) num-squares))

(define square-len
  (sqrt (square-size side-len total-squares)))

(define (gen-matrix)
  ;row-major matrix
  (make-vector total-squares #f))

(define gol-mat
  (gen-matrix))

(define (xy->index x y)
  (+ (* y side-sqrs) x))

(define (index->xy index)
  (let ([x (quotient index side-len)]
        [y (remainder index side-len)])
    (cons x y)))

(define (alive? x y)
  (vector-ref gol-mat (xy->index x y)))
  
(define (setup)
  (size side-len side-len))

(define xy
  (for*/vector ([y (in-range side-sqrs)]
                [x (in-range side-sqrs)])
    (cons x y)))

(define (draw)
  (background 0)
  (stroke 255)
  
  ;draw a nxm grid
  (for ([i (in-range 0 side-len square-len)])
    (line 0 i width i)
    (line i 0 i height))
  
  (when key-pressed
    (set! gol-mat (vector-map (λ (pair) (apply-rules (car pair) (cdr pair))) xy)))
                
  ;if a item in the vector is labelled true, display it in white
  (for* ([i (in-range side-sqrs)]
         [j (in-range side-sqrs)])
    (when (alive? i j)
      (square (* i square-len)
              (* j square-len)
              square-len))))

; Any live cell with fewer than two live neighbours dies, as if by underpopulation.
; Any live cell with two or three live neighbours lives on to the next generation.
; Any live cell with more than three live neighbours dies, as if by overpopulation.
; Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

 

(define (apply-rules x y)
  ;applies the rules to a single value in the matrix
  (let* ([is-alive         (alive? x y)]
         [is-dead          (not is-alive)]
         [neighbours       (alive-neighbours x y)]
         [neighbour-count  (count (λ (cell) (equal? cell #t)) neighbours)]
         [flip             (λ () (vector-set! gol-mat (xy->index x y) is-dead))])
         
    (cond [(and is-alive (< neighbour-count 2))
           #f]

          [(and is-alive (> neighbour-count 3))
           #f]

          [(and is-dead (= neighbour-count 3))
           #t]
          
          [else is-alive])))
  
(define (on-mouse-pressed)
  ;when the user clicks on the grid flip the status
  ;TODO: Add error handling on clicking outside the grid
  (let* ([x (floor (/ mouse-x square-len))]
         [y (floor (/ mouse-y square-len))]
         [index (xy->index x y)]
         [val (vector-ref gol-mat index)])
    (vector-set! gol-mat index (not val))))

;alternative idea:
;    pass through each value in the matrix.
;    on arrival +1 to a pixle's neighbours
(define (alive-neighbours x y)
  ;Takes in a matrix x y and returns a list of neighbours
  (let* ([get-up        (λ (x y) (alive? x (sub1 y)))]
         [get-bot       (λ (x y) (alive? x (add1 y)))]
         [get-up-right  (λ (x y) (alive? (add1 x) (sub1 y)))]
         [get-bot-right (λ (x y) (alive? (add1 x) (add1 y)))]
         [get-right     (λ (x y) (alive? (add1 x) y))]
         [get-up-left   (λ (x y) (alive? (sub1 x) (sub1 y)))]
         [get-bot-left  (λ (x y) (alive? (sub1 x) (add1 y)))]
         [get-left      (λ (x y) (alive? (sub1 x) y))]
         [side-sqrs     (sub1 side-sqrs)]) 
         
    (cond
      
      ;if we're on the top left corner
      [(and (= x 0) (= y 0))
       ;(println "TL")
       (map (λ (func) (func x y))
               (list get-right 
                     get-bot-right
                     get-bot))]

      ;if we're on the bottom left corner
      [(and (= x 0) (= y side-sqrs))
       ;(println "BL")
       (map (λ (func) (func x y))
            (list get-up
                  get-up-right
                  get-right))]
      
      ;if we're on the left edge
      [(= x 0)
       ;(println "LE")
       (map (λ (func) (func x y))
            (list get-right
                  get-bot
                  get-up
                  get-up-right
                  get-bot-right))]

      ;if we're on the top-right corner
      [(and (= x side-sqrs) (= y 0))
       ;(println "TR")
       (map (λ (func) (func x y))
            (list get-left
                  get-bot-left
                  get-bot))]

      ;if we're on the bottom-right corner
      [(and (= x side-sqrs) (= y side-sqrs))
       ;(println "BR")
       (map (λ (func) (func x y))
            (list get-up
                  get-up-left
                  get-left))]

      ;if we're on the right edge
      [(= x side-sqrs)
       ;(println "RE")
       (map (λ (func) (func x y))
            (list get-left
                  get-up
                  get-bot
                  get-up-left
                  get-bot-left))]
      
      ;if we're on the top row
      [(= y 0)
       ;(println "TR")
       (map (λ (func) (func x y))
            (list get-bot
                  get-bot-left
                  get-bot-right
                  get-left
                  get-right))]

      ;if we're on the bottom row
      [(= y side-sqrs)
      ;(println "BR")
       (map (λ (func) (func x y))
            (list get-up
                  get-up-left
                  get-up-right
                  get-left
                  get-right))]

      ;otherwise
      [else
       ;(println "ELSE")
       (map (λ (func) (func x y))
            (list get-up
                  get-up-left
                  get-up-right
                  get-left
                  get-right
                  get-bot
                  get-bot-left
                  get-bot-right))])))
