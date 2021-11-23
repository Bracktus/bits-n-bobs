#lang sketching

(require math/matrix)
(require racket/list)
(require racket/match)

;---------------colours--------------
(define yellow
  (color 255 204 0))

(define blue
  (color 0 60 255))

(define red
  (color 255 9 0))

;---------------maths----------------
;https://en.wikipedia.org/wiki/Isometric_projection#Mathematics

(define identity-mat
  (matrix [[1 0 0]
           [0 1 0]
           [0 0 1]]))

(define (matrix*vec mat vec)
  ;matrix vector multiplication
  (matrix->vector (matrix* mat (vector->matrix 3 1 vec))))

(define (3d-rotate x-ang y-ang z-ang)
  ;returns a 3d rotation on 3 axes
  (let ([x-mat (basic-rot 'x x-ang)]
        [y-mat (basic-rot 'y y-ang)]
        [z-mat (basic-rot 'z z-ang)])
    (matrix* x-mat y-mat z-mat)))

(define (basic-rot axis θ)
    ;returns a 3d rotation matrix on 1 axis
  
    (case axis
     ;https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions
     [(x)
      (matrix [[1 0 0]
               [0 (cos θ) (- (sin θ))]
               [0 (sin θ) (cos θ)]])]

     [(y)
      (matrix [[(cos θ) 0 (sin θ)]
               [0 1 0]
               [(- (sin θ)) 0 (cos θ)]])]
     [(z)
      (matrix [[(cos θ)  (- (sin θ)) 0]
               [(sin θ)  (cos θ) 0]
               [0 0 1]])]))

(define (ortho point)
  ;gets the x y values from a point in iso space
  (values (vector-ref point 0)
          (vector-ref point 1)))

(define (iso point)
  ;converts a point in 3d to a point in iso space
  
  (define (mat*scal mat scal)
    ;matrix scalar multiplication
    (matrix-map (λ (v) (* scal v)) mat))
  
  (define iso-m
    ;maps a 3d point to it's value in iso space
    (let ([s3 (sqrt 3)]
          [s2 (sqrt 2)])
      
      (mat*scal (matrix [[s3 0 (- s3)]
                         [1 2 1]
                         [s2 (- s2) s2]])
                (/ 1 (sqrt 6)))))
  
  (matrix*vec iso-m point))

;-------------shapes---------------------

(define (iso-line p1 p2)
  ;draw a edge of a cuboid
  (let-values ([(iso-x1 iso-y1) (ortho p1)]
               [(iso-x2 iso-y2) (ortho p2)])
    (line iso-x1 iso-y1 iso-x2 iso-y2))) 
      
(define (iso-face p1 p2 p3 p4)
  ;draw a face of a cuboid
  (let-values ([(iso-x1 iso-y1) (ortho p1)]
               [(iso-x2 iso-y2) (ortho p2)]
               [(iso-x3 iso-y3) (ortho p3)]
               [(iso-x4 iso-y4) (ortho p4)])
    
    (begin-shape)
    (vertex iso-x1 iso-y1)
    (vertex iso-x2 iso-y2)
    (vertex iso-x3 iso-y3)
    (vertex iso-x4 iso-y4)
    (end-shape 'close)))

(define (cuboid x y z wid hig dep mat style)
  ;https://excalidraw.com/#json=Ah-neZtQgizt8Dq-ruEFR,aYpTJ47Sv3YkWp3E_ZWLVA
  ;above is a diagram labelling all verticies
  (let ([a (iso (matrix*vec mat (vector (- x wid) (+ y hig) (+ z dep))))]
        [b (iso (matrix*vec mat (vector (- x wid) (+ y hig) (- z dep))))]
        [c (iso (matrix*vec mat (vector (+ x wid) (+ y hig) (- z dep))))]
        [d (iso (matrix*vec mat (vector (+ x wid) (+ y hig) (+ z dep))))]
        [e (iso (matrix*vec mat (vector (- x wid) (- y hig) (+ z dep))))]
        [f (iso (matrix*vec mat (vector (+ x wid) (- y hig) (+ z dep))))]
        [g (iso (matrix*vec mat (vector (+ x wid) (- y hig) (- z dep))))]
        [h (iso (matrix*vec mat (vector (- x wid) (- y hig) (- z dep))))])

   (cond [(equal? style 'wireframe)
          ;the cuboid is made of edges
          (iso-line a b)
          (iso-line a d)
          (iso-line a e)
          (iso-line c d)
          (iso-line c g)
          (iso-line d f)
          (iso-line e f)
          (iso-line f g)
          (iso-line e h)
          (iso-line g h)
          (iso-line b h)
          (iso-line b c)]

         [(equal? style 'opaque)
          ;the cuboid is made of faces
          ;this is buggy, we need a way of only display faces that are visible to the camera
          (iso-face a b c d)
          (iso-face b c g h)
          
          (iso-face a e f d)
          (iso-face c d f g)
          (iso-face e h g f)])))

;-------------turtle-----------
;the turtle can place a box next to another box (6 possible directions) DONE
;it can change the rotation of the upcoming boxes DONE
;it can change colour of boxes 
;it can save and return to a previous state DONE
;it can increase height, width, depth of the box

(class Turtle Object
   (init-field position dimension rotation color state-stack)
   (super-new)
  
   (define (vec->vals vec)
     (values (vector-ref vec 0)
             (vector-ref vec 1)
             (vector-ref vec 2)))

   (define (new-pos axis mag)
     (let-values ([(x y z) (vec->vals position)])
       (case axis
         [(x) (vector (+ x mag) y z)]
         [(y) (vector x (+ y mag) z)]
         [(z) (vector x y (+ z mag))])))
  
  (define/public (place style)
    (define-values (w h d)
      (vec->vals dimension))

    (define-values (x y z)
      (vec->vals position))

    (define-values (x-ang y-ang z-ang)
      (vec->vals rotation))
    
    (cuboid x y z w h d
            (3d-rotate x-ang y-ang z-ang)
            style))

  (define/public (rotate! axis angle)
    
    (define-values (x y z)
      (vec->vals rotation))
    
    (:= rotation (case axis
                   [(x) (vector (+ x angle) y z)]
                   [(y) (vector x (+ y angle) z)]
                   [(z) (vector x y (+ angle z))])))

  (define/public (push-state!)
    (define state
      (list position dimension rotation))
    
    (:= state-stack (cons state state-stack)))

  (define/public (pop-state!)
    
    (match-define (list pos dim rot) (car state-stack))

    (:= state-stack (cdr state-stack))
    (:= position pos)
    (:= dimension dim)
    (:= rotation rot))
      
  (define/public (move! direction)

    (define-values (w h d)
      (vec->vals dimension))

    (define-values (x y z)
      (vec->vals position))
    
    (:= position (case direction
                   [(up) (new-pos 'y (* 2 h))]
                   [(down) (new-pos 'y (- (* 2 h)))]
                   [(right) (new-pos 'x (* 2 w))]
                   [(left) (new-pos 'x (- (* 2 w)))]
                   [(forward) (new-pos 'z (* 2 d))]
                   [(backward) (new-pos 'z (- (* 2 d)))]))))


;--------------cfg stuff----------------

(define turtle
  (make-object Turtle
    (vector 0 0 0)   ;position
    (vector 5 10 15)   ;dimensions
    (vector 0 0 0)   ;rotation
    blue             ;colour
    '()))            ;state stack

(define (expand init ruleset depth)
  
  (define (hash-replace symbol)
    ;gets the expansion of a symbol
    (hash-ref ruleset symbol))
  
  (define (expand-once lst acc)
    ;expands a list of symbols once 
    (cond [(null? lst)
           (flatten (reverse acc))]
            
          [(hash-has-key? ruleset (car lst))
           (expand-once (cdr lst)
                        (cons (hash-replace (car lst)) acc))]

          [else
           (expand-once (cdr lst)
                        (cons (car lst) acc))]))
  
  (let loop ([count 0]
             [symbols init])
    (if (= count depth)
        symbols
        (loop (add1 count)
              (expand-once symbols '())))))

(define (add-rule! symbol expansion ruleset)
  (hash-set! ruleset symbol expansion))

(define (add-random-rule! left r-len ruleset)
  
  (define symbol-lst
    (list 'U 'D 'L 'R 'F 'B 'P))
  
  (let loop ([count 0]
             [acc '()])
    
    (if (= count r-len)
        (add-rule! left acc ruleset)
        (loop (add1 count)
              (cons (list-ref symbol-lst
                              (inexact->exact (round (random (sub1 (length symbol-lst))))))
                    acc)))))

(define (apply-symbols symbols ang style)
  (let loop ([lst symbols])
    (cond [(null? lst) #t]
          
          [(equal? 'U (car lst)) (turtle.move! 'up)
                                 (loop (cdr lst))]
          
          [(equal? 'D (car lst)) (turtle.move! 'down)
                                 (loop (cdr lst))]
          
          [(equal? 'L (car lst)) (turtle.move! 'left)
                                 (loop (cdr lst))]
          
          [(equal? 'R (car lst)) (turtle.move! 'right)
                                 (loop (cdr lst))]
          
          [(equal? 'F (car lst)) (turtle.move! 'forward)
                                 (loop (cdr lst))]
          
          [(equal? 'B (car lst)) (turtle.move! 'backward)
                                 (loop (cdr lst))]

          [(equal? 'X (car lst)) (turtle.rotate! 'x ang)
                                 (loop (cdr lst))]

          [(equal? 'Y (car lst)) (turtle.rotate! 'y ang)
                                 (loop (cdr lst))]

          [(equal? 'Z (car lst)) (turtle.rotate! 'z ang)
                                 (loop (cdr lst))]

          [(equal? 'P (car lst)) (turtle.place style)
                                     (loop (cdr lst))]

          [(equal? 'X* (car lst)) (turtle.rotate! 'x (- ang))
                                      (loop (cdr lst))]

          [(equal? 'Y* (car lst)) (turtle.rotate! 'y (- ang))
                                      (loop (cdr lst))]

          [(equal? 'Z* (car lst)) (turtle.rotate! 'z (- ang))
                                      (loop (cdr lst))]

          [(equal? '< (car lst)) (turtle.push-state!)
                                     (loop (cdr lst))]

          [(equal? '> (car lst)) (turtle.pop-state!)
                                     (loop (cdr lst))]
          
          [else (loop (cdr lst))])))

;-------------defining system-----------

(define ruleset
  (make-hash))

;turtle-1 w/ angle 30
;(add-rule! 'I (list 'P 'U 'X 'I) ruleset)
;(add-rule! 'U (list 'U 'X 'P) ruleset)

;turtle-2 w/ angle 90 
;(add-rule! 'I (list 'P 'F 'F 'P 'R 'R 'P 'B 'B 'P 'L 'L 'U 'U 'X 'I) ruleset)

;turtle-3 angle doesn't matter for this one
;(:= turtle.position (vector -400 -400 0))
(add-rule! 'I (list 'P 'F 'F 'P 'R 'R 'P 'B 'B 'P 'L 'L 'U 'U 'J) ruleset)
(add-rule! 'J (list 'R 'R 'R 'P 'F 'F 'P 'R 'R 'P 'B 'B 'P 'L 'L 'U 'U 'K) ruleset)
(add-rule! 'K (list 'B 'B 'B 'P 'F 'F 'P 'R 'R 'P 'B 'B 'P 'L 'L 'U 'U 'I) ruleset)

;turtle-4
;(:= turtle.position (vector 0 0 0))
;(add-rule! 'F (list 'L 'X 'Y* 'P 'D) ruleset)
;(add-rule! 'P '(B Z F U X L Y* L Y* B X F F P Y* R R P Z X Z Y* Z Z Z P F Y* Y R) ruleset)
;(add-rule! 'R '(D B Z Y L Z F U Y B) ruleset)

;turtle-5
;(add-random-rule! 'P 10 ruleset)
;(add-random-rule! 'F 5 ruleset)
;(add-random-rule! 'R 7 ruleset)

(define system
  (expand (list 'I) ruleset 10))

(println ruleset)
(println "" )
(println system)

;--------------drawing------------------

(define (setup)
  (fullscreen)
  (background 0))

(define (draw)
  (translate (/ width  2)
             (/ height 2))
  
  (stroke yellow)
  (fill blue)
  (apply-symbols system 30 'wireframe)
  ;(apply-symbols system 90 'opaque)
  (save "turtle3.png")
  (no-loop))

  
   
