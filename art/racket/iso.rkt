#lang sketching

(require math/matrix)
(require racket/list)

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
;the turtle can place a box next to another box (6 possible directions)
;it can change the rotation of the upcoming boxes
;it can change colour of boxes
;it can save and return to a previous state
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

       ;change this to a case
       (cond [(equal? axis 'x)
              (vector (+ x mag) y z)]
             
             [(equal? axis 'y)
              (vector x (+ y mag) z)]

              [(equal? axis 'z)
               (vector x y (+ z mag))])))
 
  (define/public (place)
    (define-values (w h d)
      (vec->vals dimension))

    (define-values (x y z)
      (vec->vals position))

    (define-values (x-ang y-ang z-ang)
      (vec->vals rotation))

    (cuboid x y z w h d
            (3d-rotate x-ang y-ang z-ang)
            'opaque))

  (define/public (rotate! axis angle)
    
    (define-values (x y z)
      (vec->vals rotation))
    
    (cond [(equal? axis 'x)
           (:= rotation (vector (+ x angle) y z))]
          [(equal? axis 'y)
           (:= rotation (vector x (+ y angle) z))]
          [(equal? axis 'z)
           (:= rotation (vector x y (+ angle z)))]))

  (define/public (move! direction)

    (define-values (w h d)
      (vec->vals dimension))

    (define-values (x y z)
      (vec->vals position))
                  
    ; (:= var k) is the same (set! k (var))
    (cond
      ;I *think* that you can't use case here coz it has side effects idk tho
      [(equal? direction 'up)                 
       (:= position (new-pos 'y (* 2 h)))]

      [(equal? direction 'down)
       (:= position (new-pos 'y (* 2 (- h))))]

      [(equal? direction 'right)
       (:= position (new-pos 'x (* 2 w)))]

      [(equal? direction 'left)
       (:= position (new-pos 'x (* 2 (- w))))]

      [(equal? direction 'forward)
       (:= position (new-pos 'z (* 2 d)))]

      [(equal? direction 'backward)
       (:= position (new-pos 'z (* 2 (- d))))])))


;--------------cfg stuff----------------

(define turtle
  (make-object Turtle
    (vector 0 0 0)   ;position
    (vector 10 10 10);dimensions
    (vector 0 0 0)   ;rotation
    blue             ;colour
    '()))            ;state stack

(define ruleset
  (make-hash))
  
(define (expand init rules depth)
  (define (hash-replace symbol)
    ;gets the expansion of a symbol
    (hash-ref rules symbol))
  
  (define (expand-once lst acc)
    ;expands a list of symbols once 
    (cond [(null? lst)
           (flatten (reverse acc))]
            
          [(hash-has-key? rules (car lst))
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

(define (add-rule symbol expansion)
  (hash-set! ruleset symbol expansion))

(define (apply-symbols symbols)
  (let loop ([lst symbols])
    (cond [(null? lst) #t]
          [(equal? 'U (car symbols)) (turtle.move! 'up)]
          [(equal? 'D (car symbols)) (turtle.move! 'down)]
          [(equal? 'L (car symbols)) (turtle.move! 'left)]
          [(equal? 'R (car symbols)) (turtle.move! 'right)]
          [(equal? 'F (car symbols)) (turtle.move! 'forward)]
          [(equal? 'B (car symbols)) (turtle.move! 'backward)])))
  
;--------------drawing------------------

(define (setup)
  (size 1400 1000)
  (background 0))

(define (draw)
  (translate (/ width  2)
             (/ height 2))
  
  (fill blue)
  (stroke 255)
  (turtle.place)
  (turtle.move! 'right)
  (turtle.place)
  (turtle.move! 'backward))

  
   
