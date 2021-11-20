#lang sketching

(require math/matrix)

;---------------colours--------------
(define yellow
  (color 255 204 0))

(define blue
  (color 0 60 255))

(define red
  (color 255 9 0))

;---------------maths----------------
;https://en.wikipedia.org/wiki/Isometric_projection#Mathematics

(define (3d-rotate axis θ point)
 (define rot-mat
   ;https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions
   (case axis
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

 (define fin-mat
    ;apply our rotation matrix to our point
    (matrix* rot-mat (vector->matrix 3 1 point)))
  
 (matrix->vector fin-mat))
 
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
      
      (mat*scal (matrix [[s3 0     (- s3)]
                         [1  2      1]
                         [s2 (- s2) s2]])
                (/ 1 (sqrt 6)))))
  
  (define fin-mat
    ;apply our mapping matrix to our point
    (matrix* iso-m (vector->matrix 3 1 point)))
  
  (matrix->vector fin-mat))

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

(define (cuboid x y z wid hig dep style)
  ;https://excalidraw.com/#json=Ah-neZtQgizt8Dq-ruEFR,aYpTJ47Sv3YkWp3E_ZWLVA
  ;above is a diagram labelling all verticies
  
  (let ([a (iso (vector (- x wid) (+ y hig) (+ z dep)))]
        [b (iso (vector (- x wid) (+ y hig) (- z dep)))]
        [c (iso (vector (+ x wid) (+ y hig) (- z dep)))]
        [d (iso (vector (+ x wid) (+ y hig) (+ z dep)))]
        [e (iso (vector (- x wid) (- y hig) (+ z dep)))]
        [f (iso (vector (+ x wid) (- y hig) (+ z dep)))]
        [g (iso (vector (+ x wid) (- y hig) (- z dep)))]
        [h (iso (vector (- x wid) (- y hig) (- z dep)))])

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
          (iso-face a b c d)
          (iso-face b c g h)
          
          (iso-face a e f d)
          (iso-face c d f g)
          (iso-face e h g f)])))

(define (cuboid-ang x y z wid hig dep axis θ style)
  ;https://excalidraw.com/#json=Ah-neZtQgizt8Dq-ruEFR,aYpTJ47Sv3YkWp3E_ZWLVA
  ;above is a diagram labelling all verticies
  (let ([a (iso (3d-rotate axis θ (vector (- x wid) (+ y hig) (+ z dep))))]
        [b (iso (3d-rotate axis θ (vector (- x wid) (+ y hig) (- z dep))))]
        [c (iso (3d-rotate axis θ (vector (+ x wid) (+ y hig) (- z dep))))]
        [d (iso (3d-rotate axis θ (vector (+ x wid) (+ y hig) (+ z dep))))]
        [e (iso (3d-rotate axis θ (vector (- x wid) (- y hig) (+ z dep))))]
        [f (iso (3d-rotate axis θ (vector (+ x wid) (- y hig) (+ z dep))))]
        [g (iso (3d-rotate axis θ (vector (+ x wid) (- y hig) (- z dep))))]
        [h (iso (3d-rotate axis θ (vector (- x wid) (- y hig) (- z dep))))])

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

;--------------drawing------------------

(define pos #(0 0 0))

(define a
  0)
(define (setup)
  (set-frame-rate! 30)
  (size 1400 1000))


(define (draw)
  (background 0)
  
  (translate (/ width  2)
             (/ height 2))

  (stroke red)
  (cuboid-ang 100 301 100 100 100 100 'x a 'wireframe)
  (stroke yellow)
  (cuboid-ang 100 100 301 100 100 100 'x a 'wireframe)
  (stroke blue)
  (cuboid-ang 301 100 100 100 100 100 'x a 'wireframe)
  
  (stroke red)
  (cuboid-ang 100 301 100 100 100 100 'y a 'wireframe)
  (stroke yellow)
  (cuboid-ang 100 100 301 100 100 100 'y a 'wireframe)
  (stroke blue)
  (cuboid-ang 301 100 100 100 100 100 'y a 'wireframe)

  (stroke red)
  (cuboid-ang 100 301 100 100 100 100 'z a 'wireframe)
  (stroke yellow)
  (cuboid-ang 100 100 301 100 100 100 'z a 'wireframe)
  (stroke blue)
  (cuboid-ang 301 100 100 100 100 100 'z a 'wireframe)
  ;(cuboid 0 0 0 100 100 100 'wireframe)
  (set! a (+ a 0.005))
  )
            
 
  
;-------------main----------------------

;to make our sketch let's first make a main cube

;then on one side of the cube lets make some cuboids on
;the surface of our main cube

;next, we rotate these cuboids 90 degrees.
;now reflect on the x,y,z axis to get 6 lines of symmetry





    

    
    
