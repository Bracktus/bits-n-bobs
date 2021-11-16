#lang racket

(define file-contents
  (port->string (open-input-file "day-1.input") #:close? #t))

(define (main str)
  (let loop ([elevation 0]
             [lst (string->list str)])

    (cond 
      [(null? lst)
       elevation]

      [(equal? #\( (car lst))
       (loop (add1 elevation)
             (cdr lst))]

      [(equal? #\) (car lst))
       (loop (sub1 elevation)
             (cdr lst))]
      [else 
        (loop elevation
              (cdr lst))])))
(define (main-2 str target)
  (let loop ([elevation 0]
             [lst (string->list str)]
             [count 0])

    (cond 
      [(null? lst)
       #f]
        
      [(equal? target elevation)
       count]

      [(equal? #\( (car lst))
       (loop (add1 elevation)
             (cdr lst)
             (add1 count))]

      [(equal? #\) (car lst))
       (loop (sub1 elevation)
             (cdr lst)
             (add1 count))]

      [else (loop elevation
                  (cdr lst)
                  count)])))

(main file-contents)
(main-2 file-contents -1) 
