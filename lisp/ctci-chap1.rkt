#lang racket

(define (unique-chars? string)
  ;return true if each character in the string is unique (no extra space)
  (let loop ([lst (sort (string->list string) char<?)])
    (if (null? (cdr lst))
        #t
        (and (not (equal? (car lst) (cadr lst)))
             (loop (cdr lst))))))

(define (perm? string-1 string-2)
  ; checks if string-1 is a permutation of string-2
  (if (equal? (sort (string->list string-1) char<?)
              (sort (string->list string-2) char<?))
      #t
      #f))

(define (pali-perm? string)
  ;checks if a string is a permutation of a palindrone
  (define (count-odd hash)
    (count odd? (hash-values hash)))
  
  (let loop ([hash (make-immutable-hash)]
             [lst (string->list string)])
    (if (null? lst)
        
        (> 2 (count-odd hash))
        
        (let ([count (hash-ref hash (car lst) (λ () 0))])
          (loop (hash-set hash (car lst) (add1 count))
                (cdr lst))))))

(define (one-away? string-1 string-2)
  ;edits include: insert  a char,
  ;               remove  a char,
  ;               replace a char
  (define (replace? l1 l2 count)
    (cond [(null? l1)
           (> 2 count)]
          
          [(not (equal? (car l1) (car l2)))
           (replace? (cdr l1) (cdr l2) (add1 count))]
          
          [else
           (replace? (cdr l1) (cdr l2) count)]))

  (define (insert? l1 l2)
    (cond [(null? l1)
           #t]

          [(equal? (car l1) (car l2))
           (insert? (cdr l1) (cdr l2))]

          [else
           ;when we reach a difference, compare the rest of l1 and l2
           (equal? l1 (cdr l2))]))
             
  
  (let* ([lst-1 (string->list string-1)]
         [lst-2 (string->list string-2)]
         [len-1 (length lst-1)]
         [len-2 (length lst-2)])
    
    (cond [(= len-1 len-2)
           (replace? lst-1 lst-2 0)]

          [(= (add1 len-1) len-2)
           (insert? lst-1 lst-2)]

          [(= len-1 (add1 len-2))
           (insert? lst-2 lst-1)])))

(define (compress str)

  (define (number->char num)
    (string-ref (number->string num) 0))

  (if (= (string-length str) 0) ""
    (let loop ([pos 0]
               [count 0]
               [acc '()])

      (let* ([curr-str (string-ref str pos)]
             [curr-count (add1 count)]
             [curr-count-char (number->char curr-count)]
             [curr-pos (add1 pos)]
             [diff-add (cons curr-count-char (cons curr-str acc))])

        (cond [(= (add1 pos) (string-length str))
               (list->string (reverse diff-add))]

              [(equal? (string-ref str pos)
                       (string-ref str (add1 pos)))
               (loop curr-pos curr-count acc)]

              [else 
               (loop curr-pos 0 diff-add)])))))

(define (rotated-string? string-1 string-2)
  ;(rt? "waterbottle" "erbottlewat") -> true
  )
  



(unique-chars?  "helo")
(unique-chars?  "hello")

(perm? "abcdefg" "fgebach")
(perm? "abcdefg" "gfedcba")

(pali-perm? "ttaococ")
(pali-perm? "racercar")

(one-away? "pale" "ple")
(one-away? "pales" "pale")
(one-away? "pale" "bale")
(one-away? "bake" "pale")

(compress "aabbbccccce")
(compress "")

