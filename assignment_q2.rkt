#lang racket

;This is an example implementation of ins_beg,
;It obviously doesn't do what it should, so you
;can edit this function to get started.
;
;Please note the provide function is necessary
;for the unit tests to work. Please include a
;(provide) for each function you write in your
;submitted assignment.
;
;You may delete these comments!

(provide ins_beg)

(provide ins_end)
(provide cout_top_level)
(provide count_instances)
(provide count_instances_tr)
(provide count_instances_deep)

(define (ins_beg el lst)
  (append (list el) lst)
  )

(define (ins_end el lst)
  (append  lst (list el))
  )

(define (cout_top_level lst)
  (length lst)
  )

(define (count_instances el lst)
  (c_instances el lst 0)
  )

(define (c_instances el lst total)
  (cond ((null? lst)total)
  
  [(equal? el (car lst)) (c_instances el (cdr lst) (+ 1 total))]
  [else (c_instances el (cdr lst) total)]
  ))



(define (count_instances_tr el lst)
  (cond ((null? lst)0)
    [(equal? el (car lst)) (+ 1 (count_instances_tr el (cdr lst)))]
    [else (count_instances_tr el(cdr lst)) ]
  ))


(define (count_instances_deep el lst)
  (c_instances_deep el lst 0)
  )

(define (c_instances_deep el lst total)
   (cond [(empty? lst) total]; 
    
    [(list? (car lst))(c_instances_deep el (cdr lst) (c_instances el (car lst) total))]
    [(equal? el(car lst)) (c_instances_deep el (cdr lst) (+ 1 total))]
    [else (c_instances_deep el(cdr lst) total) ]
  ))



(ins_beg 'a '(b c d))
(ins_beg '(a b) '(b c d))
(ins_end 'a '(b c d))
(ins_end '(a b) '(b c d))
(cout_top_level '(1 2 3 (4 5)))
(count_instances_tr 3 '(2 4 3 3 7))
(count_instances 3 '(3 1 2 3 3))
(count_instances_deep 3 '(3 2 4 (3 3)) )
