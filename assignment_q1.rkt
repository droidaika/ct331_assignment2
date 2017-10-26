#lang racket


( cons 1 2)
(cons 1 (cons 2 (cons 3 '())))
(cons "String" (cons 0 (cons (cons 1 (cons 2 (cons 3 '()))) '())))
(list "String" 0 '(1 2 3))
(append '("String") '(0) '((1 2 3)))

