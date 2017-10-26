#lang racket

;Create test_tree
(define test_tree'(((() 13 ()) 25 (() 12 ())) 18 ((() 19 ()) 21 (() 16 ()))))  
                   

(define (left_child bst)
  (car bst))

(define (right_child bst)
  (caddr bst))

(define (parent bst)
  (cadr bst))

;part A
;traverse tree
(define (traverse_tree tree);sort left chld then right
  (begin (cond
           [(not (empty? (left_child tree))) (traverse_tree (left_child tree))])
           (printf "~a~n" (parent tree)
          )
         (cond
           [(not (empty? (right_child tree))) (traverse_tree (right_child tree))])
         )
 )

;part B
;find item
(define (search node tree)
  (cond [(empty? tree) #f] ;if empty return #f
        [(equal? node (parent tree)) #t] ;if true return #t
        [(< node (parent tree)) (search node (left_child tree))] ;checks left tree of current node for searched for node
        [(> node (parent tree)) (search node (right_child tree))];checks left tree of current node for searched for node
  )
)

;part C
;insert node into tree
(define (insert node tree)
  (cond
    [(null? tree) (list '() node '())] ;if if empty set root to node
    [(equal? node (parent tree)) tree]   ;if node = current node then return node
    [(< node (parent tree)) (list (insert node(left_child tree))(parent tree) (right_child tree))] ;if node < current node, insert left node into current fuction
    [(> node (parent tree)) (list (left_child tree) (parent tree) (insert node(right_child tree)))];if node > current node, insert right node into current fuction
  )
)



;part D
;take a list of numbers and insert into binary tree
(define (insertList list tree)
  (if (empty? list) tree
      (insertList (cdr list) (insert (left_child list) tree))))



;part e
;take a list of items and display them in sorted order
(define (sort_list list)
  (traverse_tree(insertList list '()))) ;convert list to a tree and sort tree




;part f
;implementing a higher order version of traverse_tree
;takes a list and a function and sorts list in ascending, descending and based on last digit
(define (tree_sort_ascending list)(traverse_tree(insertList list '())))
(define (tree_sort_descending list)(traverse_tree(desertList list '())))
(define (tree_sort_ascendingLD list)(traverse_tree(insertListLD  list '())))
(define (tree_sort_descendingLD list)(traverse_tree(desertListLD  list '())))

(define (desert node tree);like insert but swapped > and <
  (cond
    [(null? tree) (list '() node '())] ;if if empty set root to node
    [(equal? node (parent tree)) tree]   ;if node = current node then return node
    [(> node (parent tree)) (list (desert node(left_child tree))(parent tree) (right_child tree))] ;
    [(< node (parent tree)) (list (left_child tree) (parent tree) (desert node(right_child tree)))];
  )
)

(define (desertList list tree);like insertlist but calls desert
  (if (empty? list) tree
      (desertList (cdr list) (desert (left_child list) tree))))



(define (insertListLD list tree);like insertlist but runs modulo 10 on child
  (if (empty? list) tree
      (insertListLD (cdr list) (insert (get_last_digit (left_child list)) tree))))

(define (desertListLD list tree);like insertlistLD but calls desert
  (if (empty? list) tree
      (desertListLD (cdr list) (desert (get_last_digit (left_child list)) tree))))

(define (get_last_digit num)
  (modulo num 10))


;test part A
(display "Sort Tree\n") 
(traverse_tree test_tree)

;test part B
(display "Test if 6 is in tree\n")
(search 6 test_tree)
(display "test if 18 is in tree #f\n")
(search 18 test_tree)

;test part C
(display "add 42 to emptry tree\n")
(insert 42 '())
(display "add 42 to test tree\n")
(insert 42 test_tree)

;test part D 
(display "Insertinglist (1 2) into test tree\n")
(insertList '(1 2 ) test_tree)

;test part e
(display "sort list by converting to tree\n")
(sort_list '(12 14 24 17 93 26 14 96 34))

;test part f
(display "ascending sort\n")
(tree_sort_ascending '(12 14 24 17 93 26 14 96 34))
(display "descending sort\n")
(tree_sort_descending '(12 14 24 17 93 26 14 96 34))
;(get_last_digit (12 14 24 17 93 26 14 96 34))
(display "ascending sort last digit\n")
(tree_sort_ascendingLD '(12 14 24 17 93 26 14 96 34))
(display "descending sort last digit\n")
(tree_sort_descendingLD '(12 14 24 17 93 26 14 96 34))
