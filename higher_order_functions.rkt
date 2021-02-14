#lang racket
; function foldr
(define (my_foldr operator value lst)
  (if (empty? lst) value
      (operator (car lst)
                (my_foldr operator value (cdr lst)))))
;input
(my_foldr + 30  '(10 20 30 40))
(my_foldr (lambda (x accum) (displayln accum) (+ x accum)) 0 '(1 2 3 4))
;(my_foldr (lambda (x y z) (+ x y z)) 30  '(10 20 30 40))
;-------------------------------------------------------------------------------------------------------------------------------------------------------;
;
;function foldl
(define (my_foldl operator value lst)
  (if (null? lst)
      value
      (my_foldl operator (operator (car lst) value) (cdr lst))))

;to display the order of evaluation :
;(my_foldl (lambda (x accum) (displayln accum) (+ x accum)) 0 '(1 2 3 4))

;-------------------------------------------------------------------------------------------------------------------------------------------------------;
;function filter
(define (my_filter case lst)
   (my_foldr (lambda (x y) (if (case x) (cons x y) y))
                  null
                  lst))
(my_filter even? '(34 23 11 16 56 89))
;-------------------------------------------------------------------------------------------------------------------------------------------------------;
;function map
(define (my_map operator lst)
  (my_foldr (lambda (element accum) (cons (operator element) accum))
            null
            lst))
;-------------------------------------------------------------------------------------------------------------------------------------------------------;
;function reduce
(define (my_reduce operator lst)
  (if
    (null? lst)
    null
 (foldl operator (car lst) (cdr lst))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------;
;function to sort list. The second parameter of function can be also '>' sign as input, so list can be sorted inincreasing and decreasing order 
(define (inserting_sort lst <)
  (define (insert element lst <)
    (cond [(null? lst) (list element)]
          [(< element (car lst))
           (cons element lst)]
          [else
           (cons (car lst)
                 (insert element (cdr lst) <))]))
  (my_foldr (lambda (elem sorted) (insert elem sorted <))
              null
              lst))
;input
(inserting_sort '(13 24 63 11 5 7) >)
;-------------------------------------------------------------------------------------------------------------------------------------------------------;
;function to sort list, example of input : (my_qsort (lambda (x y) (< (car x) (car y))) '((11 34) (23 56) (89 36)))
(define (my_qsort func lst)
  (if (null? lst)
      null
      (let ([pivot (car lst)]) ;choose as pivot the first element of list
        (append (my_qsort func (my_filter (lambda (n) (func n pivot)) (cdr lst)))
                (list pivot)
                (my_qsort func (my_filter (lambda (n) (not (func n pivot))) (cdr lst)))))))
;input 
(my_qsort (lambda (x y) (< (car x) (car y))) '((11 34) (23 56) (89 36)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------;
(define (foldtree f init tree)
    (if (null? tree)
        init
        (apply f 
            (cons
                (car tree)
                (map foldtree 
                    (build-list (length (cdr tree)) (const f))
                    (build-list (length (cdr tree)) (const init))
                    (cdr tree)
                )
            )
        )
    )
)
;2-arity  tree
(define tree1
  '(10
    (5 (3 () ())
       (7 () ())
    )
    (100 () ())
    )
)

(foldtree (lambda (x y z) (* x y z)) 1 tree1)

;3-arity tree
(define tree2
  '(10
    (20 (3 (4
            () ()() )
           () () ) 
        (22 () () () )
        (20 () () () )
       
    )           
    (15 (32 () () () )
       (21 (22
            () () () )
           () () )
       (27 () () () )
       
    )
    (20  (10 () () () )
         (5  () () () )
         (22 () () () )
    )
    )) 

(foldtree (lambda (x y z d ) (+ x y z d )) 0 tree2)
(foldtree (lambda (x y z d ) (- x y z d )) 0 tree2)
(foldtree +  0 tree2)

;4-arity tree
(define tree3
  '(30
    (2 (1 (5
             () () () () )
           () () ())
        (2 ()() () () )
        (3 ()() (6
                 ()() () () )
           ())
        (4 ()() () () )       
    )           
    (3 (7 () () () (11
                     () () () ()))
        (8 () () () () )
        (9 () () () (12
                    () () () ()))
        (10 () () () (13
                     () () () ()))
       
    )
    (4  (14 () () ()() )
         (15  () () () ())
         (16 (17
              () () ()() )
             () () () )
         (18 () () ()() )
    )
    (5  (19 (23
              () () () () )
             () () ())
         (20  () (24
                  ()()()() )
              () ())
         (21 () () () ())
         (22 () () () (25
                       () () () ()))
    )
    ))
        
(foldtree (lambda (x y z d t) (+ x y z d t)) 0 tree3)         
(foldtree - 0 tree3)
