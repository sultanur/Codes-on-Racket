#lang racket

;(prodNestedLists '(5 (6 7) 8) '(1 (2 3) 3)) -'(5 (12 21) 24)
( define (prodnestedlists  l1 l2 ) 
(if (null? l1)null 		
    (if(null? l2) null
       (if( list? ( car l1 ))
(cons ( prodnestedlists ( car l1 ) (car l2 ) ) ( prodnestedlists ( cdr l1 ) ( cdr l2 ) ) )
         (cons ( * (car l1 ) (car l2 ) ) (prodnestedlists ( cdr l1 ) ( cdr l2 ) ) ) ))))
(define (sumNeighbours lst) 
  (if (null? lst) null
      (if (null? (cdr lst)) (cons (car lst) null) 
          (cons (+ (car lst) (cadr lst)) (sumNeighbours (cdr (cdr lst)))))))
;(sumsublist '((3 1) (8) ()))->'(4 8)
;(sumsublist '((3 1) (8 4) (1 2))) ->' (4 12 3)
(define (sumsublist lst)
  (if (null? lst) '()
      (if (list? (car lst))
          (append (sumNeighbours (car lst)) (sumsublist (cdr lst)))
          (append (+ (car lst) (sumsublist (cdr lst))) (sumsublist (cdr lst))))))  
;(Nested listSum '(6 3 -2 5 (4 2 -3) 4)); ->19
(define (Nested listSum li)
  (if (null? li) 0
      (if (list? (car li))
          (+ (Nested listSum (car li)) (Nested listSum (cdr li)))
           (+ (car li) (Nested listSum (cdr li))))))
;(nestedListsSum '(2 4 6 (1 2 (3))) '(3 1 8 (1 2 (3)))); -> '(6 4 48 (1 4 (9)))
(define (nestedListsSum lst1 lst2)
  (cond
    [(null? lst1) lst2]
    [(pair? (car lst1))
     (cons (nestedListsSum (car lst1) (car lst2))
           (nestedListsSum (cdr lst1) (cdr lst2)))]
    [else (cons (* (car lst1) (car lst2))
           (nestedListsSum  (cdr lst1)  (cdr lst2)))]))


;(sums '(2 5 3)) ; => (0 2 7 10)
(define (sums l)
  (define (subsums prefix l)
    (if (null? l)
        (reverse prefix)
        (subsums (cons (+ (car prefix) (car l)) prefix) (cdr l))))
  (subsums '(0) l))
;Write a split function that splits a list into two lists of equal length.
; (mysplit ( 1  2  3  4  5 ) ) - >  ( ( 1  2  3 ) ( 4  5 ) )
(define (mysplit lst)
  (mysplit2  lst lst)) 
(define (mysplit2  lst1 lst2)
  (if (or (null? lst2) (null? (cdr lst2)))
      (cons null (cons lst1 null))
      (helper (car lst1) (mysplit2 (cdr lst1) (cdr (cdr lst2))))))      
(define (helper elem1 elem2)
  (cons (cons elem1 (car elem2)) (cdr elem2) )) 
;( myjoin ' ( 1  2  3  5  7  9  10 ) ' ( 2  4  6  8 ) ) - >  ( 1  2  2  3  4  5  6  7  9  10 )
( define ( myjoin list1 list2 ) 
  ( if  ( null? list1 ) list2
      ( if  ( null? list2 ) list1
                   ( if  ( <  ( car list1 )  ( car list2 ) ) 
              ( cons  ( car list1 )  (myjoin ( cdr list1 ) list2 ) ) 
              ( cons  ( car list2 )  ( myjoin (list1 ( cdr list2 ) ) ) )))))
;(merge-sort '(2 4 5 1 7 9)) -> '(1 2 4 5 7 9)
  (define (merge-lists xs ys)
  (cond    [(null? xs) ys]
                [(null? ys) xs]
                [(< (car xs) (car ys))
                      (cons (car xs) (merge-lists (cdr xs) ys))]
                [else  (cons (car ys) (merge-lists xs (cdr ys)))])) 
;(take 2 '(1 2 3 4 5)) ->'(1 2)
(define (take n lst)
  (if (null? lst) null
      (if (= n 0) null
          (cons (car lst) (take (- n 1)(cdr lst)))))) 

(define (merge-sort xs)
  (cond  [(or (null? xs) (null? (cdr xs))) xs]
              [(null? (cddr xs))
                        (merge-lists (list (car xs)) (cdr xs))]
               [else   (let ([x (ceiling (/ (length xs) 2))])
                          (merge-lists (merge-sort (take xs x))
                          (merge-sort (drop xs x))))]))
;(invert '((a 1) (a 2) (1 b) (2 b)))->'((1 a) (2 a) (b 1) (b 2))
(define invert
    (lambda (lst)
    (if(null? lst)     '()
    (cons (list (car (cdr (car lst))) (car (car lst))) ; reverse first lst
        (invert(cdr lst))))))
; (drop 3 '(1 2 1 23 2 1 1 2 )) ->'(23 2 1 1 2)
(define (drop n lst)
  (if (null? lst) null
      (if (= n 0)lst
          (drop (- n 1) (cdr lst)))))
;(append '(1 2 3 4) 5) ->'(1 2 3 4 5)
(define (append lst n)
  (if (equal? lst '())
      (cons n '())
      (cons  (car lst) (append (cdr lst) n))))
;(concat '(1 2 3 4) '(5 6 7 8)) ->'(1 2 3 4 5 6 7 8)
(define (concat lsta lstb)
  (if (null? lsta) lstb
      (if (null? lstb) lstb
          (cons (car lsta) (concat (cdr lsta) lstb)))))
(define (my-length lst)
  (if (empty? lst) 0     
      (+ 1 (my-length (rest lst)))))
(define (my_map oper init  lst)
  (if( null? lst) null
       (cons (oper init (car lst))(my_map oper init (cdr lst)))))

;(my_reverse '(1 2 3 4))-> '(4 3 2 1)
(define (my_reverse lst)
  (cond ((null? lst) null)
      (else (append (my_reverse (cdr lst))
             (cons (car lst) null)))))
;(remove 'c '(a c b c d c))->(a b d)
(define remove
  (lambda (s ls)
    (if (null? ls) '()
      (if (eqv? (car ls) s)
        (remove s (cdr ls))
        (cons (car ls) (remove s (cdr ls)))))))
;(Last '(1 2 3 4 5 6 7 8 9)) -> 9
(define (Last list)
  (if (null? list)
      null
      (if (null? (cdr list))
          (car list)
          (Last (cdr list)))))
;(square ‘(1 2 3 4)) -> ‘(1 4 9 16)
(define (square x)  (* x x))
(define (squarer xs)
  (if (empty? xs)  empty
      (cons (square (first xs)) (squarer (rest xs)))))
;(divisibleby 3 ‘(2 3 4 5 6 7 8 9 10)) ->'(3 6 9)
(define (divisible x lst)
    (cond [(empty? lst) empty] 
    [(or (= x (first lst))(= 0 (remainder (first lst) x))) (cons (first lst) (divisible x (rest lst)))] 
    [else (divisible x (rest lst))]))
;(findsimilar '(1 3 4 1 2 3 1 2 5 1 2)) ->'(1 1 1 1)   
(define (findsimilar A)
  (if (null? A) null
      (filter (lambda (x) (eq? (car A) x)) A)))     
;(mean '(2 3 4 8 5))-> 4.4
(define mean
  (lambda (li)
    (/ (sum li) (length li))))
(define sum
  (lambda (li)
    (if (null? li)0       
        (+ (car li) (sum (cdr li))))))
;(filterOutLt '(10 8 4 1 5 6) 8)->'(10 8)
(define (filterOutLt list num)
  (if (equal? list null)      null 
     (if (< (car list) num)
      (filterOutLt (cdr list) num)
      (cons (car list) (filterOutLt (cdr list) num)))))
;Program a function in LISP that merges sublists from a list into a list
; and removes duplicates from the list (leaving only the first occurrence)
;(deleteFromSecond  '(1 2 (1 2 3) 2)) ->'(1 2 3)
;(deleteFromSecond  '(1 2 5 (1 2 3) 2 3)) ->'(1 2 5 3)
(define (my-flatten lst) 
  (cond ((null? lst) '()) 
        ((pair? lst) 
         (append (my-flatten (car lst)) (my-flatten (cdr lst)))) 
        (else (list lst)))) 
(define (dup-rem lst) 
  (cond [(empty? lst) empty] 
    [else (cons (first lst) (dup-rem (filter (lambda (x) (not (equal? (first lst) x))) lst)))])) 
(define (deleteFromSecond lst)
  (if (null? lst) null
          (dup-rem (my-flatten lst))))
; function foldr. ;(my_foldr + 10  '(10 20 30 40)) -> 110
(define (my_foldr operator value lst)
  (if (empty? lst) value
      (operator (car lst)
                (my_foldr operator value (cdr lst))))) 
;function foldl. ;(my_foldl (lambda (x accum) (displayln accum) (+ x accum)) 0 '(1 2 3 4))
(define (my_foldl operator value lst)
  (if (null? lst)  value
           (my_foldl operator (operator (car lst) value) (cdr lst))))
;(my-pack '(a a a a b c c a a d e e e e))-> '((a a a a) (b) (c c) (a a) (d) (e e e e))
(define (my-pack xs)
  (define (my-pack-aux xs ys)
    (cond ((null? xs) null)
          ((and (not (null? (cdr xs)))
                (equal? (car xs) (cadr xs)))
           (my-pack-aux (cdr xs) (cons (car xs) ys)))
          (else (cons (cons (car xs) ys) (my-pack-aux (cdr xs) '())))))
  (my-pack-aux xs '()))
;Tail recursive Fibonacci
;(fib 10)->55
(define (fibonacci-iter a b n)
  (if (= 0 n) (+ a b)     
      (fibonacci-iter b (+ a b) (- n 1))))
(define (fibonacci n)
  (fibonacci-iter -1 1 n))
;tail recursive factorial
;(factorial 5)->120
(define (factorial n)
  (factorial_helper n 1))
(define (factorial_helper  z  answer_so_far)
  (if (=  z  1)    answer_so_far
      (factorial_helper (- z 1) (* z answer_so_far))))
;(sublist '(1 2 3 4 5)) will print all sublists
(define (sublist L)
  (if (null? L)    '(())   
      (let* ([it (first L)]
             [lose-it (sublist (rest L))]
             [use-it (map (lambda (E) (cons it E)) lose-it)])
        (append lose-it use-it ))))    
;(splitting_byNumber '(1 2 3 4 5 6 7) 2) ->'((1 2) (3 4 5 6 7))
(define (splitting_byNumber lst i)
  (list (my-take lst i)
        (my-drop lst i)))
(define (my-take lst i)
  (if (> i 0) (cons (first lst)
      (my-take (rest lst) (- i 1)))     '()))
  (define (my-drop lst i)
  (if (> i 0) (my-drop (rest lst) (- i 1))  lst))                                  

;(dup-rem '(2 5 4 5 1 2));->'(2 5 4 1)
(define (my_filter fn lat)
  (cond ([null? lat] '())
    (else (if (fn (car lat))
         (cons (car lat) (my_filter fn (cdr lat)))
         (my_filter fn (cdr lat))))))    
;(frequencies'(1 2 3 1 2 4 5 6 1 2 1 2 3));->'((1 . 4) (2 . 4) (3 . 2) (4 . 1) (5 . 1) (6 . 1))
(define (run-length-encode lst)
  (define (helperfunc val-lst cur-val cur-cnt acc)
    (if (pair? val-lst)
        (let ((new-val (car val-lst)))
          (if (eq? new-val cur-val)
              (helperfunc (cdr val-lst) cur-val (+ cur-cnt 1) acc)
              (helperfunc (cdr val-lst) new-val 1 (cons (cons cur-val cur-cnt) acc))))
        (cons (cons cur-val cur-cnt) acc)))    (if (pair? lst)
      (reverse (helperfunc (cdr lst) (car lst) 1 '())) '()))
(define (frequencies lst) 
        (run-length-encode (sort lst <)))
;(removeAll  '(1 2 1 2 3 2 1 1 2) '(1 2) ); -> (3)
;(removeAll  '(1 2 4 6 8 9 3 2 1 1 2) '(1 2) )->'(4 6 8 9 3)
(define (removeAll list-a list-b)
  (if (empty? list-b) list-a      
     (removeAll (my_filter (lambda (x) (not (eq? (first list-b) x))) list-a) (rest list-b))))

;(elem-percent '(1 2 3 1 2 4 5 6 1 2 1 2 3))->'((1 . 30.76) (2 .30.76) (3 . 15.38) (4 . 7.70) (5 . 7.70) (6 . 7.70))

(define (scale-cdr count-list total-count)
  (define (normalize pr)
    (cons (car pr) (/ (* 100 (cdr pr)) total-count)))
  (map normalize count-list))
(define (elem-percent lst)
  (scale-cdr (run-length-encode (sort lst <)) (length lst)))
;(group-by-my-rule '((a 1) (a 2) (a 3) (b 1) (e 2) (c 2) (c 3) (c 4) (d 1) (e 1) (c 1)))->'((a 1 2 3) (b 1) (e 2 1) (c 2 3 4 1) (d 1))
(define (group-by-my-rule mix-ls)
  (map (λ (ls) (cons (first (first ls)) (map second ls)))
         (group-by first mix-ls)))

 ;function filter (my_filter even? '(34 23 11 16 56 89)) ->’(34 16 56 )
;function map

(define (reduce fn lat)
  (cond ([null? (cdr lat)] (car lat))
    (else (fn (car lat) (reduce fn (cdr lat))))))
;function reduce
(define (my_reduce operator lst)
  (if  (null? lst)  null
        (foldl operator (car lst) (cdr lst))))
;(enumerate-interval 2 7) -> '(2 3 4 5 6 7)
(define (enumerate-interval low high)
  (if (> low high) null
      (cons low (enumerate-interval (+ low 1) high))))
 
(define (divides? a b) (= (remainder b a) 0))
;(find-divisor 23 2)->23
;(find-divisor 28 2)->2
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (smallest-divisor n)
  (find-divisor n 2))
;(prime? 3)->#t
(define (prime? n)
  (= n (smallest-divisor n)))
;(prime-sum? '(5 7)) -#f
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
;(make-pair-sum '(1 2));->'(1 2 3)
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;(tree->list-1 '(5 (3 () ()) (8 () ())))->'(3 5 8)
(define (tree->list tree)
  (if (null? tree)  '()     
      (append (tree->list (cadr tree))
        (cons (car tree) (tree->list (caddr tree))))))
;(union-set '(1 7 8 4 5) '( 9 0));->'(1 7 8 4 5 9 0)
(define (union-set set1 set2)
  (cond ((null? set1)  set2)        
       ((not (element-of-set? (car set1) set2))
         (cons (car set1) (union-set (cdr set1) set2)))               
        (else (union-set (cdr set1) set2))))
(define (element-of-set? x set)
  (cond ((null? set)  #f)        
        ((= x (car set))   #t)      
        ((< x (car set))  #f)        
        (else (element-of-set? x (cdr set)))))
;(filter even? '(1 2 3 4 5 6 7 8 9 10))-> '(2 4 6 8 10)
(define (filter oper lst)
  (reverse (filter-help oper lst '()))) 
(define (filter-help oper lst res)
  (cond ((null? lst) res)
        ((oper (car lst)) 
           (filter-help oper (cdr lst)  (cons (car lst) res)))
        (else (filter-help oper (cdr lst)  res))))


;(inserting_sort '(13 24 63 11 5 7) >)
(define (inserting_sort lst op)
  (define (insert element lst op)
    (cond [(null? lst) (list element)]
          [(op element (car lst))
             (cons element lst)]
          [else (cons (car lst)
                  (insert element (cdr lst) op))]))
  (my_foldr (lambda (elem sorted) (insert elem sorted op))  null lst))     
;(insertion-sort '(1 2 6 4 9 6 1 2)) ->'(1 1 2 2 4 6 6 9)
(define (insertion-sort xs)
  (cond [(empty? xs) '()]
        [else (local
                [(define (insert x ys)
                        (cond [(empty? ys) (cons x '())]
                              [(<= x (first ys)) (cons x ys)]
                              [else (cons (first ys) (insert x (rest ys)))]))]
                    (insert (first xs) (insertion-sort (rest xs))))]))
;(SecondMin '(4 5 67 1 2 9 4 3)) -> 2
(define (my_qsort func lst)
  (if (null? lst) null
      (let ([pivot (car lst)]) ;choose as pivot the first element of list
     (append (my_qsort func (filter (lambda (n) (func n pivot)) (cdr lst)))
                (list pivot)
                (my_qsort func (filter (lambda (n) (not (func n pivot))) (cdr lst)))))))
(define (SecondMin lst)
  (second (my_qsort < lst)))
  ;(sum-of-proper-divisors 25)->6
(define (sum-of-proper-divisors n)
  (let loop ((i (sub1 n)))
    (cond [(= i 1) 1]
      [(= (remainder n i) 0)
       (+ i (loop (sub1 i)))]
      [else (loop (sub1 i))])))

;(reduce1 '(1 2 3 4 5) + 10) -> 25
(define (reduce1 lst oper init)
  (if (null? lst) init
      (if (null? (cdr lst))
                 (oper (car lst) init)
          (oper (car lst) (reduce1 (cdr lst) oper init)))))
;(myMulti  '(1 2 3 4 5 6 7) '(1 2 3 4))->  '(1 4 9 16 5 6 7)
(define (myMulti lst x)
  (if(null? lst)  x
     (if(null? x)  lst
        (cons(*(car lst)(car x))(myMulti(cdr lst)(cdr x))))))          
;(evenFilter '(1 2 3 4 5 6 7)) -> '(2 4 6 )                                               
(define (evenFilter lst)
  (if(null? lst)null
     (if(equal?(remainder(car lst)2) 0)               
               (cons(car lst)(evenFilter(cdr lst)))
               (evenFilter(cdr lst)))))                   
;(mySum '(1 2 3 4 5)) ->15
(define(mySum lst)
  (if(null? lst) 0
     (+(car lst)(mySum (cdr lst)))))  
;(AddIfOdd `(1 2 3 4 5 6 7 8 9 10) 1) ;; produces '(2 2 4 4 6 6 8 8 10 10)
(define (AddIfOdd list num)
  (map (lambda (elem) (if (even? elem)
                           elem (+ elem num) ) ) list))      
;(odd-even '(1 2 3 4 5 6)) ->'((1 3 5) (2 4 6))
(define (odd-even s)
  (foldr (lambda (x b)
           (if (odd? x)
               (list (cons x (first b)) (second b))
               (list (first b) (cons x (second b)))))
               '(()()) s))
;(myRemoveElement '(1 2 3 4 5 6 7 8) 3)->'(1 2 4 5 6 7 8)
(define (myRemoveElement l e)
    (cond
        ((null? l) null)            
        ((equal? (car l) e)
            (myRemoveElement (cdr l) e))       
        (else  (cons (car l) (myRemoveElement (cdr l) e)))))   
;(remove-last-occurrence  2 '(1 2 (1 2 3) 2)) -> '(1 2 (1 2 3))
;(remove-1st  2 '(1 2 (1 2 3) 2)) -> '(1 (1 2 3) 2)
(define remove-1st
    (lambda (x ls)
        (if (null? ls) '()                           
            (if (equal? x (car ls)) (cdr ls)                                           
                (cons (car ls) (remove-1st x (cdr ls)))))))
(define remove-last-occurrence
  (lambda (x ls)
    (reverse (remove-1st x (reverse ls))))) 
  (define reverse
  (lambda (ls) (if (null? ls)  '()           
        (append (reverse (cdr ls)) (cons (car ls) '())))))       
;(slice '(1 2 3 4 5 6 7) 3 5) ->'(4 5)
;(slice '(1 2 6 4 5)1 4) ->'(2 6 4)
(define (slice list n1 n2)
  (if (equal? n1 0)
      (if (equal? n2 1)
          (cons (car list) '())
          (cons (car list) (slice (cdr list) n1 (- n2 1))))          
          (slice (cdr list) (- n1 1) (- n2 1))))
;(myHasElement  '(1 2 3 4 5 6 7 8) 3) -> #t
(define (myHasElement l e)
    (cond   ((null? l)    null)
        ((equal? (car l) e)  true)           
        (else (myHasElement (cdr l) e))))
;(myRangeSum '(1 2 6 4 5)1 4)->7
;(myRangeSum '(1 2 3 4 5 7 8 9 10) 2 5)-> 14
(define (myRangeSum lst min max)
    (cond  ((null? lst) 0)        
           ((and (>= (car lst) min) (<= (car lst) max))
            (+ (car lst)(myRangeSum (cdr lst) min max)))
           (else (myRangeSum (cdr lst) min max))))
;(contains 4 '(1 2 3  5)) -> #f
(define (contains item lst)
  (if (null? lst) #f
      (if (equal? (car lst) item) #t
          (contains item (cdr lst)))))
;(intersection '(1 2 3 4 7) '(3 4 5 6))-> '(3 4)
(define (intersection a b)
  (if (null? a) '()      
      (if (contains (car a) b)
          (cons (car a) (intersection (cdr a) b))
          (intersection (cdr a) b))))   
;(split '(1 2 3 4 5 6 7)) -> '((1 3 5 7) (2 4 6))
(define (split l)
  (cond ((null? l) '(() ()))
        ((null? (cdr l)) (list (list (car l)) '()))
        (else (map cons (list (car l) (cadr l))
                        (split (cddr l))))))
;(maximum '(18 39 57 -4 0)) -> 57
(define (maximum L)
     (if (null? (cdr L)) (car L)         
         (if (< (car L) (maximum (cdr L)))  
             (maximum (cdr L)) 
             (car L))))        
;(diffMinMax '(1 2 3 4 5 6 7))-> 6
(define (diffMinMax list)
  (minMaxItem list (car list) (car list)))
(define (minMaxItem list min max)
  (if (null? list)
      (- max min)
      (if (< (car list) min)
          (minMaxItem (cdr list) (car list) max)
          (minMaxItem (cdr list) min (car list)))))
;(add_nestedList 10  '((1 2) (3 4) (5 (6 (7 8 9))))) -> '((11 12) (13 14) (15 (16 (17 18 19))))  
(define (add_nestedList number lst)
  (cond ((null? lst)   null)
               ((list? (car lst))
         (cons (add_nestedList  number (car lst))
               (add_nestedList  number (cdr lst))))
        (else (cons (+  number (car lst))
               (add_nestedList  number (cdr lst))))))
;(minimum '(18 39 57 -4 0)) -> -4
(define (minimum lst)
  (if (null? (cdr lst)) (car lst)
            (if (> (car lst) (minimum (cdr lst)))
          (minimum (cdr lst))
          (car lst))))
;(min_max '(18 39 57 -4 0))-> '(-4 . 57)
(define (min_max lst)
   (cons (minimum lst) (maximum lst)))

;(count_occurrences 1 '(1 3 4 1 6 1)) -> 3
(define (count_occurrences x L)
  (if (null? L) 0
(if (eq? x (car L))
(+ 1 (count_occurrences x (cdr L)))
(count_occurrences x (cdr L)))))
;(recLen2 '(1 1 1 ((9 9) 9 8) 1 2 3 5 1)) ->12
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))
(define (recLen x)
    (if (null? x) 0    
        (if (atom? (car x))    (+ 1 (recLen (cdr x)))
                                           (+ (recLen (car x)) (recLen (cdr x))))))
;(divisors 30 25 );-> '(1 5)
;(divisors 100 150 )-> '(1 2 5 10 25 50)
(define (factor x) 
  (if (null? x) 0 
  (if (= x 1) 1     
      (my_filter (lambda (n) (= (remainder x n) 0))  
          (build-list x (lambda (y) (+ 1 y)))))))
   

(define (gcd a b) ; (gcd 4 12)-> 4
  (if (= b 0) a
                 (gcd b (remainder a b))))
;(Counter '(a b (a c) (a) b ) 'a) ->3
(define (Counter lst target)
  (if (empty? lst) 0
      (+ (Counter (cdr lst) target) 
         (let ((x (car lst)))
           (if (list? x)  (Counter x target) 
               (if (eqv? x target) 1 0))))))
;(select_set (lambda (x) (< 5 x)) '(2 4 8 5 3 9)) ->’(8 9)
(define (select_set p l)
  (cond
    ((null? l) '())
    ((p (car l)) (cons (car l)(select_set p (cdr l))))
    (else (select_set p (cdr l)))))
;;; Subtract set B from set A and make S = A - B ;;;
;(remove-list '(1 2 3 4) '(3 4 5 6)) ->'(1 2)
(define (remove-list orig-list elem-list)
    (if (null? elem-list)
        orig-list
        (remove-list (remove (car elem-list) orig-list) (cdr elem-list))))
;(deleteFromSublist '(1 2 (1 2 3) 2) 2) -> '(1 (1 3))      
(define (deleteFromSublist list1 item) 
    ( cond
    ((null? list1) '())
    ((pair? (car list1))
 (cons (deleteFromSublist (car list1) item)
 (deleteFromSublist (cdr list1) item)))
    ((equal? (car list1) item) (deleteFromSublist (cdr list1) item)) 
    (else (cons (car list1) (deleteFromSublist (cdr list1) item)))))
;(my_last '(1 5 (4 )6 (7) 8))->'(8)
(define (my_last list)
  (if (null? list)  null     
    (if (null? (cdr list)) list	   (my_last (cdr list)))))
;remove all occurences of elements in 1-st of elements in 1-st list
;(removefromList '(1 2 1 2 3 2 1 1 2) '(1 2)) -> (3)
;(removeAll  '(1 2 1 2 3 2 1 1 2) '(1 2) ); -> (3)
;(removeAll  '(1 2 4 6 8 9 3 2 1 1 2) '(1 2) )->'(4 6 8 9 3)
;(changeSign '(1 2 -3 -1 5 -7)) ->'(-1 -2 3 1 -5 7)	
(define (changeSign lst) 
  (cond ((null? lst) null) 
              ((list? (car lst)) 
         (cons (changeSign  (car lst)) 
               (changeSign  (cdr lst)))) 
        (else (cons (* -1 (car lst)) 
               (changeSign   (cdr lst))))))
;(add-into 4 2 '(1 5 7 8 9 10 18)) ->'(1 4 5 7 8 9 10 18)
(define (add-into new p lst)
  (cond ((null? lst)
         (cons new lst))  ((= p 1)
         (cons new lst))
        (else  (cons (car lst) (add-into new (- p 1) (cdr lst))))))
;'(1 4 9 16 25 36 49 64 81)
(filter (lambda (n) (integer? (sqrt n))) (range 1 100))
;(sort_predicate < '(8 2 5 2 3))
;(sort_predicate > '(8 2 5 2 3))
(define sort_predicate
  (lambda (pred lst)
    (define is-pred (lambda (predicate toCompare) (lambda (x) (predicate x toCompare))))
    (define is-not-pred (lambda (predicate toCompare) (lambda (x) (not (pred x toCompare)))))
    (cond ((null? lst) '())
    (else (append (sort_predicate pred (filter (is-pred pred (car lst)) (cdr lst)))
               (list (car lst))
                (sort_predicate pred (filter (is-not-pred pred (car lst)) (cdr lst))))))))
;(find_divisors 35 50 ) ->'(35 7 5 . 1)
(define (find_divisors n i)  
  (cond  ((= 1 i) 1)        
         ((= (remainder n i) 0)
         (cons i (find_divisors n (sub1 i))))         
        (else (find_divisors n (sub1 i)))))
;(factor 60)->'(1 2 3 4 5 6 10 12 15 20 30 60)

;(exactRow '((1 2 3) (4 5 6) (7 8 9)) 3) -> '(7 8 9)
( define (exactRow  lst num ) 
( if  ( = num 1 )  ( car lst ) 		
( exactRow ( cdr lst ) ( - num 1 ) ))) 
;BST represented as (H, C, L, R) ;(contains '(5 1 (3 2 () ()) (10 4 () ())) 3) -> 2 
; (contains' (5 1 (3 2 () ()) (10 4 () ( ))) 10) -> 4 
(define (bst_count  tree x ) 
( if  ( null? tree) null    								 
( if  (= ( car tree ) x ) ( cadr tree )			 			
( if  ( < x ( car tree )) 
( bst_count ( caddr tree ) x )  	
( bst_count ( cadddr tree ) x )))))  



			
;List elements from the tree with a frequency> 1 
;(bst_multiples '(10 1 (5 1 () ()) (15 3 (12 2 () ())() ))) ->'(15 12)
(define (bst_multiples  tree) 
(if(null? tree) '()     
(if (> (cadr tree) 1) 
(cons ( car tree )  ( bst_multiples ( caddr tree ) ))
                        (bst_multiples (cadddr tree )))))            
;Add an element to the tree. If there is, increase its frequency
;(bst_insert '(10 1 (5 1 () ()) () ) 5) ->'(10 1 (5 2 () ()) ())	
(define (bst_insert  tree x ) 
(if( null? tree) 
(list x 1  '()  '() ) 
(if  (=  ( car tree ) x ) 
(list ( car tree )  (+ ( cadr tree )  1 )  (caddr tree)  (cadddr tree ) ) 
(if  (< x ( car tree ) ) 
(list  ( car tree )  ( cadr tree )  ( bst_insert ( caddr tree ) x )  ( cadddr tree ) ) 
(list  ( car tree )  ( cadr tree )  ( caddr tree )  ( bst_insert ( cadddr tree ) x ) )))))
  ;(bst-insert 3 '(5()() ))->'(5 (3 () ())())
;(bst-insert 4 '(5 (3()()) ())) -> '(5 (3 () (4 () ())) ())                                    
(define (bst-insert  x  bst)
  (cond 
    [(null? bst) (list x null null)]
    [(= x (car bst)) bst]              
    [(< x (car bst))  
     (list (car bst) (bst-insert  x (cadr bst)) (caddr bst))] 
    [else (list (car bst) (cadr bst) (bst-insert x (caddr bst)))]))
;(list->BST '(5 3 8)) -> '(5 (3 () ()) (8 () ()))
(define (list->BST lst)
  (if (null? lst) null
          (let*   ([hd (car lst)]
                   [tail (cdr lst)]
                   [left (filter (lambda (x) (< x hd)) tail)]
                   [right (filter (lambda (x) (>= x hd)) tail)])
              (list hd (list->BST left) (list->BST right)))))
;(tree-sort  '(5 (3 () ()) ())) ->'(3 5)
(define (tree-sort tree)
  (if (empty? tree)  '()    
      (append (tree-sort (cadr tree))
                    (list (car tree))
                    (tree-sort (caddr tree)))))
;f(n) = (n-1)^2 - f(n-3), f(0) = 3, f(1) = 4, f(2) = 5
(define (func n)
  (if (= n 0)  3     
      (if (= n 1) 4          
          (if (= n 2) 5   (- (* (func (- n 1)) (func (- n 1))) (func(- n 3)))))))
;(fact 5) - 120
(define (fact n)
  (if (= n 0) 1   (* n (fact (- n 1)))  ))

(define (fill_fact_ curr end acc)
  (if (= curr end)  acc     
      (fill_fact_ (+ curr 1) end (cons (* curr (car acc)) acc)))) 
;(lastColumn '((1 2 3) (4 5 6) (7 8 9)  (2 3 1))) ->'(3 6 9 1)
( define (lastColumn  m ) 
( if  (null? m) null
              ( cons   ( car  ( cddr  ( car m ) ) ) ( lastColumn ( cdr m ) ) )))
; cuts the last column of the matrix: '((1 2 3) (4 5 6) (7 8 9)) -> ((1 2) (4 5) (7 8)) 
( define ( cut_last_column matrix ) 
  ( if  ( null? matrix )  ' ( ) 
      ( cons  ( cut_last_item ( car matrix ) )  ( cut_last_column ( cdr matrix ) ) ) ))
; returns a list without the last element 
( define ( cut_last_item list ) 
  ( if  ( null? ( cdr  list ) ) ' ( ) 
      ( cons  ( car  list )  ( cut_last_item ( cdr  list ) ) ) ))
; transposes matrix - '((1 2 3) (4 5 6) (7 8 9)) ->' ((1 4 7) (2 5 8) (3 6 9)) 
( define ( rotate_matrix matrix ) 
  ( if  ( null? matrix ) ' ( ) 
      ( if  ( null? ( car matrix ) )    ' ( ) 
          ( cons  ( get_first_column matrix )  ( rotate_matrix ( cut_first_column matrix ) ) ) )))    
; list the first column of the matrix 
( define ( get_first_column  matrix )  ; '((1 2 3) (4 5 6) (7 8 9)) ->' (1 4 7) 
  ( if  ( null? matrix )   ' ( ) 
      ( if  ( null? ( car matrix ) )   ' ( ) 
          ( cons  ( car  ( car matrix ) )  ( get_first_column ( cdr matrix ) ) ) )))    
; return matrix without first column '((1 2 3) (4 5 6) (7 8 9)) -> ((2 3) (5 6) (8 9)) 
( define ( cut_first_column matrix ) 
  ( if  ( null? matrix )  ' ( ) 
      ( if  ( null? ( car matrix ) )   ' ( ) 
          ( cons  ( cdr  ( car matrix ) )  ( cut_first_column ( cdr matrix ) ) ) )))
;(rem1stcol '((1 2 3) (4 5 6) (7 8 9))) ->'((2 3) (5 6) (8 9))
(define (rem1stcol  matrix ) 
(if (null? matrix) null
            ( cons(cdr (  car matrix )) ( rem1stcol (  cdr matrix )))))
;(diagonal '((1 2 3) (4 5 6) (7 8 9))) ->'(1 5 9)
(define (diagonal  matrix ) 
(if( null? matrix) null		 
(cons (car (car matrix ))( diagonal (rem1stcol(cdr matrix ) ) ) )))
(define (is-prime? n); (is-prime? 11) ->#t
  (and (> n 1)
       (or (= n 2)
           (and (not (zero? (modulo n 2)))
                (let loop ((i 3))
                  (or (> (* i i) n)
                      (and (not (zero? (modulo n i)))
                           (loop (+ i 2)))))))))   