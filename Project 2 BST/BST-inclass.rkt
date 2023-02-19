#lang racket
(struct bst-node (value left right count)#:mutable #:transparent)


;test cases 1
;(define n0 (bst-node 0 empty empty 1))   ;0
;(define n1 (bst-node 25 empty empty 1))   ;5
;(define n2 (bst-node 100 empty empty 1))  ;10
;(define n3 (bst-node 75 empty empty 1))  ;15

;(set-bst-node-left! n2 n1)
;(set-bst-node-right! n2 n3)

;constructor
(define (new-bst-node v)
  ;constructor
  (bst-node v empty empty 1)
  )

;function to define an empty list as a bst-node
(define (empty-bst-node empty-list)
  (cond
    ;((= (bst-node? empty-list) #f) #t)
    
    )
  )

(define (add-value-to-bst tree v)
(add-value-to-bst-subtree tree tree v))

(define (add-value-to-bst-subtree tree subtree v)
  (cond
    ;case 0
    ;((empty? tree) (set-bst-node-value! (bst-node empty empty empty 1) v))
;   case 1
    ((and (< v (bst-node-value subtree))(empty? (bst-node-left subtree)))
     ;insert new node
     ;construct a new node
     ;set that new node to left of subtree
     (set-bst-node-left! subtree (bst-node v empty empty 1)))

;   case 2
    ((and (< v (bst-node-value subtree))(not (empty? (bst-node-left subtree))))
     (add-value-to-bst-subtree tree (bst-node-left subtree) v))

;   case 3
    ((and (> v (bst-node-value subtree)) (empty? (bst-node-right subtree)))
    (set-bst-node-right! subtree (bst-node v empty empty 1))
     )

;   case 4
    ((and (> v (bst-node-value subtree))(not (empty? (bst-node-right subtree))))
     (add-value-to-bst-subtree tree (bst-node-right subtree) v))

;   case 5
    ((= v (bst-node-value subtree))
     (set-bst-node-count! tree (+ (bst-node-count tree) 1)))
    ))

;           n2
;        n1    n3     (<------VISUAL)


;COUNT-LIST-ELEMENTS
(define (count-list-elements x)
  (cond
    ((empty? x) 0)
    (else (+ 1(count-list-elements (cdr x))))
    ))


;FIND Nth ELEMENT OF THE LIST
(define (list-element-n x n)
  (cond
    ((= n 0) (car x))
    (else (list-element-n (cdr x)(- n 1)))
    ))

;ADD VALUE LIST TO BST
(define (add-value-list-to-bst tree number-list)
    (for (( i (in-inclusive-range 0 (- (count-list-elements number-list) 1))))
      (add-value-to-bst tree (list-element-n number-list i))
      (set! i (+ i 1))
      )
    )


;Contains
(define (contains? n x)
  (cond
    ((empty? x) #f)
    ((= (car x) n) #t)
    (else   (contains? n (cdr x))) 

  ))


;CONTAINS DUPLICATES
 (define (contains-duplicates? x)
   (cond
     ((or (empty? x) (= (count-list-elements x) 1)) #f)  ;is empty OR SINGLETON?
     ((contains? (car x)(cdr x)) #t)
     (else (contains-duplicates? (cdr x)))
     )
   )




;INORDER TRAVERSAL
(define (get-bst-value-list-inorder tree)
  (cond
    ;1st case
    ((empty? tree) (list(bst-node-value empty)))
    ;2nd case
    ((and (empty? (bst-node-left tree))        (empty? (bst-node-right tree)))
     (list(bst-node-value tree)))


    ;3rd case
    ((and (empty? (bst-node-left tree))        (not (empty? (bst-node-right tree))))
     ;(empty?(bst-node-left tree))
     ;(bst-node-value tree)
     (append (list(bst-node-value tree)) (get-bst-value-list-inorder (bst-node-right tree))))
    ;4th case
    ((and (empty? (bst-node-right tree))        (not (empty? (bst-node-left tree))))
     ;(empty?(bst-node-right tree))
     ;(bst-node-value tree)
     (append (get-bst-value-list-inorder (bst-node-left tree)) (list(bst-node-value tree)) ))
    ;5th case  stuck on this case only rest are right
    ;((not (empty? (and (bst-node-left tree) (bst-node-right tree))))
     ((and (not (empty? (bst-node-left tree))) (not (empty? (bst-node-right tree))) )
    ;(append (get-bst-value-list-inorder (bst-node-left tree))
            ;recursive call to the left
            ;(list(bst-node-value tree))
            ;append recursive call to the left with value of node
           ; (get-bst-value-list-inorder (bst-node-right tree))
            ;append the two statements with the recursive call to the right. 
(append (append (get-bst-value-list-inorder (bst-node-left tree)) (list(bst-node-value tree)))
         (get-bst-value-list-inorder (bst-node-right tree))

            )
     )
))

; (append (get-bst-value-list-inorder (bst-node-left tree))
;    (append (list(bst-node-value tree)) (get-bst-value-list-inorder (bst-node-right tree)))

;Generate RANDOM NUMBER INTEGER on RANGE (0 -> rng-1)
(define (get-random-in-range rng)
  (inexact->exact(remainder(floor(*(random)(expt 2 31))) rng))
  )

;random list generator for list of count size
(define (get-random-list-in-range rng count)
  (define num (get-random-in-range rng))
  (cond
;base case
  ((= count 1) (list num))
;General Case
 (else  (append (list num) (get-random-list-in-range rng (- count 1)))
  ))
)

;ADD RANDOM TO LIST UNIQUE
(define (add-random-to-list-unique x rng)
 (add-random-to-list-unique-1 x (cons(get-random-in-range rng)x)rng)
)

(define (add-random-to-list-unique-1 x y rng)
  (cond
    ((not (contains-duplicates? y)) y)
    (else (add-random-to-list-unique-1 x (cons (get-random-in-range rng) (cdr y)) rng))
    ))

(define (get-random-list-in-range-unique rng count)
  (cond
    ((= count 1) (add-random-to-list-unique empty rng))
    (else (append (add-random-to-list-unique (get-random-list-in-range-unique rng (- count 1)) rng )))
    ))

;Check if a list of numbers is in ascending order
(define (is-sorted? x)
  (cond

     ((empty? x) #t)
     ((not(empty? x)) #f)
     ((= (count-list-elements x) 1) #t)
     ((not(= (count-list-elements x) 1)) #f)
     (else (and (sort (append (list(list-element-n x 0)) (list(list-element-n x 1)) ) <)
                (is-sorted? (cdr x)) ) )
  )
)


;GET COUNT OF CHILDREN OF A NODE IN THE TREE
(define (get-child-count tree)
  (cond
    ((empty? tree) 0)
    ((and (empty? (bst-node-left tree)) (empty? (bst-node-right tree)) ) 0)
    ((empty? (bst-node-left tree)) 1)
    ((empty? (bst-node-right tree)) 1)
    (else 2)
  )
)

;FIND PATH OF VALUE IN TREE
(define (find-path v n)
  (find-path-1 v n empty))

;V = value searched for in the bst tree
;n = the bst tree being searched
;p = path from root of tree to current search point (a list of bst-nodes from root to node with that value)
;RETURN VALUE = list of bst-nodes from root to bst-node with value

(define (find-path-1 v n p)
  (cond

  ;Case 1
    ((empty? n) p)

  ;Case 2
    ;((= v (bst-node-value n)) (append (list(bst-node-value n)) p ))
     ((= v (bst-node-value n)) (cons n p))
  ;Case 3
   ; ((< v (bst-node-value n)) (append (list(bst-node-value n)) (find-path-1 v (bst-node-left n) p)  ))
    ((< v (bst-node-value n)) (cons n (find-path-1 v (bst-node-left n) p)  ))
  ;Case 4 (ATM on this case)
   ; ((> v (bst-node-value n)) (append (list(bst-node-value n)) (find-path-1 v (bst-node-right n) p)  ))
    ((> v (bst-node-value n)) (cons n (find-path-1 v (bst-node-right n) p)  ))
    )
)

;get in order predecessor of a node
(define (find-inorder-prev n)
  (cond
    ((null? (bst-node-right (bst-node-left n))) (list (bst-node-left n) n))
    (else (find-path-in-order-prev (bst-node-right(bst-node-left n)) (list(bst-node-left n)n) ))
   )
)

;get in order predecessor of a node
(define (find-path-in-order-prev n p)
  (cond
    ((null? (bst-node-right n)) (cons n p))
    (else (find-path-in-order-prev (bst-node-right n) (cons n p)))
  )
)
 ;delete inorder prev helper function for cases of child count 2 in delete
 ;n: node deleting 
 ;p: predecessor of n (car p)
(define (delete-inorder-prev n p)
  ;we are setting the calue of the predecessor to the value of the node we wish to delete
  ;this is a then clause for all the child count 2 cases: 2.1, 2.2, 2.3, 2.4
  (set-bst-node-value! n (bst-node-value (car p)))
  (cond
    ;2.1 pred. has 0 children and pred. is parents left child
    ;set parents left child to null
    ((and (= (get-child-count (car p)) 0) (equal? (car p)(bst-node-left (car (cdr p)))))
        (set-bst-node-left! (car (cdr p)) null))
    ;2.2 pred. has 0 children and pred. is parents right child
    ;set parents right child to null
    ((and (= (get-child-count (car p)) 0) (equal? (car p) (bst-node-right (car (cdr p)))))
         (set-bst-node-right! (car (cdr p)) null))
    ;2.3 pred. has 1 child and pred. is parents left child
    ; --> set parents left child to pred. left child
    ((and (= (get-child-count (car p)) 0) (equal? (car p) (bst-node-left (car (cdr p)))))
         (set-bst-node-left! (car (cdr p)) (bst-node-left (car p))))
    ;2.4 pred. has 1 child and pred. is parents right child (we can just use else here)
    ; --> set parents right child to pred. left child
    (else
     (set-bst-node-right! (car (cdr p)) (bst-node-left (car p)))
    )
    
  )
)
;GET PATH VALUES
;p = a path of list of bst-node references
;return value = list of values of the original list of bst-node
(define (get-path-values p)
  (cond
    ((empty? p) empty)
    
    (else (cons (bst-node-value(car p)) (get-path-values (cdr p))))

  )
)
  
(define (delete-value-from-bst v tree)
  (delete-node-from-bst (reverse(find-path v tree)) tree))

;DELETE NODE FROM BST
;(car n1) = node to delete
; (bst-node-left (car n1)) = left child of node
;(car (cdr n1)) = parent of node
;(bst-node-left (car (cdr n1))) = left child of parent 
;tree = root

(define (delete-node-from-bst n1 tree)
  (cond ;implement count condition later!
    ;case 0 EMPTY CONDITION
    ((empty? n1) tree)
    ((> (bst-node-count (car n1)) 1)  (set-bst-node-count! (car n1) (- (bst-node-count (car n1)) 1)) tree)


    ((= (get-child-count (car n1)) 0)  ;REFERENCING ALL CASES FOR NODE children are ZERO 
     (cond
        ;case 0.1: node is root tree is empty
       ((equal? (car n1) tree) empty)
        ; case 0.2 & 0.3: node is left child of parent -> set left child of parent to null otherwise ->
       ;rightchild of parent gets set to null
       ((equal? (car n1) (bst-node-left (car(cdr n1)))) (set-bst-node-left! (car (cdr n1)) empty) tree)
       (else (set-bst-node-right! (car(cdr n1)) empty) tree)
       ))
    

    ((= (get-child-count (car n1)) 1)
     (cond
       ;first general case for child count = 1 nodes
       ;if node is equal to root cases 1.1 and 1.2
       ((equal? (car n1) tree)
        (cond
          ;1.1: child is left of node? --> ()
          ((null? (bst-node-right (car n1)))
           (bst-node-left (car n1)))
          (else
           (bst-node-right (car n1)))))
          (else
           (cond
             ;1.3: node is left child of parent and child is left of node
             ; --> (set left of parent to child on left)
             ((and (equal? (car n1) (bst-node-left (car (cdr n1)))) (null? (bst-node-right (car n1))))
              (set-bst-node-left! (car(cdr n1)) (bst-node-left (car n1)) ))
             ;1.4: node is left child of parent and child is right of node
             ; --> (set left of parent to child on left)
             ((and (equal? (car n1) (bst-node-left (car (cdr n1)))) (null? (bst-node-left (car n1))))
              (set-bst-node-left! (car(cdr n1)) (bst-node-right (car n1)) ))
             ;1.5: node is right child of parent and child is left of node
             ; --> (set right of parent to child on left)
             ((and (equal? (car n1) (bst-node-right (car (cdr n1)))) (null? (bst-node-right (car n1))))
              (set-bst-node-right! (car(cdr n1)) (bst-node-left (car n1)) ))
             ;1.6: node is right child of parent and child is right of node
             ; --> (set right of parent to child on right)
             (else
              (set-bst-node-right! (car(cdr n1)) (bst-node-right (car n1)))
             )
           )
           tree
          )
        )
       )
       (else
        (delete-inorder-prev (car n1) (find-inorder-prev (car n1)))
        tree)
       )
     )




     
   

   

  








;(define value-list (get-random-list-in-range 200 3))
;"list of random numbers"
;value-list
;"-----------------------------------"






;(define tree-5 (add-value-list-to-bst n2 value-list))

;(add-value-to-bst n2 22)
;(define bst-list (get-bst-value-list-inorder n2))

;n2
;"In order traversal of BST"
;bst-list

;"-----------------------------------"
;"contains 22 in BST?"
;(contains? 22 bst-list)



;"-----------------------------------"
;"Find the path to 22"
;(define find-path-22 (find-path 22 n2))
;find-path-22
;"-----------------------------------"
;"get path values of 22"
;(get-path-values find-path-22)
;"-----------------------------------"
;"find path in order predecessor of 100"
;(get-path-values (find-inorder-prev (car (find-path 100 n2))))
;"delete value from bst 22"
;(delete-value-from-bst 22 n2)
;"-----------------------------------"
;"after delete of 22"
;(get-bst-value-list-inorder n2)




;n2
;(define random-number (get-random-in-range 50))
;random-number


(define (display-nodes-inorder n)
 (cond
 ((empty? n) (void))
 ((and (empty? (bst-node-left n)) (empty? (bst-node-right n))) (display-node n))
 ((empty? (bst-node-left n)) (display-node n) (display-nodes-inorder (bst-node-right n)))
 ((empty? (bst-node-right n)) (display-nodes-inorder (bst-node-left n)) (display-node n))
 (else (display-nodes-inorder (bst-node-left n)) (display-node n) (display-nodes-inorder (bst-node-right n)))
 ))




(define (inorder n)
 (cond
 ((empty? n) (void))
 ((and (empty? (bst-node-left n)) (empty? (bst-node-right n))) (list (bst-node-value n)))
 ((empty? (bst-node-left n)) (append (list (bst-node-value n)) (inorder (bst-node-right n))))
 ((empty? (bst-node-right n)) (append (inorder (bst-node-left n)) (list (bst-node-value n))))
 (else (append (inorder (bst-node-left n)) (list (bst-node-value n)) (inorder (bst-node-right
n))))
 ))

(define (display-node node)
 (display (format "[~a(" (bst-node-value node)))
 (cond
 ((empty? (bst-node-left node)) (display "N,"))
 (else (display (format "~a," (bst-node-value (bst-node-left node))))))
 (cond
 ((empty? (bst-node-right node)) (display "N)]"))
 (else (display (format "~a)]" (bst-node-value (bst-node-right node))))))
 )






(define n21 (bst-node 93 empty empty 1))
(define n20 (bst-node 44 empty empty 1))
(define n19 (bst-node 94 n21 empty 1))
(define n18 (bst-node 46 empty empty 1))
(define n17 (bst-node 43 empty n20 1))
(define n16 (bst-node 26 empty empty 1))
(define n15 (bst-node 24 empty empty 1))
(define n14 (bst-node 95 n19 empty 1))
(define n13 (bst-node 83 empty empty 1))
(define n12 (bst-node 65 empty empty 1))
(define n11 (bst-node 45 n17 n18 1))
(define n10 (bst-node 35 empty empty 1))
(define n9 (bst-node 25 n15 n16 1))
(define n8 (bst-node 10 empty empty 1))
(define n7 (bst-node 90 n13 n14 1))
(define n6 (bst-node 60 empty n12 1))
(define n5 (bst-node 40 n10 n11 1))
(define n4 (bst-node 20 n8 n9 1))
(define n3 (bst-node 80 n6 n7 1))
(define n2 (bst-node 30 n4 n5 1))
(define n1 (bst-node 50 n2 n3 1))


(define (delete-values-from-bst x n)
  (cond
    ((empty? x) x)
    (else(delete-value-from-bst (car x) n) )
))

(define (delete-value-list-from-bst x tree)
 (cond
 ((null? x) (void))
 (else
 (set! tree (delete-value-from-bst (car x) tree))
 (displayln (get-bst-value-list-inorder tree) (current-output-port))
 (delete-value-list-from-bst (cdr x) tree))
 ))


"inorder traversal"
(inorder n1) ; show list of values of nodes in tree rooted at n1
"-----------------------------------------------------------------------------"
"display nodes in order"
(display-nodes-inorder n1) ; like inorder, but for each node display value of left and right
;child nodes
(displayln "") ; extra newline
; use find-path to get path of nodes from root to node with value 44 starting from n1
"-----------------------------------------------------------------------------"
"get path values of node 60"
(define p44 (find-path 60 n1))
(get-path-values p44)
"find path in order predecessor of 20"
(get-path-values (find-inorder-prev (car (find-path 20 n1))))

"-----------------------------------------------------------------------------"
"We are going to delete 30, 35, 60, 83, 94, 20 from the tree"
(define delete-list '(30 35 60 83 94 20))
(define n1-1 (delete-value-list-from-bst delete-list n1))
(set! n1-1 (delete-value-from-bst 20 n1))




"Delete completeed"
"-----------------------------------------------------------------------------"
"Display resultant bst inorder of values"
(inorder n1-1)

"-----------------------------------------------------------------------------"
"Display resultant bst inorder of the nodes"
(display-nodes-inorder n1-1)
(displayln "")


