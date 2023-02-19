#lang racket
;COUNT-LIST-ELEMENTS
;counts how many elements are in the list
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





(define (is-sorted? x boolean)
 ;(define boolean #f)
  (cond

     ((or (empty? x) (= (count-list-elements x) 1)) #t)
     ((or (not (empty? x)) (not (= (count-list-elements x) 1))) #f)
     (else (is-sorted? (cdr x) (< (car x)  (car (cdr x))  ))  )                ;(and)
  )
)

;(define test '(1 2 3 10 1))
(define test '(10 1 2))
(sort test <)
;(sort (append (list(car test)) (list(car(cdr test)))) <)

;(and (sort (append (list(car test)) (list(car(cdr test)))) <) (is-sorted? (cdr test)) )
;test
(define bool #f)
"bool initial"
;bool
;(is-sorted? test bool)
;(count-list-elements test)



;sort else condition
;(sort (list(car(cdr x))) <)
;(append (list(car x)) (list(car(cdr x))))

(car test)
(car(cdr test))
(< (car test) (car(cdr test)))



;(append (list(list-element-n x 0)) (list(list-element-n x 1)))