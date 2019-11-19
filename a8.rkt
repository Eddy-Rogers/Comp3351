;@author Eddy Rogers

#lang racket
(require test-engine/racket-tests)

;Method Name: squareSum
;Function Type: An integer
;Function Parameters: An integer
;Return Value: The sum of all the values from 0 to n
(define (squareSum n)
  (if (= 0 n)
      0
      (+ (* n n) (squareSum (- n 1)))))

(check-expect (squareSum 0) 0)
(check-expect (squareSum 1) 1)
(check-expect (squareSum 2) 5)
(check-expect (squareSum 3) 14)
(check-expect (squareSum 4) 30)
(check-expect (squareSum 5) 55)
  

;Method Name: cycleList
;Function Type: A list
;Function Parameters: A list
;Return Value: The given list, but the first element has been moved to the last index
(define (cycleOne myList)
  (if (empty? myList)
      empty
      (if (= 1 (length myList))
             (list (first myList))
             (append (rest myList) (list (first myList))))))

(check-expect (cycleOne '()) '())
(check-expect (cycleOne '(0)) '(0))
(check-expect (cycleOne '(1 2)) '(2 1))
(check-expect (cycleOne '(1 2 3 4)) '(2 3 4 1))
(check-expect (cycleOne '(a b c d w)) '(b c d w a))
(check-expect (cycleOne '("this" "is" "fun")) '("is" "fun" "this"))
       

;Method Name: cycleN
;Function Type: A list
;Function Parameters: A value and a list
;Return Value: The given list, but the first element has been moved to the last index count times.
(define (cycleN count myList)
  (if (= count 0)
      myList
      (cycleN (- count 1) (cycleOne myList))))


(check-expect (cycleN 0 '()) '())
(check-expect (cycleN 100 '(a)) '(a))
(check-expect (cycleN 0 '(1 2 3 4)) '(1 2 3 4))
(check-expect (cycleN 1 '(1 2 3 4)) '(2 3 4 1))
(check-expect (cycleN 2 '(1 2 3 4)) '(3 4 1 2))
(check-expect (cycleN 5 '(a b c d e)) '(a b c d e))

;Method Name: memberOf?
;Function Type: A boolean
;Function Parameters: A value and list
;Return Value: Whether or not the value is contained in the list
(define (memberOf? value myList)
  (if (empty? myList)
      #f
      (if (equal? (first myList) value)
          #t
          (memberOf? value (rest myList)))))

(check-expect (memberOf? 0 '()) #f)
(check-expect (memberOf? 0 '(0)) #t)
(check-expect (memberOf? 0 '(0 1 2 3 4)) #t)
(check-expect (memberOf? 0 '(1 2 3 4 5)) #f)
(check-expect (memberOf? 0 '(2 3 4 5 6 0)) #t)
(check-expect (memberOf? "abc" '("def" "hij" "kmn" "abc")) #t)
  

;Method Name: intersection
;Function Type: A list
;Function Parameters: Two lists
;Return Value: A list of only elements present in both given lists
(define (intersection myList aList)
  (if (empty? myList)
      empty
      (if (memberOf? (first myList) aList)
          (cons (first myList) (intersection (rest myList) aList))
          (intersection (rest myList) aList))))

(check-expect (intersection '() '()) '())
(check-expect (intersection '(1 2 3 4) '()) '())
(check-expect (intersection '() '(1 2 3 4)) '())
(check-expect (intersection '(1 2 3 4) '(1 2 3 4)) '(1 2 3 4))
(check-expect (intersection '(1 2 3 4 5 6) '(4 5 6 7 8 9)) '(4 5 6))
(check-expect (intersection '("A" "a" "B" "b" "C" "c") '("A" "B" "C" "D" "E")) '("A" "B" "C"))

(test)
      