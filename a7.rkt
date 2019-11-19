;@author Eddy Rogers

#lang racket
(require test-engine/racket-tests)

;Method Name: allTrue
;Function Type: A boolean
;Function Parameters: A list of boolean values
;Return Value: Whether or not all of the values in the given list are true
(define (allTrue aList)
  (if (empty? aList)
      #t
      (and (first aList) (allTrue (rest aList)))))

;3 test cases for the allTrue method
(check-expect (allTrue '(#t #t #t)) #t)
(check-expect (allTrue '(#f #f #f)) #f)
(check-expect (allTrue '(#t #t #t #f #t)) #f)

;Method Name: countIncreases
;Function Type: An integer value
;Function Parameters: A list of integer values
;Return Value: How many times the consecutive values in the list increase from the directly previous value
(define (countIncreases aList)
  (if (> 2 (length aList))
      0
      (if (< (first aList) (second aList))
          (+ 1 (countIncreases (rest aList)))
          (+ 0 (countIncreases (rest aList))))))

;3 test cases for the countIncrease method
(check-expect (countIncreases '(1 2 3)) 2)
(check-expect (countIncreases '(1 3 2 4 3 5)) 3)
(check-expect (countIncreases '(5 4 3 2 1)) 0)

;Method Name: downSeries
;Function Type: A list of integer values
;Function Parameters: 3 integers: one to indicate the difference between values of the list, one to indicate the high bound, and one to indicate the low bound
;Return Value: A list of integers from the given high bound down to the low bound, seperated by the given step valu
(define (downSeries step high low)
  (if (< high low)
      '[]
      (cons high (downSeries step (- high step) low))))

;3 test cases for the countIncrease method
(check-expect (downSeries 2 10 2) '(10 8 6 4 2))
(check-expect (downSeries 1 4 2) '(4 3 2))
(check-expect (downSeries 3 11 2) '(11 8 5 2))

(test)