#lang racket
(require test-engine/racket-tests)
;semicolons are used to comment out code
(define x 5)
(define (fact n)
  (if (= n 0)
  1
  (* n (fact (- n 1)))))

(check-expect (fact 5) 120)

(define (len aList)
  (if (empty? aList)
  0
  (+ 1 (len (rest aList)))))

(define (foo x)
  (lambda (y)
         (+ x y)))

(define (fifi x)
  (λ (y)
    (+ x y)))

(test)