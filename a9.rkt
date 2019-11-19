;@author Eddy Rogers
#lang racket

(require test-engine/racket-tests)

;Method Name: streammaker
;Function Type: A stream
;Function Parameters: A function and an argument
;Return Value: (Defined in class) Creates a stream using the given function and argument
(define (streammaker func arg)
  (letrec ([f (lambda (x)
                 (cons x (lambda () (f (func x arg)))))])
    (lambda () (f arg))))

;Method Name: next-k-items
;Function Type: A list
;Function Parameters: A stream and a number
;Return Value: A list of elements containing the next k values in the stream
(define (next-k-items s k)
  (if (= 1 k)
      (list (car (s)))
      (append (list (car (s))) (next-k-items (cdr (s)) (- k 1)))))

;Method Name: kth-item
;Function Type: An integer
;Function Parameters: A stream and a number
;Return Value: The kth value of the stream
(define (kth-item s k)
  (if (= 1 k)
      (car (s))
      (kth-item (cdr (s)) (- k 1))))

;Definitions for the sets of numbers
(define ones* (streammaker (lambda (a b) 1) 1))
(define nats* (streammaker (lambda (a b) (+ a b)) 1))
(define nats** (streammaker + 1))
(define powers* (streammaker (lambda (a b) (* a 2)) 1))

;Tests for both next-k-items and kth-items
;Test cases created by Troy Andrews
(check-expect (next-k-items ones* 1) '(1))
(check-expect (next-k-items ones* 5) '(1 1 1 1 1))
(check-expect (next-k-items nats* 1) '(1))
(check-expect (next-k-items nats* 5) '(1 2 3 4 5))
(check-expect (next-k-items powers* 1) '(1))
(check-expect (next-k-items powers* 5) '(1 2 4 8 16))

(check-expect (kth-item ones* 1) 1)
(check-expect (kth-item ones* 5) 1)
(check-expect (kth-item nats* 1) 1)
(check-expect (kth-item nats* 5) 5)
(check-expect (kth-item powers* 1) 1)
(check-expect (kth-item powers* 5) 16)

(test)
