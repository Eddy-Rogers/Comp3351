#lang racket
(define (myIf a b c)
  (if a (b) (c) ))

(define (myDelay func)
  (mcons #f func))

(define (myForce thunk)
  (cond
    [(mcar thunk) (mcdr thunk)]
    [else
       (set-mcar! thunk #t)
       (set-mcdr! thunk ((mcdr thunk)))
       (mcdr thunk)]))

(define (onetwo)
  (+ 1 2))

(define onetwoThunk (myDelay onetwo))

;;; Stream Examples
(define ones (lambda () (cons 1 ones)))

(define nats
  (letrec ([f (lambda (x)
                   (cons x (lambda ()
                               (f (+ x 1)))))])
    (lambda () (f 0))))

(define powersoftwo
  (letrec ([f (lambda (x)
                   (cons x (lambda ()
                               (f (* x 2)))))])
    (lambda () (f 1))))

(define (numberUntil stream predicate)
  (letrec ([f (lambda (stream answer)
                (let ([pr (stream)])
                  (if (predicate (car pr))
                      answer
                      (f (cdr pr) (+ answer 1)))))])
    (f stream 1)))

(define (streamUntil stream predicate)
  (letrec ([f (lambda (stream answer)
                (let ([pr (stream)])
                  (if (predicate (car pr))
                      answer
                      (f (cdr pr) (append answer (list (car pr)))))))])
    (f stream '() )))

(define (streammaker func arg)
  (letrec ([f (lambda (x)
                 (cons x (lambda () (f (func x arg)))))])
    (lambda () (f arg))))

(define ones* (streammaker (lambda (a b) 1) 1))
(define nats* (streammaker (lambda (a b) (+ a b)) 1))
(define nats** (streammaker + 1))
(define powers* (streammaker (lambda (a b) (* a 2)) 1))
