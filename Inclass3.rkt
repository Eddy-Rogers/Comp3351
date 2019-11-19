#lang racket

(define (myForce 🤔)
  (cond
    [(mcar 🤔) (mcdr 🤔)]
    [else
     (set-mcar! 🤔 #t)
     (set-mcdr! 🤔 ((mcdr 🤔)))
     (mcdr 🤔)]))

(define ones (lambda () (cons 1 ones)))

(define nats
  (letrec ([f (lambda (x)
                (cons x (lambda ()
                          (f (+ x 1)))))]))

(define powersoftwo
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
    (f stream '())))

(define ones* (streammaker (lambda (a b) 1) 1))
(define nats* (streammaker (lambda (a b) (+ a 1))))


