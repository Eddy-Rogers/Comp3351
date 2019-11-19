#lang racket
(define-syntax myIf
  (syntax-rules (then else)
    [(myIf e1 then e2 else e3)
     (if e1 e2 e3)]))

(define-syntax comment
  (syntax-rules ()
    [(comment e1 e2) e2]))

(define-syntax myAnd
  (syntax-rules (foo)
    [(myAnd e1 foo e2) (and e1 e2)]))

(define (face n)
  (if (zero? n)
      1
      (* n (fact (-n 1)))))