; http://lisp.test.openjudge.org/2019hw3/7/
#lang racket
(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((and (number? a1) (= a1 0)) a2)
        ((and (number? a2) (= a2 0)) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (append (if (pair? a1)
                          a1
                          (list a1))
                      (list '+)
                      (if (pair? a2)
                          a2
                          (list a2))))))

(define (make-product a1 a2)
  (cond ((or (and (number? a1) (= a1 0))
             (and (number? a2) (= a2 0)))
         0)
        ((and (number? a1) (= a1 1)) a2)
        ((and (number? a2) (= a2 1)) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list a1 '* a2))))

(define (sum? x)
  (cond ((null? x) #f)
        ((and (pair? x) (eq? (car x) '+)) #t)
        (else (sum? (cdr x)))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (addend x)
  (define (helper-addend x)
    (if (eq? (car x) '+)
        '()
        (cons (car x)
              (helper-addend (cdr x)))))
  (if (eq? (cadr x) '+)
      (car x)
      (helper-addend x)))

(define (augend x)
  (if (eq? (car x) '+)
      (if (null? (cddr x))
          (cadr x)
          (cdr x))
      (augend (cdr x))))

(define (multiplier x)
  (car x))

(define (multiplicand x)
  (if (null? (cdddr x))
      (caddr x)
      (cddr x)))

(define (deriv exp var)
  (cond ((number? exp ) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else 
         (error "wrong format"))))

(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (begin (display (deriv a 'x)) (newline) (myloop)))))

(myloop)