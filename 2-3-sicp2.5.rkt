#lang racket
(define (car x)
  (define (iter x i)
    (if (> (remainder x 2) 0)
        i
        (iter (/ x 2) (+ i 1))))
  (iter x 0))

(define (cdr x)
  (define (iter x i)
    (if (> (remainder x 3) 0)
        i
        (iter (/ x 3) (+ i 1))))
  (iter x 0))
(define (fast-exp a n)
  (define (square x) (* x x))
  (define (iter a n result)
    (if (= n 0)
        result
        (if (even? n) 
            (iter (square a) (/ n 2) result)
            (iter (square a) (/ (- n 1) 2) (* a result)))))
  (iter a n 1))
  
(define (cons a b)
  (* (fast-exp 2 a) (fast-exp 3 b)))

(define (myloop)
  (let ((a (read))
        (b (read)))
    (if (eq? a eof)
        (void)
        (begin (display (car (cons a b)))
               (display " ")
               (display (cdr (cons a b)))
               (newline) 
               (myloop)))))

(myloop)