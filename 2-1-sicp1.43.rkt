; http://lisp.test.openjudge.org/2019hw2/1/
#lang racket
(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (db x) (* x 2))

(define (repeated f n)
  (lambda (x)
    (define (iter x i)
      (if (= i n)
          x
          (iter (f x) (+ i 1))))
    (iter x 0)))
((repeated square 2) 5)
((repeated inc 4) 6)
((repeated db 4) 6)

(display "********") (newline)

(define (myloop)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (display ((repeated square n) 2)) 
               (newline) (myloop)))))

(myloop)