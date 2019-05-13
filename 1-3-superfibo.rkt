#lang racket
(define (fib n)
  (define (iter a b c d e count)
    (if (= count 0)
        e
        (iter b c d e (- (+ e (* 4 d) (* 5 c) (* a a a)) (* 2 b b)) (- count 1))))
  (if (<= n 4)
      1
      (iter 1 1 1 1 1 (- n 4))))
(define (main n)
  (if (eq? n eof)
      (void)
      (begin (display (fib n)) (newline) (main (read)))))
(main (read))