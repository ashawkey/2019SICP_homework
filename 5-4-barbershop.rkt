; http://lisp.test.openjudge.org/2019hw5/4/

#lang racket

(define seats (make-vector 25))

(define (find t c i n)
  (if (= i n)
      (displayln "Failed")
      (if (>= t (vector-ref seats i))
          (begin (displayln i)
                 (vector-set! seats i (+ t c)))
          (find t c (+ i 1) n))))

(define (barber n m olda)
  (if (= m 0)
      (void)
      (let ((a (read))
            (b (read)))
        (find (+ olda a) b 0 n)
        (barber n (- m 1) (+ olda a)))))

(define (main)
  (let ((n (read))
        (m (read)))
    (if (eq? n eof)
        (void)
        (begin (vector-fill! seats -1) (barber n m 0) (main)))))

(main)