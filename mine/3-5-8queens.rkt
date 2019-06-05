; http://lisp.test.openjudge.org/2019hw3/5/

#lang racket
(define (iqueens L a d) ;; gives all of the queen solutions
        (if (null? L)
            (list a)
            (append-map (lambda (x)
                          (if (diagonal? x a 1) '()
                              (iqueens (remove x L) (cons x a) 1)))
                        L)))                       

(define (enumerate-interval l h)
        (if (> l h) '()
            (cons l (enumerate-interval (+ l 1) h))))

(define (diagonal? col a d)
  (cond ((null? a) #f)
        ((= (abs (- col (car a))) d) #t)
        (else (diagonal? col (cdr a) (+ d 1)))))

(define (solve n)
  (iqueens (enumerate-interval 1 n) '() 1))

(define (get l i)
  (if (= i 0)
      (car l)
      (get (cdr l) (- i 1))))

(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (list (car l)))))

(define (list-to-num l n)
  (if (null? l) n
      (list-to-num (cdr l) (+ (car l) (* 10 n)))))

(define (foo i)
  (list-to-num (reverse (get (solve 8) (- i 1))) 0))

(define (main)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (displayln (foo n)) (main)))))
(define n (read))
(main)