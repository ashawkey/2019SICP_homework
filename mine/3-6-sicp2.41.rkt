; http://lisp.test.openjudge.org/2019hw3/6/

#lang racket

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (tri-num-list n s)
  (define (construct-first i)
    (map (lambda (x) (cons i (cons x '())))
         (enumerate-interval (+ i 1) n)))
  (define (construct-second i)
    (map (lambda (x) (append i (list x)))
         (enumerate-interval (+ (cadr i) 1) n)))
  (define (filter f l)
    (if (null? l)
        '()
        (if (f (car l))
            (cons (car l) (filter f (cdr l)))
            (filter f (cdr l)))))
  (filter (lambda (x) (= (apply + x) s))
          (flatmap construct-second 
               (flatmap construct-first
                    (enumerate-interval 1 n)))))
(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)