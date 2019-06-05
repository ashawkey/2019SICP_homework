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
  (filter (lambda (x) (= s (apply + x)))
          (apply append (map (lambda (l) (map (lambda (x) (append l (list x))) (range (+ 1 (last l)) (+ n 1))))
                             (apply append (map (lambda (l) (map (lambda (x) (append l (list x))) (range (+ 1 (last l)) (+ n 1))))
                                                (map list (range 1 (+ n 1)))))))))
(define (myloop)
  (let ((n (read))
        (s (read)))
    (if (eq? n eof)
        (void)
        (begin (display (tri-num-list n s)) (newline) (myloop)))))

(myloop)