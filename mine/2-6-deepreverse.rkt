; http://lisp.test.openjudge.org/2019hw2/6/
#lang racket

(define (deep-reverse l)
  (if (null? l) '()
      (append (deep-reverse (cdr l)) (list (if (pair? (car l))
                                         (deep-reverse (car l))
                                         (car l))))))

(define (main)
  (let ((l (read)))
    (if (eq? l eof)
        (void)
        (begin (display (deep-reverse l)) (newline) (main)))))

(main)