; http://lisp.test.openjudge.org/2019hw5/5/
#lang racket

(define exit #f)

(define cont-list '())
(define len (read))

(define (set-cont-list n)
  (if (call/cc
       (lambda (cc)
         (set! cont-list (cons cc cont-list))
         #t))
      (if (= n 1)
          (void)
          (set-cont-list (- n 1)))
      (begin (displayln n)
             (if (= n 1)
                 (void)
                 (show (- n 1))))))
(define (show n)
  (define (show-helper l n)
    (if (= n 0)
        (if (continuation? (car l))
            ((car l) #f)
            (displayln "error"))
        (show-helper (cdr l) (- n 1))))
  (show-helper cont-list (- n 1)))

(define (main)
  (set-cont-list len)
  (define k (read))
  (if (eq? k eof)
      (void)
      (show k)))

(main)