; http://lisp.test.openjudge.org/2019hw4/6/
#lang racket
(define (merge l start end)
  (cond ((null? l) (list (list start end)))
        (else (let ((next-start (caar l))
                    (next-end (cadar l)))
                (cond ((<= next-start end) (merge (cdr l) start (max end next-end)))
                      (else (cons (list start end) (merge (cdr l) next-start next-end))))))))

(define (merger l)
  (if (null? l) l
      (merge (cdr l) (caar l) (cadar l))))

(define (comp A B)
  (cond ((= (car A) (car B)) (< (cadr A) (cadr B)))
        (else (< (car A) (car B)))))

(define (main)
  (let ((A (read))
        (B (read)))
    (if (eq? A eof)
        (void)
        (begin (displayln (sort (merger (sort (append A B) comp)) comp)) (main)))))

(main)