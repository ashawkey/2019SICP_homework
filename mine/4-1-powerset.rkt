; http://lisp.test.openjudge.org/2019hw4/1/

#lang racket

(define (P set)
  (cond ((null? set) (list'()))
        (else (append (P (cdr set))
                      (map (lambda (x) (cons (car set) x)) (P (cdr set)))))))

(define (comp x y)
  (cond ((null? y) #t)
        ((null? x) #f)
        ((and (pair? x) (pair? y)) (if (comp (car x) (car y))
                                       #t
                                       (comp (cdr x) (cdr y))))
        ((and (pair? x) (not (pair? y))) (< (car x) y))
        ((and (not (pair? x)) (pair? y)) (< x (car y)))
        (else (< x y))))

(define (dedup l old)
  (if (null? l) '()
      (if (equal? old (car l)) (dedup (cdr l) old)
          (cons (car l) (dedup (cdr l) (car l))))))

(define (Pn l n)
  (if (= n 0) l
      (Pn (sort (P l) comp) (- n 1))))

(define (main)
  (let ((l (read))
        (n (read)))
    (if (eq? l eof)
        (void)
        (begin (displayln (Pn (dedup (sort l comp) '(0.1)) n)) (main)))))

(main)