#lang racket
(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (list(car l)))))

(define (flat l)
  (cond ((null? l) '())
        ((pair? l) (append (flat (car l)) (flat (cdr l))))
        (else (list l))))

(define (main)
  (let ((l (read)))
    (if (eq? l eof)
        (void)
        (begin (display (reverse l)) (newline) (main)))))

(main)