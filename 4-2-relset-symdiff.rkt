; http://lisp.test.openjudge.org/2019hw4/2/
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
      (if (equal? old (car l))
          (dedup (cdr l) old)
          (cons (car l) (dedup (cdr l) (car l))))))

(define (unique l)
  (if (null? l) l
      (cons (car l) (dedup (cdr l) (car l)))))

(define (set-remove A e)
  (if (null? A) '()
      (if (equal? (car A) e)
          (set-remove (cdr A) e)
          (cons (car A) (set-remove (cdr A) e)))))

(define (relative-complement A B)
  (if (null? B) A
      (relative-complement (set-remove A (car B)) (cdr B))))

(define (set-union A B)
  (unique (sort (append A B) comp)))

(define (symmetric-difference A B)
  (set-union (relative-complement A B) (relative-complement B A)))

(define (format set)
  (unique (sort set comp)))

(define (main)
  (let ((A (read))
        (B (read)))
    (if (eq? A eof)
        (void)
        (begin (display (format (relative-complement A B)))
               (display (format (symmetric-difference A B)))
               (newline)
               (main)))))

(main)