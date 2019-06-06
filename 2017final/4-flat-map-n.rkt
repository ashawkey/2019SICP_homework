#lang racket
(define (flat-map-n lst n)
  (cond ((null? lst) '())
        ((= n 0) (if (pair? lst) lst (list lst)))
        ((list? lst) (apply append (map (lambda (lst) (flat-map-n lst (- n 1))) lst)))
        (else (list lst))))

(let main ([lst (read)]
           [n (read)])
  (unless (eq? lst eof)
    (displayln (flat-map-n lst n))
    (main (read) (read))))