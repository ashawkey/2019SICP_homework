#lang racket
; also try (combinations s) !!!

(define (power-set s)
  (if (null? s) '(())
      (let ([t (power-set (cdr s))])
        (append (map (lambda (x) (append (list (car s)) x)) t) t))))
(define (power-set-n s n)
  (if (= n 0) s
      (power-set-n (power-set s) (- n 1))))
(define (unique-sort l)
  (remove-duplicates (sort l <)))
(let main ([l (read)]
           [n (read)])
  (unless (eq? l eof)
    (begin (displayln (power-set-n (unique-sort l) n))
           (main (read) (read)))))