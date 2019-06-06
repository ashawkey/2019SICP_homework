#lang racket
(define (gcd a b)
  (cond ((> b a) (gcd b a))
        ((= b 0) a)
        (else (gcd b (remainder a b)))))
  
(define (coprime? a b)
  (= (gcd a b) 1))

(define (solve lst res)
  (if (null? lst) res
      (let ([first (car lst)]
            [rest (cdr lst)])
        (solve (cdr lst) (append res
                                 (map (lambda (x) (list first x))
                                      (filter (lambda (x) (coprime? first x))
                                              rest)))))))
(define (read-n n lst)
  (if (= n 0) lst
      (read-n (- n 1) (append lst (list (read))))))

(define (print-line lst)
  (if (null? lst) (newline)
      (begin (display (car lst)) (display " ") (print-line (cdr lst)))))

(define t (read))

(let main ([x (read)])
  (unless (eq? x eof)
    (print-line (solve (sort (read-n x '()) <) '()))
    (main (read))))