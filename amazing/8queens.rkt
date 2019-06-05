#lang racket
(define (safe? pos lst)
  (if (null? lst)
      true
      (if (or (= pos (car lst)) (= (abs (- pos (car lst))) (length lst)))
          false
          (safe? pos (cdr lst)))))

(define (queens n)
  (define (search lst)
    (if (= (length (car lst)) n) lst
        (search (apply append (map (lambda (s)
                                     (map (lambda (x)
                                            (append s (list x)))
                                          (filter (lambda (x) (safe? x s))
                                                  (range 1 (+ n 1)))))
                                   lst)))))
  (search (map list (range 1 (+ n 1)))))

(define res (queens 8))

(define (print-line lst)
  (if (null? lst) (newline)
      (begin (display (car lst)) (print-line (cdr lst))))) 

(define t (read))

(let main ((x (read)))
  (unless (eq? x eof)
    (begin (print-line (list-ref res (- x 1)))
           (main (read)))))