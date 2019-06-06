#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your my-map-ex, the program in the input can use my-map-ex
 '
(define (my-map-ex proc . args)
  (define (max-length lst)
    (if (null? lst) 0
        (max (length (car lst)) (max-length (cdr lst)))))
  (define (helper i ml lst res)
    (if (= i ml) res
        (helper (+ i 1)
                ml
                (map (lambda (x) (if (null? (cdr x)) x (cdr x))) lst)
                (append res (list (apply proc (map car lst)))))))
  (helper 0 (max-length args) args '()))
env)

(define (myloop)
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin  (displayln (eval codes env)) (myloop)))))


(myloop)