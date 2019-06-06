#lang racket
(require r5rs)
(define (exit) #f)
(define env (scheme-report-environment 5))

(eval '(define (acc-func . argf)
         (define (iter f rest)
           (if (null? rest) f
               (iter (lambda (x) ((car rest) (f x))) (cdr rest))))
         (lambda x
           (if (and (not (null? x)) (null? (cdr x)) (number? (car x)))
               ((iter (lambda (x) x) argf) (car x))
               (apply acc-func (append argf x)))))
env)


(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))
(myloop)