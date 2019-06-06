#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating this function, the program in the input can use it
 '
(define (make-pipeline-proc . arg)
  (define (helper f rest)
    (if (null? rest) f
        (helper (lambda (x) ((car rest) (f x))) (cdr rest))))
  (helper (car arg) (cdr arg)))
env)  
(define (myloop)
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (let ((result (eval codes env)))
          (if (eq? (void) result)
              (myloop)
              (begin (displayln result) (myloop)))))))
              
(myloop)