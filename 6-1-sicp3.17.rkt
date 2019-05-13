#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '
 (define (count-pairs x)
    (define vis '())
    (define (count x l)
      (if (null? l) #f
          (if (eq? x (car l)) #t
              (count x (cdr l)))))
    (define (len l)
      (if (null? l) 0
          (+ 1 (len (cdr l)))))
    (define (helper x)
      (if (not (pair? x)) 0
          (if (count x vis)
              0
              (begin (set! vis (cons x vis))
                     (helper (car x))
                     (helper (cdr x))))))
    (helper x)
    (len vis))
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