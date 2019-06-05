#lang racket
(require r5rs)
(define env (scheme-report-environment 5))
(eval '(define (stream-car stream) (car stream)) env)
(eval '(define (stream-cdr stream) (force (cdr stream))) env)
(eval '(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))])) env)

(eval '(define the-empty-stream '()) env)
(eval '(define (stream-null? stream) (null? stream)) env)
         
(eval '(define (stream-ref s n)  ;get the nth item from s. n starts from 
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
      env)

(eval '(define (display-stream s n) ;display first n items of s
  (if (= n 0)
      (displayln "")
      (begin (display (stream-car s)) (display " ") (display-stream (stream-cdr s) (- n 1)))))
      env)


(eval '
(define (partial-sums-ex op s)
  (define (iter i x y)
    (cons-stream x
                 (cons-stream y
                              (iter (+ i 2) (op x (stream-ref s i)) (op y (stream-ref s (+ i 1)))))))
  (iter 2 (stream-car s) (stream-car (stream-cdr s))))
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