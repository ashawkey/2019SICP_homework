; http://lisp.test.openjudge.org/2019hw5/1/

#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;以下是put和get的实现，不须搞明白也能完成本题
(require scheme/mpair)
(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      (void))    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define conversion-table (make-table))
(define get-coercion (conversion-table 'lookup-proc))
(define put-coercion (conversion-table 'insert-proc!))
;以上是put和get的实现，不须搞明白也能完成本题
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;------------- integer package
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))    
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) ((get 'make 'rational )  x y)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (void))

(define (make-integer n)
  ((get 'make 'integer) n))


;--------general functions
  
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
(define (priority sym)
  (cond ((eq? sym 'a) 5)
        ((eq? sym 'b) 4)
        ((eq? sym 'c) 3)
        ((eq? sym 'd) 2)
        ((eq? sym 'e) 1)
        (else (displayln "Wrong Symbol!"))))

(define (install-polynomial-package)
  (define (tag x)
    (cons 'polynomial x))
  (define (order t)
    (car t))
  (define (coeff t)
    (cadr t))
  (define (get-var poly)
    (car poly))
  (define (add-polynomial x y)
    (let ((var1 (get-var x))
          (var2 (get-var y)))
      (cond ((> (priority var1)
                (priority var2))
             (make-poly var1 (add-term (cdr x)
                                       (list (make-term 0 (make-poly (car y) (cdr y)))))))
            ((< (priority var1)
                (priority var2))
             (make-poly var2 (add-term (list (make-term 0 (make-poly (car x) (cdr x))))
                                       (cdr y))))
            (else (make-poly var1 (add-term (cdr x)
                                            (cdr y))))
            )))
  (define (add-term L1 L2)
    (cond ((null? L1) L2)
          ((null? L2) L1)
          (else
           (let ((t1 (car L1))
                 (t2 (car L2)))
             (cond ((> (order t1) (order t2))
                    (cons t1
                          (add-term (cdr L1) L2)))
                   ((< (order t1) (order t2))
                    (cons t2
                          (add-term L1 (cdr L2))))
                   (else 
                    (cons (make-term (order t1)
                                     (add (coeff t1) (coeff t2)))
                          (add-term (cdr L1)
                                    (cdr L2)))))))))
  (define (mul-polynomial x y)
(let ((var1 (get-var x))
          (var2 (get-var y)))
      (cond ((> (priority var1)
                (priority var2))
             (make-poly var1 (mul-terms (cdr x)
                                       (list (make-term 0 (make-poly (car y) (cdr y)))))))
            ((< (priority var1)
                (priority var2))
             (make-poly var2 (mul-terms (list (make-term 0 (make-poly (car x) (cdr x))))
                                       (cdr y))))
            (else (make-poly var1 (mul-terms (cdr x)
                                            (cdr y))))
            ))
    )

  (define (mul-terms L1 L2)
    (if (null? L1)
        '()
        (add-term (mul-term-by-all-terms (car L1) L2)
                  (mul-terms (cdr L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (null? L)
        '()
        (let ((t2 (car L)))
          (cons (make-term (+ (order t1) (order t2))
                           (mul (coeff t1) (coeff t2)))
                (mul-term-by-all-terms t1 (cdr L))))))
  
  (put 'add '(polynomial polynomial)
       add-polynomial)
  (put 'mul '(polynomial polynomial)
       mul-polynomial)
  (put 'make 'polynomial
       (lambda (var terms) (tag (cons var terms))))
  (put 'make 'polynomial-term
       (lambda (x y)
         (list x y)))
  )

(define (display-poly x)
  (displayln (get-poly x)))

(define (get-poly x)
  (if (is-polynomial? x)
      (cons (cadr x)
            (map (lambda (x)
                   (let ((order (car x))
                         (coeff (cadr x)))
                     (list order (get-poly coeff))))
                 (cddr x)))
      (cdr x)))

(define (build-poly x)
  (if (number? x)
      (make-integer x)
      (make-poly (car x)
                 (map (lambda (y)
                        (make-term (car y)
                                   (build-poly (cadr y)))) (cdr x)))))


(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
            (let ((type1 (car type-tags))
                  (type2 (cadr type-tags))
                  (a1 (car args))
                  (a2 (cadr args)))
              (cond ((eq? type1 'integer) 
                     (apply-generic op (make-poly (cadr a2) (list (list 0 a1))) a2))
                    ((eq? type2 'integer)
                     (apply-generic op a1 (make-poly (cadr a1) (list (list 0 a2)))))))
            (displayln "Wrong, again, Wrong!")))))

(define (is-integer? x)
  (eq? (type-tag x) 'integer))
(define (is-polynomial? x)
  (eq? (type-tag x) 'polynomial))

(install-integer-package)
(install-polynomial-package)
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))


(define (make-poly var terms)
  ((get 'make 'polynomial) var terms))
(define (make-term order coeff) 
  ((get 'make 'polynomial-term) order coeff))

(displayln "******1")
(define e1 (make-poly 'a (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3a+2
(define e2 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4a^2 + 3a
(displayln e1)
(displayln e2)
(displayln (add e1 e2))
(displayln (mul e1 e2))

(displayln "******2")

(define c1 (make-poly 'b (list (list 1 (make-integer 3)) (list 0 (make-integer 2))))) ;3b+2
(define c2 (make-poly 'b (list (list 2 (make-integer 4)) (list 1 (make-integer 3))))) ;4b^2 + 3b

(define e3 (make-poly 'a (list (list 1 c1) (list 0 (make-integer 2))))) 
(define e4 (make-poly 'a (list (list 2 (make-integer 4)) (list 1 c2)))) 

(displayln (add e3 e4))

(displayln "******")
(define (myloop)
  (let ((a (read)))
    (if (eq? a eof)
        (void)
        (let ((op (car a))
              (e1 (cadr a))
              (e2 (caddr a)))
          (if (eq? op '+)
              (display-poly (add (build-poly e1) (build-poly e2)))
              (display-poly (mul (build-poly e1) (build-poly e2))))
          (myloop)))))
              
(myloop)