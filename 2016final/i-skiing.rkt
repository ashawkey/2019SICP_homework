#lang racket

; (make-vector 10 (make-vector 10 -1)) creates 10 reference of the same vector.
(define (make-2d-vector x y init)
  (let ([v (make-vector x)])
    (let loop ([i 0])
      (if (= i y) v
          (begin (vector-set! v i (make-vector y init))
                 (loop (+ i 1)))))))

(define dp (make-2d-vector 105 105 -1))
(define m (make-2d-vector 105 105 -1))

(define (get a i j) (vector-ref (vector-ref a i) j))
(define (set a i j v) (vector-set! (vector-ref a i) j v))

(define (solve x y)
  (if (not (= -1 (get dp x y))) (get dp x y)
      (let loop ([dir 0])
        (if (> dir 3)
            (begin (when (= -1 (get dp x y)) (set dp x y 1))
                   (get dp x y))
            (let ([nx (+ x (list-ref dx dir))]
                  [ny (+ y (list-ref dy dir))])
              (when (and (>= nx 0) (< nx R)
                         (>= ny 0) (< ny C)
                         (< (get m x y) (get m nx ny)))
                (set dp x y (max (get dp x y) (+ 1 (solve nx ny)))))
              (loop (+ dir 1)))))))

(define dx '(1 -1 0 0))
(define dy '(0 0 1 -1))

(define (read-m)
  (let loop ([i 0] [j 0])
    (cond ((= i R) (void))
          ((= j C) (loop (+ i 1) 0))
          (else (set m i j (read))
                (loop i (+ j 1))))))

(define (solve-all)
  (let loop ([i 0] [j 0] [ans 0])
    (cond ((= i R) ans)
          ((= j C) (loop (+ i 1) 0 ans))
          (else (loop i (+ j 1) (max ans (solve i j)))))))

(define R (read))
(define C (read))
(read-m)
(displayln (solve-all))