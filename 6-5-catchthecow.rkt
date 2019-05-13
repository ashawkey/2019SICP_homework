; http://lisp.test.openjudge.org/2019hw6/5/

#lang racket
(define maxl 1000000)
(define queue (make-vector maxl))
(define front 0)
(define end 0)

(define (clear)
  (vector-fill! queue 0)
  (set! front 0)
  (set! end 0))

(define (push x)
  ;(displayln x)
  (if (= (modulo (+ end 1) maxl) front) (error "overflow!")
      (begin (vector-set! queue end x)
             (set! end (modulo (+ end 1) maxl)))))

(define (empty?)
  (if (= front end) #t
      #f))

(define (top)
  (vector-ref queue front))

(define (pop)
  (set! front (modulo (+ front 1) maxl)))

(define vis (make-hash))

(define (out-of-range x)
   (or (< x 0)
       (> x 100000)
       (hash-has-key? vis x)))

(define (bfs N K)
  (hash-clear! vis)
  (clear)
  (push (cons N 0))
  (define (bfs-helper)
    (if (empty?) -1
        (let* ((p (top))
               (cur (car p))
               (time (cdr p)))
          ;(displayln p)
          (if (= cur K) time
                (begin (pop)
                       (if (out-of-range (- cur 1)) (void)
                           (begin (push (cons (- cur 1) (+ time 1)))
                                  (hash-set! vis (- cur 1) 1)))
                       (if (out-of-range (* cur 2)) (void)
                           (begin (push (cons (* cur 2) (+ time 1)))
                                  (hash-set! vis (* cur 2) 1)))
                       (if (out-of-range (+ cur 1)) (void)
                           (begin (push (cons (+ cur 1) (+ time 1)))
                                  (hash-set! vis (+ cur 1) 1)))
                       (bfs-helper))))))
  (bfs-helper))

(define (main)
  (let ((N (read))
        (K (read)))
    (displayln (bfs N K))))

(main)