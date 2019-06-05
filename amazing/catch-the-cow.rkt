#lang racket
(require data/queue)

(define q (make-queue))
(define v (make-hash))

(define N (read))
(define K (read))
(enqueue! q (cons N 0))

(define (bfs)
  (if (queue-empty? q) -1
      (let* ((p (dequeue! q))
             (x (car p))
             (t (cdr p)))
        (cond ((= x K) t)
              ((or (< x 0) (> x 100000) (hash-has-key? v x)) (bfs))
              (else (begin (hash-set! v x 1)
                           (enqueue! q (cons (+ x 1) (+ t 1)))
                           (enqueue! q (cons (- x 1) (+ t 1)))
                           (enqueue! q (cons (* x 2) (+ t 1)))
                           (bfs)))))))

(displayln (bfs))