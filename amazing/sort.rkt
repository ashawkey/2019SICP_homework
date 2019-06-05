#lang racket
(define (read-line lst)
  (let ((x (read)))
    (if (eq? x eof)
        lst
        (read-line (append lst (list x))))))
(define (print-line lst)
  (for-each (lambda (x) (display x) (display " ")) lst)
  (newline))
(define (main)
  (let ((lst (read-line '())))
    (print-line (sort (set->list (list->set lst)) <))))
(main)