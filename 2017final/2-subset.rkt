#lang racket
(let main ([x (read)])
  (unless (eq? x eof)
    (displayln (combinations (sort x <)))
    (main (read))))