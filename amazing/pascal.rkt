#lang racket
[define [pascal x]
  [let loop [[i 0] [j 0] [v 1]]
    [if [= i x]
        [void]
        [begin [display v] [display " "]
               [if [= j i]
                   [begin [newline] [loop [+ i 1] 0 1]]
                   [loop i [+ j 1] [* v [/ [- i j] [+ j 1]]]]]]]]]

[let main [[x [read]]]
  [if [eq? x eof]
      [void]
      [begin [pascal x] [main [read]]]]]