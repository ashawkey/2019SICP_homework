### Misc

* `equal? = eqv?` compare value, `eq?` compare reference.

* **Difference between `'()` with `(list )`**

  ```lisp
  '(1) == (quote 1)
  (list 1 (list 2)) == '(1 (2))
  '(1 '(2)) == (list 1 quote 2)
  ```

* `(unless) (when)`

* `(length lst)`

* `(reverse lst)`

* `(sort lst <)`

* `!` means **in-place** operation.

* `(apply and '(#t #f))'` fails because `and` is a macro.

* `(define-syntax-rule ...)` defines a macro.

* `(apply proc ... lst)`

  ```lisp
  (apply + '(1 2 3))
  (apply + 1 2 '(3))
  ```

* `(compose f1 f2 ...) `

  `compose1` is faster & preferred.

  ```lisp
  (define (repeat f n)
      (if (= n 1) f
          (compose f (repeat f (- n 1)))))
  
  (define (pipeline operand . ops)
    ((apply compose1 (reverse ops)) operand))
  ```

* `(foldl proc init lst)`

* `(range x y [z])`

  same as python's range.
  
* `(last lst)`

* `(remove-duplicates (sort lst <))`

* `(lambda x (...))` all arguments are passed to x as `. args`

  ```lisp
  (define (mysum x)
      (lambda y (if (null? y) x (mysum (+ x (car y))))))
  
  (define (mysum x)
      (lambda (. y)) ...) ; illegal
  
  (define (mysum x)
      (define (f . y) ...) ; okay
      f)
  
  (define (mysum x)
      (lambda ((y '())) ...))
  ```

* default parameters:

  ```lisp
  (define (f x (y 1)) x)
  (lambda ((x 1)) x)
  ```

* how to make a quote

  ```lisp
  (define (make-quote a)
    (list 'quote a))
  ```

* named let loops

  ```lisp
  ; while
  (let loop ()
       (...)
       (loop))
  
  ; for
  (let loop ((i 0))
       (unless (= i 10)
           (...)
           (loop (+ i 1))))
  ```

  

* improper append

  ```lisp
  (append '(1) '(2)) => '(1 2)
  (append '(1) 2) => '(1 . 2) ; equals (cons 1 2)
  (append '() '(1 2 3) 4) => '(1 2 3 . 4)
  
  (append '() 1) => 1 ; hell grammar, better raising an error.
  
  ```

  

### [Pairs and Lists](<https://docs.racket-lang.org/reference/pairs.html>)

```lisp
(pair? v) ; list is pair, cons generates pair
(list? v) ; list is either '() or a pair whose cdr is a list.
(null? v) ; (equal? v '())
null ; '()

(list* v ... tail) ; use tail instead of '()

;;; API
(length lst)
(list-ref lst pos)
(list-tail lst pos) ; cdd..dr
(list-set lst pos val)
(index-of lst v) ; (index-of '(1 2 3 4) 3)  => 2
(indexes-of lst v) ; return all
(index-where lst proc)
(indexes-where lst proc)
(append lst ...)
(append lst ... v) ; improper
(reverse lst)
(map proc lst ...)
(andmap proc lst ...) ; remedy of (apply and (map proc lst ...)), which is errorneous since and is macro.
; (andmap positive? '(1 2 3)) => #t
(ormap proc lst ...)
(for-each proc lst ...)
(foldl proc init lst ...)
(foldr proc init lst ...)
(filter pred lst)
(remove v lst) ; remove the first v in lst, use equal? to determinate equality.
(remove* v lst) ; remove all v in lst
(remq v lst) ; use eq?
(sort lst less-than?)
(member v lst) ; return the list start with v, use equal?
(memq v lst) ; use eq?
(assoc v lst)
(assq v lst)

(flatten lst)
(remove-duplicates lst)
(count proc lst ...) ; (count positive? '(1 2 -1))
(shuffle lst)
(combinations lst size) 
(permutations lst)
(cartesian-product lst1 lst2 ...)

```



### Vector

```lisp
(make-vector 100 0)
(vector-set! vct i v)
(vector-ref vct i)
(vector-fill vct v)
```



### Set

```lisp
;; create
; set = equal?
; seteqv = eqv?
; seteq = eq?
(apply set lst)
(list->set lst)
;; operations
(set-member? st v)
(set-add! st v)
(set-remove! st v)
(set-empty? st)
(set-clear! st)
(set-union st0 st1 ...)
(set-intersect st0 st1 ...)
(set-subtract st0 st1 ...)
(set-symmetric-difference st0 st1 ...)

```



### Hash Set

```lisp
(define h (make-hash))
(hash-has-key? h k)
(hash-set! h k v)
(hash-clear! h)
```



### Queue

```lisp
(require data/queue)
(define q (make-queue))
(enqueue! q v)
(dequeue! q)
(queue-empty? q)

```



### String

```lisp
(make-string 5 #\z)
(string-length "abc")
(string-ref "abc" 0)
(string-set! "abc" 0 #\d)
(substring "abc" 1 2) ; "b"
(string-append str ...)
(string->list str) ; list of chars
(string=? "a" "b")
(string-upcase str)
(string-contains? str substr)
(number->string 1)

```



### Procedures

```lisp
(procedure-arity proc)
(identity v) ;=> v
(const v); a function always return v, accepting any args
(negate proc)
(conjoin f1 f2 ...) ; a function ~ (and (f1 x) (f2 x) ...)
(disjoin f1 f2 ...) ; (or ...)

```

