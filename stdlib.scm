;; the basics
(define nil ())
(define null nil)
(define true  #t)
(define false #f)
(define (pair? x) (and (list? x) (not (null? x))))
(define (not x) (if (eq? #f x) #t #f))
(define (newline) (display "\n"))

(define (list . objs) objs)
(define (id obj) obj)
(define (flip func)
  (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1)
  (lambda (arg) (func arg1 arg)))
(define (compose f g)
  (lambda (arg) (f (apply g arg))))

;; math
(define zero?       (curry = 0))
(define negative?   (curry (flip <) 0))
(define positive?   (curry (flip >) 0))
(define inc         (curry + 1))
(define dec         (curry (flip -) 1))

(define (divides? a b)
  (zero? (remainder b a)))

(define (prime? x)
  (= x (smallest-divisor x)))

;; this sort of coercion shouldn't be necessary
(define (odd? num)
  (cond ((and (float? num) (equal? num (floor num))) (= (mod (floor num) 2) 1))
        ((float? num) (display "odd? expects an integer, given:" num) #f)
        (else (= (mod num 2) 1))))
(define (even? num)
  (cond ((and (float? num) (equal? num (floor num))) (zero? (mod (floor num) 2)))
        ((float? num) (display "even? expects an integer, given:" num) #f)
        (else (zero? (mod num 2)))))

(define (square x)  (* x x))
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

;; exponentation using recursion (pretty fast)
(define (expt-rec b n)
  (if (zero? n)
      1
      (* b (expt-rec b (dec n)))))

;; exponentation using foldl (slowest)
(define (expt-fold b n)
  (fold * 1 (fill (range 1 n) b)))

;; exponentation using iteration
;; (seems a bit slower than the recursive version, but still fast)
(define (expt-iter b counter . product)
  (let product (lambda (if (null? product) 1 (car product)))
    (if (zero? counter)
        product
        (expt-iter b (dec counter) (* b product)))))

;; fast exponentation
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define expt fast-expt)

;; calculate square roots
(define (sqrt-good-enough? guess x)
  (let ((delta 0.0001))
    (< (abs (- (square guess) x))
       delta)))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (sqrt-good-enough? guess x)
      guess
      (sqrt-iter (sqrt-improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
(define ! factorial)

;; greatest common denominator
(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (remainder a b))))

(define (find-divisor n test-divisor)
  (let ((next (lambda (n)
                (if (eq? n 2)
                    3
                    (+ n 2)))))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor))))))

(define (smallest-divisor n)
    (find-divisor n 2))

(define (expmod base exp m)
  (cond ((zero? exp) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (dec exp) m))
                    m))))

;; test for prime numbers using Fermat's method
(define (fermat-test n)
  (let ((try-it (lambda (a)
                  (= (expmod a n n) a))))
    (try-it (inc (random (dec n))))))

;; this runs Fermat's test a given number of times
(define (fast-prime? n times)
  (cond ((zero? times) true)
        ((fermat-test n) (fast-prime? n (dec times)))
        (else false)))

;; folds

;; SICP calls this accumulate
(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

(define fold foldl)
(define reduce fold)

(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))

(define (sum-list lst)  (fold + 0 lst))
(define (sum . lst)     (sum-list lst))
(define (product . lst) (fold * 0 lst))
(define (average . xs)  (/ (sum-list xs) (length xs)))
(define avg average)

(define (max first . num-list) (fold (lambda (old new) (if (> old new) old new)) first num-list))
(define (min first . num-list) (fold (lambda (old new) (if (< old new) old new)) first num-list))
(define (length lst)           (fold (lambda (x y) (+ x 1)) 0 lst))
(define (reverse lst)          (fold (flip cons) '() lst))
(define (nth n lst)            (if (= n 1) (car lst) (nth (- n 1) (cdr lst))))


(define (mem-helper pred op)   (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq   obj lst)       (fold (mem-helper (curry eq?    obj) id) #f lst))
(define (memv   obj lst)       (fold (mem-helper (curry eqv?   obj) id) #f lst))
(define (member obj lst)       (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq   obj alist)     (fold (mem-helper (curry eq?    obj) car) #f alist))
(define (assv   obj alist)     (fold (mem-helper (curry eqv?   obj) car) #f alist))
(define (assoc  obj alist)     (fold (mem-helper (curry equal? obj) car) #f alist))

;; TODO define fold-k (fold w/ continuations) and use it to short-circuit these
(define (any? pred lst)        (fold (lambda (any-found x) (or any-found (pred x))) #f lst))
(define (all? pred lst)        (fold (lambda (all-matched x) (and (pred x) all-matched)) #t lst))

;; transformations
;; (define (map func lst)         (foldr (lambda (x y) (cons (func x) y)) '() lst))
;; (define (mapr func lst)        (fold (lambda (x y) (cons (func y) x)) '() lst))
(define (filter pred lst)      (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))
(define (fill lst n)           (map (lambda (x) n) lst))

;; the more general version of map, similar to mapcar in Lisp
(define (map func . seqs)
  (cond ((= 1 (length seqs))
         (foldr (lambda (x y) (cons (func x) y)) '() (car seqs)))
        ((null? (car seqs)) nil)
        (else
         (cons (apply func (map car seqs))
               (apply map (cons func (map cdr seqs)))))))


(define (caar lst)              (car (car lst)))
(define (cadr lst)              (car (cdr lst)))
(define (caddr lst)             (car (cdr (cdr lst))))
(define (cadddr lst)            (car (cdr (cdr (cdr lst)))))


;; like python's range
;; SICP calls this enumerate-interval ... i prefer range
(define (range min max)
  (if (> min max)
      nil
      (cons min (range (inc min) max))))

;; recursive method of concatenating 2 lists
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; string manipulation
(define (string-map func s)
  (list->string (map func (string->list s))))

(define (string-upcase s)
  (string-map char-upcase s))

(define (string-downcase s)
  (string-map char-downcase s))

(define (string-append . lst)
  (string-concatenate lst))