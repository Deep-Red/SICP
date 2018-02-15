#1.

*Abelson & Sussman, exercises 1.31(a), 1.32(a), 1.33, 1.40, 1.41, 1.43, 1.46*

## 1.31(a)

*The `sum` procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures. Write an analogous procedure called `product` that returns the product of the values of a function at points over a given range. Show how to define `factorial` in terms of `product`. Also use `product` to compute approximations to &#1D70B; using the formula &#1D70B;/4 = (2 \* 4 \* 4 \* 6 \* 6 \* 8 ...)/(3 \* 3 \* 5 \* 5 \* 7 \* 7 ...).*

```scheme
(define (inc n) (+ n 1))
(define (identity x) x)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (factorial x)
  (product identity 1 inc x))

(define (wallis-num x)
  (+ 2 (* 2 (floor (/ x 2)))))

(define (wallis-den x)
  (+ 3 (* 2 (floor (/ (- x 1) 2)))))

(define (wallis-pi x)
  (* 4
     (/ (product wallis-num 1 inc x)
	(product wallis-den 1 inc x))))

```
## 1.32(a)

*Show that `sum` and `product` (exercise 1.31) are both special cases of a still more general notion called accumulate that combines a collection of terms, using some general accumulation function:
`(accumulate combiner null-value term a next b)`
`Accumulate` takes as arguments the same term and range specifications as `sum` and `product`, together with a `combiner` procedure (of two arguments) that specifies how the current term is to be combined with the accumulation of the preceding terms and a `null-value` that specifies what base value to use when the terms run out. Write `accumulate` and show how `sum` and `product` can both be defined as simple calls to `accumulate`.*

```scheme
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(accumulate + 0 identity 1 inc 10)
(accumulate * 1 identity 1 inc 10)
```
## 1.33

*You can obtain an even more general version of accumulate (exercise 1.32) by introducing the notion of a filter on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting `filtered-accumulate` abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter. Write `filtered-accumulate` as a procedure. Show how to express the following using `filtered-accumulate`:
a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a `prime?` predicate already written)
b. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i < n such that GCD(i,n)=1).*

```scheme
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter a)
	  (combiner (term a)
		    (filtered-accumulate combiner null-value term (next a) next b filter))
	  (combiner null-value
		    (filtered-accumulate combiner null-value term (next a) next b filter)))))

(filtered-accumulate + 0 square a inc b prime?)
(filtered-accumulate * 1 identity 1 inc b relative-prime?)
```

## 1.40

*Define a procedure `cubic` that can be used together with the `newtons-method` procedure in expressions of the form
`newtons-method (cubic a b c) 1)`
to approximate zeros of the cubic x<sup>3</sup>+ax<sup>2</sup>+bx+c.*

```scheme
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
```

## 1.41

*Define a procedure `double` that takes a procedure of one argument as an argument and returns a procedure that applies the original procedure twice. For example, if `inc` is a procedure that adds 1 to its argument, then `(double inc)` should be a procedure that adds 2. What value is returned by `(((double (double double)) inc) 5)`*

```scheme
(define (double g)
  (lambda (x) (g (g x))))
```
21

## 1.43

*If f is a numerical function and n is a positive integer, then we can form the nth repeated application of f, which is defined to be the function whose value at x is f(f(...(f(x))...)). For example, if f is the function x &#x27FC; x + 1, then the nth repeated application of f is the functin x &#x27FC x + n. If f is the operation of squaring a number, then the nth repeated application of f is the function that raises its argument to the 2<sup>n</sup>th power. Write a procedure that takes as inputs a procedure that comutes f and a positive integer n and returns the procedure that computes the nth repeated application of f. Your procedure should be able to be used as follows:
```scheme
((repeated square 2) 5)
625
```*

