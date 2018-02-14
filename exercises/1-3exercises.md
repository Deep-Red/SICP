# Section 1.3 #

## **Exercise 1.29:** Simpson's Rule is a more accurate method of numerical integration than the method illustrated above. Using Simpson's Rule, the integral of a function f between a and b is approximated as *(image on page 60)* where h=(b-a)/n, for some even integer n, and y<sub>k</sub>=f(a+kh). (Increasing n increases the accuracy of the approximation.) Define a procedure that takes as arguments f, a, b, and n and returns the value of the integral, computed using Simpson's Rule. Use your procedure to integrate cube between 0 and 1 (with n=100 and n=1000), and compare the results to those of the integral procedure shown above. ##

```scheme
(define (simpson f a b n)
  (define (inc x) (+ x 1))
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1)
	     ((* (+ 1 (modulo k 2)) 2)))
       (y k)))
  (* (/ h 3)
     (sum term 0 inc n)
     ))
```
*This procedure returns an integer, in this case the exact result.*


---

## **Exercise 1.30:** The sum procedure above generates a linear recursion. The procedure can be rewritten so that the sum is performed iteratively. Show how to do this by filling in the missing expressions in the following definition:
```scheme
    (define (sum term a next b) 
      (define (iter a result)
        (if <??>
	    <??>
	    (iter <??> <??>)))
      (iter <??> <??>))
```
##
```scheme
    (define (sum-iter term a next b)
      (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ (term a) result))))
      (iter a 0))
```

---

## **Exercise 1.31:** a. The sum procedure is only the simplest of a vast number of similar abstractions that can be captured as higher-order procedures. Write an analogous procedure called product that returns the product of the values of a function at points over a given range. Show how to define factorial in terms of product. Also use product to compute approximations to pi using the formula *(John Wallis's formula, pictured on page 60)*
b. If your product procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.##

```scheme
    (define (product term a next b)
      (if (> a b)
          1
          (* (term a)
          (product term (next a) next b))))

    (define (factorial x)
      (define (inc n) (+ n 1))
      (define (identity n) (* n 1))
      (product identity 1 inc x))

    (define (wallis terms)
      (define (inc x) (+ x 1))
      (define (num k)
        (if (= k 1)
	  2
	  (+ 2 (* 2 (quotient k 2)))))
      (define (den k)
        (if (= k 1)
    	3
    	(+ 3 (* 2 (quotient (- k 1) 2)))))
      (define numerator (product num 1 inc terms))
      (define denominator (product den 1 inc terms))
      (/ numerator denominator))

    (define (product-iter term a next b)
      (define (iter a result)
        (if (> a b)
    	result
    	(iter (next a) (* (term a) result))))
      (iter a 1))
```

---

## **Exercise 1.32:** a. Show that sum and product (exercise 1.31) are both special cases of a still more general notion called accumulate that combines a collection of terms, using some general accumulation function: (accumulate combiner null-value term a next b). Accumulate takes as arguments the same term and range specifications as sum and product, together with a combiner procedure (of two arguments) that specifies how the current term is to be combined with the accumulation of the preceding terms and a null-value that specifies what base value to use when the terms run out. Write accumulate and show how sum and product can both be defined as simple calls to accumulate.
b. If your accumulate procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

```scheme
(define (accumulate combiner null-value term a next b)  
  (if (> a b)  
      null-value  
      (combiner (term a)  
		(accumulate combiner null-value term (next a) next b))))  

(define (prod-accumulate term a next b)  
  (accumulate * 1 term a next b))  

(define (sum-accumulate term a next b)  
  (accumulate + 0 term a next b))

(define (accumulate-iter combiner null-value term a next b)  
  (define (iter a result)  
    (if (> a b)  
	result  
	(iter (next a) (combiner (term a) result))))  
  (iter a null-value))
```

---

## **Exercise 1.33:** You can obtain an even more general version of accumulate (exercise 1.32) by introducing the notion of a filter on the terms to be combined. That is, combine only those terms derived from values in the range that satisfy a specified condition. The resulting filtered-accumulate abstraction takes the same arguments as accumulate, together with an additional predicate of one argument that specifies the filter. Write filtered-accumulate as a procedure. Show how to express the following using filtered-accumulate:
a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate already written)
b. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i < n sube that GCD(i,n) = 1).

```scheme
    (define (filtered-accumulate combiner null-value term a next b filter)
      (cond ((> a b) null-value)
    	((filter a) (combiner (term a)
    			    (filtered-accumulate combiner null-value term a next b filter)))
    	((else) (combiner (null-value)
			  (filtered-accumulate combiner null-value term a next b filter)))))

    (filtered-accumulate + 0 square a inc b prime?)

    (define (product-of-coprimes n)
      (filtered-accumulate combiner
```

---

## **Exercise 1.34:** Suppose we define the procedure `(define (f g) (g 2))` Then we have
```scheme
(f square)
4

(f (lambda (z) (* z (+ z 1))))
6
```
What happens if we (perversely) ask the interpreter to evaluate the combination `(f f)`? Explain.

*The interpreter throws an error (specifically "The object 2 is not applicable.") because f is expecting a procedure as an argument, not an integer.*

---

## **Exercise 1.35:** Show that the golden ratio ϕ (section 1.2.2) is a fixed point of the transformation x ⟼ 1 + 1/x, and use this fact to compute ϕ by means of the fixed-point procedure.

```scheme
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 0.01) 
1.6180328133318276
```

---

## **Exercise 1.36:** Modify fixed-point so that it prints the sequence of approximations it generates, using the newline and display primitives shown in exercise 1.22. Then find a solution to x<sup>x</sup>=1000 by finding a fixed point of x ⟼ log(1000)/log(x). (Use Scheme's primitive log procedure, which computes natural logarithms.) Compare the number of steps this takes with an without average damping. (Note that you cannot start fixed-point with a guess of 1, as this would cause division by log(1)=0.)

```scheme
(fixed-point (lambda (x) (/ (log 1000) (log x))) 60)
```

37 vs. 14 with a starting guess of 60.
*

---

## **Exercise 1.37:** a. An infinite continued fraction is an expression of the form *(image on page 71)*. As an example, one can show that the infinite continued fractioin expansion owith the N<sub>i</sub> and the D<sub>i</sub> all equal to 1 produces 1/ϕ where ϕ is the golden ration (described in section 1.2.2). One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation -- a so-called k-term finite continued fraction -- has the form *(image on page 71)*. Suppose that <sub>n</sub> and <sub>d</sub> are procedures of one argument (the term index i) that return the N<sub>i</sub> and the D<sub>i</sub> of ht eterms of the continued fraction. Define a procedure cont-frac such that evaluating (cont-frac n d k) computes the value of the k-term finite continued fraction. Check your procedure by approximating 1/ϕ using `(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)` for successive values of k. How large must you make k in order to get an approximation that is accurate to 4 decimal places? b. If your cont-frac procedure generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.

```scheme
(define (cont-frac n d k)
  (define (fraction i)
    (if (= i k)
	(/ (n i) (d i))
	(/ (n i) (+ (d i) (fraction (+ i 1))))))
  (fraction 1)
  )

10

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (> i k)
	(/ n result)
	(iter (+ 1 i) (+ d (/ n result)))))
  (iter 0 1))
```

---

## **Exercise 1.38:** In 1737, the Swiss mathematician Leonhard Euler published a memoir *De Fractionibus Continuis*, which included a continued fraction expansion for *e*-2, where *e* is the base of the natural logarithms. In this fraction, the N<sub>i</sub> are all 1, and the D<sub>i<sub> are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your cont-frac procedure from exercise 1.37 to approximate *e*, based on Euler's expansion.

```scheme
(define (cont-frac n d k)
  (define (fraction i)
    (if (= i k)
	(/ (n i) (d i))
	(/ (n i) (+ (d i) (fraction (+ i 1))))))
  (fraction 1)
  )

(define (euler-d x)
  (if (= 0 (modulo (+ x 1) 3))
      (* (/ (+ x 1) 3) 2)
      1))

(define (euler-e x)
  (+ 2 (cont-frac (lambda (i) 1.0) euler-d x)))

```

---

## **Example 1.39:**
A continued fraction representation of the tangent function was published in 1770 by the German mathematician J.H. Lambert: *(image on page 72)* where x is in radians. Define a procedure (tan-cf x k) that computes an approximation to the tangent function based on Lambert's formula. k specifies the number of terms to compute, as in exercise 1.37.

```scheme
(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
	x
	(- (* x x))))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac n d k))
```

---

##Exercise 1.40:##
###Define a procedure cubic that can be used together with the newtons-method procedure in expressions of the form `(newtons-method (cubic a b c) 1)` to approximate zeros of the cubic x<sup>3</sup>+ax<sup>2</sup>+bx+c.###

```scheme
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
```
---

## Exercise 1.41: ##
###Define a procedure double that takes a procedure of one argument as argument and returns a procedure that applies the original procedure twice. For example, if inc is a procedure that adds 1 to its argument, then (double inc) should be a procedure that adds 2. What value is returned by (((double (double double)) inc) 5)###

```scheme
    (define (double proc)
      (lambda (x) (proc (proc x))))

21
```
*The first call to double will increment by two, nesting it does this again incrementing by a total of four, nesting once more yields four times four, that is a total of sixteen calls to increment.
*

---

##**Exercise 1.42**##
###Let f and g be two one-argument functions. The composition f after g is defined to be the function x ⟼ f(g(x)). Define a procedure compose that implements composition. For example, if inc is a procedure that adds 1 to its argument,
((compose square inc) 6)
49
###

```scheme
(define (compose f g)
  (lambda (x) (f (g x))))
```

---
##**Exercise 1.43:**##
###If *f* is a numerical function and *n* is a positive integer, then we can form the *n*th repeated application of f, which is defined to be the function whose value at x is f(f(...(f(x))...)). For example, if *f* is the function x ⟼ x + 1, then the *n*th repeated application of *f* is the function that raises its argument to the 2<sup>n</sup>th power. Write a procedure that takes as inputs a procedure that computes *f* and a positive integer *n* and returns the procedure that computes the *n*th repeated application of *f*. Your procedure should be able to be used as follows:
((repeated square 2) 5)
625.
Hint: You may find it convenient to use compose from exercise 1.42.
###

```scheme
(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (- n 1)))
      f))
```

---

##**Exercise 1.44: **##
### The idea of smoothing a function is an important concept in signal processing. If f is a function and dx is some small number, then the smoothed version of f is the function whose value at a point x is the average of f(x - dx), f(x), and f(x + dx). Write a procedure smooth that takes as input a procedure that computes f and returns a procedure that computes the smoothed f. It is sometimes valuable to repeatedly smooth a function (that is, smooth the smoothed function, and so on) to obtained the n-fold smoothed function. Show how to generate the n-fold smoothed function of any given function using smooth and repeated from exercise 1.43.###

```scheme
(define (smooth f dx)
  (lambda (x)
    (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3))
  )

((repeated (smooth cube x) dx) n)
```

---

##**Exercise 1.45:** ##
### We saw in section 1.3.3 that attempting to compute square roots by naively finding a fixed point of y ⟼ x/y does not converge, and that this can be fixed by average damping. The same method works for finding cube roots as fixed points of the average-damped y ⟼ x/y<sup>2</sup>. Unfortunately, the process does not work for fourth roots -- a single average damp is not enough to make a fixed-point search for y ⟼ x/y<sup>3</sup> converge. On the other hand, if we average damp twice (i.e., use the average damp of the average damp of y ⟼ x/y<sup>3</sup>) the fixed-point search does converge. Do someexperiments to determine how many average damps are required to compute nth roots as a fixed-point search based upon repeated average damping of y ⟼ x/y<sup>n-1</sup>. Use this to implement a simple procedure for computing nth roots using fixed-point, average-damp, and the repeated procedure of exercise 1.43. Assume that any arithmetic operations you need are available as primitives.###


