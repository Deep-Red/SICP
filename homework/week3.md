
# 1.

*Abelson & Sussman, exercises 1.16, 1.35, 1.37, 1.38*

## 1.16

*Design a procedure that evalves an iterative exponentiation process that uses successive squaring and uses a logarithmic number of steps, as does `fast-expt`. (Hint: Using the observatin that (b<sup>n/2</sup>)<sup>2</sup>=(b<sup>2</sup>)<sup>n/2</sup>, keep, along with the exponent n and the base b, an additional state variable a, and define the state transformation in such a way that the product ab<sup>n</sup> is unchanged from state to state. At the beginning of the process a is taken to be 1, and the answer is given by the value of a at the end of the process. In general, the technique of defining an invariant quantity that remains unchanged from state to state is a powerful way to think about the design of iterative algorithms.)*

```scheme
(define (expt-iter b n a)
	(cond ((= n 0) a)
	((even? n) (expt-iter (square b) (/ n 2) a))
	(else (expt-iter b (- n 1) (* a b)))))

(define (fast-expt b n)
	(expt-iter b n 1))
```
## 1.35

*Show that the golden ratio &#966; is a fixed point of the transformation x &#27FC; 1 + 1/x and use this fact to compute &#966; by means of the `fixed-point` procedure.*

x = 1 + 1 / x  
 x<sup>2</sup> = x + 1  
x<sup>2</sup> - x - 1 = 0  
x = (1 &#177; &#221A;(-1<sup>2</sup> - (4 * 1 * -1))) / (2 * 1)  
x = (1 &#177; &#221A;(5)) / 2  
x = (1 + &#221A;5) / 2  

```scheme
(define (golden-ratio)
	(fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))
```

## 1.37(a)

*... Define a procedure `cont-frac` such that evaluating `(cont-frac n d k)` computes the value of the k-term finite continued fraction. Check your procedure by approximating 1/&#966; using `(cont-frac (lambda (i) 1.0)(lambda (i) 1.0) k)` for successive values of k. How large must you make k in order to get an approximation that is accurate to 4 decimal places?*

```scheme
(define (cont-frac n d k)
(if (= k 1)
(/ (n k) (d k))
(/ (n k) (+ (d k) (cont-frac n d (- k 1))))))
```
10

## 1.37(b)

*If your `cont-frac` generates a recursive process, write one that generates an iterative process. If it generates an iterative process, write one that generates a recursive process.*
```scheme
(define (cont-frac-iter n d k)
(define (iter i result)
(if (= i k)
result
(iter (+ i 1) (/ (n i) (+ (d i) result)))))
(iter 0 1))
```

## 1.38

*In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus Continuis, which included a continued fraction expansion for e - 2 where e is the base of the natural logarithms. In this fraction, the N<sub>i</sub> are all 1, and the D<sub>i</sub> are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your `cont-frac` procedure from exercise 1.37 to approximate e, based on Euler's expansion.*

```scheme
(define (euler-d i)
(if (> (modulo (+ i 1) 3) 0)
1
(* 2 (/ (+ i 1) 3))))

;This is not working. The formula oscillates rather than converging.
;I've tried substituting cont-frac-iter as well, with the same result.
(define (euler-e k)
(+ 2 (cont-frac (lambda (i) 1.0) euler-d k)))

# 2

* A "perfect number" is defined as a number equal to the sum of all its factors less than itself. For example, the first perfect number is 6, because its factors are 1, 2, 3, and 6, and 1+2+3=6. The second perfect number is 28, because 1+2+4+7+14=28. What is the third perfect number? Write a procedure `(next-perf n)` that tests numbers starting with n and continuing with n+1, n+2, etc. until a perfect number is found. Then you can evaluate `(next-perf 29)` to solve the problem. Hint: you'll need a `sum-of-factors` subprocedure.*

```scheme
(define (next-perf n)
(if (= (sum-of-factors (+ n 1)) (+ n 1))
(+ n 1)
(next-perf (+ n 1))))

(define (sum-of-factors n)
(define (add-factors x total)
(cond
((> x (/ n 2)) total)
((= (modulo n x) 0) (add-factors (+ x 1) (+ total x)))
(else (add-factors (+ x 1) total))))
(add-factors 2 1))
```
496

# 3

*Explain the effect of interchanging the order in which the base cases in the `cc` procedure on page 41 of Abelson and Sussman are checked. That is, describe completely the set of arguments for which the original `cc` procedure would return a different value or behave differently from a cc procedure coded as given below, and explain how the returned values would differ.
```scheme
(define (cc amount kinds-of-coins)
(cond ((or (< amount 0) (= kinds-of-coins 0)) 0)
((= amount 0) 1)
(else ...))); as in the original version
```*

On an input of n = 0 and a = 0 the newly modified procedure would return 0, whereas the old procedure would return 1 on those inputs.

# 4

*Give an algebraic formula relating the values of the parameters b, n, counter, and product of the expt and exp-iter procedures given near the top of page 45 of Ableson and Sussman. (The kind of answer we're looking for is "the sum of b, n, and the counter times product is always equal to 37."*

The product at each step is equal to b raised to the power of the difference between n and the counter.
product = b<sup>(n - counter)</sup>

# Extra:

## 1.

*The partitions of a positive integer are the different ways to break the integer into peices. The number 5 has seven partitions:
```
5   (one piece)
4, 1	 (two pieces)
3, 2	 (two pieces)
3, 1, 1	 (three pieces)  
2, 2, 1	 (three pieces)
2, 1, 1, 1	(four pieces)
1, 1, 1, 1, 1	(five pieces)
```
The order of the pieces doesn't matter, so the partition 2, 3 is the same as the partition 3, 2 and thus isn't counted twice. 0 has one partition.
Write a procedure `number-of-partitions` that computes the number of partitions of its nonnegative integer argument.*

```scheme
(define (number-of-partitions x)
(partition x x))

(define (partition x y)
(cond ((= x 0) 1)
((or (< x 0) (= y 0)) 0)
(else (+ (partition x (- y 1))
(partition (- x y) y)))))
```

## 2.

*Compare the `number-of-partitions` procedure with the `count-change` procedure by completing the following statement: 'Counting partitions is like making change, where the coins are ...`*  
Counting partitions is like making change, where the coins are available in every whole number denomination.