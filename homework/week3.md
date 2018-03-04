
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
(if (< k 1)
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

