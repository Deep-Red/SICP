
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