# 1.

*Abelson & Sussman, exercises 2.7, 2.8, 2.10, 2.12, 2.17, 2.20, 2.22, 2.23*

## 2.7

*Alyssa's program is incomplete because she has not specified the implementation of the interval abstraction. Here is a definition of the interval constructor: `(define (make-interval a b) (cons a b))` Define selectors `upper-bound` and `lower-bound` to complete the implementation.*

```scheme
(define (upper-bound x) (car x))
(define (lower-bound x) (cdr x))
```

## 2.8

*Using reasoning analogous to Alyssa's, describe how the difference of two intervals may be computed. Define a corresponding subtraction procedure, called `sub-interval`.*

```scheme
(define (sub-interval x y)
	(make-interval (- (lower-bound x) (upper-bound y))
		       (- (upper-bound x) (lower-bound y))))
```

## 2.10

*Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments that it is not clear what it means to divide by an interval that spans zero. Modify Alyssa's code to check for this condition and to signal an error if it occurs.*

```scheme
(define (div-interval x y)
	(if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0)))
	(display "Error, divisor interval spans 0.")
	(mul-interval x
		      (make-interval (/ 1.0 (upper-bound y))
		      		     (/ 1.0 (lower-bound y)))))
```

## 2.12

*Define a constructor `make-center-percent` that takes a center and a percentage tolerance and produces the desired interval. You must also define a selector `percent` that produces the percentage tolerance for a given interval. The `center` selector is the same as the one shown above.*

```scheme
(define (make-center-percent c p)
	(make-interval (- c (* c p)) (+ c (* c p))))

(define (percent i)
	(/ (width i) (center i)))
```

## 2.17

*Define a procedure `last-pair` that returns the list that contains only the last element of a given (nonempty) list:*
```scheme
(last-pair (list 23 72 149 34))
(34)
```

```scheme
(define (last-pair x)
	(if (null? (cdr x))
	x
	(last-pair (cdr x))))
```

## 2.20

*The procedures `+`, `*`, and `list` take arbitrary numbers of arguments. One way to define such procedures is to use `define` with dotted-tail notation. In a procedure definition, a parameter list that has a dot before the last parameter name indicates that, when the procedure is called, the initial parameters (if any) will have as values the initial arguments, as usual, but the final parameter's value will be a list of any remaining arguments. For instance, given the definition `(define (f x y . z) <body>)` the procedure `f` can be called with two or more arguments. If we evaluate `(f 1 2 3 4 5 6)` then in the body of `f`, `x` will be 1, `y` will be 2, and `z` will be the list `(3 4 5 6)`. Given the definition `(define (g . w) <body>)` the procedure `g` can be called with zero or more arguments. If we evaluate `(g 1 2 3 4 5 6)` then in the body of `g`, `w` will be the list `(1 2 3 4 5 6)`. Use this notation to write a procedure `same-parity` that takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first argument. For example,*
```scheme
(same-parity 1 2 3 4 5 6 7)
(1 3 5 7)

(same-parity 2 3 4 5 6 7)
(2 4 6)
```

```scheme
(define (same-parity i . l)
	(define (build-parity-list result i l)
		(cond ((empty? l) l)
		((even? (- i (car l))) (cons (car l) (build-parity-list result i (cdr l))))
		(else (build-parity-list result i (cdr l)))))
	(build-parity-list i i l))
```

## 2.22

*Louis Reasoner tries to rewrite the first `square-list` procedure of exercise 2.21 so that it evolves an iterative process:*

```scheme
(define (square-list items)
	(define (iter things answer)
		(if (null? things)
	    	    answer
		    (iter (cdr things)
		    	  (cons (square (car things))
			  	answer))))
	(iter items nil))
```
*Unfortunately, defining `square-list` this way produces the answer in the reverse order of the one desired. Why? Louis then tries to fix his bug by interchanging the arguments to `cons`:*
```scheme
(define (square-list items)
	(define (iter things answer)
		(if (null? things)
		    answer
		    (iter (cdr things)
		    	  (cons answer
			  	(square (car things))))))
	(iter items nil))
```
*This doesn't work either. Explain.*

The `iter` function goes through the list and `cons` puts each successive squared term at the beginning `answer`.

`cons` builds a pair each time, so interchanging the arguments leaves cons trying to combine a pair and a number, not a list and a number.

## 2.23

*The procedure `for-each` is similar to `map`. It takes as arguments a procedure and a list of elements. However, rather than forming a list of the results, `for-each` just applies the procedure to each of the elements in turn, from left to right. The values returned by applying the procedure to the elements are not used at all -- `for-each` is used with procedures that perform an action, such as printing. For example,*
```scheme
(for-each (lambda (x) (newline) (display x))
(list 57 321 88))
57
321
88
```
*The value returned by the call to `for-each` (not illustrated above) can be something arbitrary, such as true. Give an implementation of `for-each`.

```scheme
(define (for-each procedure operands)
  (if (empty? operands)
      #t
      (and (procedure (car operands)) (for-each procedure (cdr operands)))))
  
```
I'm not confident that this is the proper way to return a value of `true`.

# 2

*Write a procedure `substitute` that takes three arguments: a list an old word, and a new word. It should return a copy of the list, but with every occurrence of the old word replaced by the new word, even in sublists. For example:*
```scheme
> (substitute '((lead guitar) (bass guitar) (rhythm guitar) drums) 'guitar 'axe)
((lead axe) (base axe) (rhythm axe) drums)
```

```scheme
(define (substitute list old new)
  (define (build-sub-list result list old new)
    (cond ((empty? list) list)
	  ((list? (car list)) (cons (build-sub-list result (car list) old new) (build-sub-list result (cdr list) old new)))
	  ((equal? (car list) old) (cons new (build-sub-list result (cdr list) old new)))
	  (else (cons (car list) (build-sub-list result (cdr list) old new)))))
  (build-sub-list () list old new))
```

# 3

*Now write `substitute2` that takes a list, a list of old words, and a list of new words; the last two lists should be the same length. It should return a copy of the first argument, but with each word that occurs in the second argument replaced by the corresponding word of the third argument:*
```scheme
> (substitute2 '((4 calling birds) (3 french hens) (2 turtle doves)) '(1 2 3 4) '(one two three four))
((four calling birds) (three french hens) (two turtle doves))
```

The following procedure has one more call to `substitute` than should be necessary, but it returns the correct result.

```scheme
(define (substitute2 list oldl newl)
  (define (build-sub2-list list old new)
    (substitute list old new))
  (substitute list (car oldl) (car newl))
  (if (null? (cdr oldl))
      (substitute list (car oldl) (car newl))
      (substitute2 (substitute list (car oldl) (car newl)) (cdr oldl) (cdr newl))))
```