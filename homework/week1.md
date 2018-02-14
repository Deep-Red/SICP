# 1.

*Do exercise 1.6, page 25.*  
Since the function new-if is not a special form, it evaluates in applicative order, meaning that the else clause is evaluated, then the then-clause, then the predicate. Since sqrt-iter works recursively the initial else-clause calls it again, and so forth without ever actually evaluating the predicate. As a special form if evaluates the predicate first, and then only the appropriate choice between the consequent and alternative. Thus, the entire process short circuits and returns an answer the first time the if clause is true and the consequent returns the current guess.
---
# 2.

*Write a procedure `squares` that takes a sentence of numbers as its argument and returns a sentence of the squares of the numbers.*
```scheme
(define (squares args)
  (if (pair? (cdr args))
      (append (list (square (car args))) (squares (cdr args)))
      (list (square (car args)))))
```
---
# 3.

*Write a procedure `switch` that takes a sentence as its argument and returns a sentence in which every instance of the words `I` or `me` is replaced by `you`, while every instance of `you` is replaced by `me` except at the beginning of the sentence, where it's replaced by `I`. (Don't worry about capitalization of letters.)*

```scheme
(define (switch sent)
  (if (empty? (bf sent))
      (substitute-first (first sent))
      (sentence (switch (butlast sent)) (substitute (last sent)))
      ))

(define (substitute wd)
  (cond ((member? wd '(I me)) 'you)
	((eq? wd 'you) 'me)
	(else wd)))

(define (substitute-first wd)
  (cond ((member? wd '(I me)) 'you)
	((eq? wd 'you) 'i)
	(else wd)))
```
---
# 4.

*Write a predicate `ordered?` that takes a sentence of numbers as its argument and returns a true value if the numbers are in ascending order, or a false value otherwise.*
```scheme
(define (ordered? sent)
  (cond ((empty? (bf sent)) #t)
	((<= (first sent) (first (bf sent))) (ordered? (bf sent)))
	(else #f)))
```
---
# 5.

*Write a procedure ends-e that takes a sentence as its argument and returns a sentence containing only those words of the argument whose last letter is E.*
```scheme
(define (ends-e sent)
  (if (empty? (bf sent))
      (return-e (first sent))
      (sentence (return-e (first sent)) (ends-e (bf sent)))))
      
(define (return-e wd)
  (if (eq? (last wd) 'e) wd '()))
```	   