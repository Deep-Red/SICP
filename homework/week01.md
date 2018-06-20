# 1.

*Do exercise 1.6, page 25.*  
Since the function new-if is not a special form, it evaluates in applicative order, meaning that the else clause is evaluated, then the then-clause, then the predicate. Since sqrt-iter works recursively the initial else-clause calls it again, and so forth without ever actually evaluating the predicate. As a special form if evaluates the predicate first, and then only the appropriate choice between the consequent and alternative. Thus, the entire process short circuits and returns an answer the first time the if clause is true and the consequent returns the current guess.
___
# 2.

*Write a procedure `squares` that takes a sentence of numbers as its argument and returns a sentence of the squares of the numbers.*
```scheme
(define (squares args)
  (if (pair? (cdr args))
      (append (list (square (car args))) (squares (cdr args)))
      (list (square (car args)))))
```
___
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
___
# 4.

*Write a predicate `ordered?` that takes a sentence of numbers as its argument and returns a true value if the numbers are in ascending order, or a false value otherwise.*
```scheme
(define (ordered? sent)
  (cond ((empty? (bf sent)) #t)
	((<= (first sent) (first (bf sent))) (ordered? (bf sent)))
	(else #f)))
```
___
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
___
# 6.
*Most versions of Lisp provide `and` and `or` procedures like the ones on page 19. In principle there is no reason why these can't be ordinary procedures, but some versions of Lisp make them special forms. Suppose, for example, we evaluate `(or (= x 0) (= y 0) (= z 0))`
If `or` is an ordinary procedure, all three argument expressions will be evaluated before `or` is invoked. But if the variable `x` has the value 0, we know that the entire expression has to be true regardless of the values of `y` and `z`. A Lisp interpreter in which `or` is a special form can evaluate the arguments one by one until either a true one is found or it runs out of arguments.
Your mission is to devise a test that will tell you whether Scheme's `and` and `or` are special forms or ordinary functions. This is a somewhat tricky problem, but it'll get you thinking about the evaluation process more deeply than you otherwise might.
Why might it be advantageous for an interpreter to treat `or` as a special form and evaluate its arguments one at a time? Can you think of reasons why it might be advantageous to treat `or` as an ordinary function?*

```scheme
(define (or-special?)
  (display '(or is))
  (or #t (display 'NOT))
  (display '(a special function)))

(define (and-special?)
  (and (display 'and) (display 'is) #f (display 'NOT))
  (display 'a ) (display 'special) (display 'form))
```
As a special function `or` can short circuit, so that if an expression should only be run when the first is true it can be written succinctly. The only advantage to treating `or` as an ordinary function that is apparent to me right away is if you have side effects from the expressions that you wish to persist. This seems like a bad idea though.