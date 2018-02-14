### 1. ###
*Do exercise 1.6, page 25.*  
Since the function new-if is not a special form, it evaluates in applicative order, meaning that the else clause is evaluated, then the then-clause, then the predicate. Since sqrt-iter works recursively the initial else-clause calls it again, and so forth without ever actually evaluating the predicate. As a special form if evaluates the predicate first, and then only the appropriate choice between the consequent and alternative. Thus, the entire process short circuits and returns an answer the first time the if clause is true and the consequent returns the current guess.
---
### 2. ###
*Write a procedure `squares` that takes a sentence of numbers as its argument and returns a sentence of the squares of the numbers.*
```scheme
(define (squares args)
(if (pair? (cdr args))
(append (list (square (car args))) (squares (cdr args)))
(list (square (car args)))))
```
---
### 3. ###
*Write a procedure `switch` that takes a sentence as its argument and returns a sentence in which every instance of the words `I` or `me` is replaced by `you`, while every instance of `you` is replaced by `me` except at the beginning of the sentence, where it's replaced by `I`. (Don't worry about capitalization of letters.) 