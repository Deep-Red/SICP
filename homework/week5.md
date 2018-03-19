# 1.

*Abelson & Sussman, exercises 2.24, 2.26, 2.29, 2.30, 2.31, 2.32, 2.36, 2.37, 2.38, 2.54.*

## 2.24

*Suppose we evaluate the expression `(list 1 (list 2 (list 3 4)))`. Give the result printed by the interpreter, the corresponding box-and-pointer structure, and the interpretation of this as a tree (as in figure 2.6).*

`(1 (2 (3 4)))`

![box-and-pointer diagram](images/A&S2-24boxdiagram.png)

![tree diagram](images/A&S2-24treediagram.png)

## 2.26

*Suppose we define `x` and `y` to be two lists:*
```scheme
(define x (list 1 2 3))
(define y (list 4 5 6))
```
*What result is printed by the interpreter in response to evaluating each of the following expressions:*
```scheme
(append x y)
(cons x y)
(list x y)
```

```scheme
(1 2 3 4 5 6)
((1 2 3) 4 5 6)
((1 2 3) (4 5 6))
```

