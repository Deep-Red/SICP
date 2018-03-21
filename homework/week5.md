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

## 2.29

*A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length, from which hangs either a weight or another binary mobile. We can represent a binary mobile using compound data by constructing it from two branches (for example, using `list`):*
```scheme
(define (make-branch length structure)
	(list length structure))
```
*a. Write the corresponding selectors `left-branch` and `right-branch`, which return the branches of a mobile, and `branch-length` and `branch-structure`, which return the components of a branch.*
*b. Using your selectors, define a procedure `total-weight` that returns the total weight of a mobile.*
*c. A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submobiles hanging off its branches is balanced. Design a predicate that tests whether a binary mobile is balanced.*
*d. Suppose we change the representation of mobiles so that the constructors are*
```scheme
(define (make-mobile left right)
	(list left right))
(define (make-branch length structure)
	(list length structure))
```
*How much do you need to change your programs to convert to the new representation?*

### a:
```scheme
(define (left-branch mobile)
	(car mobile))
(define (right-branch mobile)
	(cadr mobile))
(define (branch-length branch)
	(car branch))
(define (branch-structure branch)
	(cadr branch))
```
### b:
```scheme
(define (total-weight mobile)
  (+
   (if (list? (branch-structure (left-branch mobile)))
       (total-weight (branch-structure (left-branch mobile)))
       (branch-structure (left-branch mobile)))
   (if (list? (branch-structure (right-branch mobile)))
       (total-weight (branch-structure (right-branch mobile)))
       (branch-structure (right-branch mobile)))))
```
### c:
```scheme
(define (balanced? mobile)
  (and
   (if (list? (branch-structure (left-branch mobile)))
       (balanced? (branch-structure (left-branch mobile)))
       true)
   (if (list? (branch-structure (right-branch mobile)))
       (balanced? (branch-structure (right-branch mobile)))
       true)
   (=
    (*
     (branch-length (left-branch mobile))
     (branch-structure (left-branch mobile)))
    (*
     (branch-length (right-branch mobile))
     (branch-structure (right-branch mobile))))))
```
It appears to me that there may be a problem with the above code where a mobile will falsely test as balanced as long as all of the terminating mobiles are themselves balanced.
### d:
`left-branch` and `branch-length` are unchanged.
```scheme
(define (right-branch mobile)
	(cdr mobile))
(define (branch-structure branch)
	(cdr branch))
```
change `list?` checks in `total-weight` and `balanced?` to `pair?`

# 2.30

*Define a procedure `square-tree` analogous to the `square-list` procedure of exercise 2.21. That is, `square list` should behave as follows:*
```scheme
>(square-tree
	(list 1
	      (list 2 (list 3 4) 5)
	      (list 6 7)))
(1 (4 (9 16) 25) (36 49))
```
*Define `square-tree` both directly (i.e., without using any higher-order procedures) and also by using `map` and recursion.*

```scheme
(define (square-tree tree)
    (cons
      (if (pair? (car tree))
	  (square-tree (car tree))
	  (square (car tree)))
      (cond ((null? (cdr tree)) nil)
	    ((pair? (cdr tree)) (square-tree (cdr tree)))
	    (square (cdr tree)))))

(define (square-tree tree)
	(map (lambda (sub-tree)
	     (if (pair? sub-tree)
	     	 (square-tree sub-tree)
		 (square sub-tree)))
	tree))
```