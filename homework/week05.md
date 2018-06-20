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
**It appears to me that there may be a problem with the above code where a mobile will falsely test as balanced as long as all of the terminating mobiles are themselves balanced.**
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

## 2.31

*Abstract your answer to exercise 2.30 to produce a procedure `tree-map` with the property that `square-tree` could be defined as `(define (square-tree tree) (tree-map square tree))`*

```scheme
(define (tree-map f tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map f sub-tree)
	     (f sub-tree)))
       tree))
```

## 2.32

*We can represent a set as a list of distinct elements, and we can represent the set of all subsets of the set as a list of lists. For example, if the set is `(1 2 3)`, then the set of all subsets is `(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))`. Complete the following definition of a procedure that generates the set of subsets of a set and give a clear explanation of why it works:*

```scheme
(define (subsets s)
	(if (null? s)
	    (list nil)
	    (let ((rest (subsets (cdr s))))
	    	 (append rest (map <??> rest)))))
```

```scheme
(define (subsets s)
	(if (null? s)
	    (list nil)
	    (let ((rest (subsets (cdr s))))
	    	 (append rest (map (lambda (x) (cons (car s) x)) rest)))))
```
The recursive call to `subsets` with `(cdr s)` as an argument appends to that the `car` of s to each subset of `(cdr s)`.
**I'm still not sure I've wrapped my head around this though.**

## 2.36

*The procedure `accumulate-n` is similar to `accumulate` except that it takes as its third argument a sequence of sequences, which are all assumed to have the same number of elements. It applies the designated accumulation procedure to combine all the first elements of the sequences, all the second elements of the sequences, and so on, and returns a sequence of the results. for instance, if `s` is a sequence containing four sequences, `((1 2 3) (4 5 6) (7 8 9) (10 11 12))`, then the value of `(accumulate-n + 0 s)` should be the sequence `(22 26 30)`. Fill in the missing expressions in the following definition of `accumulate-n`:*

```scheme
(define (accumulate-n op init seqs)
	(if (null? (car seqs))
	    nil
	    (cons (accumulate op init <??>)
	    	  (accumulate-n op init <??>))))
```

```scheme
(define (accumulate-n op init seqs)
	(if (null? (car seqs))
	    nil
	    (cons (accumulate op init (map car seqs))
	    	  (accumulate-n op init (map cdr seqs)))))
```

## 2.37
*Suppose we represent vectors v=(v<sub>i</sub>) as sequences of numbers, and matrices m = (m<sub>ij</sub>) as sequences of vectors (the rows of the matrix). For example, the matrix*
![example matrix](images/ch2-Z-G-20.gif) 
*is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation, we can use sequence operations to concisely express the basic matrix and vector operations. These operations (which are described in any book on matrix algebra) are the following:*
![example matrix](images/ch2-Z-G-21.gif)
*We can define the dot product as `(define (dot-product v w) (accumulate + 0 (map * v w)))`. Fill in the missing expressions in the following procedures for computing the other matrix operations. (The procedure `accumulate-n` is defined in exercise 2.36)*
```scheme
(define (matrix-*-vector m v)
	(map <??> m))
(define (transpose mat)
	(accumulate-n <??> <??> mat))
(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
	     (map <??> m)))
```

```scheme
(define (matrix-*-vector m v)
	(map (lambda(row) (dot-product row v)) m))
(define (transpose mat)
	(accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
	(let ((cols (transpose n)))
	     (map (lambda(row) (matrix-*-vector cols row)) m)))
```

## 2.38

*The `accumulate` procedure is known as `fold-right`, because it combines the first element of the sequence with the result of combining all the elements to the right. There is also a `fold-left`, which is similar to `fold-right` except that it combines elements working in the opposite direction:*
```scheme
(define (fold-left op initial sequence)
	(define (iter result rest)
		(if (null? rest)
		result
		(iter (op result (car rest))
		      (cdr rest))))
	(iter initial sequence))
```
*What are the values of*
```scheme
(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))
```
*Give a property that `op` should satisfy to guarantee that `fold-right` and `fold-left` will produce the same values for any sequence.*

```
1.5
1/6
(1 (2 (3 ())))
(((() 1) 2) 3)
```
`op` must be commutative in order for the results of `fold-left` and `fold-right` to reliably produce the same result for any sequence.

## 2.54

*Two lists are said to be `equal?` if they contain equal elements arranged in the same order. For example, `(equal? '(this is a list) '(this is a list))` is true, but `(equal? '(this is a list) '(this (is a) list))` is false. To be more precise, we can define `equal?` recursively in terms of the basic `eq?` equality of symbols by saying that `a` and `b` are `equal?` if they are both symbols and the symbols are `eq?`, or if they are both lists such that `(car a)` is `equal?` to `(car b)` and `(cdr a)` is `equal?` to `(cdr b)`. Using this idea, implement `equal?` as a procedure.*

```scheme
(define (equal? x y)
  (cond ((and (null? x) (null? y)) #t)
	((or (null? x) (null? y)) #f)
	((and (list? x) (list? y))
	 (and (equal? (car x) (car y))
	      (equal? (cdr x) (cdr y))))
	((or (list? x) (list? y)) #f)
	(else (eq? x y))))
```

# 2

*Extend the calculator program from lecture to include words as data, providing the operations `first`, `butfirst`, `last`, `butlast`, and `word`. Unlike Scheme, your calculator should treat words as self-evaluating expressions except when seen as the operator of a compound expression. That is, it should work like these examples:*
```
calc: foo
foo
calc: (first foo)
f
calc: (first (butfirst hello))
e
```

**Omitted - Calc program not available on server. May come back later and try to recreate based on lecture.**

