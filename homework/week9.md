# 1

*Abelson & Sussman, exercises 3.16, 3.17, 3.21, 3.25, 3.27*

## 3.16

*Ben Bitdiddle decides to write a procedure to count the number of pairs in any list structure. "It's easy," he reasons. "The number of pairs in any structure is the number in the `car` plus the number in the `cdr` plus one more to count the current pair." So Ben writes the following procedure:*
```scheme
(define (count-pairs x)
	(if (not (pair? x))
	0
	(+ (count-pairs (car x))
	   (count-pairs (cdr x))
	   1)))
```
*Show that this procedure is not correct. In particular, draw box-and-pointer diagrams representing list structures made up of exactly three pairs for which Ben's procedure would return 3; return 4; return 7; never return at all.*

```scheme
(define three-count '(foo bar baz))
;three-count -> [ | ] -> [ | ] -> [ |/]
;	     	 |        |        |
;		 v	  v	   v
;		'foo	 'bar	  'baz

(define x '(foo))
(define y (cons x x))
(define four-count (list y))
;four-count -> [ |/]
;	        |
;		v
;	       [ | ]
;	        | |
;               v v
;	       [ |/]
;               |
;               v
               'foo

(define x '(foo))
(define y (cons x x))
(define seven-count (cons y y))
;seven-count -> [ | ]
;	     	 | |
;		 v v
;		[ | ]
;                | |
;                v v
;               [ |/]
;		 |
;		 v
;		'foo

(define unlimited-count '(foo bar baz))
(set-cdr! (cddr unlimited-count) unlimited-count)
(count-pairs unlimited-count) ; Don't actually run this, it is an infinite loop.
;                     ___________________
;		     |                   |
;                    v                   |
;unlimited-count -> [ | ] -> [ | ] -> [ | ]
;		     |        |        |
;		     v	      v	       v
;		    'foo     'bar     'baz
```

## 3.17

*Devise a correct version of the `count-pairs` procedure of Exercise 3.16 that returns the number of distinct pairs in any structure. (Hint: Traverse the structure, maintaining an auxilliary data structure that is used to keep track of which pairs have already been counted.)*

```scheme
(define (count-pairs x)
  (let ((already-counted '()))
  (define (counter x)
    (if (or (not (pair? x)) (memq x already-counted))
	0
	(begin
	  (set! already-counted (cons x already-counted))
	  (+ (counter (car x))
	     (counter (cdr x))
	     1))))
    (counter x)))
```

## 3.21

*Ben Bitdiddle decides to test the queue implementation described above. He types in the procedures to the Lisp interpreter and proceeds to try them out:*

```scheme
(define q1 (make-queue))

(insert-queue! q1 'a)
((a) a)

(insert-queue! q1 'b)
((a b) b)

(delete-queue! q1)
((b) b)

(delete-queue! q1)
(() b)
```

*"It's all wrong!" he complains. "The interpreter's response shows that the last item is inserted into the queue twice. And when I delete both items, the second b is still there, so the queue isn't empty, even though it's supposed to be." Eva Lu Ator suggests that Ben has misunderstood what is happening. "It's not that the items are going into the queue twice," she explains. "It's just that the standard Lisp printer doesn't know how to make sense of the queue representation. If you want to see the queue printed correctly, you'll have to define your own print procedure for queues." Explain what Eva Lu is talking about. In particular, show why Ben's examples produce the printed results that they do. Define a procedure `print-queue` that takes a queue as input and prints the sequence of items in the queue.*

The car of the queue points to the start of the queue. This results in the list displayed inside the nested parentheses in the example above. The cdr of the queue points to the last pair in the queue. So, when the queue is displayed it shows a car that appears to be the entire queue, AND a cdr that appears to be the last item entered into the queue. The pointer in the cdr of q1 is not reset to an empty list when the queue is emptied.  
```scheme
(define (print-queue queue)
	(front-ptr queue))
```

## 3.25

*Generalizing one- and two-dimensional tables, show how to implement a table in which values are stored under an arbitrary number of keys and different values may be stored under different numbers of keys. The `lookup` and `insert!` procedures should take as input a list of keys used to access the table.*

```scheme
(define (lookup keys table)
    (if (null? (cdr keys))
        (let ((record (assoc (car keys) (cdr table))))
  	       (if record
             (cdr record)
             false))
        (let ((subtable (assoc (car keys) (cdr table))))
  	       (if subtable
  	         (lookup (cdr keys) subtable)
  	         false)))
  )

(define (insert! keys value table)
  (define (insert-loop! keys table)
    (if (pair? (cdr keys))
      (let ((subtable (assoc (car keys) (cdr table))))
        (if subtable
          (insert-loop! (cdr keys) subtable)
          (begin
            (set-cdr! table (cons (list (car keys)) (cdr table)))
            (insert-loop! keys table))))
      (let ((record (assoc (car keys) (cdr table))))
        (if record
          (set-cdr! record value)
          (set-cdr! table (cons (cons (car keys) value) (cdr table)))
          ))))

  (insert-loop! keys table)
  'ok)
```

## 3.27

*Memoization (also called tabulation) is a technique that enables a procedure to record, in a local table, values that have previously been computed. This technique can make a vast difference in the performance of a program. A memoized procedure maintains a table in which values of previous calls are stored using as keys the arguments that produced the values. When the memoized procedure is asked to compute a value, it first checks the table to see if the value is already there and, if so, just returns that value. Otherwise, it computes the new value in the ordinary way and stores this in the table. As an example of memoization, recall from 1.2.2 the exponential process for computing Fibonacci numbers:*  
```scheme
(define (fib n)
	(cond ((= n 0) 0)
		((= n 1) 1)
			(else (+ (fib (- n 1))
				(fib (- n 2))))))
```  
*The memoized version of the same procedure is*  
```scheme
(define memo-fib
	(memoize
		(lambda (n)
			(cond ((= n 0) 0)
				((= n 1) 1)
				(else
					(+ (memo-fib (- n 1))
						(memo-fib (- n 2))))))))
```
*where the memoizer is defined as*
```scheme
(define (memoize f)
	(let ((table (make-table)))
		(lambda(x)
			(let ((previously-computed-result
				(lookup x table)))
				(or previously-computed-result
					(let ((result (f x)))
						(insert! x result table)
						result))))))
```
*Draw an environment diagram to analyze the computation of (memo-fib 3). Explain why memo-fib computes the n th Fibonacci number in a number of steps proportional to n . Would the scheme still work if we had simply defined memo-fib to be (memoize fib)?*

**You don’t need to draw the environment diagram for exercise 3.27; use a trace to provide the requested explanations. Treat the table procedures lookup and insert! as primitive; i.e. don’t trace the procedures they call.  Also, assume that those procedures work in constant time. We’re interested to know about the number of times memo-fib is invoked.**

memo-fib computes each fibonacci number once, insert!s it into the table, and then looks it up from then on. This requires one call to lookup and one call to insert! for each integer up to n. After calulating the number the first time, further lookups of that number or any lower number are in constant time, as they only require one lookup.  
Yes.
