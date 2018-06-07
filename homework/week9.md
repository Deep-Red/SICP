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

