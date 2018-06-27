# 1

*Abelson & Sussman, exercises 3.50, 3.51, 3.52, 3.53, 3.54, 3.55, 3.56, 3.64, 3.66, 3.68*

## 3.50

*Complete the following definition, which generalizes `stream-map` to allow procedures that take multiple arguments, analogous to `map` in 2.2.1, Footnote 78.*

```scheme
(define (stream-map proc . argstreams)
  (if (<??> (car argstreams))
    the-empty-stream
    (<??>
      (apply proc (map <??> argstreams))
      (apply stream-map
        (cons proc
          (map <??>
            argstreams))))))
```

```scheme
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map (stream-car argstreams)))
      (apply stream-map
        (cons proc
          (map (stream-cdr
            argstreams)))))))
```

## 3.51

*In order to take a closer look at delayed evaluation, we will use the following procedure, which simply returns its argument after printing it:*

```scheme
(define (show x)
  (display-line x)
  x)
```

*What does the interpreter print in response to evaluating each expression in the following sequence?*

```scheme
(define x
  (stream-map
    show
    (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)
```

`0x`  
`1
2
3
4
55`
`6
77`

On subsequent evaluations of `(stream-ref x 5)` the output will simply be `5`.


## 3.52

*Consider the sequence of expressions*

```scheme
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq
  (stream-map
   accum
   (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z
  (stream-filter
    (lambda (x)
      (= (remainder x 5) 0)) seq))

(stream-ref y 7)
(display-stream z)
```

*What is the value of `sum` after each of the above expressions is evaluated? What is the printed response to evaluating the `stream-ref` and `display-stream` expressions? Would these reponses differ if we had implemented `(delay <exp>)` simply as `(lambda () <exp>)` without using the optimization provided by `memo-proc`? Explain.*

```scheme
0 ; sum is defined as 0
0 ; defining accum does not change the value of sum
1 ; when seq is defined accum is evaluated for the car, adding 1 to sum
6 ; seq generates 1, 3, 6... 6 is the first one that meets the filter test for even?. sum is set to 1, 3, and 6 during this time by the call to accum.
10 ; 10 is the first integer in seq that meets the filter in z.
136 ; stream-ref returns 136, which is equivalent to (stream-ref seq 15)
210 ; accum continues to be called with each successive viewing of the stream seq until it is finally set at the highest called value.
```

Without the memoization sum should be incremented repeatedly, as each time a higher stream-ref of seq is called all previous items would have to be viewed as well.

## 3.53

*Without running the program, describe the elements of the stream defined by `(define s (cons-stream 1 (add-streams s s)))`*

This should be a stream of powers of 2 - 1, 2, 4, 8...

## 3.54

*Define a procedure `mul-streams`, analagous to `add-streams`, that produces the elementwise product of its two input streams. Use this together with the stream of integers to complete the following definition of the stream whose n<sup>nt</sup> element (counting from 0) is n + 1 factorial:*

```scheme
(define factorials
  (cons-stream 1 (mul-streams <??> <??>)))
```

```scheme
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams integers factorials)))
```

## 3.55

*Define a procedure `partial-sums` that takes as argument a stream `S` and returns the stream whose elements are `S<sub>0</sub>,S<sub>0</sub> + S<sub>1</sub>,S<sub>0</sub> + S<sub>1</sub> + S<sub>2</sub>, ...`. For example, `(partial-sums integers)` should be the stream `1, 3, 6, 10, 15, ...`.*

```scheme
(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s))))
```

## 3.56

*A famous problem, first raised by R. Hamming, is to enumerate, in ascending order with no repetitions, all positive integers with no prime factors other than 2, 3, or 5. One obvious way to do this is to simply test each integer in turn to see whether it has any factors other than 2, 3, and 5. But this is very inefficient, since, as the integers get larger, fewer and fewer of them fit the requirement. As an alternative, let us call the required stream of numbers `S` and notice the following facts about it.  
- `S` begins with 1.  
- The elements of `(scale-stream S 2)` are also elements of `S`.
- The same is true for `(scale-stream S 3)` and `(scale-stream S 5)`.  
- These are all the elements of `S`.  
Now all we have to do is combine elements from these sources. For this we define a procedure `merge` that combines two ordered streams into one ordered result stream, eliminating repetitions:*

```scheme
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1
                          (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))
```

*Then the required stream may be constructed with `merge`, as follows: `(define S (cons-stream 1 (merge <??> <??>)))`. Fill in the missing expressions in the places marked <??> above.*

```scheme
(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))
```

## 3.64

*Write a procedure `stream-limit` that takes as arguments a stream and a number (the tolerance). It should examine the stream until it finds two successive elements that differ in absolute value by less than the tolerance, and return the second of the two elements. Using this, we could compute square roots up to a given tolerance by `(define (sqrt x tolerance) (stream-limit (sqrt-stream x) tolerance))`*

```scheme
(define (stream-limit stream tolerance)
  (define (compare-elements e)
    (if (> tolerance (abs (- (stream-ref stream e) (stream-ref stream (+ e 1)))))
      (stream-ref stream (+ e 1))
      (compare-elements (+ e 1))))
  (compare-elements 0))
```

## 3.66

*Examine the stream `(pairs integers integers)`. Can you make any general comments about the order in which pairs are placed into the stream? For example, approximately how many pairs precede the pair (1, 100)? (If you can make precise mathematical statements here, all the better. But feel free to give more qualitative answers if you find yourself getting bogged down.)*

- Discounting the initial pair (1, 1), there are 2<sup>n - 1</sup> pairs of the form (1, x) between the double (n, n) and the subsequent double; that is, there are 2<sup>4</sup> (16) pairs beginning with 1 between (5, 5) and (6, 6).

- Likewise there are 2<sup>n - 2</sup> pairs of the form (2, x) between the double (n, n) and the subsequent double; i.e., there are 2<sup>1</sup> (2) pairs beginning with 2 between (3, 3) and (4, 4).

- This means before each double (n, n) there are approximately 2<sup>n - 1</sup> + 2<sup>n - 2</sup> ... + 2<sup>1</sup> pairs. For (4, 4) this would mean 8 + 4 + 2 = 14 pairs preceding it. (5, 5) is the 31<sup>st</sup> (accessed by stream-ref 30) pair in the stream.

- This seems to follow the pattern of 2<sup>n</sup> - 2. So, (100, 100) would be pair number 2^100 - 1, that is stream-ref 2^100 - 2. `(stream-ref (pairs integers integers) (- (expt 2 100) 2))`

- The pair (n, n + 1) lies halfway between the pairs (n, n) and (n+1, n+1).

## 3.68

*Louis Reasoner thinks that building a stream of pairs from three parts is unnecessarily complicated. Instead of separating the pair (S<sub>0</sub>, T<sub>0</sub>) from the rest of the pairs in the first row, he proposes to work with the whole first row, as follows:*

```scheme
(define (pairs s t)
  (interleave
    (stream-map
      (lambda (x) (list (stream-car s) x)) t)
      (pairs (stream-cdr s) (stream-cdr t))))
```

*Does this work? Consider what happens if we evaluate (pairs integers integers) using Louis's definition of `pairs`.*

No, this does not work. This appears to have the opposite problem of that mentioned in the text, in that it will attempt to continue through all of the second stream before repeating the first.
