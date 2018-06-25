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
66 ; 10 is the first integer in seq that meets the filter in z. so, 1, 3, 6, and 10 are added to sum. ...I'm unsure why this is 66 rather than 26...
136 ; stream-ref returns 136,
210 ; okay so it is showing the highest calculated value so far, but I'm not following the reason, that is, I get that the memoization is keeping the value of sum from continuing to grow with each successive call, but I don't understand why it reflects the current highest call rather than the sum of each 'first call' up to this point.
```
