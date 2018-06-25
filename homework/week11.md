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
