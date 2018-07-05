# 1

*Abelson & Sussman, 4.25, 4.26, 4.28, 4.30-, 4.32-, 4.33-, 4.36-, 4.42, 4.45, 4.47-, 4.48-, 4.49, 4.50, 4.52  
(exercises with a - by them are less crucial, but encouraged)*

## 4.25

*Suppose that (in ordinary applicative-order Scheme) we define `unless` as shown above and then define `factorial` in terms of `unless` as*

```scheme
(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))
```

*What happens if we attempt to evaluate `(factorial 5)`? Will our definitions work in a normal-order language?*

An infinite process will be invoked when using applicative order, as `(factorial (- n 1))` will be evaluated for integers beyond the limit of 1, going negative until the system's resources are exhausted. This will work in normal order evaluation though, because the recursive call will only happen when the condition `(= n 1)` has not been met.
