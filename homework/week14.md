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

## 4.26

*Ben Bitdiddle and Alyssa P. Hacker disagree over the importance of lazy evaluation for implementing things such as `unless`. Ben points out that it's possible to implement `unless` in applicative order as a special form. Alyssa counters that, if one did that, `unless` would be merely syntax, not a procedure that could be used in conjunction with higher-order procedures. Fill in the details on both sides of the argument. Show how to implement `unless` as a derived expression (like `cond` or `let`), and give an example of a situation where it might be useful to have `unless` available as a procedure, rather than as a special form.*

```scheme
((unless? exp) (eval (unless->if exp) env))

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-predicate exp) (cadr exp))
(define (unless-consequence exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))
(define (unless-alternative exp) (caddr exp))
(define (unless->if exp)
  (make-if (unless-predicate exp) (unless-consequence exp) (unless-alternative exp)))
```

## 4.28

*`Eval` uses `actual-value` rather than `eval` to evaluate the operator before passing it to `apply`, in order to force the value of the operator. Give an example that demonstrates the need for this forcing.*

This will be a problem when there are nested functions on which apply will be called.

```scheme
(define g x (* x x))
(define f g x (g x))
```
In this case, if `(g x)` is not forced, `(f g x)` cannot be evaluated.
