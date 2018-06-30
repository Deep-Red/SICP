# 1

*Abelson & Sussman, exercises 4.3, 4.6, 4.7-, 4.10-, 4.11-, 4.13, 4.14, 4.15
(exercises with a - next to them are optional, but encouraged).*

## 4.3

*Rewrite `eval` so that the dispatch is done in data-directed style. Compare this with the data-directed differentiation procedure of Exercise 2.73. (You may use the `car` of a compound expression as the type of the expression, as is appropriate for the syntax implemented in this section.)*

Using the table manipulation methods defined earlier:
```scheme
(define eval-dd-table (make-table))

(define (populate-eval-dd-table)
  (insert! '(quote) text-of-quotation eval-dd-table)
  (insert! '(set!) eval-assignment eval-dd-table)
  (insert! '(define) eval-definition eval-dd-table)
  (insert! '(if) eval-if eval-dd-table)
  (insert! '(lambda) (lambda (exp env) make-procedure (lambda-parameters exp) (lambda-body exp) env) eval-dd-table)
  (insert! '(begin) (lambda (exp env) (eval-sequence (begin actions exp) env)) eval-dd-table)
  (insert! '(cond) (lambda (exp env) (eval-dd (cond->if exp) env)) eval-dd-table))

(define (eval-dd exp env)
  (cond ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((lookup (list (car exp)) eval-dd-table) ((lookup (list (car exp)) eval-dd-table) exp env))
    ((application? exp) (apply (eval (operator exp) env)))
    (else (error "Unknown expression type -- EVAL" exp))))
```

## 4.6

*Let expressions are derived expressions, because `(let ((<var<sub>1</sub>> <exp<sub>1</sub>>) ... (<var<sub>n</sub>> <exp<sub>n</sub>)) <body>)` is equivalent to `((lambda (<var<sub>1</sub>> ... <var<sub>n</sub>) <body>) <exp<sub>1</sub>> ... <exp<sub>n</sub>)`. Implement a syntactic transformation let->combination that reduces evaluating `let` expressions to evaluating combinations of the type shown above, and add the appropriate clause to `eval` to handle `let` expressions.*

```scheme
(define (let? exp)
  (tagged-list? exp 'let))
(define (let-body exp) (cddr exp))
(define (let-vars exp) (map car (cadr exp)))
(define (let-values exp) (map cadr (cadr exp)))
(define (let->combination exp)
  (cons (make-lambda (let-vars exp) (let-body exp)) (let-values exp)))

;...
((let? exp) (eval (let->combination exp) env))
;...
```

## 4.7

*`Let*` is similar to `let`, except that the bindings of the `let*` variables are performed sequentially from left to right, and each binding is made in an environment in which all of the preceding bindings are visible. For example `(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))` returns 39. Explain how a `let*` expression can be rewritten as a set of nested `let` expressions, and write a procedure `let*->nested-lets` that performs this transformation. If we have already implemented `let` (Exercise 4.6) and we want to extend the evaluator to handle `let*`, is it sufficient to add a clause to eval whose action is `(eval (let*->nested-lets exp) env)` or must we explicitly expand `let*` in terms of non-derived expressions?*

```scheme
(define (let*->nested-lets exp)
  (let ((args (let*-args exp) (body (let*-body exp)))
  (define (make-lets exps)
    (if (null? exps?)
      body
      (list 'let (list (car exprs)) (make-lets (cdr exps)))))
  (make-lets args))))
```

I don't think that `(eval (let*-> nested-lets exp) env)` will work on its own. It would construct the correct expression, but that expression would not be passed to the evaluator once it had been made...

## 4.11

*Instead of representing a frame as a pair of lists, we can represent a frame as a list of bindings, where each binding is a name-value pair. Rewrite the environment operations to use this alternative representation.*

```scheme
(define (make-frame variables values)
  (if (null? (cdr variables))
    (list (cons (car variables) (car values)))
    (cons (cons (car variables) (car values)) (make-frame (cdr variables) (cdr values)))))
(define (frame-variables frame)
  (if (null? (cdr frame))
    (caar frame)
    (cons (caar frame) (frame-variables (cdr frame)))))
(define (frame-values frame)
  (if (null? (cdr frame))
    (cdar frame)
    (cons (cdar frame) (frame-values (cdr frame)))))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))
```
