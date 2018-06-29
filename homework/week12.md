# 1

*Abelson & Sussman, exercises 4.3, 4.6, 4.7, 4.10, 4.11, 4.13, 4.14, 4.15*

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
  (insert! '(cond) (lambda (exp env) (eval-dd (cond->if exp) env)) eval-dd-table)
  )

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

*`Let&ast;` is similar to `let`, except that the bindings of the `let&ast;` variables are performed sequentially from left to right, and each binding is made in an environment in which all of the preceding bindings are visible. For example `(let&ast; ((x 3) (y (+ x 2)) (z (+ x y 5))) (&ast; x z))` returns 39. Explain how a `let&ast;` expression can be rewritten as a set of nested `let` expressions, and write a procedure `let&ast;->nested-lets` that performs this transformation. If we have already implemented `let` (Exercise 4.6) and we want to extend the evaluator to handle `let&ast;`, is it sufficient to add a clause to eval whose action is `(eval (let&ast;->nested-lets exp) env)` or must we explicitly expand `let&ast;` in terms of non-derived expressions?*

```scheme

```
