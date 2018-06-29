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
