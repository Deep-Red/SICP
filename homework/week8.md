# 1

*Abelson & Sussman, exercises 3.3, 3.4, 3.7, 3.8, 3.10, 3.11*

## 3.3
*Modify the `make-account` procedure so that it creates password-protected accounts. That is, `make-account` should take a symbol as an additional argument, as in `(define acc (make-account 100 'secret-password))` The resulting account object should process a request only if it is accompanied by the password with which the account was created, and should otherwise return a complaint: `((acc 'secret-password 'withdraw) 40) 60` `((acc 'some-other-password 'deposit) 50) "Incorrect password"`*

```scheme
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance
		     (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request: MAKE-ACCOUNT" m)))
	(error "Incorrect password")))
    
  dispatch)
```

## 3.4
*Modify the `make-account` procedure of Exercise 3.3 by adding another local state variable so that, if an account is accessed more than seven consecutive times with an incorrect password, it invokes the procedure `call-the-cops`.*

```scheme
(define (make-account balance password)
  (define attempt-counter 0)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance
		     (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops)
    (error "Too many incorrect attempts, calling the cops."))
  (define (dispatch p m)
    (if (eq? p password)
	(and
	 (set! attempt-counter 0)
	 (cond ((eq? m 'withdraw) withdraw)
	       ((eq? m 'deposit) deposit)
	       (else (error "Unknown request: MAKE-ACCOUNT" m))))
	(if (> attempt-counter 5)
	    (call-the-cops)
	    (and
	     (set! attempt-counter (+ attempt-counter 1))
	     (error "Incorrect password")))))
    
  dispatch)
```

## 3.7
*Consider the bank account objects created by `make-account`, with the password modification described in Exercise 3.3. Suppose that our banking system requires the ability to make joint accounts. Define a procedure `make-joint` that accomplishes this. `Make-joint` should take three arguments. The first is a password-protected account. The second argument must match the password with which the account was defined in order for the `make-joint` operation to proceed. The third argument is a new password. `Make-joint` is to create an additional acces to the original account using the new password. For example, if `peter-acc` is a bank account with password `open-sesame`, then `(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))` will allow one to make transactions on `peter-acc` using the name `paul-acc` and the password `rosebud`. You may wish to modify your solution to Exercise 3.3 to accommodate this new feature.*

```scheme
(define (make-joint account oldpass newpass)
  (define (dispatch p m)
    (if (eq? p newpass)
	(account oldpass m)
	(error "Incorrect password")))
  dispatch)
```

## 3.8
*When we defined the evaluation model in 1.1.3, we said that the first step in evaluating an expression is to evaluate its subexpressions. But we never specified the order in which the subexpressions should be evaluated (e.g., left to right or right to left). When we introduce assignment, the order in which the arguments to a procedure are evaluated can make a difference to the result. Define a simple procedure `f` such that evaluating `(+ (f 0) (f 1))` will return 0 if the arguments to `+` are evaluated from left to right but will return 1 if the arguments are evaluated from right to left.*

```scheme
(define (make-f loc-var)
  (define (dispatch x)
    (set! loc-var (* x loc-var))
    loc-var)
  dispatch)
(define f (make-f 1))
```

## 3.10
*In the `make-withdraw` procedure, the local variable `balance` is created as a parameter of `make-withdraw`. We could also create the local state variable explicitly, using `let`, as follows:*
```scheme
(define (make-withdraw initial-amount)
	(let ((balance initial-amount))
	     (lambda (amount)
	     	     (if (>= balance amount)
		     	 (begin (set! balance
			 	      (- balance amount))
				balance)
			"Insufficient funds"))))
```
*Recall from 1.3.2 that `let` is simply syntactic sugar for a procedure call: `(let ((<var> <exp>)) <body>)` is interpreted as an alternate syntax for `((lambda (<var>) <body>) <exp>)` Use the environment model to analyze this alternate version of `make-withdraw`, drawing figures like the ones above to illustrate the interactions `(define W1 (make-withdraw 100)) (W1 50) (define W2 (make-withdraw 100))` Show that the two versions of `make-withdraw` create objects with the same behavior. How do the environment structures differ for the two versions?*

Drawing omitted.

## 3.11
*In 3.2.3 we saw how the environment model described the behavior of procedures with local state. Now we have seen how internal definitions work. A typical message-passing procedure contains both of these aspects. Consider the bank account procedure of 3.1.1:*
```scheme
(define (make-account balance)
	(define (withdraw amount)
		(if (>= balance amount)
		    (begin (set! balance
		    	   	 (- balance amount))
			   balance)
		    "Insufficient funds"))
	(define (deposit amount)
	  (set! balance (+ balance amount))
	  balance)
	(define (dispatch m)
	  (cond ((eq? m 'withdraw) withdraw)
		((eq? m 'deposit) deposit)
		(else (error "Unknown request: MAKE-ACCOUNT" m))))
	dispatch)
```

	