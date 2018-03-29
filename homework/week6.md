# 1

*Abelson & Sussman, exercises 2.74, 2.75, 2.76, 2.77, 2.79, 2.80, 2.81, 2.83*

## 2.74
*Insatiable Enterprises, Inc., is a highly decentralized conglomerate company consisting of a large number of independent divisions located all over the world. The company's computer facilities have just been interconnected by means of a clever network-interfacing scheme that makes the entire network appear to any user to be a single computer. Insatiable's president, in her first attempt to exploit the ability of the network to extract administrative information from division files, is dismayed to discover that, although all the division files have been implemented as data structures in Scheme, the particular data structure used varies from division to division. A meeting of division managers is hastily called to search for a strategy to integrate the files that will satisfy headquarters' needs while preserving the existing autonomy of the divisions.*

*Show how such a strategy can be implemented with data-directed programming. As an example, suppose that each division's personnel records consist of a single file, which contains a set of records keyed on employees' names. The structure of the set varies from division to division. Furthermore, each employee's record is itself a set (structured differently from division to divistion) that contains information keyed under identifiers such as `address` and `salary`. In particular:*

*a. Implement for headquarters a `get-record` procedure that retrieves a specified employee's record from a specified personnel file. The procedure should be applicable to any division's file. Explain how the individual divisions' files should be structured. In particular, what type of information must be supplied?*

*b. Implement for headquarters a `get-salary` procedure that returns the salary information from a given employee's record from any division's personnel file. How should the record be structured in order to make this operation work.*

*c. Implement for headquarters a `find-employee-record` procedure. This should search all the divisions' files for the record of a given employee and return the record. Assume that this procedure takes as arguments an employee's name and a list of all the division's files.*

*d. When Insatiable takes over a new company, what changes must be made in order to incorporate the new personnel information into the central system?*

a.
```scheme
(define (get-record division employee)
	(get division employee))
```
As long as each division tags their records with a division identifier, and each employee's record is keyed to the employee's name this will return the employee's record regardless of the remaining structure of their data. In other words each record must be tagged with a division identifier and the employee's name.

b.
```scheme
(define (get-salary division employee)
	(let (employee) (get division employee))
	(get employee salary))
```
Once again, the actual structure of the data can remain untouched as long as each entry in the record is tagged - keyed with consistent identifiers, in this case 'salary'.

c.
```scheme
(define (find-employee-record (employee-name divisions))
	(if (get-record (car divisions) employee-name))
	(get-record (car divisions) employee-name)
	(find-employee-record (employee-name (cdr divisions))))
```

d.
Each division's personell file must be keyed with it's new division identifier, each employee's record must be keyed to their employee identifier (their name, in this case) and it must be verified that each piece of information in an employee's record is tagged with consistent identifiers like 'salary' and 'address'.

## 2.75
```scheme
(define (make-from-mag-ang r a)
	(define (dispatch op)
		(cond ((eq? op 'real-part) (* r (cos a)))
		      ((eq? op 'imag-part) (* r (sin a)))
		      ((eq? op 'magnitude) r)
		      ((eq? op 'angle) a)
		      (else
			(error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
	dispatch)
```

## 2.76

*As a large system with generic operations evolves, new types of data objects or new operations may be needed. For each of the three strategies -- generic operations with explicit dispatch, data-directed style, and message-passing-style --describe the changes that must be made to a system in order to add new types or new operations. Which organization would be most appropriate for a system in which new types must often be added? Which would be most appropriate for a system in which new operations must often be added?*

To add new types to a message-passing system, only the new type must be defined, in which each of it's own operations will be defined. To add a new operation, each type to which the operation applies will have to be modified.

To add new types to a data-directed system, new operations for that type must be defined in the table. To add a new operation, the new operation must be added to the table, along with the corresponding entry for that operation in each relevant type's column.

A direct dispatch style system requires that every type be aware of each of its operations and vice-versa, so both must be modified anytime a new instance of either is implemented.

If new types will be added often, it is probably best to implement a message-passing system.
If new operations will be added often, it is probably better to implement a data-directed system.

## 2.77

*Louis Reasoner tries to evaluate the expression `(magnitude z)` where `z` is the object shown in figure 2.24. To his surprise, instead of the answer 5 he gets an error message from `apply-generic`, saying there is no method for the operation `magnitude` on the types `(complex)`. He shows this interaction to Alyssa P. Hacker, who says "The problem is that the complex-number selectors were never defined for `complex` numbers, just for `polar` and `rectangular` numbers. All you have to do to make this work is add the following to the `complex` package:"*
```scheme
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)
```
*Describe in detail why this works. As an example, trace through all the procedures called in evaluating the expression `(magnitude z)` where `z` is the object shown in figure 2.24. In particular, how many times is `apply-generic` invoked? What procedure is dispatched to in each case?*

`apply-generic` is called twice. When `(magnitude (complex rectangular 7 . 6))` is called, apply-generic gets called, and looks up 'magnitude under 'complex. Then apply is called again to look up 'magnitude under 'rectangular. Essentially, if I am understanding this correctly, we are inserting a redirect whereby when `'complex 'rectangular x . y` is sought in the table, it strips the first term and looks up `'rectangular x . y`.

## 2.79

*Define a generic equality predicate `equ?` that tests the equality of two numbers, and install it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers.*

`(define (equ? x y) (apply-generic 'equ? x y))`

And the following are added to the appropriate sections of their respective installers (as can be seen in the separate scratchwork file):

```scheme
(put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (eq? x y)))

(define (equ?-rat x y)
    (and (eq? (numer x) (numer y))
	 (eq? (denom x) (denom y))))
(put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))

(define (equ?-complex z1 z2)
    (and (eq? (real-part z1) (real-part z2))
	 (eq? (imag-part z1) (imag-part z2))))
(put 'equ? '(complex complex)
       (lambda (z1 z2) (equ?-complex z1 z2)))
```

**Note: this is only designed to work on reduced rational numbers. As long as the rational number was made with the make-rat function this won't be a problem, but if one is manually created in non-reduced form it will not compare correctly unless some procedure to reduce it to lowest terms is also introduced.**

## 2.80

*Define a generic predicate `=zero?` that tests if its argument is zero, and install it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers.*

`(define (=zero? x) (apply-generic '=zero? x))`

Same as last time, the following would be put into the appropriate installation packages.

```scheme
(put '=zero? 'scheme-number
     	     (lambda (x) (eq? 0 x)))

(define (=zero?-rat x)
    	(eq? 0 x))
(put '=zero? 'rational
     	     (lambda (x) (=zero?-rat x)))

(define (=zero?-complex x)
	(eq? 0 x))
(put '=zero? 'complex
     (lambda (x) (=zero?-complex x)))
```

## 2.81

*Louis Reasoner has noticed that `apply-generic` may try to coerce the arguments to each other's type even if they already have the same type. Therefore, he reasons, we need to put procedures in the coercion table to "coerce" arguments of each type to their own type. For example, in addition to the `scheme-number->complex` coersion shown above, he would do:*
```scheme
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
	      scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)
```

*a. With Louis's coercion procedures installed, what happens if `apply-generic` is called with two arguments of type `complex` for an operation that is not found in the table for those types? For example, assume that we've defined a generic exponentiation operation: `(define (exp x y) (apply-generic 'exp x y))` and have put a procedure for exponentiation in the Scheme-number package but not in any other package:*
```scheme
;; following added to Scheme-number package
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y)))) ; using primitive expt
```
*What happens if we call `exp` with two complex numbers as arguments?*

*b. Is Louis correct that something had to be done about coercion with arguments of the same type, or does `apply-generic` work correctly as is?*

*c. Modify `apply-generic` so that it doesn't try coercion if the two arguments have the same type.*

a. The program will get caught in a loop, as there won't be a proc, `(length args)` will be 2, and `t1->t2` will succeed due to the newly added entry, calling apply generic again with the same args, restarting the process.

b. Yes, he is right, the same loop will occur if there is no proc for the argument combination.

c. Modify the second `if` clause in `apply-generic` as follows: `(if (or (= (length args) 2) (= (car type-tags) (cdr type-tags))))`.

## 2.83

*Suppose you are designing a generic arithmetic system for dealing with the tower of types shown in figure 2.25: integer, rational, real, complex. For each type (except complex), design a procedure that raises objects of that type one level in the tower. Show how to install a generic `raise` operation that will work for each type (except complex).*