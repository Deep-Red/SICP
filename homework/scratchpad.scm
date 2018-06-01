(define (squares args)
  (if (pair? (cdr args))
      (append (list (square (car args))) (squares (cdr args)))
      (list (square (car args)))))

(define (every proc sent)
  (if (pair? (cdr sent))
      (append (list (proc (car sent))) (every proc (cdr sent)))
      (list (proc (car sent)))))

(define (switch sent)
  (if (empty? (bf sent))
      (substitute-first (first sent))
      (sentence (switch (butlast sent)) (substitute (last sent)))
      ))

(define (substitute wd)
  (cond ((member? wd '(I me)) 'you)
	((eq? wd 'you) 'me)
	(else wd)))

(define (substitute-first wd)
  (cond ((member? wd '(I me)) 'you)
	((eq? wd 'you) 'i)
	(else wd)))
      	   
(define (ordered? sent)
  (cond ((empty? (bf sent)) #t)
	((<= (first sent) (first (bf sent))) (ordered? (bf sent)))
	(else #f)))
  
(define (onewordsentence? sent)
  (eq? (first sent) sent))

(define (ends-e sent)
  (if (empty? (bf sent))
      (return-e (first sent))
      (sentence (return-e (first sent)) (ends-e (bf sent)))))
      
(define (return-e wd)
  (if (eq? (last wd) 'e) wd '()))

(define (or-special? x)
  (or (display (> x 4)) (display (< x 4)) (display (= x 4)))
  (or (> x 4) (< x 4) (= x 4)))

(define (and-special? x y)
  (and (display (> x y)) (display (< x y)) (display (= x y)))
  (and (> x y) (< x y) (= x y)))

(define (or-special?)
  (display '(or is))
  (or #t (display 'NOT))
  (display '(a special function))) 

(define (and-special?)
  (and (display 'and) (display 'is) #f (display 'NOT))
  (display 'a ) (display 'special) (display 'form))
      
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter a)
	  (combiner (term a)
		    (filtered-accumulate combiner null-value term (next a) next b filter))
	  (combiner null-value
		    (filtered-accumulate combiner null-value term (next a) next b filter)))))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))


(define (inc n) (+ n 1))

(define (factorial x)
  (product identity 1 inc x))

(define (wallis-num x)
  (+ 2 (* 2 (floor (/ x 2)))))

(define (wallis-den x)
  (+ 3 (* 2 (floor (/ (- x 1) 2)))))

(define (wallis-pi x)
  (* 4
     (/ (product wallis-num 1 inc x)
	(product wallis-den 1 inc x))))

(define (double g)
  (lambda (x) (g (g x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated g n)
  (if (> n 1)
      (compose g (repeated g (- n 1)))
      g))

(define (average x guess)
  (/ (+ x guess) 2))

(define (iterative-improve good-enough? improve)
  (define (i-improve guess)
    (if (good-enough? guess)
	guess
	(i-improve (improve guess))))
  i-improve)

(define (sqrt x)
  ((iterative-improve (lambda (guess)
			(< (abs (- (square guess) x))
			   0.001))
		      (lambda (guess)
			(average guess (/ x guess))))
   1.0))

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess)
			(< (abs (- (f guess) guess))
			   0.00001))
		      (lambda (guess)
			(f guess)))
   first-guess))
      
(((lambda (n) (n n))
 (lambda (factgen)
   (lambda (n)
     (if (> 1 n)
	 1
	 (* n ((factgen factgen) (- n 1)))))))
 5)

(define (expt-iter b n a)
  (cond ((= n 0) a)
	((even? n) (expt-iter (square b) (/ n 2) a))
	(else (expt-iter b (- n 1) (* a b)))))

(define (fast-expt b n)
  (expt-iter b n 1))


(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (golden-ratio)
  (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))

(define (cont-frac n d k)
  (if (= k 1)
      (/ (n k) (d k))
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i k)
	result
	(iter (+ i 1) (/ (n i) (+ (d i) result)))))
  (iter 0 1))

(define (euler-d i)
  (if (> (modulo (+ i 1) 3) 0)
      1
      (* 2 (/ (+ i 1) 3))))

(define (euler-e k)
  (+ 2 (cont-frac (lambda (i) 1.0) euler-d k)))


(define (next-perf n)
  (if (= (sum-of-factors (+ n 1)) (+ n 1))
      (+ n 1)
      (next-perf (+ n 1))))

(define (sum-of-factors n)
  (define (add-factors x total)
    (cond
     ((> x (/ n 2)) total)
     ((= (modulo n x) 0) (add-factors (+ x 1) (+ total x)))
     (else (add-factors (+ x 1) total))))
  (add-factors 2 1))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
		 (- counter 1)
		 (* b product))))

(define (number-of-partitions x)
  (partition x x))

(define (partition x y)
  (cond ((= x 0) 1)
	((or (< x 0) (= y 0)) 0)
	(else (+ (partition x (- y 1))
		 (partition (- x y) y)))))

  
(define (number-of-partitions-iter x)
  (partitions-iter x x 0))

(define (partitions-iter amount part result)
  (cond ((= amount 0) (+ result 1))
	((< amount 0) result)
	((= part 0) result)
	(else (partitions-iter amount (- part 1) (+ result 1)))))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
      
(define (build-parity-list result i l)
  (cond ((empty? l) l)
	((even? (- i (car l))) (cons (car l) (build-parity-list result i (cdr l))))
	(else (build-parity-list result i (cdr l)))))

(define (same-parity i . l)
  (build-parity-list i i l))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items nil))

(define (for-each procedure operands)
  (if (empty? operands)
      #t
      (and (procedure (car operands)) (for-each procedure (cdr operands)))))
  
(define (substitute list old new)
  (define (build-sub-list result list old new)
    (cond ((empty? list) list)
	  ((list? (car list)) (cons (build-sub-list result (car list) old new) (build-sub-list result (cdr list) old new)))
	  ((equal? (car list) old) (cons new (build-sub-list result (cdr list) old new)))
	  (else (cons (car list) (build-sub-list result (cdr list) old new)))))
  (build-sub-list () list old new))
	  
(define (substitute2 list oldl newl)
  (define (build-sub2-list list old new)
    (substitute list old new))
  (substitute list (car oldl) (car newl))
  (if (null? (cdr oldl))
      (substitute list (car oldl) (car newl))
      (substitute2 (substitute list (car oldl) (car newl)) (cdr oldl) (cdr newl))))
    
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define mobile-1 (make-mobile
		  (make-branch 2 (make-mobile
				  (make-branch 1 4)
				  (make-branch 2 3)))
		  (make-branch 3 (make-mobile
				  (make-branch 1 (make-mobile
						  (make-branch 1 2)
						  (make-branch 2 1)))
				  (make-branch 1 0)))))

(define (total-weight mobile)
  (+
   (if (list? (branch-structure (left-branch mobile)))
       (total-weight (branch-structure (left-branch mobile)))
       (branch-structure (left-branch mobile)))
   (if (list? (branch-structure (right-branch mobile))))
       (total-weight (branch-structure (right-branch mobile)))
       (branch-structure (right-branch mobile)))))

(define (balanced? mobile)
  (and
   (if (list? (branch-structure (left-branch mobile)))
       (balanced? (branch-structure (left-branch mobile)))
       true)
   (if (list? (branch-structure (right-branch mobile)))
       (balanced? (branch-structure (right-branch mobile)))
       true)
   (=
    (*
     (branch-length (left-branch mobile))
     (branch-structure (left-branch mobile)))
    (*
     (branch-length (right-branch mobile))
     (branch-structure (right-branch mobile))))))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-tree tree)
    (cons
      (if (pair? (car tree))
	  (square-tree (car tree))
	  (square (car tree)))
      (cond ((null? (cdr tree)) nil)
	    ((pair? (cdr tree)) (square-tree (cdr tree)))
	    (square (cdr tree)))))

(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (square sub-tree)))
       tree))

(define (tree-map f tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map f sub-tree)
	     (f sub-tree)))
       tree))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda(row) (dot-product row v)) m))
(define (transpose mat)
  (accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda(row) (matrix-*-vector cols row)) m)))

(define a (list (list 1 2 -1) (list 2 0 1)))
(define b (list (list 3 1) (list 0 -1) (list -2 3)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define (equal? x y)
  (cond ((and (null? x) (null? y)) #t)
	((or (null? x) (null? y)) #f)
	((and (list? x) (list? y))
	 (and (equal? (car x) (car y))
	      (equal? (cdr x) (cdr y))))
	((or (list? x) (list? y)) #f)
	(else (eq? x y))))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface with the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (real-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types -- APPLY-GENERIC"
	   (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (apply-generic op arg) (arg op))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (eq? x y)))
  (put '=zero? 'scheme-number
       (lambda (x) (eq? 0 x)))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (error
	   "No method for these types -- APPLY-GENERIC"
	   (list op type-tags))))))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
		 (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))
  (define (equ?-rat x y)
    (and (eq? (numer x) (numer y))
	 (eq? (denom x) (denom y))))
  (define (=zero?-rat x)
    (eq? 0 (numer x)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))
  (put '=zero? 'rational
       (lambda (x) (=zero?-rat x)))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))
  (define (equ?-complex z1 z2)
    (and (eq? (real-part z1) (real-part z2))
	 (eq? (imag-part z1) (imag-part z2))))
  (define (=zero?-complex x)
    (eq? 0 x))
  ;; interface to rest of system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ?-complex z1 z2)))
  (put '=zero? 'complex
       (lambda (x) (=zero?-complex x)))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (raise x) (apply-generic 'raise x))

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

(define (make-joint account oldpass newpass)
  (define (dispatch p m)
    (if (eq? p newpass)
	(account oldpass m)
	(error "Incorrect password")))
  dispatch)

(define (make-f)
  (define loc-var 1)
  (define (dispatch x)
    (set! loc-var (* x loc-var))
    (loc-var))
  dispatch)
(define f (make-f))
