(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

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
  (cond ((> a b) null-value)
	((filter a) (combiner (term a)
			    (filtered-accumulate combiner null-value term a next b filter)))
	((else) (combiner (null-value)
			  (filtered-accumulate combiner null-value term a next b filter)))))      

(define (prod-accumulate term a next b)
  (accumulate * 1 term a next b))

(define (sum-accumulate term a next b)
  (accumulate + 0 term a next b))

(define (wallis terms)
  (define (inc x) (+ x 1))
  (define (num k)
    (if (= k 1)
	2
	(+ 2 (* 2 (quotient k 2)))))
  (define (den k)
    (if (= k 1)
	3
	(+ 3 (* 2 (quotient (- k 1) 2)))))
  (define numerator (product-iter num 1 inc terms))
  (define denominator (product-iter den 1 inc terms))
  (/ numerator denominator))

(define (factorial x)
  (define (inc n) (+ n 1))
  (define (identity n) (* n 1))
  (product identity 1 inc x))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1)
	     ((* (+ 1 (modulo k 2)) 2)))
       (y k)))
  (* (/ h 3)
     (sum term 0 inc n)
     ))     

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (/ (+ (f guess) guess) 2)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (cont-frac n d k)
  (define (fraction i)
    (if (= i k)
	(/ (n i) (d i))
	(/ (n i) (+ (d i) (fraction (+ i 1))))))
  (fraction 1)
  )
  
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (> i k)
	(/ n result)
	(iter (+ 1 i) (+ d (/ n result)))))
  (iter 0 1))

(define (euler-d x)
  (if (= 0 (modulo (+ x 1) 3))
      (* (/ (+ x 1) 3) 2)
      1))

(define (euler-e x)
  (+ 2 (cont-frac (lambda (i) 1.0) euler-d x)))

(define (print-ed x)
  (display (euler-d x))
  (if (> x 1)
      (print-ed (- x 1))))

(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
	x
	(- (* x x))))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac n d k))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
  
(define (double proc)
  (lambda (x) (proc (proc x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (- n 1)))
      f))

(define (smooth f dx)
  (lambda (x)
    (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3))
  )


(define pi 3.14159265359)
