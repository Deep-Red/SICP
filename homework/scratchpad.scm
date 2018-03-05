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
