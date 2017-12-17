(define a 3)
(define b (+ a 1))
(define (onethree x y z)
  (+
   (if (or (> x y) (> x z))
       (* x x)
       (* y y))
    
   (if (or (> z x) (> z y))
       (* z z)
       (* y y))))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))
(define (cbrt-iter guess x)
  (if (cb-good-enough? guess x)
      guess
      (cbrt-iter (cb-improve guess x)
		 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (cb-improve guess x)
  (/
   (+
    (/ x
       (* guess guess))
    (* 2 guess))
   3))

(define (average x guess)
  (/ (+ x guess) 2))

(define (good-enough? guess x)
  (< ( abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (cbrt x)
  (cbrt-iter 1.0 x))

(define (new-good-enough? guess x)
  (< ( abs (- guess (improve guess x))) 0.001))
(define (cb-good-enough? guess x)
  (< ( abs (- guess (cb-improve guess x))) 0.001))
  
	
