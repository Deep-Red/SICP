(define (squares args)
  (if (pair? (cdr args))
      (append (list (square (car args))) (squares (cdr args)))
      (list (square (car args)))))

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
      
