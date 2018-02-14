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
      
	    


  
(define (onewordsentence? sent)
  (eq? (first sent) sent))
