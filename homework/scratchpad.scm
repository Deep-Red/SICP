(define (squares args)
  (if (pair? (cdr args))
      (append (list (square (car args))) (squares (cdr args)))
      (list (square (car args)))))

(define (switch sent)
  (if (member? (first sent) '(I me))
      (sentence sent '(you))
