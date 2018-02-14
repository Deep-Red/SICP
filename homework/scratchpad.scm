(define (squares args)
  (if (pair? (cdr args))
      (cons* (square (car args)) (squares (cdr args)))
      (list (square (car args)))))

(define (switch args)
  (if (pair? (cdr args))
      (cons* (switch (car args)) (switch (cdr args)))
      (list (item 1 args))))
