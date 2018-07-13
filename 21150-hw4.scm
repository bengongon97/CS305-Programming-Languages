(define in? (lambda (el lst)

(cond

((not (list? lst)) 
(error "Your argument is not a list"))

((null? lst) #f)

((eq? (car lst) el) #t)

(else (in? el (cdr lst) ))
)))


(define set? (lambda (lst) 

(cond

((not (list? lst))
(error "Your argument is not a list!"))

((null? lst) #t)
((null? (cdr lst)) #t)

((in? (car lst)(cdr lst)) #f)

(else (set? (cdr  lst)))
)))



(define order (lambda (st)

(cond
((not (set? st))
(error "Your argument is not a set!"))
)

(if (null? st)
 0
(+ 1 (order (cdr st))))))

(define subset? (lambda (st1 st2)

    (cond
		((not (set? st1))
		(error "Your first argument is not a set!"))
		((not (set? st2))
		(error "Your second argument is not a set!"))
		((null? st1) #t)
		( (in? (car st1) st2) (subset? (cdr st1) st2) )
		( (not (in? (car st1) st2)) #f)
		)))


(define intersect (lambda (st1 st2)

        (cond
		((not (set? st1))
		(error "Your first argument is not a set!"))
		((not (set? st2))
		(error "Your second argument is not a set!")))
		
		(if (null? st1)
         '()
			(if (in? (car st1) st2)
				( cons (car st1) (intersect (cdr st1) st2) )
				(intersect (cdr st1) st2)
				)
				)
    ))

(define sub (lambda (st1 st2) 

(if (null? st1)
         st2
			(if (not (in? (car st1) st2))
      			(sub (cdr st1) st2)
                (sub (cdr st1) (delete (car st1) st2))				
			)
			)))
				

(define makeset (lambda (lst)

(cond

		((not (list? lst)) (error "Your argument is not a list!"))
		((null? lst) '())
        ((in? (car lst) (cdr lst)) (makeset (cdr lst)))
        (else (cons (car lst) (makeset (cdr lst))))))) ;well cant say that internet did not help.
		

(define merge (lambda (lst1 lst2) 

(cond

		((not (list? lst1)) (error "Your argument is not a list!"))
		((not (list? lst2)) (error "Your argument is not a list!"))

)
	(append lst1 lst2) 

))

(define union (lambda (st1 st2) 

(cond
		((not (set? st1))
		(error "Your first argument is not a set!"))
		((not (set? st2))
		(error "Your second argument is not a set!"))
		
	
		)
	(makeset (merge st1 st2))
))



(define insert (lambda (el lst) 

(cond

		((not (list? lst)) (error "Your outer \"list\" is not a list!"))
		;((not (list? lst2)) (error "Your argument is not a list!"))

)

(if (null? lst)
         '()
	(if (not (list? (car lst)))
		(error "Your list is not comprised of lists")
		(cons (cons el (car lst)) (insert el (cdr lst))))
		)

))


; (define power (lambda (st) 

; (if (null? st)
      ; (list '())
      ; (let ((rest (power (cdr st))))
        ; (append rest (map (lambda (x)
                            ; (cons (car st) x))
                          ; rest))))))



(define power (lambda (st) 

(cond
((not (set? st))
(error "Your argument is not a set!"))
( (null? st) '() )

)

      (union (power (cdr st)) (insert (car st) (power (cdr st)))	  )
	  ))






















