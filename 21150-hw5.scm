(define get-operator (lambda (op-symbol env) 
 
 (cond 
 ((equal? op-symbol '+) +) 
 ((equal? op-symbol '-) -) 
 ((equal? op-symbol '*) *) 
 ((equal? op-symbol '/) /) 
 (else (get-value op-symbol env))
 )))
 
(define if-stmt? (lambda (e)
  (and (list? e) (equal? (car e) 'if) (= (length e) 4))))

(define letstar-stmt? (lambda (e)
  (and (list? e) (equal? (car e) 'let*) (= (length e) 3))))
  
(define let-stmt? (lambda (e)
  (and (list? e) (equal? (car e) 'let) (= (length e) 3))))

(define define-stmt? (lambda (e)
  (and (list? e) (equal? (car e) 'define) (symbol? (cadr e)) (= (length e) 3))))

(define formal-list? (lambda (e)
  (and (list? e) (symbol? (car e)) (or (null? (cdr e)) (formal-list? (cdr e))))))
  
(define lambda-short-stmt? (lambda (e)
  (and (list? e) (equal? (car e) 'lambda) (formal-list? (cadr e)) (not (define-stmt? (caddr e))))))
  
(define is-normal? (lambda (o)
  (cond 
   ((eq? o '+) #t)
   ((eq? o '-) #t)
   ((eq? o '/) #t)
   ((eq? o '*) #t)
   (else #f)
  )))

 
(define get-value (lambda (var env)
	(cond 
	((null? env) (error "s8-hw5: unbound variable -->" var)) 
	((equal? (caar env) var) (cdar env)) 
	(else (get-value var (cdr env))))))
	
(define extend-env (lambda (var val old-env) 
(cons (cons var val) old-env)))

(define repl (lambda (env) 
(let* ( (dummy1 (display "cs305> ")) 
(expr (read)) 
(new-env (if (define-stmt? expr) 
(extend-env (cadr expr) (s8-hw5 (caddr expr) env) env) env)) 
(val (if (define-stmt? expr) 
(cadr expr) 
(s8-hw5 expr env))) 
(dummy2 (display "cs305: ")) 
(dummy3 (display val)) 
(dummy4 (newline)) 
(dummy4 (newline))) 
(repl new-env))))

(define s8-hw5 (lambda (e env) 

(cond 
 
 ((number? e) e)
 ((symbol? e) (get-value e env)) 
 ((not (list? e)) (error "s8-hw5: cannot evaluate -->" e))
 
 ((if-stmt? e) (if (eq? (s8-hw5 (cadr e) env) 0) 
                    ( s8-hw5 (cadddr e) env) 
                    ( s8-hw5 (caddr e) env)))
  ((let-stmt? e)
      (let ((names (map car  (cadr e)))
            (inits (map cadr (cadr e))))
        (let ((vals (map (lambda (init) (s8-hw5 init env)) inits)))
          (let ((new-env (append (map cons names vals) env)))
            (s8-hw5 (caddr e) new-env)))))
			
 
 ((letstar-stmt? e) (if (= (length (cadr e)) 1) ;if the second element is of length 1, (second element is the one right after the let*) take different action.
		(let ((l (list 'let (cadr e) (caddr e))))    (let ((names (map car (cadr l))) (inits (map cadr (cadr l)))) ;make the list, consisting of let, 2nd element and 3rd element.
		;and let it be the normal let statement. notice that rest is the same with original let statement.
																		(let ((vals (map (lambda (init) (s8-hw5 init env)) inits)))
																			(let ((new-env (append (map cons names vals) env)))
																				(s8-hw5 (caddr l) new-env)))))
		(let ((first (list 'let (list (caadr e)))) (rest (list 'let* (cdadr e) (caddr e)))) ;now since length is not 1, it is a list; then treat differently.
										(let ((l (append first (list rest)))) (let ((names (map car (cadr l))) (inits (map cadr (cadr l))))
														(let ((vals (map (lambda (init) (s8-hw5 init env)) inits)))
																		(let ((new-env (append (map cons names vals) env)))
																			(s8-hw5 (caddr l) new-env))))))))
																		
																			
 ((lambda-short-stmt? e) e)
 
 (else 
 
 (cond
	((lambda-short-stmt? (car e)) (if (= (length (cadar e)) (length (cdr e)))
											(let* ((par (map s8-hw5 (cdr e) (make-list (length (cdr e)) env))) (nenv (append (map cons (cadar e) par) env))) (s8-hw5 (caddar e) nenv))
											(error "s8-hw5: Total number of formal parameters and actual parameters do not match!")))
											
											
											
	;if normal math operators, different measures should be taken; for the exceptions etc.
	((is-normal? (car e)) (let ((operands (map s8-hw5 (cdr e) (make-list (length (cdr e)) env))) (operator (get-operator (car e) env)))
		(cond ;for the exceptions, write the cases
		                                          ((and (equal? operator '+) (= (length operands) 0)) 0) ;if no operands in addition, it is 0.
												  ((and (equal? operator '*) (= (length operands) 0)) 1) ;if no operands in multiplication, it is 1. 
												  ((and (or (equal? operator '-) (equal? operator '/)) (= (length operands) (or 0 1))) (error "s8-hw5: It needs at least two operands for this operator -->" operator))
												  (else (apply operator operands))
												  )))
	    (else (let* ((result (s8-hw5 (list (get-value (car e) env) (cadr e)) env))) result))
	)
	
 ))))
 
(define cs305 (lambda () (repl '())))