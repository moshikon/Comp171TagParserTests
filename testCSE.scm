(print-gensym #f)
; Change to your own location
(load "~/compilation/ass2/cse.scm")
(load "~/compilation/ass2/cse.so")

(define my-parse-func cse-2)
(define staff-parse-func cse)

(define replace-gensym
  (lambda (exp-lst)
    (if (null? exp) '()
    (map (lambda (el) 
	    (cond 	    	    
	    ((gensym? el) (symbol->string el))
	    ((not (pair? el)) el)
	    ((list? el) (replace-gensym el))
	    (else (append (replace-gensym (car exp-lst)) (replace-gensym (cdr exp-lst)))))) exp-lst))
))

(define testVSstaff
	(lambda (input)
		(let* ((my-res (begin (gensym-count 0) (replace-gensym (my-parse-func input))))
		      (staff-res (begin (gensym-count 0) (replace-gensym (staff-parse-func input)))))
			(display (format "~s" input))
			(display (format "\n => ~s\n" my-res))
			(cond ((equal?  staff-res  my-res)
				(display (format "\033[1;32m Success! ☺ \033[0m \n")) #t)
				(else 
				(display (format "\033[1;31m Failed! ☹\033[0m , Expected: ~s, Actual: ~s \n" staff-res my-res)) #f))
			)))
			
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display (format "\033[1m~s" tests-name))
	(display ":")
	(newline)
	(display "================\033[0m")
	(newline)
	(let ((results (map testVSstaff lst)))
	(newline)
	(cond ((andmap (lambda (exp) (equal? exp #t)) results)	
		(display (format "\033[1;32m~s Tests: SUCCESS! ☺ \033[0m\n \n" tests-name)) #t)		
		(else
		(display (format "\033[1;31m~s Tests: FAILED! ☹ \033[0m\n \n" tests-name)) #f)))
))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m !!!!!  ☺  ALL TESTS SUCCEEDED  ☺  !!!!\033[0m\n"))
		(else (display "\033[1;31m #####  ☹  SOME TESTS FAILED  ☹  #####\033[0m\n")))
		(newline))
))

(define quotedListsTests
  (list 
      '(+ '(a b c d e) '(a b c d e) '(a b c d e) '(a b c d e) '(a b c d e))
      '(g (f '('(1 2 3 4 5 6 7 8 9 0) '(a b c d e)) (list f g h) '('(1 2 3 4 5 6 7 8 9 0) '(a b c d e))) (list f g h))
))

(define myTests
  (list
    '(f (c (a b)) (a b) (c (a b)))
    '(f (c (a b)) (a b) (c (a b)) (a b))    
    '(define foo (a b b b b b b))
    '(define foo ((a b) (b b) (b c) (b b) (b c) (b b) (b c)))
    ;'(foo ((a) (a) (b) (b) (b) (c) (c) (c)))
    '(foo ((a) (a) (b) (b) (b) (b) (c) (c) (c)))
    ;'(foo ((a) (b) (c) (b) (c) (b) (c) (a)))
    ;'(begin (define foo ((a b) (b b) (b c) (b b) (b c) (b b) (b c))) (a b))
    '(define a (f (+ g h) 1 (g (+ g h) (+ g h)) 3 (g (+ g h) (+ g h)) (+ g h)))
))

(define mayerExamplesTests
  (list  
    '(+ 2 3)
    '(f (f (f (f x))))
    '(* (+ 2 3 4) (+ 2 3 4))
    '(* (+ 2 (f 3 5) 4) (+ 2 (f 3 5) 4))
    '(f (g x y) (f (g x y) z))
    '(+ (* (- x y) (* x x)) (* x x) (foo (- x y)) (goo (* (- x y) (* x x))))
    '(f (g x) (g (g x)) (h (g (g x)) (g x)) ((g x) (g x)))
    '(list (cons 'a 'b) (cons 'a 'b) (list (cons 'a 'b) (cons 'a 'b)) (list (list (cons 'a 'b) (cons 'a 'b))))
    '(list '(a b) (list '(a b) '(c d)) (list '(a b) '(c d)))

))

(display (format "\033[1mComp171 - CSE Tests\033[0m\n====================\n"))

(runAllTests
  (list
      (cons "Mayer Examples" mayerExamplesTests)     
      (cons "Quoted Lists" quotedListsTests)   
      (cons "My Tests" myTests)       
))