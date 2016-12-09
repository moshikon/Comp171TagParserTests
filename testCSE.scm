(print-gensym #f)
; Change to your own location
(load "~/compilation/ass2/cse.scm")
(load "~/compilation/ass2/cse.so")

(define my-parse-func cse-2)
(define staff-parse-func cse)

(define replace-gensym
  (lambda (exp-lst)
    (begin (gensym-count 0)
    (if (null? exp) '()
    (map (lambda (el) 
	    (cond 	    
	    ((gensym? el) (symbol->string el))
	    ((not (pair? el)) el)
	    ((list? el) (replace-gensym el))
	    (else (append (replace-gensym (car exp-lst)) (replace-gensym (cdr exp-lst)))))) exp-lst)))
))

(define testVSstaff
	(lambda (input)
		(let* ((my-res (replace-gensym (my-parse-func input)))
		      (staff-res (replace-gensym (staff-parse-func input))))
			(display (format "~s" input))
			(display (format " => ~s\n" my-res))
			(cond ((equal? (begin (gensym-count 0) staff-res) (begin (gensym-count 0) my-res))
				(display (format "\033[1;32m Success! ☺ \033[0m \n")) #t)
				(else 
				(display (format "\033[1;31m Failed! ☹\033[0m , Expected: ~s, Actual: ~s \n" staff-res my-res)) #f))
			)))
			
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display tests-name)
	(display ":")
	(newline)
	(display "================")
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

(define mayerExamples
  (list
    '(+ 2 3)
    ;'(f (f (f (f x))))
    '(* (+ 2 3 4) (+ 2 3 4))
    '(* (+ 2 (f 3 5) 4) (+ 2 (f 3 5) 4))
    '(f (g x y) (f (g x y) z))
    
;'(+ (* (- x y) (* x x))
;(* x x)
;(foo (- x y))
;(goo (* (- x y) (* x x))))
      
;'(f (g x)
;(g (g x))
;(h (g (g x)) (g x))
;((g x) (g x)))    

))

(display (format "Comp171 - CSE Tests\n====================\n"))

(runAllTests
  (list
      (cons "Mayer Examples" mayerExamples)     
))