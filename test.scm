; Change to your own location
(load "~/compilation/compiler.scm")


(define testVSstaff
	(lambda (input expected-output)
		(let ((my-res (parse input))
		      (staff-res expected-output))
			(display input)
			(display ": ")			
			(cond ((equal? my-res staff-res)
				(display "\033[1;32mSuccess!\033[0m") (newline) #t)
				(else (display "\033[1;31mFailed!\033[0m ") 
					(display ", expected: ")					
					(display staff-res)
					(display ", actual:")
					(display my-res)
					(newline)
					#f))
			)))
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display tests-name)
	(display ":")
	(newline)
	(display "=============")
	(newline)
	(let ((results (map (lambda (x) (testVSstaff (car x) (cdr x))) lst)))
	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32mSUCCESS!\033[0m\n") (newline) #t)
		(else (display "\033[1;31mFAILED!\033[0m\n") (newline) #f)))
))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m !!!!! ALL TESTS SUCCEEDED !!!!\033[0m\n"))
		(else (display "\033[1;31m ##### SOME TESTS FAILED #####\033[0m\n")))
		(newline))
))		


(define variableTests
  (list
    (cons 'abc `(var abc))
    (cons '123x `(var 123x))  
    (cons 'and 'failure)
))

(define constantTests
  (list
  
    (cons '() `(const ()))    
    (cons (list->vector (list 1 (list 2 3 4) 2 3)) `(const #(1 (2 3 4) 2 3)))   
    (cons #f `(const #f))
    (cons #\a `(const #\a))    
    (cons 34 `(const 34))
    (cons "abc" `(const "abc"))
    (cons '(quote a) `(const a))
    (cons '(quote (a b c)) `(const (a b c)))
    (cons '(quote (quote a b c)) `(const (quote a b c)))

))

(runAllTests
  (list
      (cons "Constants" constantTests)    
      (cons "Variables" variableTests)       
))