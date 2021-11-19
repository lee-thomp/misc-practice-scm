#!/usr/bin/guile -l 
!#

;;; The following is a convenient list of procedures to load into a REPL.
;;; Load directly using $ ./misc.scm or $ guile -l misc.scm


; reciprocal using (and) jank
(define reciprocal
  (lambda (x)
	(and (not (= x 0))	; Note: (and 1 #t) => #t , (and #t 1) => 1
		 (/ 1 x))))


; averaging procedure
(define average
  (lambda x
	
	; assemble average through iteration
	(define average-iter
	  (lambda (x total len)
		(if (null? x) (/ total len)
			(average-iter (cdr x) (+ total (car x)) (+ len 1)))))

	; begin recursion
	(average-iter x 0 0)))


; right fold procedure
(define foldr
  (lambda (f init lst)
	(if (null? lst) init
		(f (car lst) (foldr f init (cdr lst))))))


; left fold procedure
(define foldl
  (lambda (f init lst)
	(if (null? lst) init
		(foldl f (f init (car lst)) (cdr lst)))))


; reverse via foldl (lambda variadic)
(define reverse
  (lambda lst
	(foldl (lambda (x y) (cons y x)) '() lst)))


; multi and procdeure
(define and-l
  (lambda lst
	(if (null? lst) #t
		(if (car lst) (apply and-l (cdr lst))
			#f))))


; square number procedure
(define square
  (lambda (x)
	(* x x)))


; triangle number generation
(define triangle
  (lambda (x)
	(/ (* x (+ x 1)) 2)))


; raise number to power
(define pow
  (lambda (x n)
	(cond ((= n 0) 1)
		  ((> n 0) (* x (pow x (- n 1))))
		  (else (/ 1 (pow x (- n)))))))


; check if number is triangular
(define triangle?
  (lambda (x)
	(odd?
	  ((lambda (y)
		 (if (integer? y) y 0))
		 (sqrt (+ 1 (* 8 x)))))))	; equation returns odd int if triangular input


; check if number is square 
(define square?
  (lambda (x)
	(integer? (sqrt x))))

	
; python-style range
(define range
  (lambda (x)

	; counting up iterator procedure
	(define range-iter
	  (lambda (y)
		(cons y (if (= x y) '()
					(range-iter (+ y 1))))))

	; call iterator proc
	(range-iter 0)))


; element-wise addition (adds y to all elements in list lst)
(define .+
  (lambda (lst y)
	(map (lambda (a) (+ a y)) lst)))


; element-wise subtraction (minuses y from all elements in list lst)
(define .-
  (lambda (lst y)
	(map (lambda (a) (- a y)) lst)))


; element-wise multiplication (all elements in list lst by y)
(define .*
  (lambda (lst y)
	(map (lambda (a) (* a y)) lst)))


; element-wise division (divideds all elements in list lst by y)
(define ./
  (lambda (lst y)
	(map (lambda (a) (/ a y)) lst)))

	
; interleave two lists, (interleave A B) -> (A1 B1 A2 B2 ... )
(define interleave
  (lambda (a b)
	(if (null? a) b
		(cons (car a) (interleave b (cdr a))))))


; remove items from list (reverse of filter)
(define remove
  (lambda (f lst)
	(filter (lambda (y) (not (f y))) lst)))


; personal filter procedure
(define filter
  (lambda (f lst)
	(cond ((null? lst) '())
		  ((f (car lst)) (cons (car lst) (filter f (cdr lst))))
		  (else (filter f (cdr lst))))))


; check that y is a valid factor of x
(define factor?
  (lambda (x y)
	(integer? (/ x y))))


; check if a number isn't prime - returns either #f or smallest factor
(define not-prime?
  (lambda (x)
	
	; count up through possible factors
	(define not-prime-iter
	  (lambda (y)
		(cond ((= x y) #f)
			  ((factor? x y) y)
			  (else (not-prime-iter (+ y 1))))))

	; initial check x isn't 1, 0, or negative (need to investigate negative primes)
	(if (< x 2) #f	
		(not-prime-iter 2))))


; define number as list of prime factors
(define prime-factor
  (lambda (x)

	(define prime-factor-iter
	  (lambda (y)
		(if (not-prime? y) (cons (not-prime? y) (prime-factor-iter (/ y (not-prime? y))))
			(list y))))

	(if (= x 0) #f
		(prime-factor-iter x))))


; print all list elements on newlines, dumps 0 to function return
(define print-list
  (lambda (lst)
	(map (lambda (x) (map display (list x "\n")) 0) lst)))
