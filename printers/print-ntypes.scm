;;; These printers deal with ntype structures.

;;; Too much of this file is copied from print-types!

(define-ast-printer ntyvar (object xp)
  (let ((object (prune object)))
    (if (ntyvar? object)
	(begin
	  (write-char #\t xp)
	  (write (tyvar->number object) xp))
	(write object xp))))

;;; Various type special cases have a magic cookie in the def field.

;;; This unexpands the IO tycon to make type error messages much more
;;; readable.

(define-ast-printer ntycon (object xp)
  (let ((tycon (ntycon-tycon object))
	(args (ntycon-args object)))
    (if (eq? tycon '#f)
	(write-string "<Bogus tycon>" xp)
	(if (and (eq? tycon (core-symbol "Arrow"))
		 (ntycon? (car args))
		 (eq? (ntycon-tycon (car args)) (core-symbol "SystemState_"))
		 (ntycon? (cadr args))
		 (eq? (ntycon-tycon (cadr args)) (core-symbol "IOResult_")))
	    (print-io-tycon (car (ntycon-args (cadr args))) xp)
	    (print-general-tycon tycon args object xp)))))

(define (print-io-tycon ty xp)
  (with-ast-block (xp)
    (write-string "IO" xp)
    (write-whitespace xp)
    (write-atype ty xp)))

(define-ast-printer gtype (object xp)
  (let ((var 0)
	(res '()))
    (dolist (classes (gtype-context object))
       (let ((v (gtyvar->symbol var)))
	 (dolist (class classes)
	    (push (**context (**class/def class) v) res)))
       (incf var))
    (write-contexts (reverse res) xp)
    (write (gtype-type object) xp)))
          
(define-ast-printer gtyvar (object xp)
  (write-string (symbol->string (gtyvar->symbol (gtyvar-varnum object))) xp))

(define (gtyvar->symbol n)
  (cond ((< n 26)
	 (list-ref '(|a| |b| |c| |d| |e| |f| |g|
		     |h| |i| |j| |k| |l| |m| |n|
		     |o| |p| |q| |r| |s| |t| |u|
		     |v| |w| |x| |y| |z|)
		   n))
	(else
	 (string->symbol (format '#f "g~A" (- n 25))))))

(define-ast-printer recursive-type (object xp)
  (write (recursive-type-type object) xp))

(define *printed-tyvars* '())

(define (tyvar->number tyvar)
  (tyvar->number-1 tyvar (dynamic *printed-tyvars*) 1))

(define (tyvar->number-1 tyvar vars n)
  (cond ((null? vars)
	 (setf (dynamic *printed-tyvars*)
	       (nconc (dynamic *printed-tyvars*) (list tyvar)))
	 n)
	((eq? tyvar (car vars))
	 n)
	(else
	 (tyvar->number-1 tyvar (cdr vars) (1+ n)))))


