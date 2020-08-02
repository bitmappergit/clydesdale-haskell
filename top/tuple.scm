;;; This file creates type definitions for tuples of arbitrary size.

(define *tuple-definitions* '())

(define (init-tuples)
  (setf *tuple-definitions* '()))

(define (tuple-tycon k)
  (let ((tycon (assq k *tuple-definitions*)))
    (if (eq? tycon '#f)
	(new-tuple-tycon k)
	(tuple-2-2 tycon))))

(define (tuple-constructor k)
  (car (algdata-constrs (tuple-tycon k))))

(define (is-tuple-constructor? x)
  (and (con? x) (is-tuple-tycon? (con-alg x))))

(define (is-tuple-tycon? x)
  (and (algdata? x) (algdata-real-tuple? x)))

(define (tuple-constructor-arity x)
  (con-arity x))

(predefine (ast->gtype c t))          ; in util/type-utils.scm
(predefine (**arrow-type/l args))     ; in util/constructors.scm
(predefine (**tyvar x))               ; in util/constructors.scm

(define (new-tuple-tycon k)
  (cond ((eqv? k 0)
	 (core-symbol "UnitType"))
	(else
	 (let* ((name (make-tuple-type-name k))
		(cname (add-con-prefix name))
		(dummy-vars (gen-dummy-names k))
		(algdata (make algdata
			       (name (string->symbol name))
			       (module '*core*)
			       (unit '*core*)
			       (exported? '#t)
			       (arity k)
			       (n-constr 1)
			       (context '())
			       (tyvars dummy-vars)
			       (classes '())  ;; filled in later
			       (enum? '#f)
			       (tuple? '#t)
			       (real-tuple? '#t)
			       (deriving '())))
		(constr (make con
			      (name (string->symbol cname))
			      (module '*core*)
			      (unit '*core*)
			      (exported? '#t)
			      (arity k)
			      (types (map (function **tyvar) dummy-vars))
			      (tag 0)
			      (alg algdata)
			      (slot-strict? '())
			      (infix? '#f)))
		(tyvars (map (function **tyvar) dummy-vars))
		(tuple-type (**tycon/def algdata tyvars)))
	   (dotimes (i k)
	      (push '#f (con-slot-strict? constr)))
	   (setf (algdata-signature algdata)
		 (ast->gtype '() tuple-type))
	   (setf (con-signature constr)
		 (ast->gtype '() (**arrow-type/l
				  (append tyvars (list tuple-type)))))
	   (setf (algdata-constrs algdata)
		 (list constr))
	   (push (tuple k algdata) *tuple-definitions*)
	   algdata))))

(define (make-tuple-type-name k)
  (call-with-output-string
   (lambda (p)
     (write-string "(" p)
     (dotimes (i (1- k))
       (write-string "," p))
     (write-string ")" p))))

(define (gen-dummy-names n)
  (gen-dummy-names-1 n '()))

(define (gen-dummy-names-1 n l)
  (if (eqv? n 0)
      l
      (gen-dummy-names-1 (1- n)
			 (cons (string->symbol (format '#f "a~A" n)) l))))




