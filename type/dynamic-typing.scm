;;; Dynamic typing stuff

;;; This takes an expression and generates the type of the expression

;;; This is a stripped down version of the type-non-recursive function.
;;; The important difference is that monomorphism is not applied.

(define (dynamic-type-exp exp)
  (let* ((temp-var (create-temp-var "DT"))
	 (decl (**valdef (**var-pat/def temp-var) '() exp))
	 (old-placeholders (dynamic *placeholders*)))
    (setf (dynamic *placeholders*) '())
    (let* ((rhs-type (type-decl-rhs decl))
	   (all-tyvars (collect-tyvars rhs-type))
	   (overloaded-tyvars '()))
      (dolist (tyvar all-tyvars) ; This does not collect existential tyvars
	(when (ntyvar-context tyvar)
	    (push tyvar overloaded-tyvars)))
      ; Monomorphism does NOT apply here!!
      (if (null? overloaded-tyvars)
	  (setf (var-type temp-var) (ntype->gtype rhs-type))
	  (mlet (((gtype tyvars)
		  (ntype->gtype/env rhs-type overloaded-tyvars)))
	     (setf (var-type temp-var) gtype)
	     (dictionary-conversion/definition decl tyvars)))
      (setf (dynamic *placeholders*)
	    (process-placeholders
	     (dynamic *placeholders*) old-placeholders (list decl)))
      (values decl (rconvert-gtype (var-type temp-var)) temp-var))))

(define (type-check-type-of arg)
  (mlet (((decl ty _) (dynamic-type-exp arg)))
    (return-type (**let (list decl) ty) *signature-type*)))
     

(define (type-check-to-dynamic arg)
  (mlet (((decl ty temp) (dynamic-type-exp arg)))
    (return-type
     (**let (list decl)
	    (**app (**con/def (core-symbol "MkDynamic")) ty (**var/def temp)))
     *dynamic-type*)))

(define (type-check-from-dynamic object)
 (type-check app arg arg-type
  (type-unify arg-type *dynamic-type*
    (type-mismatch/fixed object
        "Argument of fromDynamic is not Dynamic" arg-type))
  (fresh-monomorphic-type res-type
      (let* ((temp-var (create-temp-var "DY"))
	     (temp2 (create-temp-var "Magic"))
	     (ty (ntype->gtype res-type)))
	(setf (var-type temp-var) ty)
	(setf (var-type temp2) *magic-type*)
	(return-type 
	 (**let (list (**valdef/def
		       temp-var
		       (**case (**app (**var/def (core-symbol "coerce"))
				      (app-arg object)
				      (rconvert-gtype ty))
			       (list
				(**alt/simple
				 (**pcon/def (core-symbol "DFailure") '())
				 (**abort "fromDynamic failed."))
				(**alt/simple
				 (**pcon/def (core-symbol "DSucc")
					     (list (**pat temp2)))
				 (**var/def temp2))))))
		(**var/def temp-var))
	 res-type)))))

(define-type-checker dynamic-pat
  (let* ((sig (dynamic-pat-sig object))
	 (gt (ast->gtype (signature-context sig) (signature-type sig)))
	 (new-type (skolemize-gtype gt object)))
    (type-check dynamic-pat pat pattern-type
      (type-unify pattern-type new-type
        (type-mismatch/fixed object
"The pattern type is not compatible with the signature in a dynamic type"
               pattern-type))
      (return-type object *dynamic-type*))))

(define (skolemize-gtype gtype object)
  (let ((new-types (map (function new-skolem-type) (gtype-context gtype))))
    (setf (dynamic-pat-runtime-vars object) new-types)
    (insert-skolem-types (gtype-type gtype) new-types)))

(define (new-skolem-type context)
  (let* ((tname (gensym "eType"))
	 (tvar (create-temp-var "eTD"))
	 (stype (make algdata
                  (name tname)
		  (unit *unit*)
		  (module *module-name*)
		  (n-constr 0)
		  (constrs '())
		  (context '())
		  (classes context)
		  (tyvars '())
		  (skolem-type? '#t)
		  (runtime-var tvar))))
    stype))

(define (insert-skolem-types ty new-types)
  (if (gtyvar? ty)
      (**ntycon (list-ref new-types (gtyvar-varnum ty)) '())
      (**ntycon (ntycon-tycon ty) (map (lambda (ty1)
					 (insert-skolem-types ty1 new-types))
				       (ntycon-args ty)))))

(define (remove-dynamic-type-context c)
  (if (null? c)
      c
      (if (eq? (car c) (core-symbol "DynamicType"))
	  (cdr c)
	  (cons (car c) (remove-dynamic-type-context (cdr c))))))

;;; Major bootstrap problem here: avoid looking at the sig of PatternMatchError

(define (type-check-pme object)
  (let ((fn-type (**arrow
		  (**list-of
		   (**ntycon (core-symbol "Char") '()))
		  (**list-of
		   (**ntycon (core-symbol "Dynamic") '()))
		  (**ntyvar))))
    (type-check app arg arg-type
      (fresh-type res-type
        (fresh-type arg-type-1
          (type-unify fn-type (**arrow arg-type-1 res-type)
              (type-mismatch/fixed object
		       "Attempt to call a non-function"
		       fn-type))
	  (type-unify arg-type-1 arg-type
	       (type-mismatch object
		    "Argument type mismatch" arg-type-1 arg-type))
	  (return-type object res-type))))))

