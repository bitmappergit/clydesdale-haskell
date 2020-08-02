;;; This file contains typecheckers for all expressions except vars and
;;; declarations.

;;; From valdef-structs:
;;;   valdef, single-fun-def are in type-decls

(define-type-checker guarded-rhs
  (type-check guarded-rhs rhs rhs-type
    (type-check guarded-rhs guard guard-type
      (type-unify guard-type *bool-type*
          (type-mismatch/fixed (guarded-rhs-guard object)
	   "Guards must be of type Bool" guard-type))
      (return-type object rhs-type))))

;;; These type checkers deal with patterns.

(define-type-checker as-pat
  (type-check as-pat pattern as-type
    (setf (var-type (var-ref-var (as-pat-var object))) as-type)
    (return-type object as-type)))

(define-type-checker irr-pat
  (type-check irr-pat pattern pattern-type
    (return-type object pattern-type)))

(define-type-checker var-pat
  (fresh-type var-type
    (setf (var-type (var-ref-var (var-pat-var object))) var-type)
    (return-type object var-type)))

(define-type-checker wildcard-pat
 (fresh-type pat-type
    (return-type object pat-type)))

;;; Constant patterns create a piece of code to actually to the
;;; match: ((==) k), where k is the constant.  This code is placed in the
;;; match-fn slot of the const-pat and is used by the cfn.

(define-type-checker const-pat
 (let* ((val (const-pat-value object))
	(match-fn (**app (**var/def (core-symbol "==")) val)))
   (setf (const-pat-match-fn object) match-fn)
   (type-check const-pat match-fn match-type
     (fresh-type res-type
       (type-unify match-type (**arrow res-type *bool-type*) #f)
       (return-type object res-type)))))

(define-type-checker plus-pat
  (let* ((kp (**int (plus-pat-k object)))
	 (km (**int (- (plus-pat-k object))))
	 (match-fn (**app (**var/def (core-symbol "<=")) kp))
	 (bind-fn (**app (**var/def (core-symbol "+")) km)))
    (setf (plus-pat-match-fn object) match-fn)
    (setf (plus-pat-bind-fn object) bind-fn)
    (fresh-type res-type
      (setf (ntyvar-context res-type) (list (core-symbol "Integral")))
      (type-check plus-pat match-fn match-type
        (type-check plus-pat bind-fn bind-type
          (type-check plus-pat pattern pat-type
	    (type-unify match-type (**arrow pat-type *bool-type*) #f)
	    (type-unify bind-type (**arrow pat-type pat-type) #f)
	    (type-unify res-type pat-type #f)
	    (return-type object res-type)))))))

(define-type-checker pcon
 (let ((con (pcon-con object)))
  (if (eqv? (length (pcon-pats object)) (con-arity con))
      (type-check/list pcon pats arg-types
       (fresh-type res-type
        (let ((con-type (instantiate-gtype (con-signature con)))
	      (tyvars (list res-type)))
	 (dotimes (i (con-arity con))
	     (push (**ntyvar) tyvars))
	 (type-unify con-type (**arrow/l tyvars) #f)
	 (dolist (arg (pcon-pats object))
	   (type-unify (car tyvars) (car arg-types)
	    (arg-type-mismatch-msg object arg (car arg-types)
				   con (car tyvars) con-type))
	   (setf tyvars (cdr tyvars))
	   (setf arg-types (cdr arg-types)))
	 (return-type object res-type))))
  (begin
    (phase-error 'constructor-arity-mismatch
		 "Constructor ~A should have ~A arguments:~%  ~A"
		 con (con-arity con) object)
    (type-check/list pcon pats _
      (fresh-type res-type
       (return-type object res-type)))))))

(define-type-checker list-pat
  (if (null? (list-pat-pats object))
      (return-type object (instantiate-gtype
			     (algdata-signature (core-symbol "List"))))
      (type-check/unify-list list-pat pats element-type
	   (type-mismatch/list object
	     "List elements have different types")
	(return-type object (**list-of element-type)))))

;;; These are in the order defined in exp-structs.scm

(define-type-checker lambda
 (with-new-tyvars
  (fresh-monomorphic-types (length (lambda-pats object)) arg-vars
    (type-check/list lambda pats arg-types
     (unify-list arg-types arg-vars)
     (type-check lambda body body-type
      (return-type object (**arrow/l-2 arg-vars body-type)))))))

(define-type-checker let
  (type-check/decls let decls
    (type-check let body let-type
      (return-type object let-type))))

(define-type-checker if
  (type-check if test-exp test-type
    (type-unify test-type *bool-type*
        (type-mismatch/fixed object
	 "The test in an if statement must be of type Bool"
	 test-type))
    (type-check if then-exp then-type
      (type-check if else-exp else-type
        (type-unify then-type else-type
              (type-mismatch object
		   "then and else clauses have different types"
		   then-type else-type))
	(return-type object then-type)))))

(define-type-checker case
 (add-error-handlers/case object)
 (with-new-tyvars
  (let ((case-exp object))  ; needed since object is rebound later
   (fresh-monomorphic-type arg-type
    (type-check case exp exp-type
      (type-unify arg-type exp-type #f) ; just to make it monomorphic
      (fresh-type res-type
	(dolist (object (case-alts object))
	  (recover-type-error ;;; %%% Needs work
	   (type-check alt pat pat-type
	     (type-unify pat-type arg-type
                (list
		 (list
                   "Conflict in case alternative pattern types.~%~
                    Case alternative: ~A~
                    Type inferred for case selector: ~A~
                    Type inferred for pattern: ~A"
                   (szn object 40) (sznt arg-type 40) (sznt pat-type 40))))
	     (type-check/decls alt where-decls
	       (type-check/unify-list alt rhs-list rhs-type
                 (type-mismatch/list object
		     "Guarded expressions must have the same type")
		 (type-unify rhs-type res-type
                  (list
		   (list
                     "Conflict in case alternative values.~%~
                      Case alternative: ~A~
                      Type inferred for case result: ~A~
                      Type inferred for this result: ~A"
                    (szn object 60) (sznt res-type 40)
		    (sznt rhs-type 40)))))))))
	(return-type case-exp res-type)))))))

(define-type-checker bottom
  (return-type (**save-old-exp
		object
		(make-bottom-call object (bottom-context object))) (**ntyvar)))

(define (make-bottom-call object context)
 (let ((sp (ast-node-line-number object)))
  (**abort
   (if context
       (format '#f "Call to ~Aaborted, line ~A in file ~A~%"
	       (sz (valdef-lhs context) 30)
	       (source-pointer-line sp) (source-pointer-file sp))
       (format '#f "_ evaluated at line ~A in file ~A~%"
		   (source-pointer-line sp) (source-pointer-file sp))))))

;;; Expressions with signatures are transformed into let expressions
;;; with signatures.  

;;;    exp :: type   is rewritten as
;;;    let temp = exp
;;;        temp :: type
;;;     in temp

(define-type-checker exp-sign
 (type-rewrite
  (let* ((temp-var (create-temp-var "TC"))
	 (decl (**valdef (**var-pat/def temp-var) '() (exp-sign-exp object)))
	 (let-exp (**let (list decl) (**var/def temp-var)))
	 (signature (exp-sign-signature object)))
      (setf (var-signature temp-var)
	    (ast->gtype (signature-context signature)
			(signature-type signature)))
      let-exp)))

;;; A number of special constructs have been hacked in using ordinary
;;; application syntax.  These special applications are recognized here.

(define (is-special-app? app)
  (and (var-ref? (app-fn app))
       (let ((var (var-ref-var (app-fn app))))
	 (or (eq? var (core-symbol "dictSel"))
	     (eq? var (core-symbol "typeOf"))
	     (eq? var (core-symbol "toDynamic"))
	     (eq? var (core-symbol "fromDynamic"))
	     (eq? var (core-symbol "patternMatchError"))))))

(define-type-checker app
  (if (var-ref? (app-fn object))
      (let ((var (var-ref-var (app-fn object))))
	(cond ((eq? var (core-symbol "dictSel"))
	       (type-check-dict-sel (app-arg object)))
	      ((eq? var (core-symbol "typeOf"))
	       (type-check-type-of (app-arg object)))
	      ((eq? var (core-symbol "toDynamic"))
	       (type-check-to-dynamic (app-arg object)))
	      ((eq? var (core-symbol "fromDynamic"))
	       (type-check-from-dynamic object))
	      ((eq? var (core-symbol "patternMatchError"))
	       (type-check-pme object))
	      (else
	       (type-check-ordinary-app object))))
      (type-check-ordinary-app object)))

(define (type-check-ordinary-app object)
 (fresh-type res-type
  (mlet ((top-app object)
	 ((object args tyvars) (destructure-app object '() (list res-type))))
    (type-check app fn fn-type
      (type-unify fn-type (**arrow/l tyvars)
       (let* ((fn-type-1 (fully-expand-ntype fn-type))
	      (arity (count-type-arity fn-type-1)))
        (list
	 (list
    "While type checking: ~A~
     The type of function ~Ais inferred as ~A~
     which is ~A"
        (szn top-app 60) (sz (app-fn object) 20) (sznt fn-type-1 40)
	(if (eqv? arity 0)
	    "not a function"
	    (format '#f "a function of ~A arguments" arity))))))
      (let ((tv1 tyvars))
	(dolist (object args)
	  (type-check app arg arg-type
	    (type-unify arg-type (car tv1)
	      (arg-type-mismatch-msg top-app (app-arg object) arg-type
				     (app-fn (car args)) (car tv1)
				     (fully-expand-ntype fn-type))))
	  (setf tv1 (cdr tv1)))))
    (return-type top-app res-type))))

(define (arg-type-mismatch-msg top arg arg-type fn i-type fn-type)
 (cons
  (list "While type checking ~A~
         The type of the argument ~Ais inferred as ~A~
         The function ~Arequires an argument of type ~A~
         The typing of this call of ~Ais ~A"
	(szn top 50) (sz arg 20) (sznt arg-type 20)
	(sz fn 20)
	(sznt i-type 20) (sz fn 40)
	(sznt fn-type 25))
  (show-original-type fn)))

(define (destructure-app app args tyvars)
  (if (and (app? app) (not (is-special-app? app)))
      (destructure-app (app-fn app) (cons app args) (cons (**ntyvar) tyvars))
      (values (car args) args tyvars)))

(define (show-original-type fn)
  (when (is-type? 'save-old-exp fn)
	(setf fn (save-old-exp-old-exp fn)))
  (when (con-ref? fn)
	(setf fn (con-ref-con fn)))
  (cond ((and (var-ref? fn)
	      (gtype? (var-type (var-ref-var fn)))
	      (not (null? (gtype-context (var-type (var-ref-var fn))))))
	 (list
	  (list "The signature of ~Ais ~A"
		(sz fn 20) (szn (var-type (var-ref-var fn)) 35))))
	((and (con? fn)
	      (not (null? (gtype-context (con-signature fn)))))
	 (list
	  (list "The signature of constructor ~Ais ~A"
		(sz fn 20) (szn (con-signature fn) 35))))
	(else '())))

;;; This is a special hack for typing dictionary selection as used in
;;; generic tuple functions.  This extracts a dictionary from a TupleDict
;;; object and uses is to resolve the overloading of a designated
;;; expression.  The expresion must generate exactly one new context.

(define (type-check-dict-sel arg)
  (when (or (not (app? arg))
	    (not (app? (app-fn arg))))
     (dict-sel-error))
  (let* ((exp (app-fn (app-fn arg)))
	 (dict-var (app-arg (app-fn arg)))
	 (i (app-arg arg))
	 (p (dynamic *placeholders*)))
    (mlet (((object exp-type) (dispatch-type-check exp)))
	  ; check for exactly one new context
      (when (or (eq? (dynamic *placeholders*) p)
		(not (eq? (cdr (dynamic *placeholders*)) p)))
	 (dict-sel-error))
	(mlet ((placeholder (car (dynamic *placeholders*)))
	       (tyvar (placeholder-tyvar placeholder))
	       ((dict-var-ast dict-var-type) (dispatch-type-check dict-var))
	       ((index-ast index-type) (dispatch-type-check i)))
	   (setf (ntyvar-context tyvar) '()) ; prevent context from leaking out
	   (setf (dynamic *placeholders*) p)
           (type-unify dict-var-type
			  (**ntycon (core-symbol "TupleDicts") '()) #f)
	   (type-unify index-type *int-type* #f)
	   (cond ((method-placeholder? placeholder)
		  (dict-sel-error))  ; I am lazy.  This means that
		 ; dictSel must not be passed a method
		 (else
		  (setf (placeholder-exp placeholder)
			(**app (**var/def (core-symbol "dictSel"))
			       dict-var-ast index-ast))))
	   (return-type object exp-type)))))

(define (dict-sel-error)
  (fatal-error 'dict-sel-error "Bad dictSel usage."))

(define-type-checker con-ref
  (return-type object (instantiate-gtype (con-signature (con-ref-con object)))))

(define-type-checker integer-const
  (cond ((const-overloaded? object)
	 (setf (const-overloaded? object) '#f)
	 (type-rewrite (**fromInteger object)))
	(else
	 (return-type object *Integer-type*))))

(define-type-checker float-const
  (cond ((const-overloaded? object)
	 (setf (const-overloaded? object) '#f)
	 (type-rewrite (**fromRational object)))
	(else
	 (return-type object *Rational-type*))))

(define-type-checker char-const
  (return-type object *char-type*))

(define-type-checker string-const
  (return-type object *string-type*))

(define-type-checker list-exp
  (if (null? (list-exp-exps object))
      (return-type object (instantiate-gtype
			     (algdata-signature (core-symbol "List"))))
      (type-check/unify-list list-exp exps element-type
	      (type-mismatch/list object
		 "List elements do not share a common type")
	(return-type object (**list-of element-type)))))

(define-type-checker sequence
  (type-rewrite (**enumFrom (sequence-from object))))

(define-type-checker sequence-to
  (type-rewrite (**enumFromTo (sequence-to-from object)
			      (sequence-to-to object))))

(define-type-checker sequence-then
  (type-rewrite (**enumFromThen (sequence-then-from object)
				(sequence-then-then object))))

(define-type-checker sequence-then-to
  (type-rewrite (**enumFromThenTo (sequence-then-to-from object)
				  (sequence-then-to-then object)
				  (sequence-then-to-to object))))

(define-type-checker list-comp
 (with-new-tyvars
  (dolist (object (list-comp-quals object))
    (if (is-type? 'qual-generator object)
	(fresh-type pat-type
	 (push pat-type (dynamic *non-generic-tyvars*))
	 (type-check qual-generator pat pat-type-1
	   (type-unify pat-type pat-type-1 #f)
	   (type-check qual-generator exp qual-exp-type
	     (type-unify (**list-of pat-type) qual-exp-type
                            (type-mismatch/fixed object
		 "Generator expression is not a list" qual-exp-type)))))
	 (type-check qual-filter exp filter-type
	   (type-unify filter-type *bool-type*
              (type-mismatch/fixed object
		"Filter must have type Bool" filter-type)))))
  (type-check list-comp exp exp-type
     (return-type object (**list-of exp-type)))))

(define-type-checker section-l
  (type-check section-l op op-type
    (type-check section-l exp exp-type
      (fresh-type a-type
        (fresh-type b-type
          (fresh-type c-type
            (type-unify op-type (**arrow a-type b-type c-type)
                (type-mismatch/fixed object
		     "Binary function required in section" op-type))
	    (type-unify b-type exp-type
                  (type-mismatch object
		      "Argument type mismatch" b-type exp-type))
	    (return-type object (**arrow a-type c-type))))))))

(define-type-checker section-r
  (type-check section-r op op-type
    (type-check section-r exp exp-type
      (fresh-type a-type
        (fresh-type b-type
          (fresh-type c-type
            (type-unify op-type (**arrow a-type b-type c-type)
                  (type-mismatch/fixed object
			 "Binary function required" op-type))
	    (type-unify exp-type a-type
                    (type-mismatch object
			 "Argument type mismatch" a-type exp-type))
	    (return-type object (**arrow b-type c-type))))))))

(define-type-checker omitted-guard
  (return-type object *bool-type*))

(define-type-checker con-number
  (let ((arg-type (instantiate-gtype
		   (algdata-signature (con-number-type object)))))
    (type-check con-number value arg-type1
      (type-unify arg-type arg-type1 #f)
      (return-type object *int-type*))))

(define-type-checker sel
  (let ((con-type (instantiate-gtype
		   (con-signature (sel-constructor object)))))
    (mlet (((res-type exp-type1) (get-ith-type con-type (sel-slot object))))
      (type-check sel value exp-type
        (type-unify exp-type exp-type1 #f)
	(return-type object res-type)))))

(define (get-ith-type type i)
 (let ((args (ntycon-args type)))  ; must be an arrow
  (if (eq? i 0)
      (values (car args) (get-ith-type/last (cadr args)))
      (get-ith-type (cadr args) (1- i)))))

(define (get-ith-type/last type)
  (if (eq? (ntycon-tycon type) (core-symbol "Arrow"))
      (get-ith-type/last (cadr (ntycon-args type)))
      type))

(define-type-checker is-constructor
  (let ((alg-type (instantiate-gtype
		   (algdata-signature
		    (con-alg (is-constructor-constructor object))))))
    (type-check is-constructor value arg-type
      (type-unify arg-type alg-type #f)
      (return-type object *bool-type*))))

(define-type-checker cast
  (type-check cast exp _
    (fresh-type res
      (return-type object res))))

;;; This is used for overloaded methods.  The theory is to avoid supplying
;;; the context at the class level.  This type checks the variable as if it had
;;; the supplied signature.

(define-type-checker overloaded-var-ref
  (let* ((var (overloaded-var-ref-var object))
	 (gtype (overloaded-var-ref-sig object))
	 (ovar-type (var-type var)))
    (when (recursive-type? ovar-type)
	 (error
	  "Implementation error: overloaded method found a recursive type"))
    (mlet (((ntype new-vars) (instantiate-gtype/newvars gtype))
	   (object1 (insert-dict-placeholders
		     (**var/def var) new-vars object)))
	  (return-type object1 ntype))))
