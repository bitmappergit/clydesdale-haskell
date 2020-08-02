
;;; Basic DI structure:
;;;  a. Create the set of instances
;;;  b. Expand the context of each potential instance.
;;;  c. Once b. reaches a fixpoint, fill in the ast for the generated instances

(define *di-context-changed* '#f)

(define (add-derived-instances modules)
  (let ((insts '()))
    (walk-modules modules
     (lambda () (setf insts (append (find-derivable-instances '#f) insts))))
    (walk-modules (get-all-interfaces)
     (lambda ()
       (when (interface-module? (locate-module *module-name*))
         (setf insts (append (find-derivable-instances '#t) insts)))))
    (check-di-preconditions insts)
    (compute-di-fixpoint insts)
;;; Derived instances which come from interfaces are treated specially.
;;; They must participate in the fixpoint process but are not attached to
;;; any module.  The are linked in the class but not placed in instance-defs.
    (dolist (icp insts)
     (let ((inst (car icp)))
      (when (instance-ok? inst)
       (if (instance-in-interface? inst)
	(expand-instance-decls inst '#t) ; do this now since not in a module
	(push inst (module-instance-defs
		    (locate-module (def-module (instance-algdata inst)))))))))))

;;; Create instance decls for all derived instances in a module.  Filter
;;; out underivable instances (Ix & Enum only)

(define (find-derivable-instances interface?)
  (let ((algs (module-alg-defs *module*))
	(insts '()))
    (dolist (alg algs)
      (dolist (d (algdata-deriving alg))
	 (dolist (di (deriving-instances d))
	    (let ((i (add-derivable-instance di alg d interface?)))
	      (when i (push i insts))))))
    insts))

;; This adds a provisional instance template.  Of course, there may already
;;; be an instance (error!)

(define (add-derivable-instance inst-decl alg d interface?)
  (setf alg (forward-def alg))
  (let* ((cls (forward-def (class-ref-class (instance-decl-class inst-decl))))
	 (existing-inst (lookup-instance alg cls))
	 (c (deriving-preconditions d)))
    (cond ((or (eq? existing-inst '#f) 
	       (and (not interface?) (instance-in-interface? existing-inst)))
	   ;; This links it in so that locate-instance can find it
	   (let ((inst (new-instance cls alg (algdata-tyvars alg)))
		 (fc (expand-special-context
		      (instance-decl-context inst-decl) alg)))
;;; %%% I think we also need superclass contexts here  - jcp
	     (setf (instance-context inst) (algdata-context alg))
	     (setf (ast-node-line-number inst)
		   (def-where-defined alg))
	     (setf (instance-decls inst)
		   (if interface?
		       '()
		       (create-instance-fns inst inst-decl)))
	     (setf (instance-ok? inst) '#t)
	     (setf (instance-in-interface? inst) interface?)
	     (unless interface?
 	      (setf (instance-runtime-var inst)
		    (make-new-var (string-append
				   (symbol->string (def-name cls))
				   "-"
				   (symbol->string (def-name alg))
				   "-instance"))))
	     (list inst fc c)))
	  (interface? '#f) ; there may be more than one
	  (else
	   (recoverable-error 'instance-exists
		  "An instance for type ~A in class ~A already exists;~%~
                  the deriving clause is being ignored."
	       alg cls)
	   '#f))))

(define (check-di-preconditions insts)
  (dolist (icp insts)
    (let* ((i (car icp))
	   (c (caddr icp))
	   (alg (forward-def (instance-algdata i))))
      (dolist (class-ref c)
       (let ((class (class-ref-class class-ref)))
	(cond ((eq? class (core-symbol "EnumType"))
	       (when (not (algdata-enum? alg))
		 (signal-instance-requires i "enumerated")
		 (setf (instance-ok? i) '#f)))
	      ((eq? class (core-symbol "EnumOrTupleType"))
	       (when (not (or (algdata-enum? alg) (algdata-tuple? alg)))
		 (signal-instance-requires i "enumerated or single constructor")
		 (setf (instance-ok? i) '#f)))
	      (else
	       (let ((i1 (lookup-instance alg class)))
		 (when (not i1)
 	           (signal-instance-requires-class i class)
		   (setf (instance-ok? i) '#f))))))))))

(define (signal-instance-requires inst thing)
  (phase-error 'cannot-derive-instance
    "The instance ~A cannot be derived.~%~A is not an ~A type."
     (get-object-name inst) (get-object-name (instance-algdata inst)) thing))

(define (signal-instance-requires-class inst class)
  (phase-error 'cannot-derive-instance
    "The instance ~A cannot be derived.  ~A is not in class ~A."
     (get-object-name inst) (get-object-name (instance-algdata inst))
     (get-object-name class)))

;;; This expands the context of an instance declaration in a deriving clause.
;;; The context C |t expands to C t_i for all the t_i at the top level of
;;; the type.

(define (expand-special-context c alg)
  (let ((res '()))
    (dolist (constr (algdata-constrs (forward-def alg)))
      (dolist (ty (con-types constr))
        (dolist (ctxt c)
	  (push (list (class-ref-class (context-class ctxt)) ty) res))))
    res))

;;; This is the instance context fixpoint routine.

(define (compute-di-fixpoint insts)
  (setf *di-context-changed* '#f)
  (dolist (inst insts)
    (propagate-di-context (car inst) (cadr inst)))
  (when *di-context-changed* (compute-di-fixpoint insts)))

(define (propagate-di-context inst c)
 (when (instance-ok? inst)
    (dolist (ct c)
      (let* ((class (car ct))
	     (type (cadr ct))
	     (implied-classes (propagate-ast-context class type)))
	(cond ((eq? implied-classes 'fail)
	       (phase-error 'canot-derive-instance
   	     "The instance ~A(~A) cannot be derived.~%Context ~A(~A) failed."
	         (instance-class inst) (instance-algdata inst)
		 class type)
	       (setf (instance-ok? inst) '#f)
	       (setf *di-context-changed* '#t))
	      (else
	       (dolist (ct1 implied-classes)
		 (augment-instance-context
		  inst
		  (class-ref-class (context-class ct1))
		  (context-tyvar ct1)))))))))

;;; This is the basic context propagation routine.  It takes a class and
;;; a type and returns either 'fail or a context.

(define (propagate-ast-context class type)
  (if (tyvar? type)
      (list (**context (**class/def class) (tyvar-name type)))
      (let ((tycon (tycon-def type)))
	(if (synonym? tycon)
	    (propagate-ast-context class (expand-synonym type))
	    (let ((i (lookup-instance (tycon-def type) class))
		  (args (tycon-args type)))
	      (if (or (not i) (not (instance-ok? i)))
		  'fail
		  (propagate-instance-contexts
		     (instance-context i) (instance-tyvars i) args)))))))

;;; Here's the plan for expanding Cls(Alg t1 t2 .. tn) using
;;; instance (Cls1(vx),Cls2(vy),...) => Cls(Alg(v1 v2 .. vn))
;;;   for each Clsx in the instance context, propagate Clsx to the
;;;   ti corresponding to vx, where vx must be in the set vi.

(define (propagate-instance-contexts contexts tyvars args)
  (if (null? contexts)
      '()
      (let ((c1 (propagate-ast-context
		 (class-ref-class (context-class (car contexts)))
		 (find-corresponding-tyvar
		  (context-tyvar (car contexts)) tyvars args))))
	(if (eq? c1 'fail)
	    'fail
	    (append c1 (propagate-instance-contexts 
			(cdr contexts) tyvars args))))))

;;; Given the t(i) and the v(i), return the t corresponding to a v.

(define (find-corresponding-tyvar tyvar tyvars args)
  (if (eq? tyvar (car tyvars))
      (car args)
      (find-corresponding-tyvar tyvar (cdr tyvars) (cdr args))))

;;; 1 level type synonym expansion

(define (expand-synonym type)
  (let* ((synonym (tycon-def type))
	 (args (synonym-args synonym))
	 (body (synonym-body synonym)))
  (let ((alist (map (lambda (tyvar arg) (tuple tyvar arg))
		    args (tycon-args type))))
    (copy-synonym-body body alist))))

(define (copy-synonym-body type alist)
  (if (tyvar? type)
      (tuple-2-2 (assq (tyvar-name type) alist))
      (make tycon (def (tycon-def type))
	          (name (tycon-name type))
		  (args (map (lambda (ty)
			       (copy-synonym-body ty alist))
			     (tycon-args type))))))

;;; This extends the context of an instance declaration.  It notes when the
;;; context associated with an instance changes.

(define (augment-instance-context inst class tyvar)
  (let ((c (instance-context inst)))
    (unless (single-ast-context-implies? c class tyvar)
      (setf *di-context-changed* '#t)
      (setf (instance-context inst)
	    (augment-context c class tyvar)))))

(define (single-ast-context-implies? ast-context class tyvar)
  (cond ((null? ast-context)
	 '#f)
	((eq? tyvar (context-tyvar (car ast-context)))
	 (let ((class1 (class-ref-class (context-class (car ast-context)))))
	   (or (eq? class1 class)
	       (memq class (class-super* (forward-def class1)))
	       (single-ast-context-implies? (cdr ast-context) class tyvar))))
	(else
	 (single-ast-context-implies? (cdr ast-context) class tyvar))))

;;; Add class(var) to a context, removing any contexts made redundant by
;;; the new addition.  Example: adding Ord a to (Eq a, Eq b) would yield
;;; (Ord a,Eq b).

(define (augment-context contexts cl var)
  (cons (**context (**class/def cl) var)
	(remove-implied-contexts cl var contexts)))

(define (remove-implied-contexts class1 tyvar1 contexts)
  (if (null? contexts)
      '#f
      (with-slots context (class tyvar) (car contexts)
	(let ((rest (remove-implied-contexts class1 tyvar1 (cdr contexts)))
	      (class2 (class-ref-class class)))
	  (if (and (eq? tyvar1 tyvar)
		   (memq class2 (forward-def (class-super* class1))))
	      rest
	      (cons (car contexts) rest))))))


(define (create-instance-fns inst inst-decl)
  (let ((class (instance-class inst))
	(alg (instance-algdata inst)))
    (cond ((eq? class (core-symbol "Eq")) (eq-fns alg))
	  ((eq? class (core-symbol "Ord"))(ord-fns alg))
	  ((eq? class (core-symbol "Ix")) (ix-fns alg))
	  ((eq? class (core-symbol "Enum")) (enum-fns alg))
;	  ((eq? class (core-symbol "Text"))
;	   (text-fns alg (instance-suppress-readers? inst)))
	  ((eq? class (core-symbol "Binary")) (binary-fns alg))	
	  (else (map (function copy-ast) (instance-decl-decls inst-decl))))))


