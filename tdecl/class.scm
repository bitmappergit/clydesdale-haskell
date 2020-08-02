;;; Before classes are converted, the super class relation is computed.
;;; This sets up the super and super* field of each class and
;;; checks for the following errors:
;;;  Wrong tyvar in context
;;;  cyclic class structure
;;;  Non-class in context


(define (signal-super-class-tyvar-error class class-var tyvar)
  (recoverable-error 'super-class-tyvar-error
    "The context for class ~A must only refer to type variable ~A.~%~
     Type variable ~A cannot be used here."
    (class-ref-name class) class-var tyvar))

(define (compute-class-super* class)
  (setf (class-super* class) 
	(reverse (compute-super*-1 '() (class-super class))))
  (when (memq class (class-super* class))
      (signal-cyclic-class-structure (class-super* class))))

(define (compute-super*-1 result pending)
  (if (null? pending)
      result
      (let ((c (forward-def (car pending))))
	(if (memq c result)
	    (compute-super*-1 result (cdr pending))
	    (compute-super*-1 (cons (car pending) result)
			      (append (class-super c) pending))))))

(define (signal-cyclic-class-structure classes)
  (fatal-error 'cyclic-class-structure
    "There is a cycle in the superclass relation involving these classes:~%~a"
    classes))


;;;  This sets up the following fields in the class entry:
;;;    instances '()
;;;    defaults = ast for defaults
;;;    kind
;;;    methods
;;;    signatures
;;;    method-vars
;;;    selectors
;;;  Each method is initialized with
;;;    class
;;;    signature
;;;    type
;;;  Errors detected:
;;;   signature doesnt reference class 

(define (class->def class-decl)
 (remember-context class-decl
  (with-slots class-decl (super-classes class class-var decls) class-decl
   (let ((class (class-ref-class class))
	 (super '()))
     (setf (class-instances class) '())
     (setf (class-kind class) (find-class-kind class))
     (dolist (context super-classes)
	(with-slots context (class tyvar) context
	  (when (not (eq? class-var tyvar))
	    (signal-super-class-tyvar-error class-decl class-var tyvar))
	  (resolve-class class)
	  (let ((super-def (class-ref-class class)))
	    (when (not (eq? super-def *undefined-def*))
	       (push super-def super)))))
     (setf (class-super class) (reverse super))
     (setf (class-tyvar class) class-var)
     ; sets up defaults, method signatures
     (init-methods class decls)
     (setf (class-n-methods class) (length (class-method-vars class)))
     (setf (class-runtime-var class)
	   (make-runtime-var class "-class" (core-symbol "Class")))
     class))))

(define (find-class-kind class)
  (cond ((not (module-prelude? *module*))
	 'other)
	((memq class
	       (list (core-symbol "Eq") (core-symbol "Ord")
		     (core-symbol "Text") (core-symbol "Binary")
		     (core-symbol "Ix") (core-symbol "Enum")))
	 'Standard)
	((memq class
	       (list (core-symbol "Num") (core-symbol "Real")
		     (core-symbol "Integral") (core-symbol "Fractional")
		     (core-symbol "Floating") (core-symbol "RealFrac")
		     (core-symbol "RealFloat")))
		     'Numeric)
	(else
	 'other)))

(define (init-methods class decls)
 (let* ((tyvar (class-tyvar class))
        (class-context (**context (**class/def class) tyvar))
	(annotations '()))
  (dolist (decl decls)
   (remember-context decl
    (cond ((is-type? 'signdecl decl)
	   (let* ((signature (signdecl-signature decl))
		  (vars (resolve-signature signature)))
	     (when (not (memq tyvar vars))
	       (signal-class-sig-ignores-type signature tyvar class))
	     ;; Note: signature does not include defined class yet
	     (dolist (context (signature-context signature))
               (when (eq? tyvar (context-tyvar context))
		 (signal-method-constrains-class-tyvar
		   (car (signdecl-vars decl)) class signature context)))
	     (setf signature (rename-class-sig-vars signature tyvar))
	     (let ((gtype (ast->gtype (cons class-context
					    (signature-context signature))
				      (signature-type signature))))
 	       (dolist (var-ref (signdecl-vars decl))
	         (let ((var (var-ref-var var-ref)))
		   (setf (var-type var) gtype)
		   (setf (method-var-method-signature var) signature))))))
	  ((annotation-decls? decl)
	   (setf annotations (append (annotation-decls-annotations decl)
				     annotations)))
	  (else  ; decl must be a default definition
	   (let ((vars (collect-pattern-vars (valdef-lhs decl))))
	     (dolist (var-ref vars)
               (let* ((method-name (var-ref-name var-ref))
		      (method-var (resolve-toplevel-name method-name)))
		  (if (and method-var (method-var? method-var)
			   (eq? (method-var-class method-var) class))
		   (let ((default-var
			   (make-new-var
			     (string-append
			       "default-"
			       (symbol->string (def-name method-var))))))
		     (setf (var-ref-var var-ref) default-var)
		     (setf (var-ref-name var-ref) (def-name default-var))
		     (when (not (eq? (method-var-default method-var) '#f))
		      (signal-multiple-definition-of-default method-name class))
		     (setf (method-var-default method-var) default-var)
		     (let* ((sig (method-var-method-signature method-var))
			    (context (cons class-context
					   (signature-context sig)))
			    (new-sig (**signature context
						  (signature-type sig))))
		       (add-new-module-signature default-var new-sig)))
		   (signal-default-not-in-class method-name class)))))
	     (add-new-module-decl decl)))))
  (dolist (a annotations)
    (cond ((annotation-value? a)
	   (recoverable-error 'misplaced-annotation
			      "Misplaced annotation: ~A~%" a))
	  (else
	   (dolist (name (annotation-decl-names a))
	     (attach-default-annotation
	        name (annotation-decl-annotations a) class)))
	     ))))

(define (attach-default-annotation name anns class)
  (let ((var-ref (**var name)))
    (resolve-var var-ref)
    (let ((method-var (var-ref-var var-ref)))
      (when (not (eq? method-var *undefined-def*))
	 (if (and (method-var? method-var)
		  (eq? (method-var-class method-var) class))
	     (let ((dvar (method-var-default method-var)))
	       (if dvar
		  (setf (var-annotations dvar)
			(append anns (var-annotations dvar)))
		  (recoverable-error 'bad-class-annotation
			"Method ~A has no default to annotate"
			name)))
	     (recoverable-error 'bad-class-annotation
				"~A is not in the defined class ~A"
			name class))))))

;;; This does a few things that require definitions of superclasses and
;;; type synonyms.  For interfaces, this must be done after dangling
;;; references are resolved.

(define (setup-class-slots class)
 (compute-class-super* class)
 (setf (class-dict-size class)
       (+ (class-n-methods class) (length (class-super* class)))))

(define (signal-class-sig-ignores-type signature tyvar class)
  (phase-error 'class-sig-ignores-type
    "The method signature ~a in class ~A does not reference~%~
     the overloaded type ~A."
    (sz signature 20) (get-object-name class) tyvar))


(define (signal-method-constrains-class-tyvar m class sig context)
  (phase-error 'method-constrains-class-tyvar
    "The signature of method ~A in class ~A, ~A,~%~
     may not further constrain the type associated with the class.~%~
     The context ~A can not be used in this signature."
   m (get-object-name class) sig context))

(define (signal-multiple-definition-of-default method-name class)
  (phase-error 'multiple-definition-of-default
   "There are multiple definitions of the default operation for method ~A~%~
    in class ~A."
   method-name (get-object-name class)))

(define (signal-default-not-in-class method-var class)
  (phase-error 'default-not-in-class
     "Definitions within class ~A are limited to the methods in this class.~%~
      The definition of ~A is not allowed here."
   (get-object-name class) method-var))
	   
(define (create-selector-functions class interface?)
  (let ((res '()))
    (dolist (c (cons class (class-super* class)))
      (dolist (m (class-method-vars c))
	(let ((var (make var
			  (name (string->symbol
				 (string-append "sel-"
				    (symbol->string (def-name class))
				    "/"
				    (symbol->string (def-name m)))))
			  (module (def-module class))
			  (unit (def-unit class))
			  (toplevel? '#t))))
	  (setf (var-selector-fn? var) '#t)
	  (push (tuple m var) res)
	  (unless interface?
	    (let ((arity (count-type-arity (gtype-type (var-type m)))))
	      (add-new-module-def var (create-selector-code class m arity))))))
	(setf (class-selectors class) res))))

(define (create-selector-args arity)
  (let ((result  '()))
    (dotimes (i arity)
      (declare (ignorable i))
      (push (create-local-definition (gensym "arg")) result))
    (nreverse result)))

(define (create-selector-code c m arity)
  (let ((var   (create-local-definition '|d|))
	(args  (create-selector-args arity)))
    (setf (var-force-strict? var) '#t)
    (let ((body (create-selector-code-1 c m (**var/def var))))
      (**lambda/pat (cons (**var-pat/def var)
			  (map (function **var-pat/def) args))
		    (if (null? args)
			body
			(**app/l body
				 (map (function **var/def) args)))))))

(define (create-selector-code-1 class method d)
  (let ((mcl (method-var-class method)))
    (cond ((eq? mcl class)
	   (**dsel/method class method d))
	  (else
	   (**dsel/method mcl method (**dsel/dict class mcl d))))))
	     
;;; The following code is for the alpha conversion of method
;;; signatures.  The class tyvar is unchanged; all others are renamed.
;;; This is needed because all method types are combined to form the
;;; dictionary signature and aliasing among different tyvars should be
;;; prevented.

(define (rename-class-sig-vars signature tyvar)
  (mlet (((new-context env1)
	  (rename-context-vars (signature-context signature)
			       (list (tuple tyvar tyvar))))
	 ((new-type _)
	  (rename-type-vars (signature-type signature) env1)))
      (**signature new-context new-type)))

(define (rename-context-vars contexts env)
  (if (null? contexts)
      (values '() env)
      (mlet (((new-tyvar env1)
	      (rename-sig-tyvar (context-tyvar (car contexts)) env))
	     ((rest env2)
	      (rename-context-vars (cdr contexts) env1)))
       (values (cons (**context (context-class (car contexts)) new-tyvar) rest)
	       env2))))

(define (rename-type-vars type env)
  (if (tyvar? type)
      (mlet (((tyvar env1)
	      (rename-sig-tyvar (tyvar-name type) env)))
	 (values (**tyvar tyvar) env1))
      (mlet (((new-types env1) (rename-type-vars/l (tycon-args type) env)))
        (values (**tycon/def (tycon-def type) new-types) env1))))

(define (rename-type-vars/l types env)
  (if (null? types)
      (values '() env)
      (mlet (((type1 env1) (rename-type-vars (car types) env))
	     ((new-types env2) (rename-type-vars/l (cdr types) env1)))
          (values (cons type1 new-types) env2))))

(define (rename-sig-tyvar tyvar env)
  (let ((res (assq tyvar env)))
    (if (eq? res '#f)
	(let ((new-tyvar (gentyvar (symbol->string tyvar))))
	  (values new-tyvar (cons (tuple tyvar new-tyvar) env)))
	(values (tuple-2-2 res) env))))

(define *tyvar-counter* 0)

;;; This generates a new interned tyvar name

(define (gentyvar root)
  (incf *tyvar-counter*)
  (string->symbol (format '#f "~A-~A" root *tyvar-counter*)))
