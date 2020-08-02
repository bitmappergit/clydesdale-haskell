
;;; This deals with structure declarations

;;; Each structure declaration creates a number of linked entities.
;;; Consider:
;;; structure Bar => Foo where
;;;   field1 :: Int
;;;   field2 :: Float
;;; This must create a class Foo, a constructor function, selector functions,
;;; a blank,

;;; This performs the initial setup of the structure definition.

(define (structure->def struct-decl)
 (remember-context struct-decl
  (with-slots structure-decl (super struct decls) struct-decl
   (mlet ((tycon (get-simple-tycon struct))
	  (struct-def (tycon-ref-tycon tycon))
	  (tyvars (map (function tyvar-name) (get-simple-args struct)))
	  (super-defs '())
	  (simple? (null? tyvars))
	  (class (structure-class struct-def))
	  (slots '())
	  (pos 0))
     (setf (structure-simple? struct-def) simple?)
     (dolist (s super)
       (let ((stycon (get-simple-tycon s))
	     (args (get-simple-args s)))
	 (remember-context stycon
	  (when args
            (signal-cant-inherit-polymorphic-structs s struct-def))
	  (let ((def (fetch-top-def (tycon-ref-name stycon) 'struct)))
	    (unless (structure? def)
	      (signal-structure-name-required (tycon-ref-name tycon) def))
	    (push def super-defs)))))
     (setf (structure-super struct-def) (reverse super-defs))
     (setf (structure-tyvars struct-def) tyvars)
     (dolist (decl decls)
       (remember-context decl
        (when (is-type? 'signdecl decl)
	   (let* ((signature (signdecl-signature decl))
		  (vars (resolve-signature signature)))
	     (dolist (v vars)
	       (when (not (memq v tyvars))
		(signal-bad-structure-component-sig signature v struct)))
	     (when (not (null? (signature-context signature)))
		 (signal-no-structure-overloading signature struct))
	       (dolist (var-ref (signdecl-vars decl))
		 (let* ((slot (var-ref-var var-ref))
		        (slot-name (symbol->string (def-name slot))))
		  (if simple?
  	           (begin (setf (slot-pos slot) pos)
			  (incf pos))
		   (let* ((getter-name
			   (string->symbol (string-append "get-" slot-name)))
			  (getter-def (create-top-definition
				       getter-name 'method-var))
			  (setter-name
			   (string->symbol (string-append "set-" slot-name)))
			  (setter-def (create-top-definition
				       setter-name 'method-var)))
		      (setf (def-where-defined getter-def)
			    (ast-node-line-number var-ref))
		      (setf (def-where-defined setter-def)
			    (ast-node-line-number var-ref))
		      (setf (method-var-class getter-def) class)
		      (setf (method-var-class setter-def) class)		
		      (setf (slot-getter slot) getter-def)
		      (setf (slot-setter slot) setter-def)))
		   (setf (slot-type slot) (signature-type signature))
		   (push slot slots)))))))
      (setf (structure-slots struct-def) (reverse slots))
      struct-def))))

(define (expand-structure-definition struct-decl)
 (remember-context struct-decl
  (mlet ((struct (structure-decl-struct struct-decl))
	 (decls (structure-decl-decls struct-decl))
	 (tycon (get-simple-tycon struct))
	 (struct-def (tycon-ref-tycon tycon))
	 (simple? (structure-simple? struct-def))
	 ((super* slots*)
	  (gather-super-structures
	   struct-def '() (structure-super struct-def)
	                  (structure-slots struct-def))))
    (setf (structure-super* struct-def) super*)
    (setf (structure-slots* struct-def) slots*)
    (create-structure-alg struct-def '())
; %%% add deriving someday:     (structure-decl-deriving struct-decl))
    (create-empty-structure struct-def)
    (when simple?
	(create-structure-class/instance struct-def))
    (create-structure-init-fn struct-def decls))))

;;; Compute super* and slots* for a structure

(define (gather-super-structures top seen to-see all-slots)
  (cond ((null? to-see)
	 (values seen all-slots))
	((eq? (car to-see) top)
	 (signal-circular-structs top))
	((memq (car to-see) seen)
	 (gather-super-structures top seen (cdr to-see) all-slots))
	(else
	 (gather-super-structures
	  top
	  (cons (car to-see) seen)
	  (append (structure-super (car to-see)) (cdr to-see))
	  (append (structure-slots (car to-see)) all-slots)))))

;;; Create a data type for the structure.

(define (create-structure-alg struct deriving)
  (let* ((slots (structure-slots* struct))
	 (alg (structure-alg struct))
	 (tyvars (structure-tyvars struct))
	 (con (car (algdata-constrs alg)))
	 (constr (make constr
		       (constructor (**con/def con))
		       (types (map (lambda (x)
				     ;; '() is the annotations
				     (tuple (slot-type x) '()))
				   slots))))
	 (alg-decl
	  (make data-decl
            (context '())
	    (simple (**tycon/def alg (map (function **tyvar) tyvars)))
	    (constrs (list constr))
	    (deriving deriving)
            (annotations '()))))
    (setf (module-alg-defs *module*)
	  (cons (algdata->def alg-decl) (module-alg-defs *module*)))))

;;; This defines an empty structure - all slots contain bottom.  This is used
;;; by the init code for the structure.  To detect a slot that needs
;;; defaulting, it is compared to the slot in this structure.

(define (create-empty-structure struct)
  (let* ((empty-var (make-new-var
		     (string-append
		      "empty-" (symbol->string (def-name struct)))))
	 (con (structure-con struct))
	 (slots (map (lambda (s)
		       (**app (**var/def (core-symbol "uninitializedSlot"))
			      (**string (symbol->string (def-name s)))
			      (**string (symbol->string (def-name struct)))))
		     (structure-slots* struct))))
    (setf (structure-empty-val struct) empty-var)
    (add-new-module-def empty-var (**app/l (**con/def con) slots))
    slots))

;;; This creates the structure initialization function.  This will be attached
;;; to the constructor for the structure type.    

(define (create-structure-init-fn struct decls)
  (let ((slots* (structure-slots* struct))
	(local-inits '())
	(init-fn-var (make-new-var
		      (string-append
		       "init-" (symbol->string (def-name struct))))))
    (dolist (decl decls)
      (when (valdef? decl)
	(let ((vars (collect-pattern-vars (valdef-lhs decl))))
	  (dolist (var-ref vars)
	    (let* ((slot-name (var-ref-name var-ref))
		   (slot (resolve-toplevel-name slot-name)))
	      (unless (eq? slot *undefined-def*)
	       (if (or (not (slot? slot)) (not (memq slot slots*)))
		   (signal-init-not-slot struct slot decl)
		   (setf local-inits
		      (setup-slot-init-fn struct slot var-ref local-inits)))))))
	(add-new-module-decl decl)))
    (let* ((all-inits (find-slot-init-fns slots* local-inits))
	   (init-fn (**app (**var/def (core-symbol "doStructureInit"))
			   (**var/def (structure-empty-val struct))
			   (**tuple all-inits))))
      (add-new-module-def init-fn-var init-fn)
      (setf (structure-init-fn struct) init-fn-var))))

;;; This attached an initialization function to a single slot.  Different
;;; structs may have different inits for the same slot.

(define (setup-slot-init-fn struct slot var-ref local-inits)
  (let ((init-var (make-new-var
		   (string-append "init-" (symbol->string (def-name struct))
				  "-" (symbol->string (def-name slot))))))
    (setf (var-ref-var var-ref) init-var)
    (setf (var-ref-name var-ref) (def-name init-var))
    (when (assq slot local-inits)
	  (signal-multiple-slot-default slot struct))
    (push (tuple slot init-var) local-inits)
    (when (and (structure-simple? struct) ;; Only set init-fn for local slots
	       (memq slot (structure-slots struct)))
       (setf (slot-init-fn slot) init-var))
    (let* ((s-sig (get-struct-type struct))
	   (sig (**signature (signature-context s-sig)
			     (**arrow-type
			      (slot-type slot)
			      (signature-type s-sig)
			      (signature-type s-sig)))))
      (add-new-module-signature init-var sig))
    local-inits))

;;; This determines which init function applies to a slot.  Use either
;;; the definition in the current decl, the one in the defining decl of the
;;; slot, or none at all.  None at all is indicated by the value False.

(define (find-slot-init-fns slots local-inits)
 (if (null? slots)
     '()
     (let* ((slot (car slots))
	    (rest (find-slot-init-fns (cdr slots) local-inits))
	    (local-fn (assq slot local-inits)))
       (cons
	(if local-fn
	    (tuple-2-2 local-fn)
	    (or (slot-init-fn slot) (**con/def (core-symbol "False"))))
	rest))))

;;; This returns the type of a structure object

(define (get-struct-type struct)
  (if (structure-simple? struct)
      (**signature (list (**context
			  (**class/def (structure-class struct))
			  '|a|))
		   (**tyvar '|a|))
      (**signature '() (**tycon/def struct (map (function **tycon)
						(structure-tyvars struct))))))

;;; For simple structures, create class and instance decls.   

(define (create-structure-class/instance struct)
  (let ((class (structure-class struct)))
    ;; Make this struct an instance of its class and all superclasses.
    (dolist (s (cons struct (structure-super* struct)))
      (add-struct-instance-decl struct s))
    ;; Use `a' to denote the struct type (OK since no polymorphic type here)
    (let* ((super-classes (map (lambda (x)
				 (**context
				  (**class/def (structure-class x))
				  '|a|))
				 (structure-super struct)))
	   ;; Each slot has two methods: getter and setter.
	   (decls (concat (map (lambda (s)
				 (list
				  (**signdecl/def
				   (list (slot-getter s))
				   (**signature '()
				       (**arrow-type
					(**tyvar '|a|)
					(slot-type s))))
				  (**signdecl/def
				   (list (slot-setter s))
				   (**signature '()
				      (**arrow-type
				       (slot-type s)
				       (**tyvar '|a|)
				       (**tyvar '|a|))))))
			       (structure-slots struct))))
	   (class-decl (make class-decl
			    (class (**class/def class))
			    (super-classes super-classes)
			    (class-var '|a|)
			    (decls decls))))
      (class->def class-decl)
      (push class (module-class-defs *module*)))))

;;; Put a structure into a class.

(define (add-struct-instance-decl struct i-struct)
  (let ((i-defs '())
	(con (structure-con struct)))
    (dolist (s (structure-slots i-struct))
      (let ((i (get-slot-position s struct)))
	(push (**define (slot-getter s) '(|x|) (**sel con (**var '|x|) i))
	      i-defs)
	(push (**define (slot-setter s) '(|x|) (**update con (list (**var '|x|))
						           (list i)))
	      i-defs)))
    (let ((res (make instance-decl
		     (context '())
		     (class (**class/def (structure-class i-struct)))
		     (simple (**tycon/def (structure-alg struct) '()))
		     (decls i-defs))))
      (push res (module-instance-defs *module*)))))

(define (get-slot-position slot struct)
  (get-slot-position-1 slot (structure-slots* struct) 0))

(define (get-slot-position-1 slot slots i)
  (if (null? slots)
      (error "Non-existent slot!")
      (if (eq? slot (car slots))
	  i
	  (get-slot-position-1 slot (cdr slots) (1+ i)))))

(define (structure-con s)
  (car (algdata-constrs (structure-alg s))))

;;; Error handlers

(define (signal-cant-inherit-polymorphic-structs s struct-def)
  (phase-error/objs 'poly-superstruct (list struct-def)
    "The polymorphic structure ~A cannot be used as a super structure of ~A"
    s (get-object-name struct-def)))

(define (signal-structure-name-required name def)
  (phase-error/objs 'superstruct-not-structure (list def)
     "The inherited structure ~A of ~A is not a structure"
     name (get-object-name def)))

(define (signal-bad-structure-component-sig signature v struct)
  (phase-error/objs 'bad-struct-tyvar (list struct)
 "The type variable ~A in the signature ~A must be a parameter to structure ~A"
      v signature (get-object-name struct)))

(define (signal-no-structure-overloading signature struct)
  (phase-error/objs 'bad-struct-overloading (list struct)
   "The signature ~A must not contain overloaded type variables in~%~
    the definition of to structure ~A"
    signature (get-object-name struct)))

(define (signal-circular-structs s)
  (phase-error/objs 'circular-super-structs (list s)
    "The structure ~A inherits itself."
    (get-object-name s)))

(define (signal-init-not-slot struct slot decl)
  (phase-error/objs 'default-not-a-slot (list struct)
    "~A is not a slot in ~A in the default definition ~A"
    slot (get-object-name struct) decl))

(define (signal-multiple-slot-default slot struct)
  (phase-error/objs 'multiple-struct-default (list struct)
    "~A has multiple default definitions in ~A"
    slot (get-object-name struct)))
