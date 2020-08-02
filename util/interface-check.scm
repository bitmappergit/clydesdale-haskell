
;;; This is called by the compiler after all compilation is complete

(define (check-interfaces mods)
  (let ((mod-names (map (function module-name) mods)))
    (dolist (imod (get-all-interfaces))
      (when (memq (module-name imod) mod-names)
	(let ((mod (locate-module (module-name imod))))
	  (check-export-table mod imod)))
      (dolist (alist (module-interface-definitions imod))
	(when (memq (car alist) mod-names)
	  (dolist (def (cdr alist))
            (check-interface def (def-forward-to def)))))
      (dolist (inst (module-instance-defs imod))
        (let ((class (forward-def (instance-class inst)))
	      (type (forward-def (instance-algdata inst))))
	  (when (and (not (def-interface? class))
		     (not (def-interface? type)))
	    (let ((i1 (lookup-instance type class)))
	      (when (instance-in-interface? i1)
                 (phase-error/objs 'missing-interface-instance (list inst)
  "An instance for ~A was defined in an interface but is not present~%~
   in the implementation."
                   (get-object-name inst))))))))))
      
(define (check-export-table mod imod)
  (let ((iet (module-export-table imod))
	(met (module-export-table mod)))
    (table-for-each (lambda (name group)
		      (declare (ignore group))
		      (let ((i-group (table-entry iet name)))
			(check-export-group name i-group imod)))
		    met)
    (table-for-each (lambda (name group)
		      (declare (ignore group))
		      (let ((m-group (table-entry met name)))
			(check-export-group name m-group mod)))
		    iet)))

(define (check-export-group name g mod)
  (when (not g)
    (recoverable-error 'missing-entity
     "~A ~A does not export ~A as indicated by the corresponding~%~
      interface or implementation."
      (get-object-kind mod) (get-object-name mod) (symbol->string name))))

;;; This compares the definition found in an interface with the one
;;; in an implementation.

(define (check-interface idef def)
 (unless (method-var? idef)  ; method vars are checked as a part of the class
  (cond ((algdata? idef)
	 (if (not (algdata? def))
	     (interface-mismatch idef def
	        (format '#f
  "The interface for data type ~A is not implemented as a data type." idef))
	     (compare-algdatas idef def)))
	((synonym? idef)
	 (if (not (synonym? def))
	     (interface-mismatch idef def
	        (format '#f
  "The interface for synonym ~A is not implemented as a synonym." idef))
	     (compare-synonyms idef def)))
        ((class? idef)
	 (if (not (class? def))
	     (interface-mismatch idef def
	        (format '#f
  "The interface for class ~A is not implemented as a class." idef))
	     (compare-classes idef def)))
        ((var? idef)
	 (if (method-var? def)
	     (interface-mismatch idef def
  	       (format '#f
  "The interface for variable ~A is implemented by a class method." idef))
	     (compare-vars idef def)))
        ((instance? idef)
	 (compare-instances idef def))
	(else 'con))))

;;; This is used for dangling defs found in interface files.  As long as the
;;; types match and the tycon arity matches all is OK

(define (check-interface/dangling idef def)
  (if (class? idef)
      (unless (class? def)
	 (interface-mismatch idef def "Name used as both a class and type"))
      (if (class? def)
	  (interface-mismatch idef def "Name used as both a class and type")
	  (when (not (eqv? (tycon-def-arity idef)
			   (tycon-def-arity def)))
	     (interface-arity-mismatch idef def)))))

(define (interface-mismatch def1 def2 msg)
  (phase-error/objs 'interface-definition-mismatch (list def1 def2)
    "The definition of ~A is not consistant.~%~A~%"
    (get-object-name def1) msg))

(define (interface-arity-mismatch idef def)
  (phase-error/objs 'interface-tycon-arity-mismatch def
    "The arity of ~A is not the same as the arity used in an interface."
    (get-object-name idef)))

;;; The first algdata may be abstract.  In this case only the arity needs to
;;; be checked.

(define (compare-algdatas idef def)
  (cond ((not (eqv? (tycon-def-arity idef) (tycon-def-arity def)))
	 (interface-arity-mismatch idef def))
	((null? (algdata-constrs idef))
	 'OK)
	((not (eqv? (algdata-n-constr idef) (algdata-n-constr def)))
	 (interface-mismatch idef def
		"Data types have different number of constructors"))
	((not (eq? (algdata-implemented-by-lisp? idef)
		   (algdata-implemented-by-lisp? def)))
	 (interface-mismatch idef def
	      "Only one definition has an associated ImportLispType"))
	(else
	 (compare-algdata-constrs? (algdata-constrs idef)
				   (algdata-constrs def) idef def))))
	      
	     
(define (compare-algdata-constrs? c1 c2 idef def)
  (if (and (pair? c1)
	   (same-con? (car c1) (car c2) idef def))
      (compare-algdata-constrs? (cdr c1) (cdr c2) idef def)
      'OK))

(define (same-con? con1 con2 idef def)
  (cond ((not (eq? (def-name con1) (def-name con2)))
	 (interface-mismatch idef def
	  (format '#f
	   "Data types have different constructors: ~A does not match ~A"
	   con1 con2))
	  '#f)
	((not (same-signature? (con-signature con1) (con-signature con2)))
	 (interface-mismatch idef def
	  (format '#f
 "Constructors have different types:~% ~A :: ~A does not match~% ~A :: ~A"
	   con1 (con-signature con1) con2 (con-signature con2)))
	  '#f)
	((not (equal? (con-slot-strict? con1) (con-slot-strict? con2)))
	 (interface-mismatch idef def
	  (format '#f
		  "Constructor ~A have differing strictness properties" con1))
	  '#f)
	((not (same-fixity? (con-fixity con1) (con-fixity con2)))
	 (interface-mismatch idef def
	  (format '#f "Constructor ~A has differing fixities~%" con1))
	 '#f)
	(else '#t)))

(define (same-fixity? f1 f2)
  (or (and (not f1) (not f2))
      (and (is-type? 'fixity f1) (is-type? 'fixity f2)
	   (eq? (fixity-associativity f1) (fixity-associativity f2))
	   (eqv? (fixity-precedence f1) (fixity-precedence f2)))))

(define (compare-synonyms idef def)
  (when (or (not (equal? (synonym-args idef) (synonym-args def)))
	    (not (same-ast-type? (synonym-body idef) (synonym-body def))))
   (interface-mismatch
     idef def "Type synonym definitions are not identical")))

(define (same-ast-type? ty1 ty2)
  (or (and (tyvar? ty1) (tyvar? ty2)
	   (eq? (tyvar-name ty1) (tyvar-name ty2)))
      (and (tycon? ty1) (tycon? ty2)
	   (same-tycon? (tycon-def ty1) (tycon-def ty2))
	   (same-ast-type/l (tycon-args ty1) (tycon-args ty2)))))

(define (same-ast-type/l t1 t2)
  (or (and (null? t1) (null? t2))
      (and (pair? t1) (pair? t2) (same-ast-type? (car t1) (car t2))
	                         (same-ast-type/l (cdr t1) (cdr t2)))))

(define (compare-classes idef def) ;;
 (cond ((not (every
	      (function same-class?) (class-super idef) (class-super def)))
	(interface-mismatch idef def "Super classes do not match"))
       ((not (eqv? (class-n-methods idef) (class-n-methods def)))
	(interface-mismatch idef def
	      "Classes contain differing number of methods"))
       ((not (every (function same-method?)
		     (class-method-vars idef) (class-method-vars def)))
	'bad)
       (else 'OK)))

(define (same-method? var1 var2)
  (cond ((not (eq? (def-name var1) (def-name var2)))
	 (interface-mismatch (method-var-class var1) (method-var-class var2)
	    (format '#f "Class method ~A does not match ~A" var1 var2))
	 '#f)
	(else
	 (compare-vars var1 var2)
	 '#t)))

(define (compare-vars idef def)
   (cond ((not (same-signature? (var-type idef) (var-type def)))
	  (interface-mismatch idef def
            (format '#f "Signature ~A does not match ~A"
		    (var-type idef) (var-type def))))
	 ((not (same-fixity? (var-fixity idef) (var-fixity def)))
	  (interface-mismatch idef def "Fixity mismatch"))
	 (else 'OK)))

(define (compare-instances idef def)
  (unless (equal? (instance-gcontext idef) (instance-gcontext def))
    (interface-mismatch idef def
       "Instances have different contexts")))

;;; This compares gtype signatures
(define (same-signature? sig1 sig2)
  (and (same-context? (gtype-context sig1) (gtype-context sig2))
       (same-ntypes? (gtype-type sig1) (gtype-type sig2))))

(define (same-context? c1 c2)
  (or (and (null? c1) (null? c2))
      (and (pair? c1) (pair? c2)
	   (same-context-1 (car c1) (car c2))
	   (same-context? (cdr c1) (cdr c2)))))

(define (same-context-1 c1 c2)
  (or (and (null? c1) (null? c2))
      (and (pair? c1) (pair? c2)
	   (same-class? (car c1) (car c2))
	   (same-context-1 (cdr c1) (cdr c2)))))

(define (same-class? c1 c2)
  (or (eq? c1 c2)
      (and (eq? (def-name c1) (def-name c2))
	   (eq? (def-module c1) (def-module c2)))))

(define (same-ntypes? ty1 ty2)
  (or (and (gtyvar? ty1) (gtyvar? ty2) (eqv? (gtyvar-varnum ty1)
					     (gtyvar-varnum ty2)))
      (and (ntycon? ty1) (ntycon? ty2)
	   (same-tycon? (ntycon-tycon ty1) (ntycon-tycon ty2))
	   (every (function same-ntypes?)
		  (ntycon-args ty1) (ntycon-args ty2)))))

(define (same-tycon? def1 def2)
  (or (eq? def1 def2)
      (and (eq? (def-name def1) (def-name def2))
	   (eq? (def-module def1) (def-module def2)))))

;;; This is used to select the 'most specific' version of a definition
;;; for use in a dummy interface.  This doesn't matter much in that
;;; this only helps find errors sooner.

;;; Only look at algdatas for now - replace definitions without constructors.

(define (def-more-specific? def1 def2)
  (and (algdata? def1) (algdata? def2) (null? (algdata-constrs def2))))


(define (check-interface/dummy idef def)
  (if (and (algdata? def) (null? (algdata-constrs def))) ; %%%
      (check-interface def idef)
      (check-interface idef def)))

