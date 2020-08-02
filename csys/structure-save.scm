;;;==================================================================
;;; Dump structure traversal
;;;==================================================================

;;; This is the general object dumper.  It recognizes the basic Lisp
;;; objects and dumps them.  Given an object, this generates lisp code
;;; to recreate the object at load time.

(define (dump-object x)
  (cond ((struct? x)
	 (dump x))
	((or (eq? x '#t) (eq? x '#f))
	 ;; True and false are self-evaluating.
	 x)
	((or (symbol? x) (null? x))
	 ;; Symbols and lists must be quoted.
	 `',x)
	((or (number? x)
	     (eq? x '#t)
	     (eq? x '#f)
	     (string? x)   ; This makes dumped strings immutable.
	     (char? x))
	 ;; These objects are self-evaluating.
	 x)
	((list? x)
	 ;; True lists
	 `(list ,@(map (function dump-object) x)))
	((pair? x)
	 `(cons ,(dump-object (car x))
		,(dump-object (cdr x))))
	((vector? x)
	 `(vector ,@(map (function dump-object) (vector->list x))))
	((table? x)
	 `(list->table ,@(dump-object (table->list x))))
	(else
	 (error "Don't know how to dump ~A." x))))


;;; *** Should install the walker in the type descriptor.

(define-walker dump)

(define (dump x)
  (when (and (is-type? 'def x) (def-forward-to x))
     (setf x (def-forward-to x)))
  (call-walker dump x))



;;;==================================================================
;;; Dumpers for defs
;;;==================================================================


;;; All walkers for def structures should call this macro.  The body
;;; is invoked only if the def belongs to the current compilation unit
;;; and hasn't already been traversed.  Within the body, the 
;;; variable "v" is bound to a form that will evaluate to the 
;;; corresponding def structure at run time.  This is also
;;; the return value from the macro.

(define-local-syntax (with-new-def (v d stat-var) . body)
  (let ((temp   (gensym))
	(expvar (gensym)))
    `(let ((,temp  ,d)
	   (,expvar '#f))
       (if (not (def-dump-code ,temp))
	   (begin
	     (cond ((not (def-prelude? ,temp))
		    (setf ,expvar
			  (list 'def-n
				(dynamic *dump-def-counter*)))
		    (incf (dynamic *dump-def-counter*))
		    (push ,temp *dump-defs*))
		   (else
		    (setf ,expvar
			  (make-core-symbol-name
			   (def->core-name-string ,temp)))))
	     (setf (def-dump-code ,temp) ,expvar)
	     (when (eq? (def-unit ,temp) *unit*)
	       (incf (dynamic ,stat-var))
	       (let ((,v  ,expvar))
		 ,@body))
	     ,expvar)
	   (def-dump-code ,temp)))))


;;; Helper function for above.

(define (def->core-name-string def)
  (if (con? def)
      (remove-con-prefix (symbol->string (def-name def)))
      (symbol->string (def-name def))))


;;; Dumpers for specific definitions

(define-walker-method dump var (var)
  (dump-var/n var))

(define (dump-var/n var)
  (with-new-def (dexp var *number-vars-dumped*)
    (do-dump-var dexp var '#f)))

(define (do-dump-var dexp var method-var?)
 (when (not (def-prelude? var))
    (setf dexp (def-dump-index var)))
 (mlet ((exported?       (def-exported? var))
	(toplevel?       (var-toplevel? var))
	(type            (var-type var))
	(simple?         (var-simple? var))
	(strict?         (var-strict? var))
	(always-inline?  (var-always-inline? var))
	(arity           (var-arity var))
	(strictness      (var-strictness var))
	(opt-entry       (var-optimized-entry var))
	(complexity      (var-complexity var))
	(fixity          (var-fixity var))
	(value           (var-value var))
	(inline-value    (var-inline-value var))
	(sel?            (var-selector-fn? var))
	(specializers    (var-specializers var))
	((file line)     (dump-source-pointer (def-where-defined var))))
    ;; Some slots are useless for vars that don't name functions.
    (if (eqv? arity 0)
	(add-dump-init
	  `(init-var-slots
	     ,dexp
	     ,exported?
	     ,toplevel?
	     ,(dump-object type)
	     ,simple?
	     ,strict?

	     ',file
	     ',line))
	(add-dump-init
	  `(init-fn-slots
	     ,dexp
	     ,exported?
	     ,toplevel?
	     ,(dump-object type)
	     ,simple?

	     ,strict?
	     ,arity
	     ,(dump-strictness strictness)
	     ',opt-entry
	     ',file

	     ',line)))
    ;; This is a special case for the prelude only
    (when (def-prelude? var)
      (add-dump-init `(setf (def-module ,dexp) ',(def-module var))))
    ;; These slots rarely need to be tweaked from the default.
    (when sel?
      (add-dump-init `(set-var-selector-fn? ,dexp #t)))
    (when always-inline?
      (add-dump-init `(set-var-always-inline? ,dexp #t)))
    (when complexity
      (add-dump-init `(set-var-complexity ,dexp ,complexity)))
    (when fixity
      (add-dump-init `(set-var-fixity ,dexp
				      ',(fixity-associativity fixity)
				      ,(fixity-precedence fixity))))
    ;; Save values of simple variables to permit inlining.
    ;; Save values of structured constants to permit folding of flic-sel
    ;; operations -- this is necessary to optimize dictionary lookups.
    ;; If value is not used, zap it to free up memory.
    (if (or sel?
	    (and value
		 (is-type? 'flic-app value)
		 (structured-constant-app?
		  (flic-app-fn value) (flic-app-args value)))
	    ;; optimizer uses inline-value in preference to value anyway!
	    (and simple? (not inline-value)))
	(add-dump-init `(set-var-value ,dexp ,(dump-flic-top value)))
	(setf (var-value var) '#f))
    (when inline-value
      (add-dump-init
        `(set-var-inline-value ,dexp ,(dump-flic-top inline-value))))
    (when (not (null? specializers))
      (add-dump-init
        `(set-var-specializers
	    ,dexp
	    (list ,@(map (lambda (s)
			   `(cons ,(dump-object (car s))
				  ,(dump-flic-top (cdr s))))
			 specializers)))))
    ;; Save extra stuff for method vars
    (when method-var?
      (add-dump-init
        `(init-method-var-slots
	   ,dexp
	   ,(dump-object (method-var-class var))
	   ,(dump-object (method-var-default var))
	   ,(dump-object (method-var-method-signature var)))))
    ))


(define-walker-method dump method-var (var)
  (dump-method-var/n var))

(define (dump-method-var/n var)
  (with-new-def (dexp var *number-vars-dumped*)
    (do-dump-var dexp var '#t)))

(define-walker-method dump con (con)
  (dump-con/n con))

(define (dump-con/n con)
  (with-new-def (dexp con *number-types-dumped*)
    (when (not (def-prelude? con))
      (setf dexp (def-dump-index con)))
    (mlet (((file line) (dump-source-pointer (def-where-defined con))))
      (add-dump-init
       `(init-con-slots
	 ,dexp
	 ,(con-arity con)
	 ,(dump-object (con-types con))
	 ,(dump-object (con-signature con))
	 ,(con-tag con)

	 ,(dump-object (con-alg con))
	 ,(dump-object (con-fixity con))
	 ,(con-infix? con)
	 ',file
	 ',line)))
    (when (memq '#t (con-slot-strict? con))
      (add-dump-init
        `(set-con-slot-strict? ,dexp
			       ,(dump-strictness (con-slot-strict? con)))))
    (when (not (null? (con-lisp-fns con)))
      (add-dump-init
        `(set-con-lisp-fns ,dexp ',(con-lisp-fns con))))))

(define-walker-method dump algdata (alg)
  (dump-algdata/n alg))

(define (dump-algdata/n alg)
  (with-new-def (dexp alg *number-types-dumped*)
    (when (not (def-prelude? alg))
      (setf dexp (def-dump-index alg)))
    (mlet (((file line) (dump-source-pointer (def-where-defined alg))))
     (add-dump-init
      `(init-algdata-slots
	 ,dexp
	 ,(def-exported? alg)
	 ,(algdata-arity alg)
	 ,(algdata-n-constr alg)
	 ,(dump-object (algdata-constrs alg))

	 ,(dump-object (algdata-context alg))
	 ,(dump-object (algdata-tyvars alg))
	 ,(dump-object (algdata-signature alg))
	 ,(algdata-enum? alg)
	 ,(algdata-tuple? alg)

	 ,(algdata-real-tuple? alg)
	 ,(algdata-implemented-by-lisp? alg)
	 ,(dump-object (algdata-runtime-var alg))
	 ',file
	 ',line)
      ))))
		

(define-walker-method dump synonym (syn)
  (dump-synonym/n syn))

(define (dump-synonym/n syn)
  (with-new-def (dexp syn *number-types-dumped*)
    (when (not (def-prelude? syn))
      (setf dexp (def-dump-index syn)))
    (mlet (((file line) (dump-source-pointer (def-where-defined syn))))
     (add-dump-init
      `(init-synonym-slots
         ,dexp
	 ,(def-exported? syn)
	 ,(synonym-arity syn)
	 ,(dump-object (synonym-args syn))
	 ,(dump-object (synonym-body syn))
	 ',file
	 ',line)
      ))))

(define-walker-method dump deriving (deriving)
  (dump-deriving/n deriving))

(define (dump-deriving/n deriving)
  (with-new-def (dexp deriving *number-types-dumped*)
    (when (not (def-prelude? deriving))
      (setf dexp (def-dump-index deriving)))
    (add-dump-init
      `(init-deriving-slots
         ,dexp
	 ,(dump-object (deriving-preconditions deriving))
	 ,(dump-object (deriving-instances deriving))
      ))))

(define-walker-method dump class (class)
  (dump-class/n class))

(define (dump-class/n class)
  (with-new-def (dexp class *number-classes-dumped*)
    (when (not (def-prelude? class))
      (setf dexp (def-dump-index class)))
    (mlet (((file line) (dump-source-pointer (def-where-defined class))))
     (add-dump-init
      `(init-class-slots
	 ,dexp
	 ,(dump-object (def-exported? class))
	 ,(dump-object (class-super class))
	 ,(dump-object (class-super* class))
	 ,(dump-object (class-tyvar class))
	 ,(dump-object (class-method-vars class))

	 ,(dump-object (class-selectors class))
	 ,(dump-object (class-kind class))
	 ,(class-n-methods class)
	 ,(class-dict-size class)
	 ,(dump-object (class-runtime-var class))
	 
	 ',file
	 ',line))
      )))

;;; The deriving definition needs to save instance decls

(define-dumper-methods
  (instance-decl (valdef depend-val dictionary-args extra-decls)
   single-fun-def guarded-rhs as-pat irr-pat var-pat wildcard-pat
   const-pat plus-pat pcon list-pat dynamic-pat lambda let if case
   (alt test) exp-sign app con-ref integer-const char-const
   string-const list-exp sequence omitted-guard))

(define-walker-method dump var-ref (var)
  (if (eq? (var-ref-var var) *undefined-def*)
      `(**var ',(var-ref-name var))
      `(**var/def ,(dump-object (var-ref-var var)))))
	


;;;==================================================================
;;; Dumpers for type-related structs
;;;==================================================================

;;; This section contains dumpers to handle type-related structs that
;;; are referenced by the various def guys.


(define-walker-method dump instance (o)
  (if (not (instance-ok? o))
      (error "Attempt to dump instance that's not ok!"))
  (mlet (((file line) (dump-source-pointer (ast-node-line-number o))))
     `(make-new-instance
       ,(dump-object (instance-algdata o))
       ,(dump-object (instance-tyvars o))
       ,(dump-object (instance-class o))
       ,(dump-object (instance-context o))
       ,(dump-object (instance-gcontext o))

       ,(dump-object (instance-dictionary o))
       ,(dump-object (instance-methods o))
       ,(dump-object (instance-runtime-var o))
       ',file
       ',line)))

(define-walker-method dump gtype (o)
  (let ((context  (gtype-context o))
	(type     (dump-gtype-type (gtype-type o))))
    (if (every (function null?) context)
	`(gtype/null ,(length context) ,type)
	`(gtype/n (list ,@(map (function dump-class-list) context)) ,type))))

(define (dump-class-list o)
  `(list ,@(map (function dump-class/n) o)))

(define (dump-gtype-type o)
  (let* ((code   (dump-gtype-type-aux o))
	 (entry  (assoc code *dump-types*)))
    (when (not entry)
      (push (setf entry (cons code *dump-type-counter*)) *dump-types*)
      (incf *dump-type-counter*))
    (cdr entry)))

(define (dump-gtype-type-aux o)
  (cond ((gtyvar? o)
	 `(**gtyvar ,(gtyvar-varnum o)))
	((ntyvar? o)
	 (dump-gtype-type-aux (prune o)))
	(else
	 (dump-ntycon o))))

(define (dump-ntycon o)
  (let* ((tycon  (ntycon-tycon o))
	 (stuff  (if (algdata? tycon)
		     (dump-algdata/n tycon)
		     (dump-synonym/n tycon)))
	 (args   (ntycon-args o)))
    (cond ((eq? tycon (core-symbol "Arrow"))
	   (dump-arrow-ntycon
	     (list (dump-gtype-type (car args)))
	     (cadr args)))
	  ((eq? tycon (core-symbol "List"))
	   `(list/n ,(dump-gtype-type (car args))))
	  ((not (def-prelude? tycon))
	   `(ntycon/def-n ,(def-dump-index tycon)
			  ,@(map (function dump-gtype-type) args)))
	  (else
	   `(ntycon/def ,stuff
			,@(map (function dump-gtype-type) args)))
	  )))

(define (dump-arrow-ntycon head next)
  (if (and (ntycon? next)
	   (eq? (ntycon-tycon next) (core-symbol "Arrow")))
      (let ((args  (ntycon-args next)))
	(dump-arrow-ntycon
	  (cons (dump-gtype-type (car args)) head)
	  (cadr args)))
      `(arrow/n ,@(nreverse head) ,(dump-gtype-type next))))


(define-walker-method dump fixity (o)
  `(**fixity ',(fixity-associativity o) ,(fixity-precedence o)))

(define-walker-method dump class-ref (o)
  `(**class/def ,(dump-object (class-ref-class o))))

(define-walker-method dump context (o)
  (let* ((class (class-ref-class (context-class o)))
	 (tyvar (context-tyvar o))
	 (stuff (dump-object class)))
    (if (def-prelude? class)
	`(context/def ,stuff ,(dump-object tyvar))
	`(context/def-n ,(def-dump-index class) ,(dump-object tyvar)))))

(define-walker-method dump tyvar (o)
  `(**tyvar ',(tyvar-name o)))


;;; Use this shorthand for tyvars in tycon/def and friends.

(define (dump-type o)
  (if (tyvar? o)
      `',(tyvar-name o)
      (dump-object o)))

(define-walker-method dump tycon (o)
  (let ((def   (tycon-def o))
	(args  (tycon-args o)))
    (if (eq? def (core-symbol "Arrow"))
	(dump-arrow-tycon (list (dump-type (car args))) (cadr args))
	(let ((stuff     (dump-object def))
	      (arg-code  (map (function dump-type) args)))
	  (if (def-prelude? def)
	      `(tycon/def ,stuff ,@arg-code)
	      `(tycon/def-n ,(def-dump-index def) ,@arg-code))))))

(define (dump-arrow-tycon head next)
  (if (and (tycon? next)
	   (eq? (tycon-def next) (core-symbol "Arrow")))
      (let ((args  (tycon-args next)))
	(dump-arrow-tycon
	  (cons (dump-type (car args)) head)
	  (cadr args)))
      `(tycon/arrow ,@(nreverse head) ,(dump-type next))))


(define-walker-method dump default-decl (o)
  `(make default-decl (types ,(dump-object (default-decl-types o)))))

(define-walker-method dump signature (o)
  `(make-sig ,(dump-object (signature-context o))
	     ,(dump-object (signature-type o))))

;;; All ntyvars should be instantiated at this point

; (define-walker-method dump ntyvar (o)
;  (dump-object (prune o)))



;;;==================================================================
;;; Dumpers for strictness
;;;==================================================================

;;; Precompute and cache argument strictness lists in a table.

(define *pre-defined-strictness-size* 7)  ; length of max strictness list
(define *pre-defined-strictness-table*
  (let* ((size  (expt 2 (1+ (dynamic *pre-defined-strictness-size*))))
	 (table (make-vector size)))
    (setf (vector-ref table 1) '())
    (do ((i 1 (1+ i))
	 (j 1 (* j 2))
	 (k 2 (* k 2)))
	((> i *pre-defined-strictness-size*))
	(do ((l 0 (1+ l)))
	    ((>= l j))
	    (setf (vector-ref table (+ k l))
		  (cons '#f (vector-ref table (+ j l))))
	    (setf (vector-ref table (+ k j l))
		  (cons '#t (vector-ref table (+ j l))))))
    table))

(define (dump-strictness s)
  (if (null? s)
      ''()
      (dump-strictness-1 s s 0 0)))

(define (dump-strictness-1 s s1 n size)
  (if (null? s1)
      (if (> size *pre-defined-strictness-size*)
	  (dump-big-strictness (- size *pre-defined-strictness-size*) s)
	  (let ((k (+ n (expt 2 size))))
	    `(strictness-n ,k)))
      (dump-strictness-1 s (cdr s1) (+ (* 2 n) (if (car s1) 1 0)) (1+ size))))

(define (dump-big-strictness k s)
  (if (= k 0)
      (dump-strictness s)
      `(cons ',(car s)
	     ,(dump-big-strictness (1- k) (cdr s)))))


;;; Runtime support for the above

(define (strictness-n x)
  (vector-ref (dynamic *pre-defined-strictness-table*) x))




;;;==================================================================
;;; Runtime support functions
;;;==================================================================

(define (lookup-imported-mod i)
  (vector-ref *modules-imported* i))

(define (lookup-defined-mod i)
  (vector-ref *modules-loaded* i))

(define (interface-def/n sym indices)
  (cons sym (map (function def-n) indices)))

(define (def-n i)
  (vector-ref *defs-referenced* i))

(define (set-def-n/method-var i module name)
  (setf (vector-ref *defs-referenced* i)
	(create-definition/inner
	  (if (symbol? module)
	      module
	      (module-name (lookup-defined-mod module)))
	  name
	  'method-var)))

(define (set-def-n/var i module name)
  (setf (vector-ref *defs-referenced* i)
	(create-definition/inner
	  (if (symbol? module)
	      module
	      (module-name (lookup-defined-mod module)))
	  name
	  'var)))

(define (set-def-n/con i module name)
  (setf (vector-ref *defs-referenced* i)
	(create-definition/inner
	  (if (symbol? module)
	      module
	      (module-name (lookup-defined-mod module)))
	  name
	  'con)))

(define (set-def-n/synonym i module name)
  (setf (vector-ref *defs-referenced* i)
	(create-definition/inner
	  (if (symbol? module)
	      module
	      (module-name (lookup-defined-mod module)))
	  name
	  'synonym)))

(define (set-def-n/deriving i module name)
  (setf (vector-ref *defs-referenced* i)
	(create-definition/inner
	  (if (symbol? module)
	      module
	      (module-name (lookup-defined-mod module)))
	  name
	  'di)))

(define (set-def-n/algdata i module name)
  (setf (vector-ref *defs-referenced* i)
	(create-definition/inner
	  (if (symbol? module)
	      module
	      (module-name (lookup-defined-mod module)))
	  name
	  'algdata)))

(define (set-def-n/class i module name)
  (setf (vector-ref *defs-referenced* i)
	(create-definition/inner
	  (if (symbol? module)
	      module
	      (module-name (lookup-defined-mod module)))
	  name
	  'class)))

(define (set-def-n/tuple-con i arity)
  (setf (vector-ref *defs-referenced* i)
	(tuple-constructor arity)))

(define (set-def-n/tuple-tycon i arity)
  (setf (vector-ref *defs-referenced* i)
	(tuple-tycon arity)))

(define (set-def-n/import i module-index name)
  (setf (vector-ref *defs-referenced* i)
	(table-entry (module-symbol-table (lookup-imported-mod module-index))
		     name)))


(define (type-n i)
  (vector-ref *types-referenced* i))

(define (set-type-n i value)
  (setf (vector-ref *types-referenced* i) value))


(define (set-export/def-n table index)
  (let* ((def (vector-ref *defs-referenced* index))
	 (key (def-name def)))
    (setf (table-entry table key)
	  (list (cons key def)))))

(define (set-export/def-n/list table indices)
  (dolist (index indices)
    (let* ((def (vector-ref *defs-referenced* index))
	   (key (def-name def)))
      (setf (table-entry table key)
	    (list (cons key def))))))

(define (set-export/def-n/key table index key)
  (let ((def (vector-ref *defs-referenced* index)))
    (setf (table-entry table key)
	  (list (cons key def)))))

(define (set-export/def table def)
  (let ((key  (def-name def)))
    (setf (table-entry table key)
	  (list (cons key def)))))

(define (set-export/def/key table def key)
  (setf (table-entry table key)
	(list (cons key def))))


(define (set-symtab/def-n table index)
  (let ((def  (vector-ref *defs-referenced* index)))
    (setf (table-entry table (def-name def)) def)))

(define (set-symtab/def-n/list table indices)
  (dolist (index indices)
    (let ((def  (vector-ref *defs-referenced* index)))
      (setf (table-entry table (def-name def)) def))))

(define (set-symtab/def-n/key table index key)
  (let ((def  (vector-ref *defs-referenced* index)))
    (setf (table-entry table key) def)))

(define (set-symtab/def table def)
  (setf (table-entry table (def-name def)) def))

(define (set-symtab/def/key table def key)
  (setf (table-entry table key) def))
	

(define (make-fixity-table data)
  (let ((table  (make-table)))
    (dolist (d data)
      (let ((key  (car d))
	    (ass  (cadr d))
	    (prec (caddr d)))
	(setf (table-entry table key)
	      (make fixity (associativity ass) (precedence prec)))))
    table))


(define (init-var-slots
	 var exported? toplevel? type simple?
	 strict? file line)
  (when (integer? var) (setf var (def-n var)))
  (setf (def-exported? var) exported?)
  (setf (var-toplevel? var) toplevel?)
  (setf (var-type var) type)
  (setf (var-simple? var) simple?)
  (setf (var-strict? var) strict?)
  (setf (def-where-defined var) (restore-source-pointer file line))
  var)

(define (init-fn-slots
	    var exported? toplevel? type simple?
	    strict? arity strictness opt-entry file
	    line)
  (when (integer? var) (setf var (def-n var)))
  (setf (def-exported? var) exported?)
  (setf (var-toplevel? var) toplevel?)
  (setf (var-type var) type)
  (setf (var-simple? var) simple?)
  (setf (var-strict? var) strict?)
  (setf (var-arity var) arity)
  (setf (var-strictness var) strictness)
  (setf (var-optimized-entry var) opt-entry)
  (setf (def-where-defined var) (restore-source-pointer file line))
  var)

(define (set-var-selector-fn? var value)
  (when (integer? var) (setf var (def-n var)))
  (setf (var-selector-fn? var) value))

(define (set-var-always-inline? var value)
  (when (integer? var) (setf var (def-n var)))
  (setf (var-always-inline? var) value))

(define (set-var-complexity var value)
  (when (integer? var) (setf var (def-n var)))
  (setf (var-complexity var) value))

(define (set-var-fixity var ass prec)
  (when (integer? var) (setf var (def-n var)))
  (setf (var-fixity var) (**fixity ass prec)))

(define (set-var-value var value)
  (when (integer? var) (setf var (def-n var)))
  (setf (var-value var) value))

(define (set-var-inline-value var value)
  (when (integer? var) (setf var (def-n var)))
  (setf (var-inline-value var) value))

(define (set-var-specializers var value)
  (when (integer? var) (setf var (def-n var)))
  (setf (var-specializers var) value))

(define (init-method-var-slots
	 var class default method-signature)
  (when (integer? var) (setf var (def-n var)))
  (setf (method-var-class var) class)
  (setf (method-var-default var) default)
  (setf (method-var-method-signature var) method-signature)
  var)

(define (init-con-slots
	   con arity types signature tag
	   alg fixity infix? file line)
  (when (integer? con) (setf con (def-n con)))
  (setf (con-arity con) arity)
  (setf (con-types con) types)
  (setf (con-signature con) signature)
  (setf (con-tag con) tag)
  (setf (con-alg con) alg)
  (setf (con-fixity con) fixity)
  (setf (con-infix? con) infix?)
  (when (null? (con-slot-strict? con))
    (dotimes (i arity)
      (push '#f (con-slot-strict? con))))
  (setf (def-where-defined con) (restore-source-pointer file line))
  con)

(define (set-con-slot-strict? con value)
  (when (integer? con) (setf con (def-n con)))
  (setf (con-slot-strict? con) value))

(define (set-con-lisp-fns con value)
  (when (integer? con) (setf con (def-n con)))
  (setf (con-lisp-fns con) value))
  

(define (init-algdata-slots
	   alg exported? arity n-constr constrs context
	   tyvars signature enum? tuple? real-tuple?
	   implemented-by-lisp? r file line)
  (when (integer? alg) (setf alg (def-n alg)))
  (setf (def-exported? alg) exported?)
  (setf (algdata-arity alg) arity)
  (setf (algdata-n-constr alg) n-constr)
  (setf (algdata-constrs alg) constrs)
  (setf (algdata-context alg) context)
  (setf (algdata-tyvars alg) tyvars)
  (setf (algdata-signature alg) signature)
  (setf (algdata-enum? alg) enum?)
  (setf (algdata-tuple? alg) tuple?)
  (setf (algdata-real-tuple? alg) real-tuple?)
  (setf (algdata-implemented-by-lisp? alg) implemented-by-lisp?)
  (setf (algdata-runtime-var alg) r)
  (setf (def-where-defined alg) (restore-source-pointer file line))
  alg)

(define (init-synonym-slots
      syn exported? arity args body
      file line)
  (when (integer? syn) (setf syn (def-n syn)))
  (setf (def-exported? syn) exported?)
  (setf (synonym-arity syn) arity)
  (setf (synonym-args syn) args)
  (setf (synonym-body syn) body)
  (setf (def-where-defined syn) (restore-source-pointer file line))
  syn)

(define (init-deriving-slots deriving classes insts)
  (when (integer? deriving) (setf deriving (def-n deriving)))
  (setf (deriving-preconditions deriving) classes)
  (setf (deriving-instances deriving) insts)
  deriving)

(define (init-class-slots
	   class exported? super super* tyvar
	   method-vars selectors kind n-methods dict-size
	   r file line)
  (when (integer? class) (setf class (def-n class)))
  (setf (def-exported? class) exported?)
  (setf (class-super class) super)
  (setf (class-super* class) super*)
  (setf (class-tyvar class) tyvar)
  (setf (class-method-vars class) method-vars)
  (setf (class-selectors class) selectors)
  (setf (class-kind class) kind)
  (setf (class-n-methods class) n-methods)
  (setf (class-dict-size class) dict-size)
  (setf (class-runtime-var class) r)
  (setf (def-where-defined class) (restore-source-pointer file line))
  class)
  

(define (make-new-instance
	   algdata tyvars class context gcontext
	   dictionary m r file line)
  (make instance
	(algdata algdata)
	(tyvars tyvars)
	(class class)
	(context context)
	(gcontext gcontext)
	(dictionary dictionary)
	(methods m)
	(ok? '#t)
	(runtime-var r)
	(line-number (restore-source-pointer file line))))

(define (tycon/arrow . args)
  (**arrow-type/l (map (function munge-tyvar) args)))

(define (tycon/def def . args)
  (**tycon/def def (map (function munge-tyvar) args)))

(define (tycon/def-n n . args)
  (**tycon/def (def-n n) (map (function munge-tyvar) args)))

(define (munge-tyvar arg)
  (if (symbol? arg)
      (**tyvar arg)
      arg))

(define (context/def def tyvar)
  (**context (**class/def def) tyvar))

(define (context/def-n n tyvar)
  (**context (**class/def (def-n n)) tyvar))

(define (make-sig context type)
  (make signature (context context) (type type)))



;;; All of these constructors for type-related objects permit an integer
;;; index into the type vector as a type argument, as well as real
;;; type objects.

(define (gtype/null n arg)
  (**gtype (make-list n '()) (expand-type arg)))

(define (gtype/n context arg)
  (**gtype context (expand-type arg)))

(define (ntycon/def-n n . args)
  (**ntycon (def-n n) (expand-types args)))

(define (ntycon/def def . args)
  (**ntycon def (expand-types args)))

(define (arrow/n . args)
  (**arrow/l (expand-types args)))

(define (list/n arg)
  (**list-of (expand-type arg)))

(define (expand-types args)
  (map (function expand-type) args))

(define (expand-type arg)
  (if (integer? arg)
      (type-n arg)
      arg))

;;; Stuff to support saving definition points

(define (dump-source-pointer sp)
  (if sp
      (values
       (dump-file-name (source-pointer-file sp)) (source-pointer-line sp))
      (values '#f '#f)))  

(define (dump-file-name str)
  (dump-file-name-1 str *dump-file-names* 0))

(define (dump-file-name-1 str names i)
  (cond ((null? names)
	 (setf *dump-file-names* (append *dump-file-names* (list str)))
	 i)
	((eq? str (car names))
	 i)
	(else (dump-file-name-1 str (cdr names) (1+ i)))))

(define (restore-source-pointer file line)
  (if file
      (make source-pointer (file (list-ref *dump-file-names* file))
	    (line line))
      '#f))
