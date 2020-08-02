
;;; This creates runtime definitions for the datatypes

(define (add-runtime-type-definitions)
  (dolist (alg (module-alg-defs *module*))
    (add-new-module-def
     (algdata-runtime-var alg)
     (**app (**con/def (core-symbol "MkDataType"))
	    (**string (symbol->string (def-name alg)))  ; Name
	    (**string-to-symbol                         ; FullName
  	      (**string (string-append
			 (symbol->string (def-module alg))
			 ":"
			 (symbol->string (def-name alg)))))
	    (**int (length (algdata-tyvars alg)))       ; Arity
	    (generate-con-list alg)
	    (**bool (algdata-tuple? alg))               ; Tuple
	    (**bool (algdata-enum? alg))                ; Enum
	    (**bool (algdata-real-tuple? alg))          ; RealTuple
	    (**app (**var/def (core-symbol "fetchInstances"))
		   (**var/def (algdata-runtime-var alg))
		   (**var/def (core-symbol "allInstances")))
	    (generate-con-test alg))))

    ;; Note: major magic is used to plug the instances into the data types
    ;; at runtime.  See runtime-utils.scm.

  (dolist (class (module-class-defs *module*))
    (add-new-module-def
     (class-runtime-var class)
     (**app (**con/def (core-symbol "MkClass"))
	    (**string (symbol->string (def-name class)))  ; Name
	    (**string-to-symbol                         ; FullName
	      (**string (string-append
			 (symbol->string (def-module class))
			 ":"
			 (symbol->string (def-name class)))))
	    (**list/l
	     (map (lambda (x) (**var/def (class-runtime-var x)))
		  (class-super* class)))
	    (**list/l
	     (map (lambda (c) (**cast
                               (**lambda '(x)
				(**cast
				 (**dsel/dict class c (**cast (**var 'x)))))))
		  (class-super* class))))))
  (dolist (inst (module-instance-defs *module*))
    (add-new-module-def
     (instance-runtime-var inst)
     (**app (**con/def (core-symbol "MkInstance"))
	    (rconvert-tycon (instance-algdata inst))
	    (**var/def (class-runtime-var (forward-def (instance-class inst))))
	    (make overloaded-var-ref
		  (var (instance-dictionary inst))
		  (sig (**gtype '() (**ntycon (core-symbol "Magic") '()))))
	    (rconvert-context (instance-gcontext inst)))))
  (add-new-module-def
    (make-new-var (symbol->string (module-instance-var-name *module*)))
    (**list/l (map (lambda (i) (**var/def (instance-runtime-var i)))
		   (module-instance-defs *module*)))))

(define (con->pat c)
  (let ((l '()))
    (dotimes (i (con-arity c))
      (push '_ l))
    (cons c l)))

(define (create-runtime-selector-fns con i n)
  (if (>= i n)
      '()
      (cons (**cast (**lambda '(x)
			      (**sel con (**var 'x) i)))
	    (create-runtime-selector-fns con (1+ i) n))))

(define (rconvert-fixity f)
  (if (eq? f '#f)
      (**con/def (core-symbol "NoFixity"))
      (let ((a (fixity-associativity f)))
       (**app (**con/def (cond ((eq? a 'l) (core-symbol "InfixL"))
			       ((eq? a 'r) (core-symbol "InfixR"))
			       ((eq? a 'n) (core-symbol "InfixN"))))
	      (**int (fixity-precedence f))))))

(define (**string-to-symbol x)
  (**app (**var/def (core-symbol "stringToSymbol")) x))

;;; Stuff for constructors

;;; This function produces Haskell code that will create a list
;;; of Constructor objects for a data type

(define (generate-con-list alg)
  (if (algdata-enum? alg)
      (if (algdata-implemented-by-lisp? alg)
	  (**app
	      (**var/def (core-symbol "createLispEnumConstructors"))
	      (**var/def (algdata-runtime-var alg))
	      (**string (create-enum-constructor-name-string alg))
	      (**list/l
	       (map (lambda (con) (**cast (**con/def con)))
		    (algdata-constrs alg))))
	  (**app 
              (**var/def (core-symbol "createEnumConstructors"))
	      (**var/def (algdata-runtime-var alg))
	      (**string (create-enum-constructor-name-string alg))))
      (mlet (((s ac at) (create-constructor-name-string alg))
	     (class-list (**list/l (map (lambda (c)
					  (**var/def (class-runtime-var c)))
					ac)))
	     (type-list (**list/l (map (function rconvert-tycon) at))))
	 (**app (**var/def (core-symbol "createConstructors"))
		(**var/def (algdata-runtime-var alg))
		(**string s)
		class-list
		type-list
		(if (algdata-implemented-by-lisp? alg)
		    (**list/l (map
			       (lambda (c)
			         (let ((fns (create-runtime-selector-fns
					     c 0 (con-arity c))))
				   (**list/l 
				    (cons (**cast (**con/def c)) fns))))
			       (algdata-constrs alg)))
		    (**null))))))

(define (create-enum-constructor-name-string alg)
  (call-with-output-string
   (lambda (p)
     (dolist (c (algdata-constrs alg))
       (format p "~A~%" (remove-con-prefix (symbol->string (def-name c))))))))

(define (create-constructor-name-string alg)
 (let* ((all-types '())
	(all-classes '())
	(str
	 (call-with-output-string
	  (lambda (p)
	    (dolist (c (algdata-constrs alg))
	     (format p "~A;" (remove-con-prefix (symbol->string (def-name c))))
	     (when (con-fixity c)
	       (format p "~A~A" (fixity-associativity (con-fixity c))
		                (fixity-precedence (con-fixity c))))
	     (format p ";")
	     (mlet (((ac at)
		 (encode-signature (con-signature c) p all-classes all-types)))
	      (setf all-classes ac)
	      (setf all-types at)
	      (format p ";")
	      (dolist (s (con-slot-strict? c))
		 (format p "~A" (if s "S" "N")))
	      (format p ";~A;~%" (if (con-infix? c) "I" ""))))))))
    (values str all-classes all-types)))

(define (encode-signature s p ac at)
  (let ((ac1 (encode-context (gtype-context s) ac p)))
    (values ac1 (encode-type (gtype-type s) at p))))

(define (encode-context cs ac p)
  (write-char '#\[ p)
  (let ((s '#t))
    (dolist (c cs)
       (if s
          (setf s '#f)
	  (write-char '#\, p))
       (write-char #\[ p)
       (let ((s1 '#t))
	 (dolist (ctxt c)
	   (if s1
             (setf s1 '#f)
	     (write-char '#\, p))
           (mlet (((ac1 i) (encode-ct (forward-def ctxt) ac ac 0)))
	     (format p "~A" i)
	     (setf ac ac1)))
	 (write-char '#\] p)))
    (write-char '#\] p))
  ac)

(define (encode-ct ct cts all i)
  (if (null? cts)
      (values (append all (list ct)) i)
      (if (eq? ct (car cts))
	  (values all i)
	  (encode-ct ct (cdr cts) all (1+ i)))))

(define (encode-type ty at p)
  (setf ty (expand-ntype-synonym ty))
  (if (gtyvar? ty)
      (begin 
	(format p "~A" (gtyvar-varnum ty))
	at)
      (mlet (((at1 i) (encode-ct (forward-def (ntycon-tycon ty)) at at 0)))
	(format p "~A(" i)
	(encode-type/l (ntycon-args ty) '#t at1 p))))

(define (encode-type/l tys s at p)
  (cond ((null? tys)
	 (write-char #\) p)
	 at)
	(else
	 (unless s (write-char #\, p))
	 (let ((at1 (encode-type (car tys) at p)))
	   (encode-type/l (cdr tys) '#f at1 p)))))

;;; This returns a Haskell function which will return a constructor object
;;; at rutime.

(define (generate-con-test alg)
  (cond ((algdata-implemented-by-lisp? alg)
	 (**app (**var/def (core-symbol "makeLispConstrFn"))
		(**var/def (algdata-runtime-var alg))
		(**list/l
		 (map (lambda (con)
			(**cast
			 (**lambda '(x) (**is-constructor (**var 'x) con))))
		      (algdata-constrs alg)))))
	((algdata-tuple? alg)
	 (**app (**var/def (core-symbol "makeGTupleConstrFn"))
		(**var/def (algdata-runtime-var alg))))
	((algdata-enum? alg)
	 (**app (**var/def (core-symbol "makeEnumConstrFn"))
		(**var/def (algdata-runtime-var alg))))
	(else
	 (**app (**var/def (core-symbol "makeConstrFn"))
		(**var/def (algdata-runtime-var alg))))))


		     

