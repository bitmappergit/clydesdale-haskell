;;; This generates code for vars defined in an interface.  This looks at
;;; annotations and fills in the slots of the var definition.

;;; This is all rather crufty.  Here are some of the issues:
;;;  There is a lot of crud in the Haskell type system that needs fiddling:
;;;  The IO monad should be removed - this really just indicates to
;;;   Haskell that the global state is being used.  Thus a signature of
;;;    Int -> IO Float
;;;   is really Int -> Float at the Lisp / C level.  There is also a dummy
;;;   system state argument to be removed too.
;;;  Lists of chars and Strings are two different data types in Lisp.  For
;;;   the automatic conversion, the types String and [Char] are treated
;;;   differently!!!
;;;  Type synonyms are NOT expanded - we use synonyms to define Lisp / C
;;;   datatypes which may be ambiguous or unavailable at the Haskell level.
;;;   For example, a int and an unsigned are both just Integer in Haskell
;;;   but synonyms indicate what sort of conversion is used across the
;;;   interface.

(define (haskell-codegen/interface mods)
  (let ((functions  '())
	(predefines '())
	(inits      '())
	(has-foreigns? '#f)
	(has-non-foreigns? '#f))
    (dolist (m mods)
      (when (or (module-alg-defs m) (module-synonym-defs m)
		(module-class-defs m) (module-synonym-defs m))
	    (setf has-non-foreigns? '#t))
      (dolist (d (module-decls m))
	(when (not (signdecl? d))
	  (error "Bad decl in interface file: ~s" d))
	(dolist (v (signdecl-vars d))
	  (multiple-value-bind (fn predefine init)
	      (codegen/interface (var-ref-var v))
	    (when (or (memq 'codegen (dynamic *printers*))
		      (memq 'codegen-flic (dynamic *printers*)))
	      (when fn (pprint* fn))
	      (when init (pprint* init)))
	    (if (or fn predefine init)
		(setf has-foreigns? '#t)
		(setf has-non-foreigns? '#t))
	    (when fn (push fn functions))
	    (when predefine (push predefine predefines))
	    (when init (push init inits))))))
    (when (and has-foreigns? has-non-foreigns?)
      (phase-error 'mixed-interface
         "Foreign interface module contains non-foreign definitions"))
    (when has-foreigns?
      (dolist (m mods)
	;; Definitions in a stand alone module never need forwarding
	;; or checking against implementations
	(setf (module-interface-definitions m) '()) 
	(setf (module-stand-alone? m) '#t)))
    `(begin ,@(nreverse predefines)
	    ,@(nreverse functions)
	    ,@(codegen-initcode (nreverse inits)))))


;;; Loop over all signature declarations in the interface.  Staple the
;;; types to the variables.  The following annotations are processed:
;;;  Complexity - attached to var
;;;  LispName - attached to van & wrapper generated
;;;  CName - attached to van & wrapper generated

(define (codegen/interface v)
  (let ((a  '#f))
    (setf (var-type v) (var-signature v))
    (setf (var-toplevel? v) '#t)
    (when (setf a (lookup-annotation v '|Complexity|))
      (setf (var-complexity v)
	    (car (annotation-value-args a))))
    (cond ((setf a (lookup-annotation v '|LispName|))
	   (generate-lisp-entry v a))
	  ((setf a (lookup-annotation v '|CName|))
	   (generate-c-entry v a))
	  (else
	   (values '#f '#f '#f))
	  )))


;;; This generates a wrapper for a Lisp functions.  The wrapper function
;;; does the following:
;;;   Converts a fixed library of datatypes between Lisp & Haskell
;;;   representations
;;;   Adds the dummy arguemnt to the monad.  Since IO a = 
;;;      SystemState_ -> IOResult_ a an argument will be passed for SystemState;
;;;      this is ignored.

(define (generate-lisp-entry v a)
  (mlet ((lisp-name (read-lisp-object (car (annotation-value-args a))))
	 ((args res io?) (massage-haskell-type (gtype-type (var-type v)))))
    (setf (var-optimized-entry v) lisp-name)
    (if (and (null? args) (not io?))
	(codegen-lisp-const v res)
	(codegen-lisp-fn v args res io?))))


;;; When the name does not have a functional or IO type, it is a simple
;;; constant.  It is always assumed to be non-strict.  A conversion is
;;; inserted if necessary.  (Bug: this doesn't look at the NoConversion flag).

(define (codegen-lisp-const var type)
  (let ((lisp-conversion-fn (output-lisp-conversion-fn type))
	(lispname           (var-optimized-entry var)))
    (when (not (bound? lispname))
      (signal-undefined-lisp-variable lispname))
    (setf (var-strict? var) '#f)
    (setf (var-arity var) 0)
    (setf (var-strictness var) '())
    (values
      '#f
      `(define ,(fullname var) '#f)
      `(setf ,(fullname var)
             (delay
	       ,(apply-conversion lisp-conversion-fn lispname))))
    ))

;;; This generates the Haskell level wrapper for a function and
;;; sets up Haskell level attributes.

(define (codegen-foreign-function var strictness fn-code)
  (setf (var-strict? var) '#t)
  (setf (var-arity var) (length strictness))
  (setf (var-strictness var) strictness)
  `(setf ,(fullname var)
     ,(maybe-make-box-value
       (codegen-curried-fn fn-code strictness)
       '#t)))

(define (codegen-lisp-fn var arg-types res-type io?)
  (let* ((wrapper?  (foreign-fn-needs-wrapper? var arg-types res-type io?))
	 (strictness-ann (lookup-annotation var '|Strictness|))
	 (strictness (determine-strictness strictness-ann arg-types io?))
	 (lispname   (var-optimized-entry var))) 
    (when (not (fbound? lispname))
      (signal-undefined-lisp-function lispname))
    ;; The optimized-entry slot contains the lisp function name - when a
    ;; wrapper is added this is moved into the wrapper and the optimized
    ;; entry receives the wrapper function name.
    (if wrapper?
	(multiple-value-bind (wrapper-code name)
	    (make-wrapper-fn var lispname arg-types res-type io?)
	  (setf (var-optimized-entry var) name)
	  (values
	    wrapper-code
	    `(define ,(fullname var) '#f)
	    (codegen-foreign-function
	      var strictness `(function ,name))))
	;; Since the definition may be a macro of some sort wrap a
	;; lambda around the function name.
	(values
	  '#f
	  `(define ,(fullname var) '#f)
	  (codegen-foreign-function
  	    var
	    strictness
	    (if (syntax? lispname)
		(let ((temps (gen-temp-names strictness)))
		  `(lambda ,temps (,lispname ,@temps)))
	      `(function ,lispname))))
	)))


(define (signal-undefined-lisp-variable name)
  (haskell-warning 'undefined-lisp-variable
		   "Undefined Lisp variable ~s in interface." name))

(define (signal-undefined-lisp-function name)
  (haskell-warning 'undefined-lisp-function
		   "Undefined Lisp function ~s in interface." name))




;;; This computes the strictness of an imported Lisp / C function.  If the
;;; strictness is explicitly provided use this (not available for C
;;; functions) otherwise make every argument strict.  If the IO monad is
;;; referenced, make the hidden system state parameter nonstrict.

(define (determine-strictness a args io?)
  (let ((s (if (eq? a '#f)
	       (map (lambda (x) (declare (ignore x)) '#t) args)
	       (parse-strictness (car (annotation-value-args a))))))
	;; IO functions have a hidden extra argument that is always
	;; nonstrict.
        (if io? (append s '(#f)) s)))


;;; This processes a Haskell type and delivers:
;;;   argument types
;;;   result type
;;;   flag to indicate whether the IO monad is being used
;;;  In general, type synonyms are not expanded.   Synonyms which expand 
;;;  to arrow types (other than Dialogue) DON'T WORK!!

(define (massage-haskell-type ty)
  (cond ((arrow-type? ty)
	 (multiple-value-bind (args res io?)
	     (massage-haskell-type (cadr (ntycon-args ty)))
	   (values (cons (car (ntycon-args ty)) args) res io?)))
	((ntycon? ty)
	 (let ((tycon (ntycon-tycon ty)))
	   (cond ((eq? tycon (core-symbol "IO"))
		  (values '() (car (ntycon-args ty)) '#t))
		 (else (values '() ty '#f)))))
	(else (values '() ty '#f))))

;;; This generates a Lisp input conversion function (represented by a function
;;; which is applied to the code which evaluates to the argument; no conversion
;;; would be the identity function but we use #f instead so that we can easily
;;; detect arguments with no conversion and avoid an assignment).

;;; Numerics and Booleans do not need conversion since these have the same
;;; representation in Lisp and Haskell.  Characters, strings, and lists
;;; are converted.  Note that type String is converted to a Lisp string
;;; while [Char] would be converted to a list of characters.  Also, the
;;; list conversion recursively converts the inner type.

;;; Characters are represented as integers in Haskell; integer->char makes
;;; the Lisp characters.

;;; Both strings & lists are strictified.

(define (input-lisp-conversion-fn ty)
  (if (ntycon? ty)
      (let ((tycon (ntycon-tycon ty)))
	(cond ((eq? tycon (core-symbol "String"))
	       (lambda (x) `(haskell-string->string ,x)))
	      ((eq? tycon (core-symbol "List"))  ; needs to convert elements
	       (let ((var (gensym "X"))
		     (inner-fn (input-lisp-conversion-fn (car (ntycon-args ty)))))
		 (lambda (x) `(haskell-list->list
			       (lambda (,var)
				 ,(if (eq? inner-fn '#f)
				      var
				      (funcall inner-fn var)))
			       ,x))))
	      ((eq? tycon (core-symbol "Char"))
	       (lambda (x) `(integer->char ,x)))
	      (else '#f)))
      '#f))

;;; This is similar to the input conversion function except that a
;;; couple of extra special cases exist.

;;;  When the output is of the unit type, the actual value returned by
;;;  the Lisp function is ignored and the unit is generated directly.

(define (output-lisp-conversion-fn ty)
  (if (ntycon? ty)
      (let ((tycon (ntycon-tycon ty)))
	(cond ((eq? tycon (core-symbol "String"))
	       (lambda (x) `(make-haskell-string ,x)))
	      ((eq? tycon (core-symbol "List"))
	       (let ((var (gensym "X"))
		     (inner-fn (output-lisp-conversion-fn
				(car (ntycon-args ty)))))
		 (lambda (x) `(list->haskell-list
			       (lambda (,var)
				 ,(if (eq? inner-fn '#f)
				      var
				      (funcall inner-fn var)))
			       ,x))))
	      ;; For the unit type we need to evaluate the value - insert
	      ;; unit type evaluates its arg and returns the unit.
	      ((eq? tycon (core-symbol "UnitType"))
	       (lambda (x) `(insert-unit-value ,x)))
	      ((eq? tycon (core-symbol "Char"))
	       (lambda (x) `(char->integer ,x)))
	      (else '#f)))
      '#f))

;;; This makes #f behave as the identity function for value conversion.

(define (apply-conversion fn x)
  (if (eq? fn '#f)
      x
      (funcall fn x)))

;;; This determines whether a foreign function actually needs a wrapper
;;; (This wrapper is distinct from the standard Haskell wrapper used for
;;; uncurrying and strictness optimizations).  When no conversions are
;;; needed or the NoConversion annotation is present the wrapper is
;;; omitted.

(define (foreign-fn-needs-wrapper? var args res io?)
 (cond ((lookup-annotation var '|NoConversion|)  '#f)
       (io?                                      '#t)
       ((output-lisp-conversion-fn res)               '#t)
       (else (some (lambda (x) (input-lisp-conversion-fn x)) args))))

;;; This creates the code for the wrapper.

(define (make-wrapper-fn var fn arg-types res-type io?)
  (let* ((new-fn (symbol-append (fullname var) '|/wrapper|))
	 (avars (gen-temp-names arg-types))
	 (arg-conversions (collect-conversion-fns
			   (function input-lisp-conversion-fn)
			   avars arg-types))
	 (res-conversion (output-lisp-conversion-fn res-type))
	 (fn-call (apply-conversion res-conversion `(,fn ,@avars))))
     (values
      (if io?
	  `(define (,new-fn ,@avars ignored-state)
	     (declare (ignore ignored-state))
	     ,@arg-conversions
	     (io-return ,fn-call))
	  `(define (,new-fn ,@avars)
	     ,@arg-conversions
	     ,fn-call))
      new-fn)))

;;; This converts incoming values by generating a setf for each arg
;;; needing conversion.

(define (collect-conversion-fns fn avars arg-types)
  (if (null? arg-types)
      '()
      (let ((cfn (funcall fn (car arg-types)))
	    (rest (collect-conversion-fns fn (cdr avars) (cdr arg-types))))
	(if cfn
	    `((setf ,(car avars) ,(funcall cfn (car avars))) ,@rest)
	    rest))))

;;; Some random utilities

(define (arrow-type? x)
  (and (ntycon? x)
       (eq? (ntycon-tycon x) (core-symbol "Arrow"))))

(define (systemstate? x)
  (and (ntycon? x)
       (eq? (ntycon-tycon x) (core-symbol "SystemState_"))))


;;; Stuff to support the C interface

(define (generate-c-entry v a)
  (mlet ((c-name (car (annotation-value-args a)))
	 ((args res io?) (massage-haskell-type (gtype-type (var-type v)))))
    (if (and (null? args) (not io?))
	;; I'm too lazy to write the interface to C constants
	(phase-error 'c-routine-error "Not a function type: ~A" v)
	(codegen-c-fn v c-name args res io?))))

;;; Every C function generates a C type template, a Lisp wrapper, and a Haskell
;;; wrapper.

(define (codegen-c-fn var c-name arg-types res-type io?)
  (let* ((strictness (determine-strictness '#f arg-types io?))
	 (c-types (map (function convert-to-c-type) arg-types))
	 (c-res-type (convert-to-c-type res-type)))
    (multiple-value-bind (code c-type-def name)
	(make-c-wrapper-fn var c-name c-types c-res-type io?)
      (setf (var-optimized-entry var) name)
      (values
        `(begin ,code ,c-type-def )
	`(define ,(fullname var) '#f)
	(codegen-foreign-function var strictness
				  `(function ,(var-optimized-entry var)))))))

;;; This generates a C -> Lisp wrapper.

(define (make-c-wrapper-fn var c-name arg-types res-type io?)
  (mlet ((new-fn (symbol-append (fullname var) '|/wrapper|))
	 (new-lisp-fn (symbol-append (fullname var) '|/c-entry|))
	 (avars (gen-temp-names arg-types))
	 (arg-conversions (collect-conversion-fns
			   (function input-c-conversion-fn)
			   avars arg-types))
	 (res-conversion (output-c-conversion-fn res-type))
	 (fn-call (apply-conversion res-conversion `(,new-lisp-fn ,@avars))))
     (values
      (if io?
	  `(define (,new-fn ,@avars ignored-state)
	     (declare (ignore ignored-state))
	     ,@arg-conversions
	     ,fn-call)
	  `(define (,new-fn ,@avars)
	     ,@arg-conversions
	     ,fn-call))
      `(define-c-function ,c-name ,new-lisp-fn ,res-type ,@arg-types)
      new-fn)))

(define (input-c-conversion-fn ty)
  (cond ((eq? ty ':c-string)
	 (lambda (x) `(haskell-string->string ,x)))
	((eq? ty ':char)
	 (lambda (x) `(integer->char ,x)))
	(else
	 '#f)))

(define (output-c-conversion-fn ty)
  (cond ((eq? ty ':c-string)
	 (lambda (x) `(make-haskell-string ,x)))
	((eq? ty ':void)
	 (lambda (x) `(insert-unit-value ,x)))
	((eq? ty ':char)
	 (lambda (x) `(char->integer ,x)))
	(else
	 '#f)))


;;; This converts a Haskell type to a C type.  The one special case here is
;;; that a unit type is translated to :void.

(define (convert-to-c-type ty)
  (if (and (ntycon? ty)
	   (eq? (ntycon-tycon ty) (core-symbol "UnitType")))
      ':void
      (let ((ctype (haskell-type->c-type ty)))
	(or ctype
	    (phase-error 'c-type-required "Not a C type: ~A" ty)))))

;;; This is the basic type converter - a fixed set of C types, as declared in
;;; PreludeC, is translated into the C type system used by mumble.

(define (haskell-type->c-type ty)
  (if (not (ntycon? ty))
      '#f
      (let* ((tycon (ntycon-tycon ty))
     	     (name (def-name tycon)))
	(if (not (def-core? tycon))
	    '#f
	    (cond ((eq? name '|C_char|)
		   :char)
	          ((eq? name '|C_short|)
		   :short)
	          ((eq? name '|C_int|)
		   :int)
	          ((eq? name '|C_long|)
		   :long)
	          ((eq? name '|C_unsigned_char|)
		   :unsigned-char)
	          ((eq? name '|C_unsigned_short|)
		   :unsigned-short)
	          ((eq? name '|C_unsigned_int|)
		   :unsigned-int)
	          ((eq? name '|C_unsigned_long|)
		   :unsigned-long)
	          ((eq? name '|C_float|)
		   :float)
	          ((eq? name '|C_double|)
		   :double)
	          ((eq? name '|C_void|)
		   :void)
	          ((eq? name '|C_bool|)
		   :bool)
	          ((eq? name '|C_string|)
		   :c-string)
		  (else (error "Invalid C type ~s." tycon))
		  )))))
