;;; codegen.scm -- compile flic code to Lisp
;;;
;;; Author :  Sandra Loosemore
;;; Date   :  29 Apr 1992
;;;
;;; to do:  check completeness of special cases for constructors
;;;         constants still need work
;;;         optimized entry points
;;;
;;; The code generated here uses the following helper functions:
;;; (make-curried-fn opt-fn strictness)
;;;   make a curried function that calls opt-fn after collecting the
;;;   arguments and processing them according to strictness.  Both
;;;   the arguments are evaluated.
;;; (make-tuple-constructor arity strictness)
;;;   return a function that makes an untagged data structure with "arity" 
;;;   slots.  "arity" is a constant.
;;; (make-tuple . args)
;;;   uncurried version of the above
;;; (make-tagged-data-constructor n arity strictness)
;;;   return a function that makes a data structure with tag "n" and
;;;   "arity" slots.
;;; (make-tagged-data n . args)
;;;   uncurried version of the above
;;; (tuple-select arity i object)
;;;   extract component "i" from untagged "object"
;;; (tagged-data-select arity i object)
;;;   extract component "i" from tagged "object"
;;; (constructor-number object)
;;;   return the tag from "object"
;;; (delay form)
;;;   returns a delay object with unevaluated "form".
;;; (box form)
;;;   returns a delay object with evaluated "form".
;;; (force delay)
;;;   return the value of the delay object.
;;; (make-haskell-string string)
;;;   Converts a Lisp string lazily to a haskell string (using a magic
;;;   delay function).  Returns an unboxed result.



;;;======================================================================
;;; Code walker
;;;======================================================================


;;; Here is the main entry point.

(define *interface-vars-referenced*  '())

(define (codegen-top big-let)
  (if (flic-void? big-let)
      `(begin
	 ,@(codegen-initcode '()))
      (dynamic-let ((*interface-vars-referenced*  '()))
        (do ((bindings (flic-let-bindings big-let) (cdr bindings))
	     (functions   '())
	     (predefines  '())
	     (inits       '()))
  	    ((null? bindings)
	     `(begin ,@(nreverse predefines)
		     ,@(map (lambda (v) `(predefine ,(fullname v)))
			    (dynamic *interface-vars-referenced*))
		     ,@(nreverse functions)
		     ,@(codegen-initcode (nreverse inits))))
 	    (let ((var  (car bindings)))
	      (when (or (memq 'codegen (dynamic *printers*))
		        (memq 'codegen-flic (dynamic *printers*)))
	        (format '#t "~%Codegen of ~A  " (def-name var))
	        (when (not (var-strict? var))
		  (format '#t "Nonstrict  "))
	        (when (not (eq? (var-strictness var) '()))
		  (format '#t "Strictness: ")
		  (dolist (s (var-strictness var))
		    (format '#t (if s "S " "N "))))
		(when (var-simple? var)
		  (format '#t " Inline "))
		(format '#t "~%")
		(when (memq 'codegen-flic (dynamic *printers*))
		  (pprint* (var-value var))))
	      (multiple-value-bind (function predefine init)
		  (codegen-definition var (var-value var))
		(when (or (memq 'codegen (dynamic *printers*))
			  (memq 'codegen-flic (dynamic *printers*)))
		  (when function (pprint* function))
		  (when init (pprint* init)))
		(when function (push function functions))
		(when predefine (push predefine predefines))
		(when init (push init inits))))))))


;;; Chunk initialization forms into reasonably-sized functions to avoid
;;; giving the Lisp compiler too much trouble with them.

(define *initcode-chunk-size* 20)
(define *initcode-function-name* 'initcode)
(define *initcode-function* '#f)


(define (codegen-initcode inits)
  (let ((fn-name  (dynamic *initcode-function-name*)))
    `(,@(if (> (the fixnum (length inits)) (the fixnum *initcode-chunk-size*))
	    (multiple-value-bind (fn-defs fn-calls)
		(codegen-initcode-aux inits fn-name 1)
	      `(,@fn-defs
		(define (,fn-name) ,@fn-calls '#t)))
	    `((define (,fn-name) ,@inits '#t)))
      (setf *initcode-function* (function ,fn-name))
      (,fn-name))))
       

(define (codegen-initcode-aux inits fn-name part)
  (let ((tail  (list-tail inits *initcode-chunk-size*))
	(name  (string->symbol (format '#f "~a-part~a" fn-name part))))
    (if (or (null? tail) (null? (cdr tail)))
	(values
	  (list `(define (,name) ,@inits))
	  (list `(,name)))
	(let ((next  (cdr tail)))
	  (setf (cdr tail) '())
	  (multiple-value-bind (fn-defs fn-calls)
	      (codegen-initcode-aux next fn-name (1+ part))
	    (values
	      (cons `(define (,name) ,@inits) fn-defs)
	      (cons `(,name) fn-calls))))
      )))



;;; For top-level definitions bound to lambda expressions, make both
;;; a standard entry point (with possibly unboxed arguments) and
;;; a standard entry point.

(define (codegen-definition var exp)
  (let ((fullname  (fullname var)))
    (if (not (flic-lambda? exp))
	;; Simple variable definition only.
	(values
	  '#f
	  `(define ,fullname '#f)
	  `(setf ,fullname ,(do-codegen exp)))
	(let* ((optname  (optname var))
	       (lambda   (codegen-lambda-aux exp))
	       (def      `(define (,optname ,@(cadr lambda))
			    ,@(cddr lambda))))
	  (if (var-selector-fn? var)
	      ;; Standard entry point for selectors is never used,
	      ;; so don't generate variable definition.
	      (values
	        def
		'#f
		'#f)
	      ;; Generate both function and variable definitions.
	      (values
	        def
		`(define ,fullname '#f)
		`(setf ,fullname
		       ,(maybe-make-box-value
			  (codegen-curried-fn
			   `(function ,optname) (var-strictness var))
			  (var-strict? var))))
	    )))))



;;; See box.scm for more information about this...

(define (do-codegen object)
  (let ((x               (codegen object))
	(unboxed?        (flic-exp-unboxed? object))
	(strict-result?  (flic-exp-strict-result? object))
	(cheap?          (flic-exp-cheap? object)))
    (if unboxed?
	(if strict-result?
	    x
	    (if cheap?
		`(unbox ,x)
		(make-force x)))
	(if strict-result?
	    (if (and (pair? x)
		     (or (eq? (car x) 'delay-funcall)
			 (eq? (car x) 'delay-funcall/force)))
		;; see flic-app, below
		x
		(if cheap?
		    `(box ,x)
		    `(delay ,x ,(thunk-name object))))
	    (if cheap?
		x
		`(delay (force ,x) ,(thunk-name object)))))
    ))


;;; Some accessors have equivalents that do the force.

(define *force-equivalents*
  '((car . force-car)
    (cdr . force-cdr)
    (tuple-select . force-tuple-select)
    (tagged-data-select . force-tagged-data-select)
    (car/force . force-car/force)
    (cdr/force . force-cdr/force)
    (tuple-select/force . force-tuple-select/force)
    (tagged-data-select/force . force-tagged-data-select/force)
    ))

(define (make-force x)
  (if (pair? x)
      (let ((stuff  (assq (car x) *force-equivalents*)))
	(if stuff
	    `(,(cdr stuff) ,@(cdr x))
	    `(force ,x)))
      `(force, x)))


;;; This is used to give thunks meaningful function names for debugging
;;; purposes.

(define (thunk-name object)
  (dynamic-let ((*print-length*  3)
		(*print-level*   3)
		(*print-pretty*  '#f))
    (string->gensym (format '#f "Delay ~s" object))))
    



;;; Here is the code walker definition.

(define (do-codegen-list list)
  (map (function do-codegen) list))

(define-flic-walker codegen (object))


(define (codegen-lambda-list vars)
  (map (function fullname) vars))

(define (codegen-curried-fn opt-fn strictness)
  (if (null? (cdr strictness))
      ;; one-argument special cases
      (if (car strictness)
	  `(make-curried-fn-1-strict ,opt-fn)
	  `(make-curried-fn-1-nonstrict ,opt-fn))
      ;; general case
      `(make-curried-fn ,opt-fn ',strictness)))


;;; Curry lambdas.  Functions always return an unboxed value.
;;; Also note that anonymous lambdas always have all non-strict arguments.

(define-codegen flic-lambda (object)
  (codegen-curried-fn
    (codegen-lambda-aux object)
    (map (function var-strict?) (flic-lambda-vars object))))

(define (codegen-lambda-aux object)
  (let* ((vars    (flic-lambda-vars object))
	 (ignore  '())
	 (args    (codegen-lambda-list vars)))
    (dolist (v vars)
      (if (eqv? (var-referenced v) 0)
	  (push (fullname v) ignore)))
    `(lambda ,args
       ,@(if (not (null? ignore))
	     `((declare (ignore ,@ignore)))
	     '())
       ,(do-codegen (flic-lambda-body object)))))


;;; This is only for non-top-level lets.
;;; The boxing of the value of each of the bindings is controlled by its
;;; strict? property.

(define-codegen flic-let (object)
  (let ((bindings   (flic-let-bindings object))
	(body       (flic-let-body object))
	(recursive? (flic-let-recursive? object)))
    (if recursive?
	(codegen-letrec bindings body)
	(codegen-let*   bindings body))))


;;; For efficiency reasons, we want to make all the function bindings
;;; in the function namespace (some implementations do not do tail-recursion
;;; or other optimizations correctly otherwise).  This means we have
;;; to sort out the variable bindings from the function bindings here.

(define (codegen-letrec bindings body)
  (let ((let-bindings     '())
	(labels-bindings  '()))
    (dolist (var bindings)
      (let ((value    (var-value var))
	    (fullname (fullname var))
	    (strict?  (var-strict? var)))
	(if (flic-lambda? value)
	    ;; Some functions may need only the optimized or standard
	    ;; entry points, but not both.
	    (let ((optname     (optname var))
		  (lambda      (codegen-lambda-aux value))
		  (optimized?  (var-optimized-refs? var))
		  (standard?   (var-standard-refs? var)))
	      (when standard?
		(push (list fullname
			    (maybe-make-box-value
			      (codegen-curried-fn
			        (if optimized? `(function ,optname) lambda)
				(var-strictness var))
			      strict?))
		      let-bindings))
	      (when optimized?
		(push (cons optname (cdr lambda)) labels-bindings)))
	    (push (list fullname (do-codegen value)) let-bindings))))
    (setf let-bindings (nreverse let-bindings))
    (setf labels-bindings (nreverse labels-bindings))
    (cond ((null? let-bindings)
	   `(labels ,labels-bindings ,(do-codegen body)))
	  ((null? labels-bindings)
	   `(letrec ,let-bindings ,(do-codegen body)))
	  (t
	   `(let ,(map (lambda (b) `(,(car b) '#f)) let-bindings)
	      ;; *** This maybe ought to do something about ignore
	      ;; *** declarations.
	      (labels ,labels-bindings
		      ,@(map (lambda (b) `(setf ,@b)) let-bindings)
		      ,(do-codegen body))))
	  )))

(define (codegen-let* bindings body)
  (if (null? bindings)
      (do-codegen body)
      (let* ((var       (car bindings))
	     (value     (var-value var))
	     (fullname  (fullname var))
	     (strict?   (var-strict? var))
	     (body      (codegen-let* (cdr bindings) body)))
	(if (flic-lambda? value)
	    ;; Some functions may need only the optimized or standard
	    ;; entry points, but not both.
	    (let ((optname     (optname var))
		  (lambda      (codegen-lambda-aux value))
		  (optimized?  (var-optimized-refs? var))
		  (standard?   (var-standard-refs? var)))
	      (when standard?
		(setf body
		      (add-let-binding
		        (list fullname
			      (maybe-make-box-value
			        (codegen-curried-fn
				  (if optimized? `(function ,optname) lambda)
				  (var-strictness var))
				strict?))
			body
			'())))
	      (when optimized?
		(setf body `(flet ((,optname ,@(cdr lambda))) ,body)))
	      body)
	    (add-let-binding
	      (list fullname (do-codegen value))
	      body
	      (if (eqv? (var-referenced var) 0)
		  `((declare (ignore ,fullname)))
		  '()))
	    ))))

(define (add-let-binding binding body ignore)
  (if (and (pair? body) (eq? (car body) 'let*))
      `(let* (,binding ,@(cadr body)) ,@ignore ,@(cddr body))
      `(let* (,binding) ,@ignore ,body)))


(define-codegen flic-app (object)
  (let ((result  (codegen-flic-app-aux object)))
    (if (and (not (flic-exp-unboxed? object))
	     (not (flic-exp-cheap? object))
	     (memq 'delays (dynamic *optimizers*))
	     (every (function can-evaluate-eagerly?) (flic-app-args object))
	     (or (flic-app-saturated? object)
		 (can-evaluate-eagerly? (flic-app-fn object))
		 (is-type? 'flic-ref (flic-app-fn object)))
	     (pair? result))
	;; Push delay out-of-line when possible.
	;; The idea is to turn
	;; (delay (funcall f arg1 arg2 ...))
	;; into
	;; (delay-funcall f arg1 arg2 ...)
	;; when f and all of the arguments are cheap enough to evaluate
	;; eagerly.
	(cond ((eq? (car result) 'funcall)
	       `(delay-funcall ,@(cdr result)))
	      ((eq? (car result) 'funcall/force)
	       `(delay-funcall/force ,@(cdr result)))
	      ((not (syntax? (car result)))
	       `(delay-funcall (function ,(car result)) ,@(cdr result)))
	      (else
	       result))
	result)))

(define (can-evaluate-eagerly? object)
  (if (flic-exp-unboxed? object)
      (flic-exp-cheap? object)
      '#t))

(define (codegen-flic-app-aux object)
  (let ((fn         (flic-app-fn object))
	(args       (flic-app-args object))
	(saturated? (flic-app-saturated? object)))
    (cond ((and saturated? (flic-pack? fn))
	   ;; Saturated call to constructor
	   (codegen-constructor-app-aux
	     (flic-pack-con fn)
	     (do-codegen-list args)))
	  ((and saturated? (flic-ref? fn))
	   ;; Saturated call to named function
	   (codegen-named-funcall (flic-ref-var fn) args))
	  (else
	   ;; Have to make a curried call to standard entry point.
	   (let ((fncode   (do-codegen fn))
		 (argcode  (do-codegen-list args)))
	     (if (and (pair? fncode)
		      (eq? (car fncode) 'force))
		 `(funcall/force ,(cadr fncode) ,@argcode)
		 `(funcall ,fncode ,@argcode))))
	  )))

(define (codegen-constructor-app-aux con argcode)
  (let ((alg  (con-alg con)))
    (cond ((eq? con (core-symbol ":"))
	   `(cons ,@argcode))
	  ((algdata-implemented-by-lisp? alg)
	   (apply-maybe-lambda (cadr (con-lisp-fns con)) argcode))
	  ((algdata-tuple? alg)
	   `(make-tuple ,@argcode))
	  (else
	   `(make-tagged-data ,(con-tag con) ,@argcode)))))


;;; Look for hacks for generating better code for certain primitive
;;; functions.

(define (codegen-named-funcall var args)
  (cond ((eq? var (core-symbol "strict2"))
	 ;; Could be smarter about detecting do-nothing cases for
	 ;; the first argument here....
	 `(begin ,@(do-codegen-list args)))
	((and (eq? var (core-symbol "error"))
	      (is-type? 'flic-const (car args)))
	 `(prim.abort ,(flic-const-value (car args))))
	((and (eq? var (core-symbol "stringToSymbol"))
	      (is-type? 'flic-const (car args)))
	 `',(string->symbol (flic-const-value (car args))))
	((and (eq? var (core-symbol "applyIO"))
	      (is-type? 'flic-lambda (cadr args))
	      (null? (cdr (flic-lambda-vars (cadr args))))
	      (var-strict? (car (flic-lambda-vars (cadr args)))))
	 (codegen-applyio
	   (car args)
	   (car (flic-lambda-vars (cadr args)))
	   (flic-lambda-body (cadr args))))
	(else
	 (let ((optname (optname var))
	       (argcode (do-codegen-list args)))
	   `(,optname ,@argcode)))))


;;; Turn nested applyio calls into LET* constructs, e.g.
;;;   applyio (p s) (\x -> applyio (q x s) (\y -> r y s))
;;; turns into
;;;   (let* ((x    (p s))
;;;          (y    (q x s)))
;;;      (r y s))

(define (codegen-applyio arg1 var body)
  (let ((arg1-code  (do-codegen arg1))
	(body-code  (do-codegen body))
	(ignore     (if (eqv? (var-referenced var) 0)
			`((declare (ignore ,(fullname var))))
			'())))
    (if (and (pair? body-code)
	     (eq? (car body-code) 'let*))
	`(let* ((,(fullname var) ,arg1-code)
		,@(cadr body-code))
	   ,@ignore
	   ,@(cddr body-code))
	`(let* ((,(fullname var) ,arg1-code))
	   ,@ignore
	   ,body-code))))


;;; We need to keep track of external variables from interface files
;;; so we can do something to suppress compiler warnings about references
;;; to undeclared variables.

(define-codegen flic-ref (object)
  (let ((var  (flic-ref-var object)))
    (when (def-interface? var)
      (when (not (memq var (dynamic *interface-vars-referenced*)))
	(push var (dynamic *interface-vars-referenced*))))
    (fullname var)))


(define-codegen flic-const (object)
  (let ((value   (flic-const-value object)))
    (cond ((string? value)
	   `(make-haskell-string ,value))
	  ((char? value)
	   ;; *** I think the parser ought to convert characters to their
	   ;; *** ASCII codes instead of doing it here.  There are problems
	   ;; *** with valid Haskell characters that can't be represented
	   ;; *** portably as Lisp characters.
	   (char->integer value))
	  ((number? value)
	   value)
	  (else
	   ;; It must be a ratio.  This is a bit of a hack - this depends on
	   ;; the fact that 2 tuples are represented in the same manner as
	   ;; rationals.  Hacked for strict rationals - jcp
	   `(make-tuple ,(car value) ,(cadr value)))
	  )))


;;; Returns a function or constant, so doesn't need to delay result.
;;; See flic-app for handling of saturated constructor calls.

(define-codegen flic-pack (object)
  (let* ((con        (flic-pack-con object))
	 (arity      (con-arity con))
	 (alg        (con-alg con))
	 (tuple?     (algdata-tuple? alg))
	 (strictness (con-slot-strict? con))
	 (index      (con-tag con)))
    (cond ((eq? con (core-symbol "Nil"))
	   ''())
	  ((eq? con (core-symbol "True"))
	   ''#t)
	  ((eq? con (core-symbol "False"))
	   ''#f)
	  ((eq? con (core-symbol ":"))
	   '(function cons-constructor))
	  ((algdata-implemented-by-lisp? alg)
	   (let ((fn (cadr (con-lisp-fns con))))
	     (if (eqv? (con-arity con) 0)
		 fn
		 (codegen-curried-fn
		  (if (and (pair? fn) (eq? (car fn) 'lambda))
		      fn
		      `(function ,fn))
		  strictness))))
	  ((algdata-enum? alg)
	   ;; All constructors have 0 arity; represent them just
	   ;; by numbers.
	   index)
	  (tuple?
	   ;; Only a single constructor for this type.
	   `(make-tuple-constructor ,arity ',strictness))
	  ((eqv? arity 0)
	   ;; No arguments to this constructor.
	   `(make-tagged-data ,index))
	  (else
	   ;; General case.
	   `(make-tagged-data-constructor ,index ,arity ',strictness))
	  )))



;;; These expressions translate directly into their Lisp equivalents.

(define-codegen flic-case-block (object)
  `(block ,(flic-case-block-block-name object)
     ,@(do-codegen-list (flic-case-block-exps object))))

(define-codegen flic-return-from (object)
  `(return-from ,(flic-return-from-block-name object)
		,(do-codegen (flic-return-from-exp object))))

(define-codegen flic-and (object)
  `(and ,@(do-codegen-list (flic-and-exps object))))

(define-codegen flic-if (object)
  `(if ,(do-codegen (flic-if-test-exp object))
       ,(do-codegen (flic-if-then-exp object))
       ,(do-codegen (flic-if-else-exp object))))

(define-codegen flic-sel (object)
  (codegen-flic-sel-aux
    (flic-sel-con object)
    (flic-sel-i object)
    (do-codegen (flic-sel-exp object))))

(define (codegen-flic-sel-aux con index exp)
  (let* ((alg      (con-alg con))
	 (tuple?   (algdata-tuple? alg))
	 (arity    (con-arity con))
	 (force?   (and (pair? exp)
			(eq? (car exp) 'force))))
    (cond ((eq? con (core-symbol ":"))
	   (if (eqv? index 0)
	       (if force?
		   `(car/force ,(cadr exp))
		   `(car ,exp))
	       (if force?
		   `(cdr/force ,(cadr exp))
		   `(cdr ,exp))))
	  ((algdata-implemented-by-lisp? alg)
	   (apply-maybe-lambda (list-ref (cddr (con-lisp-fns con)) index)
			       (list exp)))
	  (tuple?
	   (if force?
	       `(tuple-select/force ,arity ,index ,(cadr exp))
	       `(tuple-select ,arity ,index ,exp)))
	  (else
	   (if force?
	       `(tagged-data-select/force ,arity ,index ,(cadr exp))
	       `(tagged-data-select ,arity ,index ,exp)))
	  )))

(define-codegen flic-is-constructor (object)
  (codegen-flic-is-constructor-aux
    (flic-is-constructor-con object)
    (do-codegen (flic-is-constructor-exp object))))

(define (codegen-flic-is-constructor-aux con exp)
  (let ((type (con-alg con)))
    (cond ((eq? type (core-symbol "Bool"))
	   (if (eq? con (core-symbol "True"))
	       exp
	       `(not ,exp)))
	  ((eq? type (core-symbol "List"))
	   (if (eq? con (core-symbol ":"))
	       `(pair? ,exp)
	       `(null? ,exp)))
	  ((algdata-implemented-by-lisp? type)
	   (let ((fn (car (con-lisp-fns con))))
	     (apply-maybe-lambda fn (list exp))))
	  ;; Use the is-constructor to actually force the data value.
	  ;; The expression must be referenced to ensure the forcing
	  ;; takes place.
	  ((algdata-tuple? type)  ; this may force the exp
	   `(begin ,exp '#t))
	  ((algdata-enum? type)
	   `(eqv? (the fixnum ,exp) (the fixnum ,(con-tag con))))
	  (else
	   `(eqv? (the fixnum (constructor-number ,exp))
		  (the fixnum ,(con-tag con))))
	  )))


(define-codegen flic-con-number (object)
  (let ((type   (flic-con-number-type object))
	(exp    (do-codegen (flic-con-number-exp object))))
    `(the fixnum
	  ,(cond ((eq? type (core-symbol "Bool"))
		  `(if ,exp 1 0))
		 ((eq? type (core-symbol "List"))
		  `(if (pair? ,exp) 0 1))
		 ((algdata-tuple? type)
		  ;; This should never happen.
		  0)
		 ((algdata-implemented-by-lisp? type)
		  (let ((var (gensym)))
		    `(let ((,var ,exp))
		       (cond ,@(map (lambda (con)
				      `(,(apply-maybe-lambda
					  (car (con-lisp-fns con))
					  (list var))
					',(con-tag con)))
				    (algdata-constrs type))
			     (else (error "No constructor satisfies ~A.~%"
					  ',(def-name type)))))))
		 ((algdata-enum? type)
		  exp)
		 (else
		  `(constructor-number ,exp))
		 ))
    ))




(define-codegen flic-update (object)
  (let* ((con    (flic-update-con object))
	 (arity  (con-arity con))
	 (slots  (flic-update-slots object))
	 (exp    (flic-update-exp object))
	 (temp   (gensym))
	 (args   '()))
    (dotimes (i arity)
      (let ((s  (assv i slots)))
	(if s
	    (push (do-codegen (cdr s)) args)
	    (push (codegen-flic-sel-aux con i temp) args))))
    `(let ((,temp  ,(do-codegen exp)))
       ,(codegen-constructor-app-aux con (nreverse args)))
    ))


;;;======================================================================
;;; Utility functions
;;;======================================================================

;;; Here are some helper functions for handing boxing and unboxing
;;; of values.
;;; maybe-make-box-delay is used to box forms that are "expensive" to
;;; compute; maybe-make-box-value is used to box forms like constants
;;; or functions that are "cheap" to compute eagerly.
;;; Maybe-unbox is used to unbox a form that returns a boxed result.

(define (maybe-make-box-delay form unboxed?)
  (if unboxed?
      form
      `(delay ,form)))

(define (maybe-make-box-value form unboxed?)
  (if unboxed?
      form
      `(box ,form)))

(define (maybe-unbox form unboxed?)
  (if unboxed?
      `(force ,form)
      form))


;;; These two var slots are filled in lazily by the code generator,
;;; since most vars generated don't need them.  You should always
;;; use these functions instead of accessing the structure slot
;;; directly.

(define (fullname var)
  (or (var-fullname var)
      (setf (var-fullname var)
	    (if (var-toplevel? var)
		;; For toplevel names, use module name glued onto base names.
		;; These are always interned symbols.
		(if (def-core? var)
		    (symbol-append '|*Core:| (def-name var))
		    (symbol-append (def-module var) '\: (def-name var)))
		;; Otherwise, make sure we have a gensym.
		;; The uniquification of interned symbols is required
		;; because there may be multiple nested bindings of the
		;; same name, and we want to be able to distinguish between
		;; the different bindings for debugging purposes.
		(let ((name  (def-name var)))
		  (if (gensym? name)
		      name
		      (gensym (symbol->string name))))))
      ))

(define (optname var)
  (or (var-optimized-entry var)
      (setf (var-optimized-entry var)
	    (if (var-toplevel? var)
		(string->symbol
		  (string-append "Function "
				 (symbol->string (fullname var))))
		(string->gensym
		  (string-append "Local Function "
				 (symbol->string (fullname var))))))
      ))



;;;======================================================================
;;; Exported functions
;;;======================================================================

;;; This handles types exported to lisp from Haskell
;;; *** Is this really supposed to create variable bindings as
;;; *** opposed to function bindings???
;;; *** I assume all of these functions want strict arguments and return
;;; *** strict results, even if the data structures contain boxed values.

(define (codegen-exported-types mods)
  (let ((defs '()))
    (dolist (m mods)
      (dolist (a (module-alg-defs m))
        (when (algdata-export-to-lisp? a)
	  (dolist (c (algdata-constrs a))
	    (setf defs (nconc (codegen-constr c) defs))))))
    `(begin ,@defs)))

(define (codegen-constr c)
  (let ((lisp-fns (con-lisp-fns c)))
    (if c
        (let ((res
	       `(,(codegen-lisp-predicate (car lisp-fns) c)
		 ,(codegen-lisp-constructor (cadr lisp-fns) c)
		 ,@(codegen-lisp-accessors
		    (cddr lisp-fns) (con-slot-strict? c) c 0))))
	  (when (memq 'codegen (dynamic *printers*))
	    (dolist (d res)
	      (pprint* d)))
	  res)
	'())))

(define (codegen-lisp-predicate name c)
  `(define (,name x)
     ,(codegen-flic-is-constructor-aux c 'x)))

(define (codegen-lisp-constructor name c)
  (let ((strictness (con-slot-strict? c))
	(args       '())
	(exps       '()))
    (dolist (s strictness)
      (let ((arg  (gensym)))
	(push arg args)
	(push (if s arg `(box ,arg)) exps)))
    `(define (,name ,@(nreverse args))
	 ,(codegen-constructor-app-aux c (nreverse exps)))))

(define (codegen-lisp-accessors names strictness c i)
  (declare (type fixnum i))
  (if (null? names)
      '()
      (let ((body  (codegen-flic-sel-aux c i 'x)))
	(when (not (car strictness))
	  (setf body `(force ,body)))
	(cons `(define (,(car names) x) ,body)
	      (codegen-lisp-accessors (cdr names) (cdr strictness) c (+ i 1))))
    ))



;;; This allows the user to place lambda defined functions in ImportLispType
;;; Note: lambda applications without the funcall are valid in Common Lisp, 
;;; but not in mumble.  

(define (apply-maybe-lambda fn args)
  (if (and (pair? fn)
	   (eq? (car fn) 'lambda))
      `(funcall ,fn ,@args)
      `(,fn ,@args)))
