;;; scope.scm -- variable scoping and precedence parsing phase
;;; 
;;; author :  John & Sandra
;;; date   :  11 Feb 1992
;;;
;;;


;;;===================================================================
;;; Basic support
;;;===================================================================

(define (scope-modules modules)
  (watch-for-undefined-symbols)
  (walk-modules
    modules
    (lambda ()
      (setf (module-decls *module*) (scope-ast-decls (module-decls *module*)))
      (dolist (a (module-annotations *module*))
	;; This is currently bogus since it assumes only vars are annotated.
	(when (annotation-decl? a)
	  (dolist (n (annotation-decl-names a))
	    (let ((v (table-entry *symbol-table* n)))
	      (cond ((or (eq? v '#f) (not (var? v)))
		     (signal-undefined-annotated-var n a))
		    ((not (eq? (def-module v) *module-name*))
		     (signal-bad-annotated-var v a))
		    (else
		     (setf (var-annotations v)
			   (append (var-annotations v)
				   (annotation-decl-annotations a)))
		     (do-specialize-annotation-magic
		      v (annotation-decl-annotations a) '#t)
		     ))))))))
  (show-undefined-symbols))

;;; Define the basic walker and some helper functions.

(define-walker scope ast-td-scope-walker)

(define (scope-ast-1 x env)
  (remember-context x
    (call-walker scope x env)))


(define (scope-ast/list l env)
  (scope-ast/list-aux l env)
  l)

(define (scope-ast/list-aux l env)
  (when (not (null? l))
    (setf (car l) (scope-ast-1 (car l) env))
    (scope-ast/list-aux (cdr l) env)))



;;; This filters out signdecls from decl lists.  These declarations are moved
;;; into the var definitions.

(define (scope-ast/decl-list l env)
  (if (null? l)
      '()
      (let ((x (scope-ast-1 (car l) env))
	    (rest (scope-ast/decl-list (cdr l) env)))
	(if (or (annotation-decls? x)
		(and (signdecl? x)
		     (not (eq? (module-type *module*) 'interface))))
	    rest
	    (begin
	      (setf (car l) x)
	      (setf (cdr l) rest)
	      l)))))



;;; Magic to cons up and collect decls for defs introduced by
;;; "Specialize" annotations.  These decls are added to the same group
;;; where the var being specialized is bound.  Attach the specified
;;; signature to the new var, and store a pointer to it on the def
;;; for the original var.

(define *specializer-decls* '())
(define *specializer-count* 0)

(define (scope-ast/decl-list/inner l env)
  (dynamic-let ((*specializer-decls* '()))
    (let ((list  (scope-ast/decl-list l env)))
      (nconc (dynamic *specializer-decls*) list))))

(define (do-specialize-annotation-magic var annotation-values toplevel?)
  (dolist (val annotation-values)
    (when (eq? (annotation-value-name val) '|Specialize|)
      (let* ((sig    (car (annotation-value-args val)))
	     (gtype  (begin
		       (resolve-signature sig)
		       (ast->gtype (signature-context sig)
		 		   (signature-type sig))))
	     (name   (format '#f "~a-specialized-~a"
			     var (incf (dynamic *specializer-count*))))
	     (svar   (if toplevel?
			 (create-top-definition (string->symbol name) 'var)
			 (create-local-definition (gensym name))))
	     (decl   (**valdef/def svar (**var/def var))))
	(setf (var-signature svar) gtype)
	(setf (var-specializers var)
	      (nconc (var-specializers var) (list (list svar))))
	(if toplevel?
	    (push decl (module-decls *module*))
	    (push decl (dynamic *specializer-decls*)))
	))))



;;; This is the main entry point.  It is called by the driver
;;; on each top-level decl in the module.

(define (scope-ast-decls x)
  (let ((result  (scope-ast/decl-list x '())))
    result))


;;; All top-level names are entered in the *symbol-table* hash table.
;;; This is done by the import/export phase of the compiler before
;;; we get here.
;;; The env is a list of a-lists that associates locally-defined names with
;;; their definitions.  Each nested a-list corresponds to a "level" or
;;; scope.
;;; *** If many variables are being added in each scope, it might be
;;; *** better to use a table instead of an alist to represent each contour.

(define (lookup-name name env)
  (if (null? env)
      (lookup-toplevel-name name)
      (let ((info  (assq name (car env))))
	(if info
	    (cdr info)
	    (lookup-name name (cdr env))))))


;;; Some kinds of names (e.g. type definitions) appear only at top-level,
;;; so use this to look for them directly.

(define (lookup-toplevel-name name)
  (or (resolve-toplevel-name name)
      (begin
        (signal-undefined-symbol name 'var)
	*undefined-def*)))


;;; Some kinds of lookups (e.g., matching a signature declaration)
;;; require that the name be defined in the current scope and not
;;; an outer one.  Use this function.

(define (lookup-local-name name env place)
  (if (null? env)
      (lookup-toplevel-name name)
      (let ((info  (assq name (car env))))
	(if info
	    (cdr info)
	    (begin
	      (signal-undefined-local-symbol name place)
	      *undefined-def*)))))


;;; Add local declarations to the environment, returning a new env.
;;; Do not actually walk the local declarations here.

(define *scope-info* '())

(define (add-local-declarations decls env)
  (if (null? decls)
      env
      (let ((contour   '()))
	(dolist (d decls)
	  (if (is-type? 'valdef d)
	      (setf contour
		    (add-bindings (collect-pattern-vars (valdef-lhs d))
				  contour d))))
	(cons contour env))))


;;; Similar, but for adding lambda and function argument bindings to the
;;; environment.

(define (add-pattern-variables patterns env place)
  (if (null? patterns)
      env
      (let ((contour   '()))
	(dolist (p patterns)
	  (setf contour (add-bindings (collect-pattern-vars p) contour place)))
	(cons contour env))))


;;; Given a list of var-refs, create defs for them and add them to
;;; the local environment.
;;; Also check to see that there are no duplicates.

(define (add-bindings var-refs contour place)
  (dolist (v var-refs)
   (when (eq? (var-ref-var v) *undefined-def*)
    (let* ((name     (var-ref-name v))
	   (def      (create-local-definition name)))
      (setf (var-ref-var v) def)
      (setf (def-where-defined def) 
	    (ast-node-line-number place))
      (let ((old (assq name contour)))
	(if old
	    (signal-multiple-bindings name place def (tuple-2-2 old))
	    (push (cons name def) contour))))))
  contour)


;;; Error signalling utilities.

(define (signal-undefined-local-symbol name place)
  (phase-error 'undefined-local-symbol
    "In declaration ~A~%The name ~a is not defined by this declaration group."
    place name))

(define (signal-multiple-signatures name)
  (phase-error 'multiple-signatures
    "There is more than one type signature for ~a."
    name))

(define (signal-multiple-bindings name place def1 def2)
  (if (and (def-where-defined def1)
	   (not (eq? (def-where-defined def1) (def-where-defined def2))))
      (phase-error/objs 'multiple-bindings (list def1 def2)
        "The name `~A' is defined twice in the same scope." name)
      (phase-error 'multiple-bindings
        "There is more than one binding of ~A in ~A"
	name (sz place 40))))
  
(define (signal-undefined-annotated-var n a)
  (recoverable-error 'undefined-annotated-var
       "The variable ~A in annotation ~%~A~%is undefined" n a))

;;;===================================================================
;;; Default traversal methods
;;;===================================================================


(define-local-syntax (make-scope-code slot type)
  (let ((stype  (sd-type slot))
	(sname  (sd-name slot)))
    (cond ((and (symbol? stype)
		(or (eq? stype 'exp)
		    (subtype? stype 'exp)))
	   `(setf (struct-slot ',type ',sname object)
		  (scope-ast-1 (struct-slot ',type ',sname object) env)))
	  ((and (pair? stype)
		(eq? (car stype) 'list)
		(symbol? (cadr stype))
		(or (eq? (cadr stype) 'exp)
		    (subtype? (cadr stype) 'exp)))
	   `(setf (struct-slot ',type ',sname object)
		  (scope-ast/list (struct-slot ',type ',sname object) env)))
	  (else
;	   (format '#t "Scope: skipping slot ~A in ~A~%"
;		   (sd-name slot)
;		   type)
	   '#f))))


(define-modify-walker-methods scope
  (guarded-rhs  ; exp slots
   if           ; exp slots
   app          ; exp slots
   integer-const float-const char-const string-const  ; no slots
   list-exp     ; (list exp) slot
   sequence sequence-to sequence-then sequence-then-to ; exp slots
   section-l section-r ; exp slots
   omitted-guard overloaded-var-ref bottom ; no slots
   negate ; no slots
   sel
   con-number cast
   is-constructor
   )
  (object env)
  make-scope-code)


;;;===================================================================
;;; valdef-structs
;;;===================================================================


;;; Signature declarations must appear at the same level as the names
;;; they apply to.  There must not be more than one signature declaration
;;; applying to a given name.

(define-walker-method scope signdecl (object env)
  (let ((signature  (signdecl-signature object)))
    (resolve-signature signature)
    (let ((gtype (ast->gtype (signature-context signature)
			     (signature-type signature))))
      (dolist (v (signdecl-vars object))
	(when (eq? (var-ref-var v) *undefined-def*)
	      (setf (var-ref-var v)
		    (lookup-local-name (var-ref-name v) env object)))
	(let ((def  (var-ref-var v)))
	  (when (not (eq? def *undefined-def*))
	    ;; The lookup-local-name may fail if there is a program error.
	    ;; In that case, skip this.
	    (if (var-signature def)
		(signal-multiple-signatures (var-ref-name v))
		(setf (var-signature def) gtype))))))
    object))

;;; This attaches annotations to locally defined vars in the same
;;; manner as signdecl annotations.

(define-walker-method scope annotation-decls (object env)
  (let ((anns (annotation-decls-annotations object)))
    (dolist (a anns)
      (cond ((annotation-value? a)
	     (recoverable-error 'unknown-annotation
				"Unknown annotation: ~A" a))
	    ((annotation-decl? a)
	     (dolist (v (annotation-decl-names a))
	       (let ((name (lookup-local-name v env a)))
		 (cond ((eq? name *undefined-def*)
			(signal-bad-annotated-var v a))
		       (else
			(setf (var-annotations name)
			      (append (var-annotations name)
				      (annotation-decl-annotations a)))
			(do-specialize-annotation-magic
			 name (annotation-decl-annotations a) '#f)))))))))
  object)

(define-walker-method scope exp-sign (object env)
  (resolve-signature (exp-sign-signature object))
  (setf (exp-sign-exp object) (scope-ast-1 (exp-sign-exp object) env))
  object)

;;; By the time we get to walking a valdef, all the variables it
;;; declares have been entered into the environment.  All we need to
;;; do is massage the pattern and recursively walk the definitions.

(define-walker-method scope valdef (object env)
  (setf (valdef-module object) *module-name*)
  (setf (valdef-lhs object) (massage-pattern (valdef-lhs object)))
  (dolist (sfd (valdef-definitions object))
	(scope-single-fun-def sfd env (valdef-lhs object)))
  object)


;;; For a single-fun-def, do the where-decls first, and then walk the
;;; rhs in an env that includes both the where-decls and the args.

(define (scope-single-fun-def object env lhs)
  (setf env (add-pattern-variables
	     (single-fun-def-args object)
	     env 
	     ;; This is just for the error message
	     (make valdef (lhs lhs) (definitions (list object)))))
  (setf env (add-local-declarations (single-fun-def-where-decls object) env))
  (setf (single-fun-def-where-decls object)
	(scope-ast/decl-list/inner (single-fun-def-where-decls object) env))
  (setf (single-fun-def-args object)
	(massage-pattern-list (single-fun-def-args object)))
  (setf (single-fun-def-rhs-list object)
	(scope-ast/list (single-fun-def-rhs-list object) env)))

;;;===================================================================
;;; exp-structs
;;;===================================================================

(define-walker-method scope lambda (object env)
  (setf env (add-pattern-variables (lambda-pats object) env object))
  (setf (lambda-pats object) (massage-pattern-list (lambda-pats object)))
  (setf (lambda-body object) (scope-ast-1 (lambda-body object) env))
  object)

(define-walker-method scope let (object env)
  (setf env (add-local-declarations (let-decls object) env))
  (setf (let-decls object) (scope-ast/decl-list/inner (let-decls object) env))
  (setf (let-body object) (scope-ast-1 (let-body object) env))
  object)


;;; Case alts are treated very much like single-fun-defs.

(define-walker-method scope case (object env)
  (setf (case-exp object) (scope-ast-1 (case-exp object) env))
  (dolist (a (case-alts object))
    (let ((env  (add-pattern-variables (list (alt-pat a)) env (alt-pat a))))
      (setf env (add-local-declarations (alt-where-decls a) env))
      (setf (alt-where-decls a)
	    (scope-ast/decl-list/inner (alt-where-decls a) env))
      (setf (alt-pat a) (massage-pattern (alt-pat a)))
      (setf (alt-rhs-list a)
	    (scope-ast/list (alt-rhs-list a) env))))
  object)


(define-walker-method scope var-ref (object env)
  (when (eq? (var-ref-var object) *undefined-def*)
	(setf (var-ref-var object)
	      (lookup-name (var-ref-name object) env)))
  object)

(define-walker-method scope con-ref (object env)
  (declare (ignore env))
  (when (eq? (con-ref-con object) *undefined-def*)
	(setf (con-ref-con object)
	      (lookup-toplevel-name (con-ref-name object))))
  object)

(define-walker-method scope list-comp (object env)
  (dolist (q (list-comp-quals object))
    (cond ((is-type? 'qual-generator q)
	   (setf (qual-generator-exp q)
		 (scope-ast-1 (qual-generator-exp q) env))
	   (setf env
		 (add-pattern-variables
		  (list (qual-generator-pat q)) env (qual-generator-pat q)))
	   (setf (qual-generator-pat q)
		 (massage-pattern (qual-generator-pat q))))
	  ((is-type? 'qual-filter q)
	   (setf (qual-filter-exp q)
		 (scope-ast-1 (qual-filter-exp q) env)))))
  (setf (list-comp-exp object) (scope-ast-1 (list-comp-exp object) env))
  object)

(define-walker-method scope pp-exp-list (object env)
  (massage-pp-exp-list (scope-ast/list (pp-exp-list-exps object) env)))

