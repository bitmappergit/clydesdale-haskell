
;;; This copies ast structure

(define-walker copy ast-td-copy-ast-walker)

(define (copy-ast ast)
  (copy-ast-1 ast '()))

(define (copy-ast-1 ast env)
  (call-walker copy ast env))

(define (copy-ast/list l env)
  (if (null? l)
      l
      (cons (copy-ast-1 (car l) env)
	    (copy-ast/list (cdr l) env))))

;;; Walkers for special structs

;;; assume env already has lhs definitions in it
(define-walker-method copy valdef (object env)
    (make valdef
	  (lhs (copy-ast-1 (valdef-lhs object) env))
	  (definitions (copy-ast/list (valdef-definitions object) env))
	  (module *module-name*)))

(define-walker-method copy single-fun-def (object env)
  (with-slots single-fun-def (args rhs-list where-decls infix?) object
    (let* ((env1 (add-local-patterns/copy args env))
	   (env2 (add-local-decls/copy where-decls env1)))
      (make single-fun-def
	 (args (copy-ast/list args env2))
	 (rhs-list (copy-ast/list rhs-list env2))
	 (where-decls (copy-ast/list where-decls env2))
	 (infix? infix?)))))

(define-walker-method copy var-ref (object env)
  (with-slots var-ref (name var infix?) object
     (make var-ref
	(name name)
	(var (rename-var/copy var env))
	(infix? infix?))))

(define-walker-method copy lambda (object env)
  (let ((env1 (add-local-patterns/copy (lambda-pats object) env)))
    (make lambda (pats (copy-ast/list (lambda-pats object) env1))
	         (body (copy-ast-1 (lambda-body object) env1)))))

(define-walker-method copy let (object env)
  (let ((env1 (add-local-decls/copy (let-decls object) env)))
    (make let (decls (copy-ast/list (let-decls object) env1))
	      (body (copy-ast-1 (let-body object) env1)))))

(define-walker-method copy alt (object env)
  (with-slots alt (pat rhs-list where-decls) object
    (let* ((env1 (add-local-pattern/copy pat env))
	   (env2 (add-local-decls/copy where-decls env1)))
    (make alt (where-decls (copy-ast/list where-decls env2))
	      (rhs-list (copy-ast/list rhs-list env2))
	      (pat (copy-ast-1 pat env2))))))

(define-copy-walker-methods 
  (guarded-rhs as-pat irr-pat var-pat wildcard-pat (const-pat match-fn)
   (plus-pat match-fn bind-fn) pcon list-pat dynamic-pat if case
   exp-sign app con-ref integer-const float-const char-const
   string-const list-exp sequence sequence-to sequence-then
   sequence-then-to omitted-guard))

;;; %%% List comprehension stuff not done.

;;; Environment stuff

(define (add-local-decls/copy decls env)
  (dolist (d decls)
    (when (valdef? d)
      (setf env (add-local-pattern/copy (valdef-lhs d) env))))
  env)

(define (add-local-patterns/copy ps env)
  (if (null? ps)
      env
      (add-local-patterns/copy (cdr ps)
			       (add-local-pattern/copy (car ps) env))))

(define (add-local-pattern/copy p env)
  (add-bind/copy (collect-pattern-vars p) env))

(define (add-bind/copy vars env)
  (if (null? vars)
      env
      (add-bind/copy
       (cdr vars)
       (cons (tuple (var-ref-var (car vars))
		    (create-local-definition (var-ref-name (car vars))))
	     env))))

(define (rename-var/copy var env)
  (let ((new (assq var env)))
    (if new (tuple-2-2 new) var)))
