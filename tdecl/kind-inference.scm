
;;; This does kind inference for constructor classes.

;;; The basic kind inference strategy here is to

(define *remembered-kind-obj* '#f)

(define (do-kind-inference modules)
  (walk-modules modules
   (lambda ()
     (dolist (alg (module-alg-defs *module*))
	(setf (tycon-def-k alg) (**kind-var)))
     (dolist (class (module-class-defs *module*))
	(setf (class-k class) (**kind-var)))))
  ;; Now apply kind constraints to all objects associated with data values
  (walk-modules modules
   (lambda ()
     (dolist (alg (module-alg-defs *module*))
       (let* ((sig (algdata-signature alg))
	      (env (map (function get-class-kind-constraints)
			(gtype-context sig))))
	 (dolist (con (algdata-constrs alg))
	   (setf *remembered-kind-obj* con)
	   (kind-unify/star
	    (kind-inference/ntype
	     (gtype-type (con-signature con)) env)))
	 (setf *remembered-kind-obj* alg)
	 (kind-unify (tycon-def-k alg) (make-kind-app/star env))))
     (dolist (class (module-class-defs *module*))
       (let ((kind (class-k class)))
	 (dolist (c1 (class-super class))
	   (setf *remembered-kind-obj* class)
           (kind-unify kind (class-k c1)))
	 (dolist (m (class-method-vars class))
	   (let* ((sig (var-type m))
		  (env (map (function get-class-kind-constraints)
			    (gtype-context sig))))
	   (setf *remembered-kind-obj* m)
	     (kind-unify/star
	      (kind-inference/ntype (gtype-type sig) env))))))))
  (walk-modules modules
   (lambda ()
     (dolist (alg (module-alg-defs *module*))
       (setf (tycon-def-k alg) (kind-prune/recursive (tycon-def-k alg)))
       (setf (tycon-def-arity alg) (compute-kind-arity (tycon-def-k alg))))
     (dolist (class (module-class-defs *module*))
       (setf (class-k class) (kind-prune/recursive (class-k class))))
     (dolist (syn (module-synonym-defs *module*))
       (mlet ((ty (**tycon/def syn (map (function **tyvar) (synonym-args syn))))
	      (gty (ast->gtype/vars '() ty (synonym-args syn)))
	      (env (map (function **kind-var) (gtype-context gty)))
	      (k (kind-inference/ntype (gtype-type gty) env))
	      ((k1 res) (make-kind-app env)))
	 (setf *remembered-kind-obj* syn)
         (kind-unify res k)
	 (setf (tycon-def-k syn) (kind-prune/recursive k1))
	 (setf (tycon-def-arity syn) (compute-kind-arity (tycon-def-k syn)))
	 (format '#t "Kind of ~A is ~A~%" syn (tycon-def-k syn))))
     ;; Fill in kinds for all gtypes in algs, classes, synonyms, insts(?)
     (dolist (alg (module-alg-defs *module*))
       (format '#t "Kind of ~A is ~A~%" alg (tycon-def-k alg))
       (add-gtype-kinds (algdata-signature alg))
       (dolist (c (algdata-constrs alg))
         (add-gtype-kinds (con-signature c))))
     (dolist (c (module-class-defs *module*))
       (format '#t "Kind of ~A is ~A~%" c (class-k c))
       (dolist (m (class-method-vars c))
	 (add-gtype-kinds (var-type m))))
     )))

(define (kind-inference/gtype gtype)
  (let ((kind-env (map (function get-class-kind-constraints)
		       (gtype-context gtype))))
    (kind-inference/ntype (gtype-type gtype) kind-env)))

;;; This determines an initial kind constraint for a type variable.  The
;;; kinds of all classes are unified.

(define (get-class-kind-constraints ctxt)
  (if (null? ctxt)
      (**kind-var)
      (let ((res (class-k (car ctxt))))
	(dolist (c (cdr ctxt))
	   (kind-unify res (class-k c)))
	res)))

;;; This does the actual kind inference.

(define (kind-inference/ntype ty env)
  (cond ((gtyvar? ty)
	 (list-ref env (gtyvar-varnum ty)))
	((ntycon? ty)
	 (or (tycon-def-k (ntycon-tycon ty))
	     (fatal-error 'undefined-kind "Undefined kind for ~A" ty)))
	((ty-app? ty)
	 (let ((fn (ty-app-fn ty)))
	   (if (and (ntycon? fn)
		    (synonym? (ntycon-tycon fn))
		    (not (tycon-def-k (ntycon-tycon fn))))
	       (kind-inference/ntype (expand-ntype-synonym ty) env)
	       (let ((fn-type (kind-inference/ntype fn env))
		     (arg-types
		      (map (lambda (ty1) (kind-inference/ntype ty1 env))
			   (ty-app-args ty))))
		 (kind-unify/app fn-type arg-types)))))
	(else (error "Bad type in kind-inference/ntype"))))

(define (kind-unify k1 k2)
  (let ((k1 (kind-prune k1))
	(k2 (kind-prune k2)))
    (cond ((eq? k1 k2)
	   'OK)
	  ((kind-var? k1)
	   (setf (kind-var-value k1) k2))
	  ((kind-var? k2)
	   (setf (kind-var-value k2) k1))
	  ((and (star? k1) (star? k2))
	   'OK)
	  ((and (k-app? k1) (k-app? k2))
	   (kind-unify (k-app-arg k1) (k-app-arg k2))
	   (kind-unify (k-app-res k1) (k-app-res k2)))
	  (else
	   (kind-unification-error)))))

(define (kind-unify/app fn args)
  (if (null? args)
      fn
      (let ((fn (kind-prune fn)))
	(cond ((star? fn)
	       (kind-unification-error))
	      ((kind-var? fn)
	       (mlet (((fn-type res-type) (make-kind-app args)))
		     (setf (kind-var-value fn) fn-type)
		     res-type))
	      ((k-app? fn)
	       (kind-unify (k-app-arg fn) (car args))
	       (kind-unify/app (k-app-res fn) (cdr args)))
	      (else (error "Bad type in kind-unify"))))))

(define (make-kind-app args)
  (if (null? args)
      (let ((ty (**kind-var)))
	(values ty ty))
      (mlet (((fn-type res-type) (make-kind-app (cdr args))))
        (values (**k-app (car args) fn-type) res-type))))
 
(define (make-kind-app/star args)
  (if (null? args)
      (**star)
      (**k-app (car args) (make-kind-app/star (cdr args)))))

(define (kind-unify/star k)
  (let ((k (kind-prune k)))
    (cond ((kind-var? k)
	   (setf (kind-var-value k) (**star)))
	  ((star? k)
	   'OK)
	  (else
	   (kind-unification-error)))))

;;; This also handles defaulting

(define (kind-prune/recursive k)
  (cond ((star? k)     k)
	((kind-var? k) (if (kind-var-value k)
			   (kind-prune/recursive (kind-var-value k))
			   (**star)))
	((k-app? k)    (**k-app (kind-prune/recursive (k-app-arg k))
				(kind-prune/recursive (k-app-res k))))))

;;; Kinds that correspond to ordinary type constructor arities are treated
;;; separately.  These are *, * -> *, * -> (* -> *), ...

(define (compute-kind-arity k)
  (cond ((star? k)   0)
	((k-app? k)  (let ((a (compute-kind-arity (k-app-res k))))
		       (if (and a (star? (k-app-arg k)))
			   (1+ a)
			   '#f)))
	(else         (error "Bad kind"))))

;;; This augments a gtype with kinds for both tyvars and applications.

(define (add-gtype-kinds gtype)
  (setf *remembered-kind-obj* gtype)
  (let* ((env (map (function get-class-kind-constraints)
		   (gtype-context gtype)))
	 (k (kind-inference/ntype (gtype-type gtype) env))
	 (ks (map (function kind-prune/recursive) env)))
    (kind-unify/star k)
    (unless (every (function star?) ks)
	(setf (gtype-kinds gtype) ks))
    (add-gtype-kinds/internal (gtype-type gtype) ks)))

;;; This is used to decorate internal nodes of the gtype


(define (add-gtype-kinds/internal ty env)
  (cond ((gtyvar? ty)
	 (list-ref env (gtyvar-varnum ty)))
	((ntycon? ty)
	 (setf (ntype-kind ty) (tycon-def-k (ntycon-tycon ty))))
	(else
	 (let ((fn (add-gtype-kinds/internal (ty-app-fn ty))))
	   (dolist (a (ty-app-args ty))
   	     (add-gtype-kinds/internal a env)
	     (setf fn (k-app-res fn)))
	   (setf (ntype-kind ty) fn)))))

(define (kind-unification-error)
  (phase-error/objs 'kinding-error (list *remembered-kind-obj*)
     "Kind conflict while inferring kind for ~A"
     (if (gtype? *remembered-kind-obj*)
	 *remembered-kind-obj*
	 (get-object-name *remembered-kind-obj*))))
