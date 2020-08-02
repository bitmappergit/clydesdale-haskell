
;;; This is the main entry point to the type checker.


(define (do-haskell-type-check object modules)
  (type-init modules)
  (attach-interface-signatures modules)
  (when (is-type? 'let object) ; may be void
    (dynamic-let ((*non-generic-tyvars* '())
		  (*placeholders* '())
		  (*enclosing-decls* '()))
      (type-check/decls let decls
	 (setf (dynamic *non-generic-tyvars*) '())
         (process-placeholders (dynamic *placeholders*) '() '()))))
  'done)

;;; This is the main recursive entry to the type checker.

(define (dispatch-type-check exp)
 (remember-context exp
  (call-walker type exp)))

(define (do-type-check/list exps)
  (if (null? exps)
      (values '() '())
      (mlet (((obj1 type1) (dispatch-type-check (car exps)))
	     ((objs types) (do-type-check/list (cdr exps))))
	(values (cons obj1 objs) (cons type1 types)))))

(define (type-init modules)
  ;; Built in types
  (setf *char-type* (**ntycon (core-symbol "Char") '()))
  (setf *string-type* (**ntycon (core-symbol "List")
				(list *char-type*)))
  (setf *bool-type* (**ntycon (core-symbol "Bool") '()))
  (setf *int-type* (**ntycon (core-symbol "Int") '()))
  (setf *integer-type* (**ntycon (core-symbol "Integer") '()))
  (setf *rational-type* (**ntycon (core-symbol "Ratio")
				  (list *integer-type*)))
  (setf *dynamic-type* (**ntycon (core-symbol "Dynamic") '()))
  (setf *signature-type* (**ntycon (core-symbol "Signature") '()))
  (setf *magic-type* (**ntycon (core-symbol "Magic") '()))
  (setf *default-decls* '())
  (dolist (m modules)
    (let ((default-types '()))
      (dolist (d (default-decl-types (module-default m)))
        (let* ((ty (ast->gtype '() d))
	       (ntype (gtype-type ty)))
	  (cond ((not (null? (gtype-context ty)))
		 (recoverable-error 'not-monotype
		   "~A is not a monotype in default decl" ty))
		((not (type-in-class? ntype (core-symbol "Num")))
		 (recoverable-error 'not-Num-class
		   "~A is not in class Num" ty))
		(else
		 (push ntype default-types)))))
      (push (tuple (module-name m) (reverse default-types)) *default-decls*))))

(define (remember-placeholder placeholder)
  (push placeholder (dynamic *placeholders*)))

;;; This is for interface files.

;;; When an interface file supplies a type signature for a var,
;;; this signature is either attached to the var if no local signature is
;;; supplied or compared to the implementation signature.

(define (attach-interface-signatures mods)
  (let ((mod-names (map (function module-name) mods)))
    (dolist (mod (get-all-interfaces))
      (dolist (alist (module-interface-definitions mod))
        (when (memq (car alist) mod-names)
	  (let ((st (module-symbol-table (locate-module (car alist)))))
	    (dolist (idef (cdr alist))
	      (let ((def (table-entry st (def-name idef))))
		(when (and def (var? def) (not (var-signature def)))
		   (setf (var-signature def) (var-signature idef)))))))))))
