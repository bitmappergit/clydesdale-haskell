;;;
;;; This processes the deriving declaration
;;;

(predefine (scope-single-fun-def sfd env lhs1))

(define (deriving-decl->deriving decl)
  (let* ((simple (deriving-decl-simple decl))
	 (di (tycon-def simple))
	 (c (deriving-decl-constraints decl))
	 (tyvars (simple-tyvar-list simple))
	 (i-decls (deriving-decl-inst-decls decl)))
    (cond ((or (null? tyvars) (not (null? (cdr tyvars))))
	   (phase-error 'bad-deriving
		     "Deriving clause must have one exactly one parameter"))
	  (else
	   (resolve-signature-aux tyvars c)
	   (dolist (constraint c)
             (push (context-class constraint) (deriving-preconditions di)))
	   (setf (deriving-tyvar di) (car tyvars))
	   (dolist (d i-decls)
	     (with-slots instance-decl (context class simple decls) d
	       (when (not (and (tyvar? simple)
			       (eq? (tyvar-name simple) (car tyvars))))
		  (phase-error 'bad-deriving
		    "Instance declaration must refer to type ~A" (car tyvars)))
	       (resolve-signature-aux tyvars context)
	       (resolve-class class)
	       (dolist (d1 decls)
		 (dolist (sfd (valdef-definitions d1))
		     (scope-single-fun-def sfd '() (valdef-lhs d1))))))
	   (setf (deriving-instances di) i-decls)))))
