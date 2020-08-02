
;;; This adds stuff to a valdef to handle pattern matching errors

(define (add-error-handlers valdef)
  (let* ((defs (valdef-definitions valdef))
	 (last-def (car (reverse defs)))
	 (last-rhs (car (single-fun-def-rhs-list last-def))))
    (when (and (single-fun-def-args (car defs))
	       (not (all-var-pats? last-def)))
	  (setf (valdef-definitions valdef)
		(append defs (list (make-valdef-fallthrough valdef)))))
    (when (and (is-type? 'omitted-guard (guarded-rhs-guard last-rhs))
	       (is-type? 'bottom (guarded-rhs-rhs last-rhs)))
	  (setf (bottom-context (guarded-rhs-rhs last-rhs)) valdef))))

(define (add-error-handlers/case case-exp)
  (let ((alt (car (reverse (case-alts case-exp)))))
    (unless (and (non-guarded-pattern? (alt-rhs-list alt))
		 (always-matches? (alt-pat alt)))
      (let* ((temp (create-temp-var 'case-sel))
	     (c (ast-node-line-number case-exp))
	     (msg (if c
		      (format '#f "case statement at line ~A in file ~A"
			      (source-pointer-line c) (source-pointer-file c))
		     "some case statement"))
	     (error-call
	      (**app (**var/def (core-symbol "patternMatchError"))
		     (**string msg)
		     (**list (**app (**var/def (core-symbol "toDynamic"))
				    (**var/def temp))))))
       (let ((new-alt (**alt (**pat temp)
			 (list (make guarded-rhs
				     (guard (**omitted-guard))
				     (rhs error-call)))
			 '())))
	 (setf (alt-avoid-printing? new-alt) '#t)
	 (setf (case-alts case-exp)
	       (append (case-alts case-exp) (list new-alt))))))))

(define (all-var-pats? sfd)
  (and (non-guarded-pattern? (single-fun-def-rhs-list sfd))
       (all-var-pats-1 (single-fun-def-args sfd))))

(define (non-guarded-pattern? gds)
  (is-type? 'omitted-guard (guarded-rhs-guard (car gds))))

(define (all-var-pats-1 pats)
  (every (function always-matches?) pats))

(define (always-matches? p)
  (or (is-type? 'var-pat p)
      (is-type? 'wildcard-pat p)
      (and (pcon? p)
	   (con? (pcon-con p))
	   (algdata-tuple? (con-alg (pcon-con p)))
	   (all-var-pats-1 (pcon-pats p)))))

(define (make-valdef-fallthrough valdef)
  (let* ((fname (def-name (var-ref-var (var-pat-var (valdef-lhs valdef)))))
	 (c (ast-node-line-number valdef))
	 (msg (if c
		  (format '#f "function ~A at line ~A in file ~A"
  		    fname (source-pointer-line c) (source-pointer-file c))
		  (format '#f "function ~A" fname)))
	 (vars (map (lambda (p)
		      (declare (ignore p))
		      (create-temp-var 'err))
		    (single-fun-def-args (car (valdef-definitions valdef)))))
	 (error-call
	  (**app (**var/def (core-symbol "patternMatchError"))
		 (**string msg)
		 (**list/l (map (lambda (v)
				  (**app (**var/def (core-symbol "toDynamic"))
					 (**var/def v)))
				vars)))))
    (make single-fun-def
	  (args (map (function **pat) vars))
	  (rhs-list
	   (list (make guarded-rhs
		       (guard (**omitted-guard))
		       (rhs error-call))))
	  (where-decls '())
	  (avoid-printing? '#t)
	  (infix? '#f))))

