;;; This implements the pattern binding rule.

(define (apply-pattern-binding-rule? decls)
 (not
  (every (lambda (decl)
	   (or (function-binding? decl)
	       (simple-pattern-binding-with-signature? decl)))
	 decls)))

(define (function-binding? decl)
  (let ((defs (valdef-definitions decl)))
    (not (null? (single-fun-def-args (car defs))))))

(define (simple-pattern-binding-with-signature? decl)
  (let ((lhs (valdef-lhs decl))
	(defs (valdef-definitions decl)))
    (and (is-type? 'var-pat lhs)
	 (null? (single-fun-def-args (car defs)))
	 (not (eq? (var-signature (var-ref-var (var-pat-var lhs))) '#f)))))

(define (do-pattern-binding-rule decls necessary-tyvars ng-list)
  (setf ng-list (append necessary-tyvars ng-list))
  (find-exported-pattern-bindings decls)
  ng-list)

(define (find-exported-pattern-bindings decls)
  (dolist (decl decls)
    (dolist (var-ref (collect-pattern-vars (valdef-lhs decl)))
     (let ((var (var-ref-var var-ref)))
      (when (def-exported? var)
	(recoverable-error 'exported-pattern-binding
"Monomorphism restriction violation (see section 4.5.4, rule 2 in the report)~%~
 the overloaded variable ~A cannot be exported"
        var-ref))
      (when (not (eq? (var-signature var) '#f))
         (recoverable-error 'entire-group-needs-signature
   "A group of mutually recursive overloaded definitions, ~%~A~%~
    must have signatures for every definition if any is to have a signature~%~
    Signature for ~A ignored (see 4.5.4 in the report)."
            (gather-all-group-vars decls) var))))))

(define (gather-all-group-vars decls)
  (let ((res '()))
    (dolist (decl decls)
      (setf res (append res (collect-pattern-vars (valdef-lhs decl)))))
    res))
