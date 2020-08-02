;;; This file contains error handlers for the type checker.

(define (type-error . msg)
  (let ((msg2 (report-non-local-type-error)))
    (haskell-error 'type-error 'phase (cons msg msg2))
    (continue-from-type-error)))

(define (report-non-local-type-error)
  (if (pair? (dynamic *type-error-handlers*))
      (funcall (car (dynamic *type-error-handlers*)))
      '()))

(define (continue-from-type-error)
  (funcall (car (dynamic *type-error-recovery*))))

(define (type-mismatch/fixed object msg type)
 (list
  (list "While type checking ~A~A~%Inferred type: ~A"
	(szn object 40) msg (sznt type 50))))

(define (type-mismatch object msg type1 type2)
 (list
  (list "While type checking ~A~A~%Types: ~A       ~A"
	(szn object 40) msg (sznt type1 60) (sznt type2 60))))

(define (type-mismatch/list types object msg)
 (cons
  (list "While typing ~A~A~%Types: ~%" (szn object 40) msg)
  (map (lambda (type)
	 (list"~A" (sznt type 60)))
       types)))

;;; Error handlers

(define (signature-mismatch var)
 (if (eq? (def-name var) *magic-temp-name*)
  (list
   (list
      "The expression `~A' is not a Dialogue"
      (source-pointer-file (def-where-defined var))))
  (list
   (list
      "Signature mismatch for ~A~%Inferred type: ~ADeclared type: ~A"
      var
      (szn (remove-type-wrapper (ntype->gtype (var-type var))) 50)
      (szn (var-signature var) 50)))))

(define (remove-type-wrapper ty)
  (if (recursive-type? ty) (recursive-type-type ty) ty))


	