
(define *annotation-escape* '())

(define (parse-annotations context)
 (let ((save-layout (dynamic *layout-stack*)))
  (setf (dynamic *layout-stack*) '())
  (advance-token)
  (let/cc annotation-escape
   (setf *annotation-escape* (lambda () 
			       (setf (dynamic *layout-stack*) save-layout)
			       (advance-to-annotation-end)
			       (funcall annotation-escape '())))
   (let ((res (start-layout (lambda (in-layout?)
			      (parse-annotation-list-1 in-layout? context)))))
    (setf (dynamic *layout-stack*) save-layout)
    (token-case
     (end-annotation res)
     (else (signal-annotation-error)))))))

(define (parse-annotation-list-1 in-layout? context)
  (let ((kind (get-annotation-kind)))
    (cond ((eq? kind 'decl)
	   (let ((d (parse-annotation-decl context)))
	     (token-case
	      (\; (cons d (parse-annotation-list-1 in-layout? context)))
	      (else (close-layout in-layout?)
		    (list d)))))
	  ((eq? kind 'value)
	   (let ((d (parse-annotation-value context 'val)))
	     (token-case
	      (\; (cons d (parse-annotation-list-1 in-layout? context)))
	      (else (close-layout in-layout?)
		    (list d)))))
	  (else
	   (close-layout in-layout?)
	   '()))))

(define (get-annotation-kind)
  (token-case
   ((no-advance end-annotation) 'end)
   ((no-advance \() 'decl)
   ((var con)
    (let ((next (peek-1-type)))
      (cond ((eq? next '|,|)
	     'decl)
	    ((eq? next '|::|)
	     'decl)
	    (else
	     'value))))
   (else 'error)))

(define (parse-annotation-decl context)
  (let* ((names (parse-aname-list))
	 (decls (parse-aval-list context)))
    (make annotation-decl (names names) (annotations decls))))

(define (parse-aname-list)
 (let ((name 'foo))
  (token-case
   (var
    (setf name (var->symbol)))
   (con
    (setf name (con->symbol)))
   (else (signal-annotation-error)))
  (token-case (\, (cons name (parse-aname-list)))
	      (|::| (list name))
	      (else (signal-annotation-error)))))


(define (parse-aval-list context)
  (let ((ann (parse-annotation-value context 'decl)))
    (token-case (\, (cons ann (parse-aval-list context)))
		(else (list ann)))))

(define (parse-annotation-value context type)
  (token-case
   (name (mlet ((name (token->symbol))
		((arg-types place ty) (get-annotation-description name))
		(args (parse-annotation-args name arg-types)))
	   (unless (eq? type ty)
	      (if (eq? ty 'decl)
		  (signal-annotation-needs-decl-error name)
		  (signal-annotation-is-value-error name)))
	   (unless (memq context place)
               (signal-annotation-place-error name))
	   (make annotation-value (name name) (args args))))))

(define (parse-annotation-args name types)
 (if (null? types)
     (token-case
      (\( (signal-annotation-arg-error name))
      (else '()))
     (token-case
      (\( (parse-annotation-args-1 name types))
      (else (signal-annotation-arg-error name)))))

;;; This routine can invoke special parsers for the arguments

(define (parse-annotation-args-1 name types)
  (if (null? types)
      (signal-annotation-arg-error name)
      (let ((arg (parse-annotation-arg (car types) name)))
	(token-case
	 (\) (if (null? (cdr types))
		 (list arg)
		 (signal-annotation-arg-error name)))
	 (\, (cons arg (parse-annotation-args-1 name (cdr types))))
	 (else (signal-annotation-arg-error name))))))

(define (parse-annotation-arg type name)
  (cond ((eq? type 'string)
	 (token-case
	  ((string no-advance)
	   (let ((res (car *token-args*)))
	     (advance-token)
	     res))
	  (else (signal-annotation-arg-error name))))
	;; The following is for a datatype import/export.  It is
	;; Type(Con1(strs),Con2(strs),...)
	((eq? type 'integer)
	 (token-case
	  ((integer no-advance) (token->integer))
	  (else (signal-annotation-arg-error name))))
	((eq? type 'constr-list)
	 (parse-annotation-constr-list name))
	((eq? type 'signature)
	 (parse-signature))
	(else
	 (signal-annotation-error))))
	   
(define (parse-annotation-constr-list name)
  (token-case
   (tycon (let ((type-name (token->symbol)))
	    (token-case (\( (let* ((args (parse-acl1 name))
				   (res (tuple type-name args)))
			      (token-case  ; leave the close to end the args
			       ((no-advance \)) (list res))
			       (\, (cons res
					 (parse-annotation-constr-list name)))
			       (else (signal-annotation-arg-error name)))))
			(else (signal-annotation-arg-error name)))))
   (else (signal-annotation-arg-error name))))

(define (parse-acl1 name)
  (token-case
   (con (let ((con-name (con->symbol)))
	  (token-case (\( (let ((str-args (parse-string-list name)))
			    (token-case
			     (\, (cons (tuple con-name str-args)
				       (parse-acl1 name)))
			     (\) (list (tuple con-name str-args)))
			     (else (signal-annotation-arg-error name)))))
		      (else (signal-annotation-arg-error name)))))
   (else (signal-annotation-arg-error name))))

(define (parse-string-list name)
  (token-case
   ((string no-advance)
    (let ((res (read-lisp-object (car *token-args*))))
      (advance-token)
      (token-case
       (\) (list res))
       (\, (cons res (parse-string-list name)))
       (else (signal-annotation-arg-error name)))))
   (else (signal-annotation-arg-error name))))

(define (advance-to-annotation-end)
  (token-case
   (eof '())
   (end-annotation
     '())
   (else
    (advance-token)
    (advance-to-annotation-end))))
  
;;; This table is used to check the syntax of all annotation declarations.
;;; There are two parts to the syntax: the annotation arguments and the
;;; placement of the annotation.  Each annotation is described by a triple:
;;;  argument-types, place, kind.
;;; The argument types are a list corresponding to the type of each annotation
;;; argument.  Types are:
;;;    string
;;;    constr-list - used in import-lisp-type, export-lisp-type
;;;    integer
;;;    signature  - used in specialize
;;; The places are
;;;    interface  - an interface file
;;;    decl       - part of an inner decl list
;;;    topdecl    - a top level declaration
;;;    constructor - associated with a constructor slot in a type decl
;;; The kinds are
;;;    val  - the annotation appears by itself
;;;    decl - the annotation is associated with a list of variables using ::

(define *known-annotations* '(
  (|LispName| (args string) (place interface) (kind decl))
  (|CName| (args string) (place interface) (kind decl))
  (|Prelude| (args) (place topdecl interface) (kind val))
  (|Strictness| (args string) (place topdecl interface decl) (kind decl))
;  (|Strict| (args) (place constructor) (kind val))  - I think this is a bug!
  (|NoConversion| (args) (place interface) (kind decl))
  (|Inline| (args) (place topdecl decl) (kind decl))
  (|AlwaysInline| (args) (place topdecl decl) (kind decl))
  (|STRICT| (args) (place constructor) (kind val))
  (|ImportLispType| (args constr-list) (place topdecl interface) (kind val))
  (|ExportLispType| (args constr-list) (place topdecl interface) (kind val))
  (|Complexity| (args integer) (place interface topdecl decl) (kind decl))
  (|Specialize| (args signature) (place topdecl decl) (kind decl))
  ))

(define (get-annotation-description annotation)
  (let ((s (assq annotation *known-annotations*)))
    (cond ((eq? s '#f)
	   (parser-error/recoverable 'unknown-annotation
"Annotation ~A is not defined in this system.~%Annotation comment ignored."
	       annotation)
	   (funcall *annotation-escape*))
	  (else
	   (values (tuple-2-2 (assq 'args (tuple-2-2 s)))
		   (tuple-2-2 (assq 'place (tuple-2-2 s)))
		   (car (tuple-2-2 (assq 'kind (tuple-2-2 s)))))))))

(define (signal-annotation-error)
  (parser-error/recoverable 'annotation-error
     "Annotation syntax error"))

(define (signal-annotation-arg-error name)
  (parser-error/recoverable 'annotation-error
     "Annotation syntax error: arguments to the ~A annotation are incorrect"
      name)
  (funcall *annotation-escape*))

(define (signal-annotation-place-error name)
  (parser-error/recoverable 'annotation-error
     "Annotation syntax error: the ~A annotation is not valid in this context"
      name)
  (funcall *annotation-escape*))

(define (signal-annotation-needs-decl-error name)
  (parser-error/recoverable 'annotation-error
"Annotation syntax error: the ~A annotation must be attached to names using ::"
      name)
  (funcall *annotation-escape*))

(define (signal-annotation-is-value-error name)
  (parser-error/recoverable 'annotation-error
     "Annotation syntax error: ~A cannot be associated with names using ::"
      name)
  (funcall *annotation-escape*))


