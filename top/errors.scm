;;; This file contains general error handling routines.

;;; This is the general error handler.  It has three arguments: an
;;; id, error type, and an error message.  The message is a list of
;;; format, arglist combinations.

;;; The error types are:
;;;   warning       -> control returns and compilation proceeds
;;;                    The message may be suppressed
;;;   recoverable   -> control returns and compilation proceeds
;;;   phase         -> control returns but compilation is aborted
;;;                         after the phase in *abort-point*.
;;;   fatal         -> control goes back to the top level
;;;   internal      -> enters the break loop or does a fatal error

;;; Two globals control error behavior:
;;;   *break-on-error?* enter the break loop on any error
;;;   *never-break?* never enter the break loop, even for internal errors.

(define *break-on-error?* '#f)
(define *never-break?* '#f)

;;; The global *error-output-port* controls where errors are printer.

(define (format-error-msg . args)
  (apply (function format) *error-output-port* args))

;;; The strategy here is to first write a banner message based on the id and
;;; type, write out the messages, and then take action depending on the type.

(define *in-error-handler?* '#f)

(define (haskell-error id type messages)
  (declare (ignore id))
  (dolist (m messages)
    (apply (function format) *error-output-port* m)
    (fresh-line *error-output-port*))
  (maybe-show-context (dynamic *context*))
  (format *error-output-port* "~%")  ; add a blank between error messages
  (if (dynamic *in-error-handler?*)
      (error "Recursive error in haskell-error.")
      (begin
        (dynamic-let ((*in-error-handler?*  '#t))
	  (cond (*break-on-error?*
		 (haskell-breakpoint))
		((eq? type 'internal)
		 (if *never-break?*
		     (abort-compilation)
		     (haskell-breakpoint)))
		((eq? type 'fatal)
		 (abort-compilation))
		((eq? type 'phase)
		 (halt-compilation))))
	(when (and (memq type '(recoverable phase))
		   (dynamic *recoverable-error-handler*))
	  (funcall (dynamic *recoverable-error-handler*)))
	'ok)))

(define (err-type->banner err-type)
  (cond ((eq? err-type 'warning)
	 "Warning: ")
	((eq? err-type 'recoverable)
	 "")
	((eq? err-type 'phase)
	 "")
	((eq? err-type 'fatal)
	 "")	
	((eq? err-type 'internal)
	 "Internal-error: ")
	(else "???")))

(define (maybe-show-context context)
  (when context
    (with-slots source-pointer (line file) (ast-node-line-number context)
      (fresh-line *error-output-port*)
      (if (< line 0)
	  (format *error-output-port* "Error occured in `~A'" file)
	  (format *error-output-port* "Error occurred at line ~A in file ~A.~%"
		  line file)))))

(define (get-context-file context)
  (if context (source-pointer-file (ast-node-line-number context)) "unknown"))

(define (get-context-line context)
  (if context (source-pointer-line (ast-node-line-number context)) "unknown"))

;;; A few entry points into the error system.
;;; As a matter of convention, there should be a signaling function defined
;;; for each specific error condition that calls one of these functions.
;;; Error messages should be complete sentences with proper punctuation
;;; and capitalization.  The signaling function should use the message
;;; to report the error and not do any printing of its own.

(define (fatal-error id . msg)
 (haskell-error id 'fatal (list msg)))

(define (haskell-warning id . msg)
 (haskell-error id 'warning (list msg)))

(define (recoverable-error id . msg)
 (haskell-error id 'recoverable (list msg)))

(define (compiler-error id . msg)
 (haskell-error id 'internal (list msg)))

(define (phase-error id . msg)
 (haskell-error id 'phase (list msg)))

(define (phase-error/objs id objs . msg)
  (haskell-error id 'phase
    (cons msg (concat (map (function show-definition-point) objs)))))

;;; This function puts the compiler into the lisp breakloop.  this may
;;; want to fiddle the programming envoronment someday.

(define (haskell-breakpoint)
 (error "Haskell breakpoint."))


;;; This deals with error at runtime

(define *haskell-backtrace-depth* 50)
(define *haskell-backtrace* '#f)

(define *runtime-abort* '())

(define (haskell-runtime-error msg)
  (format '#t "~&~%Haskell runtime abort.~%~A~%" msg)
  (haskell-backtrace)
  (funcall (dynamic *runtime-abort*)))

(define (haskell-backtrace)
  (when *haskell-backtrace*
    (backtrace *haskell-backtrace-depth*)))


;; Some common error handlers

(define (signal-unknown-file-type filename)
  (fatal-error 'unknown-file-type
    "The filename ~a has an unknown file type."
    filename))

(define (signal-file-not-found filename)
  (fatal-error 'file-not-found
    "The file ~a doesn't exist."
    filename))
                                                       
;;; This is support for undefined name messages.

(define *undefined-syms* '())

(define (watch-for-undefined-symbols)
  (setf *undefined-syms* '()))

(define (remember-undefined-symbol ty name)
  (let ((alist (assq *module-name* *undefined-syms*))
	(r (list ty name *context*)))
    (if alist
	(setf (cdr alist) (cons r (cdr alist)))
	(push (cons *module-name* (list r)) *undefined-syms*))))

(define (show-undefined-symbols)
  (if (null? *undefined-syms*)
      'ok
      (let* ((first-syms (cdr (car *undefined-syms*)))
	     (first-context (caddr (car first-syms)))
	     (l (get-context-line first-context)))
	(if (and (number? l) (< l 0))
	    (if (null? (cdr first-syms))
		(format-error-msg
		   "The name `~A' is undefined in the expression `~A'~%"
		   (cadr (car first-syms))
		   (get-context-file first-context))
		(begin
		  (format-error-msg
		   "The expression `~A' contains undefined names:~%"
		   (get-context-file first-context))
		  (show-undefined-syms (tuple-2-2 (car *undefined-syms*)) '#f)))
	    (begin
	      (format-error-msg "Undefined names are present~%")
	      (dolist (alist *undefined-syms*)
		 (format-error-msg "In module ~A (file ~A):~%"
		      (tuple-2-1 alist)
		      (get-context-file (caddr (car (tuple-2-2 alist)))))
		 (show-undefined-syms (tuple-2-2 alist) '#t))))
	(halt-compilation))))

(define (show-undefined-syms refs show-lines?)
  (when refs
   (mlet ((kind (car (car refs)))
	  (name (cadr (car refs)))
	  ((line-nums refs1) (find-refs-to-name kind name refs))
	  (kind-name (cond ((eq? kind 'var) "Variable")
			   ((eq? kind 'algdata) "Type")
			   ((eq? kind 'class) "Class")
			   ((eq? kind 'deriving) "Deriving")
			   ((eq? kind 'con) "Constructor")
			   (else kind))))
      (if show-lines?
	  (begin
	    (format-error-msg "~A ~A, referenced at line~A "
			 kind-name
			 name
			 (if (null? (cdr line-nums)) "" "s"))
	    (let ((c '#f))
	      (dolist (l (reverse line-nums))
	        (format-error-msg "~A~A" (if c ", " "") l)
		(setf c '#t)))
	    (format-error-msg "~%"))
	  (format-error-msg "~A ~A~%" kind-name name))
       (show-undefined-syms refs1 show-lines?))))

(define (find-refs-to-name ty name refs)
  (if refs
      (mlet (((r1 rest) (find-refs-to-name ty name (cdr refs))))
	(if (and (eq? ty (car (car refs))) (eq? name (cadr (car refs))))
	    (values (cons (get-context-line (caddr (car refs))) r1) rest)
	    (values r1 (cons (car refs) rest))))
      (values '() '())))

;;; These routines show where an object is defined.

(define (show-definition-point obj)
  (let ((sp (cond ((is-type? 'def obj)
		   (def-where-defined obj))
		  ((is-type? 'ast-node obj)
		   (ast-node-line-number obj))
		  (else '#f))))
    (if sp
	(list (list "~A ~A is defined at line ~A in file ~A"
		    (get-object-kind obj)
		    (get-object-name obj)
		    (source-pointer-line sp)
		    (source-pointer-file sp)))
	'())))

(define (get-object-name def)
  (cond ((eq? def (core-symbol "List")) "[]")
	((eq? def (core-symbol "UnitType")) "()")
	((eq? def (core-symbol "Arrow")) "(->)")
	((con? def)
	 (remove-con-prefix (symbol->string (def-name def))))
	((is-type? 'deriving def)
	 (remove-di-prefix/string (symbol->string (def-name def))))
	((is-type? 'def def)
	 (symbol->string (def-name def)))
	((instance? def)
	 (format '#f "~A(~A)" (get-object-name (instance-class def))
		              (get-object-name (instance-algdata def))))
	((is-type? 'module def)
	 (symbol->string (module-name def)))
	(else "unknown object")))

(define (get-object-kind def)
  (cond ((method-var? def)
	 "Method variable")
	((var? def)
	 "Variable")
	((con? def)
	 "Constructor")
	((algdata? def)
	 "Data type")
	((synonym? def)
	 "Type synonym")
	((class? def)
	 "Class")
	((instance? def)
	 "Instance")
	((and (is-type? 'module def)
	      (interface-module? def))
	 "Interface")
	((is-type? 'module def)
	 "Module")
	((is-type? 'deriving def)
	 "Derived instance")
	(else "unknown")))


;;; While it would be nice to redo the formatting language a bit for error
;;; messages, for the moment most formatting is done outside the format
;;; string using a few simple functions.  These all have short names so that

;;; The basic routine here is to limit the size of an object embedded within
;;; a message.

(define (sz x n)  ; limit the size of x to n; don't add a newline
  (format-sized x n '#f))

(define (szn x n)
  (format-sized x n '#t)) ; limit x and add a newline

(predefine (ntype->gtype ty))

(define (szt x n)  ; as sz except an ntype is printed.
  (format-sized (ntype->gtype x) n '#f))

(define (sznt x n)  ; as sz except an ntype is printed.
  (format-sized (ntype->gtype x) n '#t))

;;; Some generic error handlers

(define (signal-bad-annotated-var n a)
  (recoverable-error 'non-local-name-in-annotation
"The variable ~A in annotation ~%~A~%is not defined by this declaration group"
    n a))

