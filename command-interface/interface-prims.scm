
;;;=========================================================================
;;; Top-level command loop
;;;=========================================================================


(define *init-complete* '#f)

;;; The savesys code arranges for this function to be called at 
;;; startup time.

(define (heval)
  (initialize-haskell-system)
  (format '#t "~&Yale Haskell ~A~A   ~A~%Type :? for help.~%"
	  *haskell-compiler-version*
	  *haskell-compiler-update*
	  (identify-system))
  (funcall *haskell-initialize-hook*)
  (do () ('#f)
      (let/cc croak
	(dynamic-let ((*abort-compilation*
		        (lambda ()
			  (when *haskell-compilation-error-hook*
			    (funcall *haskell-compilation-error-hook*))
			  (funcall croak '#f)))
		      (*abort-phase*        '#f)
		      (*phase*              'toplevel)
		      (*in-error-handler*   '#f))
	  (funcall *haskell-command-hook*)))))


;;; This is an alternative entry point for emulating a standalone
;;; executable.

(define (hrun file args)
  (initialize-haskell-system)
  (funcall *haskell-initialize-hook*)
  (let/cc croak
    (dynamic-let ((*abort-compilation*
		   (lambda ()
		     (when *haskell-compilation-error-hook*
		       (funcall *haskell-compilation-error-hook*))
		     (funcall croak '#f)))
		  (*abort-phase*        '#f)
		  (*phase*              'toplevel)
		  (*in-error-handler*   '#f))
      (compile/run file args)
      (force-output)
      (exit))))



;;;=========================================================================
;;; Eval and run in extension
;;;=========================================================================

(define (haskell-eval exp extension-name extension module-name maybe-file)
  (declare (ignore extension-name))
  (when (memq 'interactive (dynamic *printers*))
    (format '#t "~%Evaluating ~a...~%" exp))
  (when maybe-file
    (compile/load maybe-file))
  (let* ((module    (find-executable-module module-name))
	 (module+pad (add-pad-definitions module extension)))
    (haskell-exec-aux
      exp
      (format '#f "~A = putText (~a)~%" *magic-temp-name* exp)
      module+pad
      )))

(define (haskell-run exp extension-name extension module-name maybe-file)
  (declare (ignore extension-name))
  (when (memq 'interactive (dynamic *printers*))
    (format '#t "~%Running ~a...~%" exp))
  (when maybe-file
    (compile/load maybe-file))
  (let* ((module     (find-executable-module module-name))
	 (module+pad (add-pad-definitions module extension)))
    (haskell-exec-aux
      exp
      (format '#f "~a = ~a~%~a :: IO ()~%"
	      *magic-temp-name* exp *magic-temp-name*)
      module+pad)))

(define (haskell-run-print exp extension-name extension module-name maybe-file)
  (declare (ignore extension-name))
  (when (memq 'interactive (dynamic *printers*))
    (format '#t "~%Running ~a...~%" exp))
  (when maybe-file
    (compile/load maybe-file))
  (let* ((module     (find-executable-module module-name))
	 (module+pad (add-pad-definitions module extension)))
    (haskell-exec-aux
      exp
      (format '#f "~a = (~a) >>= putText~%~a :: IO ()~%"
	      *magic-temp-name* exp *magic-temp-name*)
      module+pad)))


(define (haskell-report-type exp extension-name extension
			     module-name maybe-file)
  (declare (ignore extension-name))
  (when (memq 'interactive (dynamic *printers*))
    (format '#t "~%Type checking ~a...~%" exp))
  (when maybe-file
    (compile/load maybe-file))
  (let* ((module     (find-executable-module module-name))
	 (module+pad (add-pad-definitions module extension)))
    (haskell-report-type-aux
      exp
      (format '#f "~A = ~A~%" *magic-temp-name* exp)
      module+pad)))

(define (add-pad-definitions module pad)
  (if (string=? pad "")
      module
      (dynamic-let ((*printers*
		     (if (memq 'pad (dynamic *printers*))
			 (dynamic *printers*)
			 (if (memq 'time *printers*)
			     '(time)
			     '()))))
	(let ((new-module
	       (parse-fragment
		(module-name module)
		"-pad"
		pad
		(format '#f "~A pad" (module-name module))
		'#t)))
	  (fragment-initialize new-module module)
	  (eval (modules->lisp-code (list new-module)))
	  new-module))))

(define (haskell-exec-aux extension-name fragment module)
  (dynamic-let ((*printers* '()))
    (prepare-execution)
    (let ((new-module  (parse-fragment
			(module-name module)
			"-exp"
			fragment
			extension-name
			'#f)))
      (fragment-initialize new-module module)
      (eval (modules->lisp-code (list new-module)))
      (run-dialogue *magic-temp-name* new-module)
      new-module)))

(define (haskell-report-type-aux extension-name fragment module)
  (dynamic-let ((*printers* '()))
    (let ((new-module  (parse-fragment
			(module-name module)
			"-exp"
			fragment
			extension-name
			'#f)))
      (fragment-initialize new-module module)
      (modules->lisp-code (list new-module))
      (report-type fragment *magic-temp-name* new-module)
      new-module)))


;;; Helper functions for above

(define (parse-fragment mod-name mod-type fragment filename has-lines?)
  (let* ((new-mod-name (string-append (symbol->string mod-name) mod-type))
	 (module (parse-module-body-from-string
		  (string->symbol new-mod-name)
		  fragment 
		  filename
		  has-lines?)))
    (when (not (null? (module-imports module)))
      (signal-import-decl-in-extension))
    module))

(define (signal-import-decl-in-extension)
  (fatal-error 'import-decl-in-extension
               "Import declarations are not allowed in scratch pads."))


(define (fragment-initialize new old)
  (setf (module-type new) 'extension)
  (setf (module-unit new) (module-unit old))
  (setf (module-uses-standard-prelude? new)
	(module-uses-standard-prelude? old))
  (setf (module-inherited-env new) old)
  (setf (module-fixity-table new)
	(copy-table (module-fixity-table old)))
  (setf (module-default new) (module-default old)))


(define (report-type exp name module)
  (let ((var  (table-entry (module-symbol-table module) name)))
    (if (var? var)
        (format '#t "~&~A :: ~A~%" exp (var-type var))
	(signal-no-definition-of-var name module))))



;;;=========================================================================
;;; Support for operations on files
;;;=========================================================================

;;; This keeps track of modules which are currently available.

(define *modules-available* '())


;;; These both load the code associated with the file
;;; Do NOT mess with the return values from these functions.
;;; The stuff in vanilla.scm depends on getting the unit back.

(define (compile/compile file)
  (compile/common file *compile/compile-cflags*))

(define (compile/load file)
  (compile/common file *compile/load-cflags*))

(define (compile/common file flags)
  (let ((unit  (haskell-compile file flags)))
    (setf *modules-available* (ucache-modules unit))
    unit))
	

    
;;; This loads the compilation unit, then runs the dialogue called 
;;; "main".  If there's more than one module, it looks for the one
;;; called "Main".

(define *haskell-command-line-args* '())
(define *haskell-program-name* "haskell")

(define (compile/run file args)
  (let ((unit (compile/load file)))
    (when unit
      (when (memq 'interactive (dynamic *printers*))
	(format '#t "~%Running main...~%"))
      (let ((mod (find-executable-module/inner '|Main|)))
	(unless mod
	   (if (eqv? (length (ucache-modules-defined unit)) 1)
	       (setf mod (car (ucache-modules unit)))
	       (fatal-error 'ambiguous-module "No Main module found.")))
	(dynamic-let ((*haskell-command-line-args* args)
		      (*haskell-program-name* file))
  	  (run-dialogue '|main| mod))))
    unit))


(define (find-executable-module mod-name)
  (or (find-executable-module/inner mod-name)
      (signal-module-not-found mod-name)))

(define (find-executable-module/inner mod-name)
  (find-executable-module-1 mod-name *modules-available*))

(define (find-executable-module-1 name mods)
  (if (null? mods)
      '#f
      (if (and (eq? name (module-name (car mods)))
	       (not (interface-module? (car mods))))
	  (car mods)
	  (find-executable-module-1 name (cdr mods)))))

(define (signal-module-not-found mod-name)
  (fatal-error 'module-not-found
	       "Module ~a is not currently compiled and loaded."
	       mod-name))




;;;=========================================================================
;;; Support for running dialogues
;;;=========================================================================

(define (run-dialogue name module)
  (let ((var  (table-entry (module-symbol-table module) name)))
    (cond ((not var)
	   (signal-no-definition-of-var name module))
	  ((not (is-dialogue? var))
	   (signal-var-not-dialogue var module))
	  (else
	   (run-dialogue-aux var)))))

(predefine (init-runtime-system)) ; in runtime/runtime-utils.scm

(define (run-dialogue-aux var)
  (multiple-value-bind (result sec)
      (time-execution
        (lambda ()
	  (let/cc x
	    (setf *runtime-abort* (lambda () (funcall x 'error)))
	    (with-io-system
	      ;; Unless Lisp debugging is enabled, establish an error
	      ;; handler that catches all Lisp errors and turns them
	      ;; into Haskell IOErrors that may be caught with the
	      ;; Haskell error handling mechanism.  (The default behavior
	      ;; is just to print the message and exit to the
	      ;; *runtime-abort* continuation established above.)
	      (if *haskell-debug-in-lisp*
		  (run-dialogue-aux-aux var)
		  (with-error-handler
		      (lambda (s) (haskell-other-error "~a" s))
		    (run-dialogue-aux-aux var)))))))
    (declare (ignore result))
    (when (memq 'time *printers*)
      (format '#t "~%Execution time: ~A seconds.~%" sec)))
  'done)


(define (run-dialogue-aux-aux var)
  (let ((fn (eval (fullname var))))
    (unless (var-strict? var)
      (setf fn (force fn)))
    (funcall fn (box 'state))))

(define (is-dialogue? var)
  (let ((gtype (var-type var)))
    (when (not (gtype? gtype))
      (error "~s is not a Gtype." gtype))
    (and (null? (gtype-context gtype))
	 (let ((type  (expand-ntype-synonym (gtype-type gtype))))
	   (and (ntycon? type)
		(eq? (ntycon-tycon type) (core-symbol "Arrow"))
		(let* ((args (ntycon-args type))
		       (a1 (expand-ntype-synonym (car args)))
		       (a2 (expand-ntype-synonym (cadr args))))
		  (and (ntycon? a1)
		       (eq? (ntycon-tycon a1) (core-symbol "SystemState_"))
		       (ntycon? a2)
		       (eq? (ntycon-tycon a2) (core-symbol "IOResult_")))))))
    ))


(define (signal-no-definition-of-var name module)
  (fatal-error 'no-definition-of-var
	       "Module ~a has no definition of `~a'."
	       (module-name module) name))

(define (signal-var-not-dialogue var module)
  (phase-error/objs 'var-not-dialogue (list var)
   "The definition of `~a' in module ~a is not of type IO()."
   (get-object-name var) (get-object-name module))
  (abort-compilation))


;;;=========================================================================
;;; Miscellaneous helper functions
;;;=========================================================================


;;; set printers/optimizers

(define (set-printers args mode)
  (set-switches *printers* (strings->syms args)
                mode *all-printers* "printers"))

(define (set-optimizers args mode)
  (set-switches *compiled-code-optimizers* (strings->syms args)
                mode *all-optimizers* "optimizers"))

(define (set-switches current new mode all name)
  (dolist (s new)
    (when (and (not (eq? s 'all)) (not (memq s all)))
      (signal-invalid-value s name all)))
  (let ((res (cond ((eq? mode '+)
                    (set-union current new))
                   ((eq? mode '-)
                    (set-difference current new))
                   ((eq? mode '=)
                    (if (equal? new '(all))
                        all
                        new)))))
    res))

(define (signal-invalid-value s name all)
  (recoverable-error 'invalid-value
    "~A is not one of the valid ~A.  Possible values are: ~%~A"
    s name all))

(define (print-file file)
  (call-with-input-file file (function write-all-chars)))

(define (write-all-chars port)
  (let ((line  (read-line port)))
    (if (eof-object? line)
        'ok
        (begin
          (write-line line)
          (write-all-chars port)))))

(define (strings->syms l)
  (map (lambda (x)
         (string->symbol (string-upcase x)))
       l))


;;; This is used to initialize the system before executing any code
;;; First check that all implementations are present.  Then run
;;; initcode to clean up all modules

(define (prepare-execution)
  (unless *init-complete*
    (dolist (fn *current-initcode*)
       (funcall fn))
    (setf *init-complete* '#t)))

