
;;; This is similar to symbol-table.scm except that this maintains the
;;; set of modules which form a program.

;;; The following functions deal with the module table (*modules*):

;;;  (initialize-module-table) - this clears out all modules from the
;;;      symbol table.  Every compilation should start with this.
;;;  (add-modules-to-environment modules) - this takes a module asts,
;;;      either from a .exp file or previous compilation with the same
;;;      incarnation of the compiler and adds it to the set of `known'
;;;      modules.
;;;  (add-module-to-program module)
;;;  (locate-module name) 
;;;      return the module, interface or implementation,
;;;  (get-interface-modules

(define (initialize-module-table)
  (setf *modules* (make-table))
  (setf *implementations* '())
  (setf *interfaces* '())
  (setf *modules-being-compiled* '()))

(define (add-modules-to-environment mods)
  (dolist (mod mods)
    (let ((old-mod (table-entry *modules* (module-name mod))))
      (unless (eq? mod old-mod)
        (cond ((not old-mod)
	       (setf (table-entry *modules* (module-name mod)) mod)
	       (if (interface-module? mod)
		   (push mod *interfaces*)
		   (push mod *implementations*)))
	      ((interface-module? old-mod)
	       (if (interface-module? mod)
		   (multiple-interface-error mod old-mod)
		   (begin
		     (push mod *implementations*)
		     (setf (table-entry *modules* (module-name mod))
			   mod))))
	      ((not (interface-module? mod))
	       (multiple-implementation-error mod old-mod))
	      (else ; must be an interface for an implementation
	            ; ignore them for now (ignore separate comp issue too!)
	       (unless (memq mod *interfaces*)
		  (push mod *interfaces*))))))))

(define (multiple-interface-error mod1 mod2)
  (phase-error 'multiple-interfaces
	       "Two different interfaces for module ~A exist."
	       (module-name mod1))
  mod2)

(define (multiple-implementation-error mod1 mod2)
  (phase-error 'multiple-interfaces
	       "Two different interfaces for module ~A exist."
	       (module-name mod1))
  mod2)

;;; This saves all interface modules in *interfaces*.  It also allows
;;; an interface to be replaced by a standard module.

(define (add-modules-to-program mods)
 (setf *modules-being-compiled* (append *modules-being-compiled* mods))
 (dolist (module mods)
   (let* ((name (module-name module))
	  (old-module (table-entry *modules* name)))
    (when (and old-module (not (eq? (module-type module) 'extension))
	       (not (interface-module? old-module)))
       (signal-module-double-definition module old-module))
    (setf (table-entry *modules* name) module))))

(define (locate-module name)
  (table-entry *modules* name))

(define (get-all-interfaces)
  *interfaces*)

(define (get-compiled-modules)
  *implementations*)

(define (get-all-modules)
  (append *modules-being-compiled* *implementations* *interfaces*))

;;;  (walk-modules mod-list fn) - this calls fn for each module in the
;;;      mod-list.  It also binds the global variable *module* to the
;;;      current module, *symbol-table* to the local symbol
;;;      table.  The fixity table is also placed in a global.

(define (walk-modules mods fn)
  (dolist (mod mods)
    (dynamic-let ((*module* mod)
		  (*module-name* (module-name mod))
		  (*symbol-table* (module-symbol-table mod))
		  (*fixity-table* (module-fixity-table mod))
		  (*inverted-symbol-table* (module-inverted-symbol-table mod)))
       (funcall fn))))

(define (signal-module-double-definition m1 m2)
  (phase-error/objs 'module-double-definition (list m1 m2)
      "Module ~s is defined more than once." (module-name m1) ))



