;;; This is the main driver for the import / export routine

(define (import-export modules)
  (add-modules-to-program modules)
  (walk-modules modules
      (lambda () (init-module-structure)))
  (import-export/fixpoint modules '#t)
  (walk-modules modules (lambda () (check-missing-names)))
  (when (memq 'import (dynamic *printers*))
    (show-export-tables modules))
  (walk-modules (get-all-interfaces)
      (lambda () (attach-external-definitions modules)))
  (walk-modules (get-all-interfaces)
     (lambda () (forward-dangling-references)))
  modules)

(define (import-export/interface modules)
  (add-modules-to-program modules)
  (walk-modules modules
      (lambda () (init-module-structure)))
  (walk-modules modules
      (lambda () (create-top-definitions)
	         (attach-fixities))))

(define (import-export/fixpoint modules initial-cycle?)
  (setf *new-exports-found?* '#f)
  (walk-modules modules
   (lambda ()
     (setf (module-fresh-exports *module*) '())
     (when initial-cycle?
       (create-top-definitions)
       (attach-fixities)
       (import-non-local))
     (locally-import)
     (locally-export)))
  (when *new-exports-found?*
      (import-export/fixpoint modules '#f)))

;;; This does the non-local importing from previously defined modules

(define (import-non-local)
  (setf (module-imports *module*)
	(process-non-local-imports (module-imports *module*))))

(define (process-non-local-imports imports)
  (if (null? imports)
      '()
      (let* ((import (car imports)))
        (with-slots import-decl (module mode specs renamings) import
	  (cond ((eq? *unit* (module-unit module))
		 (cons import (process-non-local-imports (cdr imports))))
		((eq? mode 'all)
		 (import-all-entities module specs renamings import)
		 (process-non-local-imports (cdr imports)))
		(else
		 (import-named-entities module specs renamings import)
		 (process-non-local-imports (cdr imports))))))))

(define (import-all-entities module hiding renamings import-decl)
  (table-for-each
   (lambda (name group)
     (declare (ignore name))
     (cond ((in-hiding-list? group hiding)
	    (setf hiding (remove-entity group hiding)))
	   (else
	    (import-group (rename-group group renamings) module))))
   (module-export-table module))
  (when (not (null? hiding))
    (remember-context import-decl
      (dolist (h hiding)
	(signal-unused-hiding (entity-name h) (module-name module)))))
  (find-unused-renamings renamings import-decl))

(define (import-named-entities mod specs renamings import-decl)
  (dolist (entity specs)
    (let ((group (locate-entity/export-table entity mod '#t)))
      (when (not (eq? group 'error))
	(setf group (rename-group group renamings))
	(import-group group mod))))
  (find-unused-renamings renamings import-decl))

;;; This takes a module and processes the import declarations, moving as
;;; many entities from the freshly exported components of other modules into
;;; the current module.

(define (locally-import)
  (dolist (import (module-imports *module*))
    (with-slots import-decl (module mode specs renamings) import
      (if (eq? mode 'all)
	  (import-fresh-entities import module specs renamings)
	  (setf (import-decl-specs import)
		(import-entities specs module renamings))))))

(define (import-fresh-entities import module hiding renamings)
  (dolist (group (module-fresh-exports module))
    (cond ((in-hiding-list? group hiding)
	    (setf hiding (remove-entity group hiding)))
	   (else
	    (import-group (rename-group group renamings) module))))
  (setf (import-decl-specs import) hiding))

(define (import-entities entities module renamings)
  (if (null? entities)
      '()
      (let ((group (locate-entity/export-table (car entities) module '#f)))
	(cond ((eq? group 'not-found)
	       (cons (car entities)
		     (import-entities (cdr entities) module renamings)))
	      ((eq? group 'error)
	       (import-entities (cdr entities) module renamings))
	      (else
	       (setf group (rename-group group renamings))
	       (import-group group module)
	       (import-entities (cdr entities) module renamings))))))

;;; This imports a group into *module*.  module is the place the group is
;;; taken from.

(define (import-group group module)
  (when (memq module (module-exported-modules *module*))
    (export-group group))
  (dolist (n-d group)
    (insert-top-definition (tuple-2-1 n-d) (tuple-2-2 n-d))))

;;; This takes as yet unresolved exports and moves them to the export table.

(define (locally-export)
  (setf (module-exports *module*)
	(export-entities (module-exports *module*))))

(define (export-entities entities)
  (if (null? entities)
      '()
      (let* ((entity (car entities))
	     (group (locate-entity entity)))
	(cond ((eq? group 'error)
	       (export-entities (cdr entities)))
	      ((eq? group 'not-found)
	       (cons entity (export-entities (cdr entities))))
	      (else
	       (export-group group)
	       (export-entities (cdr entities)))))))


;;; This moves a group into the export table.  If this export is new,
;;; a flag is set.

(define (export-group group)
  (let* ((export-table (module-export-table *module*))
	 (old-group (table-entry export-table (group-name group))))
    (when (or (eq? old-group '#f)
	      (and (hidden-constructors? old-group)
		   (not (hidden-constructors? group))))
      (setf (table-entry export-table (group-name group)) group)
      (dolist (n-d group)
        (setf (def-exported? (tuple-2-2 n-d)) '#t))
      (push group (module-fresh-exports *module*))
      (setf *new-exports-found?* '#t))))

(define (show-export-tables modules)
  (walk-modules modules
    (lambda ()
      (format '#t "~%Exports from module ~A~%" *module-name*)
      (let ((exports '()))
	(table-for-each (lambda (key val)
			  (push (cons key val) exports))
			(module-export-table *module*))
	(setf exports (sort-list exports
				 (lambda (x y)
				   (string-ci<? (symbol->string (car x))
						(symbol->string (car y))))))
	(dolist (e exports)
          (print-exported-group (car e) (group-definition (cdr e))
				(cdr (cdr e))))))))

(define (print-exported-group name def extras)
  (if (eq? (def-module def) *module-name*)
      (format '#t " ")
      (format '#t "*"))
  (cond ((synonym? def)
	 (format '#t "type  "))
	((algdata? def)
	 (format '#t "data  "))
	((class? def)
	 (format '#t "class "))
	(else
	 (format '#t "      ")))
  (format '#t "~A" name)
  (when (not (eq? name (def-name def)))
     (format '#t "[~A]" (def-name def)))
  (when extras
     (format '#t " (")
     (print-exported-group-1 extras (algdata? def)))
  (format '#t "~%"))

(define (print-exported-group-1 extras alg?)
  (let* ((name (tuple-2-1 (car extras)))
	 (ns (symbol->string name))
	 (def (tuple-2-2 (car extras))))
    (format '#t "~A" (if alg? (remove-con-prefix ns) ns))
    (when (not (eq? name (def-name def)))
      (let ((name1 (symbol->string (def-name def))))
	  (format '#t "[~A]" (if alg? (remove-con-prefix name1) name1))))
    (if (null? (cdr extras))
	(format '#t ")")
	(begin
	  (format '#t ",")
	  (print-exported-group-1 (cdr extras) alg?)))))



;;; This is for interfaces used by the program.  This walks over all known
;;; interfaces and distributes their definitions to the appropriate module.
;;; The forward-to field of every def in the interfaces is initialized here.

(define (attach-external-definitions mods)
   ;; The interface-definitions slot contains an alist of module-name and
   ;; defs located in that module.
   (dolist (alist (module-interface-definitions *module*))
     (let* ((mod (car alist))
	    (defs (cdr alist))
	    (mod-ast (locate-module mod)))
       (cond ((not mod-ast)
	      ;; The module is totally unknown - create a dummy module
	      ;; to hold definitions from it.
	      (let ((dummy-module (make module
					(name mod)
					(type 'psuedo-interface)
					(symbol-table (make-table)))))
		(let ((st (module-symbol-table dummy-module)))
		  (dolist (def defs)
		    (setf (table-entry st (def-name def)) def)))
		(add-modules-to-environment (list dummy-module))))
	     ((eq? (module-type mod-ast) 'standard)
	      ;; The module is defined as an implementation.  In this case,
	      ;; the defs are forwarded to local symbols.
	      (let ((st (module-symbol-table mod-ast))
		    (compiled-now? (memq mod-ast mods)))
		(dolist (def defs)
		  (let ((new-def (table-entry st (def-name def))))
		    (cond (new-def
			;; This needs more error checking - we might forward
			;; a class to a type or some such!
			   (setf (def-forward-to def) new-def)
			   (unless compiled-now?
				   (check-interface def new-def)))
			  (else
			   ;; The interface said it was here but NO!
			   (missing-interface-symbol-error def)))))))
	     ((eq? (module-type mod-ast) 'interface)
	      (if (eq? mod-ast *module*)
		  (dolist (def defs)
 	           (setf (def-forward-to def) '#f)) ; clean out old forwarding
		  (let ((st (module-symbol-table mod-ast)))
		    (dolist (def defs)
		      (let ((new-def (table-entry st (def-name def))))
			(cond (new-def
			       (check-interface def new-def))
			      (else
			       ;; The interface said it was here but NO!
			       (missing-interface-symbol-error def))))))))
	     (else
	      ;; Must be a dummy interface
	      (let ((st (module-symbol-table mod-ast)))
		(dolist (def defs)
		  (setf (def-forward-to def) '#f)
		  (let ((new-def (table-entry st (def-name def))))
		    (cond (new-def
			    (check-interface/dummy def new-def)
			    (when (def-more-specific? def new-def)
			       (setf (table-entry (module-symbol-table mod-ast)
						  (def-name def))
				     def)))
			  (else
			   ;; The interface said it was here but NO!
			   (missing-interface-symbol-error def)))))))))))


(define (forward-dangling-references)
  (dolist (def (module-unresolved-symbols *module*))
    (let ((mod (locate-module (def-module def))))
      (if mod
	  (let ((def1 (table-entry (module-symbol-table mod) (def-name def))))
	    (if def1
		(begin
		  (check-interface/dangling def def1)
		  (setf (def-forward-to def) (forward-def def1)))
		(missing-interface-symbol-error def)))
	  (missing-interface-symbol-error def)))))

(define (missing-interface-symbol-error def)
  (phase-error/objs 'missing-interface-symbol (list def)
    "Interface ~A requires a definition of ~A which is not present"
      *module-name* (get-object-name def)))


	    