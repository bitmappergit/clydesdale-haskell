;;; Error checks & calls for the import-export code

;;; this is called at the end of import-export to look for
;;;  a) exported entities that were never found
;;;  b) imported entities that were never found
;;;  c) renamed entities that were never found
;;;  d) hidden entities that were never found

;;;  %%% Lots of these should be phase errors - jcp

(define (maybe-remove-con-prefix name)
  (let ((str (symbol->string name)))
    (if (has-con-prefix? str)
	(remove-con-prefix str)
	str)))

(define (check-missing-names)
  (dolist (export (module-exports *module*))
    (remember-context export
      (signal-missing-export export)))
  (dolist (import-decl (module-imports *module*))
    (remember-context import-decl
      (with-slots import-decl (mode specs renamings) import-decl
        ;; *** I'm confused.  Aren't these errors already detected
	;; *** by import-all-entities and import-named-entities?
	;; jcp: no - a final check is needed after all symbols have moved.
        (cond ((eq? mode 'all)
	       (dolist (entity specs)
		 (signal-unused-hiding
		   (entity-name entity)
		   (import-decl-module-name import-decl))))
	      (else
	       (dolist (entity specs)
		 (signal-entity-not-found
		   (entity-name entity)
		   (import-decl-module-name import-decl)))))
	(find-unused-renamings renamings import-decl)))))

(define (find-unused-renamings renamings import-decl)
  (dolist (r renamings)
    (when (not (renaming-referenced? r))
      (remember-context r
	(signal-unused-renaming (renaming-from r)
				(import-decl-module-name import-decl))))))

(define (check-duplicates l entity)
  (when (not (null? (find-duplicates l)))
    (signal-duplicate-names-in-entity entity)))

;;; There are a ton of possible errors in import-export.  All error
;;; calls are found here:

(define (signal-missing-export export)
  (recoverable-error 'missing-export
    "Module ~A exports ~A, but provides no definition for it."
    *module-name* export))

(define (signal-unused-renaming name module-name)
  (recoverable-error 'unused-renaming
    "The name ~a is included in the renaming list of an import declaration,~%~
     but is not among the entities being imported from module ~a."
    name module-name))

(define (signal-unused-hiding name module-name)
  (recoverable-error 'unused-hiding
    "The name ~a is included in the hiding list of an import declaration,~%~
     but is not among the entities exported from module ~a."
    name module-name))

(define (signal-multiple-name-conflict name old-local-name def)
  (recoverable-error 'multiple-name-conflict
    "In module ~A, the symbol ~A from module ~A~%is known as both ~A and ~A."
    *module-name* (def-name def) (def-module def) name old-local-name))

;;; This should show the modules available to the user!

(define (signal-undefined-module-import name)
  (phase-error 'undefined-module-import
      "Cannot find module ~A, imported by module ~A."
	       name *module-name*))

(define (signal-self-import name)
  (phase-error 'self-import
	       "Module ~A cannot import itself."
	       name))

(define (signal-missing-prelude)
  (fatal-error 'missing-prelude "Can't find module Prelude."))

(define (signal-missing-prelude-core)
  (fatal-error 'missing-prelude "Can't find module PreludeCore."))

(define (signal-export-not-imported name)
  (recoverable-error 'export-not-imported
    "Module ~A is exported from ~A,~%~
     but is not also imported into that module."
    name *module-name*))

(define (signal-entity-not-found name module-name)
  (phase-error 'entity-not-found
    "The entity ~a is not exported from module ~a." name module-name))

(define (signal-synonym-needs-dots name module-name)
  (declare (ignore module-name))
  (phase-error 'synonym-needs-dots
    "The entity ~a is a type synonym; to name it in an import or export~%~
     list, you must use `~a(..)'."
    name name))

(define (signal-wrong-definition expected name module-name)
  (phase-error 'wrong-definition
    "The entity ~a does not name a ~a in module ~a."
    name expected module-name))

(define (signal-abstract-type name module-name)
  (phase-error 'abstract-type
    "The entity ~a names an abstract type in module ~a;~%~
     the constructors for this type are not available."
    name module-name))

(define (signal-extra-constituent entity name what)
  (phase-error 'extra-constituent
    "The entity ~a includes the ~a name ~a,~%~
     which is not present in its definition."
    (sz entity 20) what (maybe-remove-con-prefix name)))

(define (signal-missing-constituent entity name what)
  (phase-error 'missing-constituent
    "The entity ~a does not include the ~a name ~a,~%~
     which is part of its definition."
    (sz entity 20) what (maybe-remove-con-prefix name)))

(define (signal-duplicate-names-in-entity entity)
  (phase-error 'duplicate-names-in-entity
    "The entity ~a includes duplicate names."
    (sz entity 30)))

(define (signal-export-method-var name def)
  (phase-error/objs 'export-method-var (list def)
    "The method variable ~a must be exported with its class.~%~
     It can not be named individually in an export list."
    name))

(define (signal-prelude-renaming def name)
  (recoverable-error 'cant-rename-core
     "Names in PreludeCore cannot be renamed: ~a was renamed to ~a"
     (def-name def) name))

(define (signal-non-local-fixity op)
  (recoverable-error 'fixity-must-be-local
     "The fixity for ~A will be ignored since it is not defined in this module"
     op))

;;; This should be syntacticly illegal

(define (signal-fixity-not-var/con op)
  (recoverable-error 'fixity-requires-var-or-con
     "The fixity for ~A will be ignored since it is not a value or constructor"
     op))

