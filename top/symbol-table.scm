;;; These routines deal with the global symbol table.  The symbol table
;;; is represented in two stages: a module table which maps module names
;;; onto module structures and local tables within each module which
;;; map names (symbols) to definitions.

;;; create-definition makes a new definition object 

(define (create-definition module name type)
  (cond ((module-prelude? module)
	 (let ((def (table-entry *core-symbols* name)))
	   (cond ((eq? def '#f)
		  (create-definition/non-core module name type))
		 (else
		  (setf (def-unit def) *unit*)
		  (setf (def-module def) (module-name module))
		  ;; *** Should any other properties be reinitialized here?
		  (cond ((or (eq? type 'var) (eq? type 'method-var))
			 (setf (var-fixity def) '#f)
			 (setf (var-signature def) '#f))
			((eq? type 'con)
			 (setf (con-fixity def) '#f)))
		  (when (eq? (module-type *module*) 'interface)
			(add-interface-definition (def-module def) def))
		  def))))
	(else (create-definition/non-core module name type))))

(define (create-definition/non-core module name type)
  (let ((mname  (module-name module)))
    (when (eq? (module-type *module*) 'interface)
       (mlet (((mod name1) (rename-interface-symbol name)))
         (setf mname mod)
	 (setf name name1)))
    (let ((res (create-definition/inner mname name type)))
      (when (eq? (module-type *module*) 'interface)
	 (add-interface-definition mname res))
      res)))

(define (create-definition/inner mname name type)
    (cond ((eq? type 'var)
	   (make var (name name) (module mname) (unit *unit*)))
	  ((eq? type 'con)
	   (make con (name name) (module mname) (unit *unit*)))
	  ((eq? type 'synonym)
	   (make synonym (name name) (module mname) (unit *unit*)))
	  ((eq? type 'algdata)
	   (make algdata (name name) (module mname) (unit *unit*)))
	  ((eq? type 'class)
	   (make class (name name) (module mname) (unit *unit*)))
	  ((eq? type 'method-var)
	   (make method-var (name name) (module mname) (unit *unit*)))
	  ((eq? type 'di)
	   (make deriving (name name) (module mname) (unit *unit*)))
	  (else
	   (error "Bad type argument ~s." type))))

(define (create-top-definition name type)
  (let ((def (create-definition *module* name type)))
    (insert-top-definition name def)
    def))

;;; Interfaces have a special table which resolves imports in the
;;; interface.  Given a name in an interface module this returns the
;;; corresponding full name: a (module,original-name) pair.  Symbols not
;;; imported are assumed to be defined in the interface.

(define (rename-interface-symbol name)
  (let ((res (assq name (module-interface-imports *module*))))
    (if (eq? res '#f)
	(values *module-name* name)
	(values (tuple-2-1 (tuple-2-2 res))
		(tuple-2-2 (tuple-2-2 res))))))

;;; This creates a locally defined var node.

(define (create-local-definition name)
  (let ((var     (make var (name name) (module *module-name*) (unit *unit*))))
    (setf (var-fixity var) (table-entry *fixity-table* name))
    var))

;;; This maintains a list of definitions referenced in an interface, sorted by
;;; module (2 level alist).

(define (add-interface-definition module def)
 (when (not (def-core? def))
  (setf (def-interface? def) '#t)
  (let ((alist (assq module (module-interface-definitions *module*))))
    (if alist
	(setf (cdr alist) (cons def (cdr alist)))
	(setf (module-interface-definitions *module*)
	      (cons (tuple module (list def))
		    (module-interface-definitions *module*)))))))

;;; This function creates a new variable. 
;;; The "root" may be either a symbol or a string.
;;; *unit* defines the home module of the variable.

;;; *** Maybe it would be possible to hack this so that it doesn't
;;; *** create any symbol at all until the name is demanded by something,
;;; *** but that seems like a rather sweeping change.

(define (create-temp-var root)
  (let* ((name   (gensym (if (symbol? root) (symbol->string root) root)))
	 (module  *unit*))
    (make var (name name) (module module) (unit *unit*))))


;;; The following routines install top level definitions into the symbol
;;; table.

(predefine (signal-multiple-name-conflict name old-local-name def))
    ; in import-export/ie-errors.scm

(define (insert-top-definition name def)
  (let ((old-definition (resolve-toplevel-name name)))
    (cond ((eq? old-definition '#f)
	   (when (not (def-core? def))
	       (setf (table-entry *symbol-table* name) def))
	   (when (and (var? def) (not (eq? (var-fixity def) '#f)))
             (setf (table-entry *fixity-table* name)
		   (var-fixity def)))
	   (when (and (con? def) (not (eq? (con-fixity def) '#f)))
             (setf (table-entry *fixity-table* name)
		   (con-fixity def)))
	   (when (not (def-core? def))
 	    (if (eq? (local-name def) '#f)
		(setf (table-entry *inverted-symbol-table* def) name)
		(signal-multiple-name-conflict name (local-name def) def))))
	  ((eq? old-definition def)
	   'OK)
	  ((def-core? old-definition)
	   (signal-core-redefinition name def))
	  ((and (module-uses-standard-prelude? *module*)
		(table-entry *prelude-symbol-table* name))
	   (if (eq? (def-module def) *module-name*)
	       (signal-prelude-redefinition name def)
	       (signal-prelude-reimport name (def-module def) def)))
	  ((eq? (def-module def) *module-name*)
	   (signal-multiple-definition-in-module
	    name *module-name* old-definition def))
	  ((eq? (def-module old-definition) *module-name*)
	   (signal-redefinition-by-imported-symbol
	    name *module-name* def old-definition))
	  (else
	   (signal-multiple-import name *module-name* def old-definition)))))

;;; Gets the fixity of a name.

(define (get-local-fixity name)
  (table-entry *fixity-table* name))

;;; These routines support general scoping issues.  Only vars have local
;;; definitions - all other names are resolved from the global symbol table.

;;; This is used when the name must be in the top symbols.

(define (fetch-top-def name type)
  (let ((def (resolve-toplevel-name name)))
    (cond ((eq? def '#f)
	   (cond ((interface-module? *module*)
		  (mlet (((mod name1) (rename-interface-symbol name)))
		    (if (eq? mod *module-name*)
			(undefined-topsym name type)
			(if (and (module-prelude? *module*)
				 (table-entry *core-symbols* name1))
			    (let ((def (table-entry *core-symbols* name1)))
			      (insert-top-definition name1 def)
			      def)
			    (create-interface-type name mod name1 type)))))
		 (else
		  (undefined-topsym name type))))
	  (else def))))

(define (undefined-topsym name type)
  (signal-undefined-symbol name type)
  *undefined-def*)

;;; Interfaces may contain references to unknown type system objects.
;;; In this case, dummy objects must be created.

(define (create-interface-type local-name mod name type)
  (let ((new-def (create-definition/inner mod name type)))
    (insert-top-definition local-name new-def)
    (push new-def (module-unresolved-symbols *module*))
    new-def))

(define (resolve-toplevel-name name)
 (forward-def
  (let ((pc (table-entry *prelude-core-symbols* name)))
    (cond ((not (eq? pc '#f))
	   pc)
	  ((module-uses-standard-prelude? *module*)
	   (let ((res (table-entry *prelude-symbol-table* name)))
	     (if (eq? res '#f)
		 (resolve-toplevel-name-1 name)
		 res)))
	  (else
	   (resolve-toplevel-name-1 name))))))

(define (resolve-toplevel-name-1 name)
  (cond ((eq? (module-inherited-env *module*) '#f)
	 (table-entry *symbol-table* name))
	(else
	 (let ((res (search-inherited-tables
		     name (module-inherited-env *module*))))
	   (if (eq? res '#f)
	       (table-entry *symbol-table* name)
	       res)))))

(define (search-inherited-tables name mod)
  (if (eq? mod '#f)
      '#f
      (let ((res (table-entry (module-symbol-table mod) name)))
	(if (eq? res '#f)
	    (search-inherited-tables name (module-inherited-env mod))
	    res))))

(define (forward-def def)
  (and def (or (def-forward-to def) def)))

;;; Con-ref's are special in that the naming convention (;Name) ensures
;;; that if a def is found it must be a con.

(define (resolve-con con-ref)
  (when (eq? (con-ref-con con-ref) *undefined-def*)
    (remember-context con-ref
      (let ((def (fetch-top-def (con-ref-name con-ref) 'con)))
	(setf (con-ref-con con-ref) def)))))

(define (resolve-class class-ref)
  (when (eq? (class-ref-class class-ref) *undefined-def*)
    (remember-context class-ref
      (let ((def (fetch-top-def (class-ref-name class-ref) 'class)))
	(when (and (not (eq? def *undefined-def*)) (not (class? def)))
	  (signal-class-name-required def (class-ref-name class-ref)))
	(setf (class-ref-class class-ref) def)))))

(define (resolve-tycon tycon)
  (when (eq? (tycon-def tycon) *undefined-def*)
    (remember-context tycon
      (let ((def (fetch-top-def (tycon-name tycon) 'algdata)))
	(when (class? def)
	  (signal-tycon-name-required (tycon-name tycon) def))
	(setf (tycon-def tycon) def)))))

;;; This should be used after the local environment has been searched.
;;; Other routines dealing with variable scoping are elsewhere.

(define (resolve-var var-ref)
  (when (eq? (var-ref-var var-ref) *undefined-def*)
    (remember-context var-ref
      (let ((def (fetch-top-def (var-ref-name var-ref) 'var)))
	(setf (var-ref-var var-ref) def)))))

;;; *** The inverted-symbol-table is the only table in the whole
;;; *** system that is not keyed off of symbols.  If this is a problem,
;;; *** things that use it could probably be rewritten to do something
;;; *** else, like store an a-list on the def itself.

;;; This does not need to consult the inherited-env flag because when this
;;; is used in extensions only new symbols get inserted.

(define (local-name def)
  (cond ((def-core? def)
	 (def-name def))
	((module-uses-standard-prelude? *module*)
	 (let ((res (table-entry *prelude-inverted-symbol-table* def)))
	   (if (eq? res '#f)
	    (table-entry *inverted-symbol-table* def)
	    res)))
	(else
  	 (table-entry *inverted-symbol-table* def))))
    
(define (print-name x)
  (let ((res (local-name x)))
    (if (eq? res '#f)
	(def-name x)
	res)))


;;; Error signalling routines.

(define (signal-multiple-definition-in-module name modname def1 def2)
 (if (eq? (module-type *module*) 'extension)
     (phase-error 'cant-redefine-in-extension
        "An extension for module ~A cannot redefine the symbol ~A"
	modname name)
     (phase-error/objs 'multiple-definition-in-module (list def1 def2)
        "There is more than one definition for the name ~a in module ~a."
	name modname)))

(define (signal-redefinition-by-imported-symbol name modname def1 def2)
  (phase-error/objs 'redefinition-by-imported-symbol (list def1 def2)
    "The name ~a is defined in module ~a, and cannot be imported."
    name modname))

(define (signal-core-redefinition name def)
  (phase-error/objs 'prelude-redefinition (list def)
    "The name ~a is defined in PreludeCore and cannot be redefined."
    name))

(define (signal-prelude-redefinition name def)
  (phase-error/objs 'prelude-redefinition (list def)
    "The name ~a is defined in the prelude.~%You must hide it if you wish to use this name."
    name))

(define (signal-prelude-reimport name modname def)
  (phase-error/objs 'prelude-redefinition (list def)
    "The name ~a is both imported from ~A and defined in the prelude.~%"
    name modname))

(define (signal-multiple-import name modname def1 def2)
  (phase-error/objs 'multiple-import (list def1 def2)
    "The name ~a is imported into module ~a multiple times."
    name modname))

(define (signal-undefined-symbol name type)
  (if (has-con-prefix? (symbol->string name))
      (remember-undefined-symbol 'con (remove-con-prefix/symbol name))
      (remember-undefined-symbol type name)))

(define (signal-class-name-required name def)
  (phase-error/objs 'class-name-required (list def)
    "The name ~A is used in a context where a class name is required."
    name))

(define (signal-tycon-name-required name def)
  (phase-error/objs 'tycon-required (list def)
    "The name ~A is used in a context where a data type is required."
    name))
