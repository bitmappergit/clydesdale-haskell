;;;==================================================================
;;; Dump code generator
;;;==================================================================

(define (def-dump-index def)
  (let ((code  (def-dump-code def)))
    (cond ((not code)
	   (error "No dump code for def ~s." def))
	  ((and (pair? code)
		(eq? (car code) 'def-n))
	   (cadr code))
	  (else
	   (error "Weird dump code for def ~s." def)))))

;;; This saves slot initialization code.

(define (add-dump-init code)
  (push code *dump-slot-init-code*))


;;; Here is the top-level call.

(define (create-dump-code unit modules load-prelude?)
  (dynamic-let ((*unit* (module-unit (car modules)))
		(*dump-defs*  '())
		(*dump-types*  '())
		(*dump-slot-init-code*  '())
		(*dump-def-counter* 0)
		(*dump-def-code-table* (make-table))
		(*dump-file-names* '())
		(*dump-type-counter* 0)
		(*number-vars-dumped* 0)
		(*number-types-dumped* 0)
		(*number-classes-dumped* 0))
    (let ((res (create-dump-code-aux unit modules load-prelude?)))
      (when (memq 'dumper (dynamic *printers*))
        (pprint* res))
      (when (memq 'dump-stat (dynamic *printers*))
	(format '#t
	  "~&Dumped ~A definitions, ~A type objects, and ~A classes.~%"
          *number-vars-dumped* *number-types-dumped*
	  *number-classes-dumped*)
	(format '#t "Used ~A definitions and ~A type cells.~%"
		*dump-def-counter* *dump-type-counter*))
      res)))

;;; This assumes all modules are in the same compilation unit and that
;;; *unit* is set to that unit.
;;; imod-code establishes local bindings for all the imported modules.
;;; dmod-code establishes local bindings for all the modules defined in
;;; this compilation unit.

(define (create-dump-code-aux unit modules load-prelude?)
  (let* ((imod-counter  0)
	 (imod-alist    '())
	 (explicit-imports (collect-all-imported-modules unit))
	 (all-imports   (if load-prelude?
			    (append (collect-prelude-modules) explicit-imports)
			    explicit-imports))
	 (imod-code     (map (lambda (m)
			       (push (cons (module-name m) imod-counter)
				     imod-alist)
			       (incf imod-counter)
			       `(locate-module ',(module-name m)))
			     all-imports))
	 (dmod-counter  0)
	 (dmod-alist    '())
	 (dmod-code     (map (lambda (m)
			       (push (cons (module-name m) dmod-counter)
				     dmod-alist)
			       (incf dmod-counter)
			       `(make module
				      (unit ',(module-unit m))
				      (name ',(module-name m))
				      (type ',(module-type m))))
			     modules)))
    ;; This actually does most of the work.  It dumps the module asts by
    ;; placing inits for each slot into *dump-slot-init-code*.  A list of
    ;; definitions referenced is maintained in *dump-defs*.
    (dolist (m modules)
      (dump-module m (cdr (assq (module-name m) dmod-alist))))
    ;; This creates the final code
    `(begin
       (setf *writer-version* ',*haskell-compiler-version*)
       (setf *modules-imported* (vector ,@imod-code))
       (setf *modules-loaded* (vector ,@dmod-code))
       ;; This sets the elements individually instead of using the vector
       ;; function, because the vector may be longer than
       ;; call-arguments-limit.
       (setf *defs-referenced*
	     (make-vector ,(dynamic *dump-def-counter*)))
       ,@(map (lambda (d) (make-def-init-code d imod-alist dmod-alist))
	      (nreverse *dump-defs*))
       (setf *types-referenced*
	     (make-vector ,(dynamic *dump-type-counter*)))
       ,@(map (lambda (n)
		`(set-type-n ,(cdr n) ,(car n)))
	      (nreverse *dump-types*))
       (setf *dump-file-names* ',*dump-file-names*)
       ,@(dynamic *dump-slot-init-code*)
       )
    ))




;;; This computes the transitive closure of all modules available to
;;; a unit.

(define (collect-all-imported-modules unit)
  (collect-all-modules-1 (ucache-imported-units unit) '() '()))

(define (collect-all-modules-1 units mods-so-far units-seen)
  (cond ((null? units)
	 mods-so-far)
	((mem-string (car units) units-seen)
	 (collect-all-modules-1 (cdr units) mods-so-far units-seen))
	(else
	 (let ((u (lookup-compilation-unit (car units))))
	   (collect-all-modules-1
	    (append (ucache-imported-units u) (cdr units))
	    (append (ucache-modules u) mods-so-far)
	    (cons (ucache-ufile u) units-seen))))
	))

(define (collect-prelude-modules)
  (let ((prelude-unit (lookup-compilation-unit *prelude-unit-filename*)))
    (append (ucache-modules prelude-unit)
	    (collect-all-imported-modules prelude-unit))))



;;; This code returns the load time definition for an object.  When the
;;; object is a core symbol or in a different unit, previously
;;; created definitions are returned.  Otherwise, a new definition is
;;; created.
  
(define (make-def-init-code d imod-alist dmod-alist)
  (when (def-forward-to d)         ;; If the definition came from an
     (setf d (def-forward-to d)))  ;; interface forward to to the real def
  (cond ((def-prelude? d)
	 ;; Core symbols should never have an entry in *defs-referenced*.
	 ;; See with-new-def.
	 (error "Bad core symbol ~s in *defs-referenced*." d))
	((eq? (def-unit d) *unit*)
	 `(,(cond ((method-var? d) 'set-def-n/method-var)
		  ((var? d) 'set-def-n/var)
		  ((con? d) 'set-def-n/con)
		  ((synonym? d) 'set-def-n/synonym)
		  ((algdata? d) 'set-def-n/algdata)
		  ((class? d) 'set-def-n/class)
		  ((is-type? 'deriving d) 'set-def-n/deriving))
	   ,(def-dump-index d)
	   ,(or (cdr (assq (def-module d) dmod-alist))
		;; This can happen if we have a forward reference to
		;; a def imported from another compilation unit.
		`',(def-module d))
	   ',(def-name d)))
	((is-tuple-constructor? d)
	 `(set-def-n/tuple-con
	   ,(def-dump-index d)
	   ,(tuple-constructor-arity d)))
	((is-tuple-tycon? d)
	 `(set-def-n/tuple-tycon
	   ,(def-dump-index d)
	   ,(tuple-constructor-arity (car (algdata-constrs d)))))
	(else
	 (let ((m (assq (def-module d) imod-alist)))
	   ;; This is a bogus error message.  The problem is that nothing
	   ;; so far ensures units are closed under import/export: some
	   ;; modules may be referenced that are accidentally in the symbol
	   ;; table.  The unit file for the current module needs to be
	   ;; updated when this happens.
	   (when (eq? m '#f)
	     (fatal-error 'symbol-not-in-unit
 "Reference to symbol ~A in module ~A: not in compilation unit.~%"
                (def-name d) (def-module d)))
	   `(set-def-n/import
	     ,(def-dump-index d)
	     ,(cdr m)
	     ',(def-name d))))
	))


;;; Once a module has been compiled, most of its slots are useless.
;;; All we really need to save are the identifying information,
;;; symbol table, and export table.
;;; Instances also need to be dumped here instead of with class objects;
;;; this is because links can go across compilation unit boundaries.
;;; They are fixed up when pulling units out of the cache.
;;; The identifying info is stored when the module variable is bound.


(define (dump-module module index)
  (let ((mod-exp `(lookup-defined-mod ,index))
	(save-all-symbols (or (eq? (module-type module) 'standard)
			      (eq? (module-type module) 'interface)
			      (eq? (module-name module) '|Prelude|))))
    ;; Dump symbol table entries only for defs for which this is
    ;; the "home" module.  (In other words, ignore imported defs.)
    ;; The purpose of this is to allow references from other
    ;; interface files to be resolved; see make-def-init-code.
    ;; Jcp: we need to save the complete symbol table for incremental
    ;; compilation to work.
    (let ((code  '())
	  (defs  '()))
      (table-for-each
        (lambda (key val)
	  (when (or save-all-symbols
		    (eq? (def-module val) (module-name module)))
		(let ((stuff  (dump-object val)))
	      (if (def-prelude? val)
		  (if (eq? key (def-name val))
		      (push `(set-symtab/def tab ,stuff) code)
		      (push `(set-symtab/def/key tab ,stuff ',key) code))
		  (if (eq? key (def-name val))
		      (push (def-dump-index val) defs)
		      (push `(set-symtab/def-n/key
			       tab ,(def-dump-index val) ',key)
			    code))))))
	(module-symbol-table module))
      (add-dump-init `(setf (module-symbol-table ,mod-exp)
			    (let ((tab  (make-table)))
			      (set-symtab/def-n/list tab ',defs)
			      ,@code
			      tab))))
    ;; dump the fixity table - needed by the incremental compiler
    (when save-all-symbols
      (let ((data  '()))
	(table-for-each
	  (lambda (key val)
	    (let ((ass   (fixity-associativity val))
		  (prec  (fixity-precedence val)))
	      (push (list key ass prec) data)))
	  (module-fixity-table module))
	(add-dump-init `(setf (module-fixity-table ,mod-exp)
			      (make-fixity-table ',data)))))
    ;; Save the definition point of the module
    (mlet (((file line) (dump-source-pointer (ast-node-line-number module))))
       (add-dump-init `(setf (ast-node-line-number ,mod-exp)
			     (restore-source-pointer ',file ',line))))
    ;; Dump all export table entries.  This is used by the import/export
    ;; phase to resolve references.  
    (let ((code  '())
	  (defs  '()))
      (table-for-each
        (lambda (key val)
	  ;; val is an a-list of (sym . def) pairs.
	  ;; Look for shortcut to reduce size of generated code.
	  (if (and (null? (cdr val))
		   (eq? (car (car val)) key))
	      (let* ((def    (cdr (car val)))
		     (stuff  (dump-object def)))
		(if (def-prelude? def)
		    (if (eq? key (def-name def))
			(push `(set-export/def tab ,stuff) code)
			(push `(set-export/def/key tab stuff ',key) code))
		    (if (eq? key (def-name def))
			(push (def-dump-index def) defs)
			(push `(set-export/def-n/key
				 tab ,(def-dump-index def) ',key)
			      code))))
	      (push `(setf (table-entry tab ',key) ,(dump-object val))
		    code)))
	(module-export-table module))
      (add-dump-init `(setf (module-export-table ,mod-exp)
			    (let ((tab  (make-table)))
			      (set-export/def-n/list tab ',defs)
			      ,@code
			      tab))))
    ;; Dump the instances.
    (add-dump-init `(setf (module-instance-defs ,mod-exp)
			  ,(dump-object (module-instance-defs module))))
    (add-dump-init `(setf (module-default ,mod-exp)
			  ,(dump-object (module-default module))))
    (add-dump-init `(setf (module-uses-standard-prelude? ,mod-exp)
			  ,(dump-object
			    (module-uses-standard-prelude? module))))
    (add-dump-init `(setf (module-interface-definitions ,mod-exp)
			  (list ,@(map (function dump-interface-definitions)
				       (module-interface-definitions module)))))
    (add-dump-init `(setf (module-unresolved-symbols ,mod-exp)
			  ,(dump-object (module-unresolved-symbols module))))
    (add-dump-init `(setf (module-stand-alone? ,mod-exp)
			  ,(dump-object (module-stand-alone? module))))
    ))


;;; It looks to me like core-symbols never appear in the 
;;; module-interface-definitions lists (see top/symbol-table.scm).

(define (dump-interface-definitions entry)
  (let ((module-name (car entry))
	(defs        (cdr entry)))
    `(interface-def/n
       ',module-name
       ',(map (lambda (d)
		(let ((stuff  (dump-object d)))
		  (declare (ignore stuff))
		  (if (def-prelude? d)
		      (error "Hey!  Core symbols not allowed here!")
		      (def-dump-index d))))
	      defs))
    ))



