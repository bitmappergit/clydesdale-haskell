
;;; This processes type declarations (data, type, instance, class)
;;; Static errors in type declarations are detected and type decls
;;; are replaced by type definitions.  All code (class and instance
;;; definitions) is moved to the module decls.

(define *synonym-refs* '())

(predefine (add-derived-instances modules))
   ; in derived/derived-instances.scm

(define (process-type-declarations modules)
;;; Convert data & type decls to definitions
 (let ((interface? (interface-module? (car modules))))
  (setf *synonym-refs* '())
  (watch-for-undefined-symbols)
  (walk-modules modules
   (lambda ()
     (setf (module-alg-defs *module*)
	   (map (function algdata->def) (module-algdatas *module*)))
     (setf (module-synonym-defs *module*)
	   (map (function synonym->def) (module-synonyms *module*)))
     (when (not interface?)
	(dolist (ty (default-decl-types (module-default *module*)))
		(resolve-type ty)))))
        ;; A test to see that ty is in Num and is a monotype is needed here.
  ;; In an interface you can't do the superclasses or synonyms since
  ;; definitions of imported objects may not be available.
  (unless interface?
   (multiple-value-bind (ty vals) (topsort *synonym-refs*)
     (when (eq? ty 'cyclic) (signal-recursive-synonyms vals))))
  ;; Convert class declarations and instance declarations to definitions.
  (walk-modules modules
   (lambda ()
     (setf (module-class-defs *module*)
	   (map (function class->def) (module-classes *module*)))))
  (unless interface?
    (walk-modules modules
       (lambda ()
          (dolist (class (module-class-defs *module*))
            (setup-class-slots class))))
    (walk-modules modules
       (lambda ()
          (dolist (class (module-class-defs *module*))
            (create-selector-functions class '#f))))
    (install-instance-links))
  (walk-modules modules
    (lambda ()
     (setf (module-instance-defs *module*) '())
     (dolist (inst-decl (module-instances *module*))
       (let ((inst (instance->def inst-decl interface?)))
	 (when (not (eq? inst '#f))
            (push inst (module-instance-defs *module*)))))
     ;; Deriving decls live in the symbol table
     (dolist (deriving-decl (module-derivings *module*))
       (deriving-decl->deriving deriving-decl))))
  (unless interface? (add-derived-instances modules))
  (walk-modules modules
   (lambda ()
     (dolist (inst (module-instance-defs *module*))
       (expand-instance-decls inst interface?))
       (unless interface? (add-runtime-type-definitions))))
  (unless interface?
   (walk-modules modules
    (lambda ()
     (dolist (inst (module-instance-defs *module*))
	 (check-inst-type inst))
     (dolist (ty (default-decl-types (module-default *module*)))
	(resolve-type ty))))
   ;;; Here we clean up class definitions coming in from interfaces.  These
   ;;; don't have super* set up.  This really doesn't need to be done
   ;;; every time we see the class but it's not worth caching.
   (walk-modules (get-all-interfaces)
	(lambda ()
	  (dolist (class (module-class-defs *module*))
	    (setup-class-slots class)
	    (create-selector-functions class '#t))))
   (show-undefined-symbols)
   )))

(define (signal-recursive-synonyms vals)
  (fatal-error 'recursive-synonyms
    "There is a cycle in type synonym definitions involving these types:~%~a"
    vals))

(define (add-new-module-decl decl)
  (setf (module-decls *module*) (cons decl (module-decls *module*))))

(define (add-new-module-def var value)
  (add-new-module-decl
   (**define var '() value)))

(define (add-new-module-signature var signature)
  (add-new-module-decl
   (**signdecl/def (list var) signature)))
