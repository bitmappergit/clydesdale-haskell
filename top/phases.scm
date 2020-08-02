
;;; This is the top-level phase structure of the compiler.

;;; Compilation phase support

(define *phase* '#f)
(define *abort-phase* '#f)         ; abort when this phase completes
(define *abort-compilation*
  (lambda ()
    (error "No error continuation defined here!")))

(define *module-asts* '())   ; a global only for debugging purposes

;;; Later add the printing and timing stuff here

(define-local-syntax (phase-body phase-name body printer)
  `(dynamic-let ((*phase*       ',phase-name))
     (when (memq ',phase-name (dynamic *printers*))
       (format '#t "~%Phase ~a:~%" ',phase-name)
       (force-output))
     (let* ((phase-start-time (get-run-time))
	    (result ,body)
	    (current-time  (get-run-time)))
       (when (eq? (dynamic *abort-phase*) ',phase-name)
	 (abort-compilation))
       ,@(if (eq? printer '#f)
	     '()
	     `((when (memq ',phase-name (dynamic *printers*))
		 (funcall ,printer result)
		 (force-output))))
       (when (memq 'phase-time *printers*)
	 (let ((elapsed-time (- current-time phase-start-time)))
	   (format '#t "~&~A complete: ~A seconds~%"
		   ',phase-name elapsed-time)
	   (force-output)))
       result)))

;;; This function compiles a set of files in a compilation environment.
;;; The compilation environment consists of a set of previously compiled
;;; implementations and interfaces.  This environment is not passed explicitly
;;; as a parameter here; instead it lives in the background within some
;;; global variables.

;;; Interface files are handled specially.  Since interfaces are completely
;;; standalone, they are compiled in a null environment one source file at
;;; a time.

;;; Returns 2 values: module ast's and lisp code.

;;; %%% This is set up to allow interfaces and implementations to be
;;;     mixed in a program but this can't happen; the compilation system
;;;     separates .hi and .hs source files into different units.

(define (compile-haskell-files files)
  (dynamic-let ((*abort-phase*                '#f))
     (let ((all-mods       (haskell-parse-files files))
	   (interface-mods '())
	   (regular-mods   '()))
       (if (null? all-mods)
	   (values '() '(begin))
	   (dolist (m all-mods)
	     (if (interface-module? m)
		 (push m interface-mods)
		 (push m regular-mods))))
       (dynamic-let ((*unit*  (get-compilation-unit-name (car all-mods))))
	 (values
	   all-mods
	   `(begin
	      ,@(if interface-mods
		    (list (compile-interface-modules (nreverse interface-mods)))
		    '())
	      ,@(if regular-mods
		    (list (compile-modules (nreverse regular-mods)))
		    '()))
	   )))))

;;; Some module from the list supplies the unit name.  The name is
;;; different for interfaces so that there is no chance that an interface
;;; and implementation of a module will share the same unit name.

(define (get-compilation-unit-name mod)
  (if (interface-module? mod)
      (symbol-append (module-name mod) '|-interface|)
      (module-name mod)))

(define (compile-modules mods)
  (dynamic-let ((*context*                    '#f)
		(*recoverable-error-handler*  '#f)
		(*abort-phase*                '#f)
		(*unique-name-counter*        1))
  	  (haskell-import-export mods '#f)
	  (haskell-process-type-declarations mods)
	  (haskell-scope mods)
	  (let ((big-let (haskell-dependency-analysis mods)))
	    (haskell-type-check big-let mods)
	    (setf big-let (haskell-cfn big-let))
	    (setf big-let (haskell-dependency-reanalysis big-let))
	    (setf big-let (haskell-ast-to-flic big-let))
	    (setf big-let (haskell-optimize big-let))
	    (setf big-let (haskell-strictness big-let))
	    (let ((res (haskell-codegen big-let mods)))
	      (haskell-check-interfaces mods)
	      (dolist (m mods)
		(zap-module-slots m))
	      res))))

(define (modules->lisp-code modules)
  (dynamic-let ((*unit* (module-name (car modules))))
    (compile-modules modules)))

(define (abort-compilation)
  (format *error-output-port* "Compilation aborted.~%")
  (funcall (dynamic *abort-compilation*)))

(define (halt-compilation)
  (setf (dynamic *abort-phase*) (dynamic *phase*)))

;;; Zap slots of module data structures that contain pointers to things
;;; that are no longer needed once compilation has finished.  This
;;; frees up lots of memory.
;;; The slots that are saved are the ones that are also saved by
;;; dump-interface, namely things like the symbol table and export table,
;;; and stuff like the module name and type.

(define (zap-module-slots m)
  ;; Throw out all the AST structure built by the parser
  (setf (module-exports m) '())
  (setf (module-imports m) '())
  (setf (module-fixities m) '())
  (setf (module-synonyms m) '())
  (setf (module-algdatas m) '())
  (setf (module-classes m) '())
  (setf (module-instances m) '())
  (setf (module-derivings m) '())
  (setf (module-annotations m) '())
  (setf (module-decls m) '())
  ;; Throw out other slots that are used internally by the compiler
  (setf (module-synonym-defs m) '())
  (setf (module-alg-defs m) '())
  (setf (module-class-defs m) '())
  (setf (module-fresh-exports m) '())
  (setf (module-exported-modules m) '())
  (setf (module-interface-imports m) '())
  ;; FLIC associated with top-level variables in the symbol table that
  ;; isn't needed later for inlining is zapped elsewhere -- see
  ;; csys/structure-save.scm.
  )


;;; Here are the actual phase bodies

(predefine (parse-files files))

(define (haskell-parse-files filenames)
  (phase-body parse
    (let ((mods (parse-files filenames)))
      mods)
    #f))

(predefine (import-export modules))  ; in import-export/import-export.scm
(predefine (import-export/interface modules))

(define (haskell-import-export modules interface?)
  (phase-body import
    (if interface?
	(import-export/interface modules)
	(import-export modules))
    #f))


(predefine (process-type-declarations modules)) 
    ; in tdecl/type-declaration-analysis.scm

(define (haskell-process-type-declarations modules)
  (phase-body type-decl
    (begin
      (process-type-declarations modules))
    #f))


(predefine (scope-modules x))  ; in prec/scope.scm
(predefine (print-full-module x . maybe-stream)) ; in the printers

(define (haskell-scope modules)
  (phase-body scope
    (scope-modules modules)
    (lambda (result)
      (declare (ignore result))
      (dolist (m modules) (print-full-module m)))
    ))


(predefine (do-dependency-analysis x))  ; in depend/dependency-analysis.scm

(define (haskell-dependency-analysis modules)
  (phase-body depend
    (do-dependency-analysis modules)
    (function pprint*)))


(predefine (do-haskell-type-check big-let mods))

(define (haskell-type-check big-let modules)
  (phase-body type
    (do-haskell-type-check big-let modules)
    #f))

(predefine (cfn-ast x))  ; in cfn/main.scm

(define (haskell-cfn big-let)
  (phase-body cfn
    (cfn-ast big-let)
    (function pprint*)))


(predefine (analyze-dependency-top x))  ; in depend/dependency-analysis.scm

(define (haskell-dependency-reanalysis big-let)
  (phase-body depend2
    (begin
      (analyze-dependency-top big-let)
      big-let)
    (function pprint*)))


(predefine (ast-to-flic x))		; in flic/ast-to-flic.scm

(define (haskell-ast-to-flic big-let)
  (phase-body flic
    (ast-to-flic big-let)
    (function pprint*)))


(predefine (optimize-top x))  ; in backend/optimize.scm

(define (haskell-optimize big-let)
  (phase-body optimize
    (optimize-top big-let)
    (function pprint*)))

(predefine (strictness-analysis-top x)) ; in backend/strictness.scm
(predefine (strictness-analysis-printer x))

(define (haskell-strictness big-let)
  (phase-body strictness
    (strictness-analysis-top big-let)
    (function strictness-analysis-printer)))


(predefine (codegen-top x))  ; in backend/codegen.scm
(predefine (codegen-exported-types x)) ; "
(predefine (codegen-prim-entries x))  ; ditto

(define (haskell-codegen big-let mods)
  (phase-body codegen
    `(begin
       ,(codegen-exported-types mods)
       ,(codegen-top big-let))
    #f))

(predefine (check-interfaces mods))

(define (haskell-check-interfaces mods)
  (phase-body interface-check
      (check-interfaces mods)
  #f))
	       
;;; This is for interface modules.

(predefine (haskell-codegen/interface mods))

(define (compile-interface-modules mods)
 (dynamic-let ((*context*                    '#f)
	       (*recoverable-error-handler*  '#f)
	       (*abort-phase*                '#f))
     (haskell-import-export mods '#t)
     (haskell-process-type-declarations mods)
     (haskell-scope mods)
     (let ((res (haskell-codegen/interface mods)))
       res)))

