;;; compiler-driver.scm -- compilation unit management
;;;
;;; author :  John & Sandra
;;;
;;;


;;; Flags for controlling various low-level behaviors of the compiler.
;;; You might want to tweak these in the system-building scripts for
;;; different Lisps, but users don't normally need to mess with them.

(define *compile-interface* '#f)
(define *interface-code-quality* 2)
(define *interface-chunk-size* '#f)
(define *default-code-quality* 2)
(define *optimized-code-quality* 3)
(define *code-chunk-size* 300)

;;; File extension definitions

(define *source-file-extensions* '(".hs" ".lhs"))
(define *unit-file-extension* ".hu")
(define *interface-file-extensions* '(".hi" ".lhi"))
(define *lisp-file-extensions* '(".lisp" ".scm"))
(define *foreign-object-file-extensions* '(".o"))

;;; Other stuff

(define *unit-stack* '())    ; used to detect circular dependencies
(define *units-loaded* '())  ; used to detect units already loaded



;;;=====================================================================
;;; Main entry point
;;;=====================================================================

;;; This is the top level driver for the compiler.  It takes a file name
;;; and output controls.

(define (haskell-compile filename cflags)
  ;; used to detect circular loads
  (setf (dynamic *unit-stack*) '())
  ;; used to avoid reloading previously loaded units
  (setf (dynamic *units-loaded*) '())
  (initialize-haskell-system)
  (let ((unit (load-compilation-unit
	       (add-extension filename *unit-file-extension*)
	       cflags)))
    ;; This checks for interfaces which have no implementations
    (setf *implementations-needed* '())
    (table-for-each (lambda (mod-name mod)
		      (when (and (memq (module-type mod)
				       '(interface psuedo-interface))
				 (not (module-stand-alone? mod)))
			 (push mod-name *implementations-needed*)))
		      *modules*)
    (setf *current-initcode* (ucache-initcode unit))
    (unless (null? *implementations-needed*)
      (format '#t
        "Evaluation disabled in this unit.~%~
         Implementations needed for module(s) ~A~%"
	(show-symbol-list/no-downcase *implementations-needed*)))
    ;; Do NOT mess with the return value from this function.
    ;; The command interface relies on this return value.
    unit))


;;;=====================================================================
;;; Guts
;;;=====================================================================


;;; This is the main entry to the compilation system.  This causes a
;;; unit to be compiled and/or loaded.  All units are initially marked as
;;; available.

;;; When a unit is not in the cache, it is created by the parser.

(define (load-compilation-unit filename cflags)
  (let ((cunit (lookup-compilation-unit filename)))
    (cond ((eq? cunit '#f)
	   ;; Unit not found in cache.
	   (load-compilation-unit-aux
	     (parse-compilation-unit filename) cflags))
	  ;; Is unit already loaded?
	  ((memq cunit (dynamic *units-loaded*))
	   cunit)
	  ;; Is the unit in the process of loading??
	  ((memq cunit (dynamic *unit-stack*))
	   (signal-circular-unit filename))
	  (else
	   ;; Reload unit from cache.
	   (load-compilation-unit-aux cunit cflags)))))


;;; This is the basic treatment of compilation units.  Push onto stack
;;; to detect circularities.  Load all imported units and then
;;; decide whether to
;;;   a) pull the unit out of the unit cache
;;;   b) load previously compiled code
;;;   c) recompile from scratch
;;; Full recompilation is required when either a source file is out of date
;;; or the compiled files are out of date or an new output file needs to be
;;; written.
;;;

(define (load-compilation-unit-aux c cflags)
  (dynamic-let ((*unit-stack*  (cons c (dynamic *unit-stack*))))
    ;; Load all the imported units first.
   (let ((imported-units
	   (map (lambda (filename) (load-compilation-unit filename cflags))
		(ucache-imported-units c))))
     (cond ((unit-already-loaded? c cflags)
	   ;; An up-to-date version of this unit has already been loaded.
	   'no-action)
	  ((unit-already-compiled? c cflags)
	   ;; Load the previously compiled unit.
	   (load-compiled-unit c (cflags-load-code? cflags) imported-units))
	  (else
	   ;; Recompile the unit.
	   (locally-compile c cflags imported-units)))
    (push c (dynamic *units-loaded*))
    ;; Hack, hack.  When loading the prelude, make sure magic symbol
    ;; table stuff is initialized.
    (when (string=? (ucache-ufile c) *prelude-unit-filename*)
      (init-prelude-globals))
    c)))


;;; This brings in a previously compiled unit.  Everything is up to date at
;;; this point.

(define (load-compiled-unit c load-code? imported-units)
  (setf (dynamic *init-complete*) '#f)
  (initialize-module-table)
  (dolist (u imported-units)
     (add-modules-to-environment (ucache-modules u)))
  (when (and load-code? (not (ucache-code-loaded c)))
    (when (memq 'loading *printers*)
      (format '#t "~&Loading ~s.~%" (ucache-ufile c))
      (force-output))
    (load-lisp-files c)
    (load-foreign-files c)
    (dynamic-let ((*initcode-function*  '#f))
      ;; choose the compiled code if available
      (cond ((valid-compiled-codefiles-written? c)
  	     (load (ucache-cfile c))
 	     (setf (ucache-code-compiled c) '#t))
	    (else
	     (load (ucache-sfile c))
	     (setf (ucache-code-compiled c) '#f)))
      (if (dynamic *initcode-function*)
	  (setf (ucache-initcode c) 
		(combine-initcode imported-units (dynamic *initcode-function*)))
	  (error "No initcode defined in this file!"))
      (setf (ucache-code-loaded c) '#t)))
  (when (not (ucache-ifile-loaded c))
     (read-binary-interface c)
     (setf (ucache-ifile-loaded c) '#t))
  (mark-conflicting-code c)
  (setf (ucache-modules c) (get-all-modules)))

;;; Load or compile lisp files.

(define (load-lisp-files u)
  (dolist (f (ucache-lisp-files u))
     (load-more-recent-file (cdr f) (car f))))

(define (compile-lisp-files u)
  (dolist (f (ucache-lisp-files u))
    (let ((source  (car f))
	  (binary  (cdr f)))
      (when (not (lisp-binary-current source binary))
	(compile-file source binary))
      (load binary))))

(define (load-foreign-files u)
  (dolist (f (ucache-foreign-files u))
    (if (file-exists? f)
	(load-foreign-file/cached f)
	(fatal-error 'missing-object-file "File ~A not found." f))))

;;; This determines whether the unit is already loaded and ready to go.
;;; The interface and code (if desired) must be present and the unit
;;; must either be stable or completely up to date with sources and
;;; imports.

(define (unit-already-loaded? c cflags)
  (or (and (ucache-stable? c)
	   (ucache-code-loaded c)
	   (ucache-ifile-loaded c))
      (and (ucache-ifile-loaded c)
	   (or (not (cflags-load-code? cflags))
	       (and (ucache-code-loaded c)
		    (or (ucache-code-compiled c)
			(not (cflags-compile-code? cflags)))))
	   (everything-current? c)
	   (output-files-ok c cflags))))

(define (everything-current? c)
  (let ((udate  (ucache-udate c)))
    (and (all-imports-current (ucache-imported-units c) udate)
	 (all-sources-current (ucache-source-files c) udate)
	 (all-lisp-sources-current (ucache-lisp-files c) udate))))

(define (all-sources-current sources unit-write-date)
  (every (lambda (s)
	   (let ((d  (file-write-date s)))
	     (and d (>= unit-write-date d))))
	 sources))

(define (all-imports-current imports unit-write-date)
  (every (lambda (s)
	     (let ((u (lookup-compilation-unit s)))
	       (or (ucache-stable? u)
		   (>= unit-write-date (ucache-udate u)))))
	 imports))

(define (all-lisp-sources-current lisp-files unit-write-date)
  (every (lambda (s)
	   (let ((d  (file-write-date (car s))))
	     (and d (>= unit-write-date d))))
	 lisp-files))

;;; This determines whether all required output files are already present and
;;; up to date.

(define (output-files-ok u cflags)
 (or
  (not (cflags-write-code? cflags))   ; must want to write an output file
  (and (valid-interface-written? u)
       (if (cflags-compile-code? cflags)
	   (valid-compiled-codefiles-written? u)
	   (valid-codefiles-written? u)))))

(define (valid-interface-written? u)
  (or 
      (and (file-exists? (ucache-cifile u))
	   (>= (file-write-date (ucache-cifile u)) (ucache-udate u)))
      (and (file-exists? (ucache-sifile u))
	   (>= (file-write-date (ucache-sifile u)) (ucache-udate u)))))

;;; Fudge a little here and assume that lisp files are also compiled
;;; and present of compiled codefile is present.

(define (valid-compiled-codefiles-written? u)
  (and (file-exists? (ucache-cfile u))
       (>= (file-write-date (ucache-cfile u)) (ucache-udate u))))

(define (valid-uncompiled-codefiles-written? u)
  (and (file-exists? (ucache-sfile u))
       (>= (file-write-date (ucache-sfile u)) (ucache-udate u))))

(define (valid-codefiles-written? u)
  (or (valid-compiled-codefiles-written? u)
      (valid-uncompiled-codefiles-written? u)))


;;; To make use of a previously compiled unit, we need the interface
;;; (either in memory or in a file), all desired output files, and possibly
;;; a code file.

(define (interface-unit? c)
 (let ((sources (ucache-source-files c)))
   (and (pair? sources) (null? (cdr sources))
	(interface-extension? (filename-type (car sources))))))

(define (unit-already-compiled? c cflags)
  (setup-unit-date c)
  (setf (ucache-ifile-loaded c) '#f)
  (setf (ucache-code-loaded c) '#f)
  (and (valid-interface-written? c)
       (output-files-ok c cflags)
       (or (not (cflags-load-code? cflags))
	   (if (cflags-compile-code? cflags)
	       (valid-compiled-codefiles-written? c)
	       (valid-codefiles-written? c)))))

(define (all-lisp-binaries-current lisp-files)
  (every (lambda (s)
	   (lisp-binary-current (car s) (cdr s)))
	 lisp-files))

(define (lisp-binary-current source binary)
  (and (file-exists? binary)
       (let ((sd  (file-write-date source))
	     (bd  (file-write-date binary)))
	 (and sd bd (> bd sd)))))


;;; This does the actual job of compilation.

(define (locally-compile c cflags imported-units)
  (setf (dynamic *init-complete*) '#f)
  (dynamic-let ((*printers*
		  (if (ucache-printers-set? c)
		      (ucache-printers c)
		      (dynamic *printers*)))
		(*optimizers*
		  (if (ucache-optimizers-set? c)
		      (ucache-optimizers c)
		      (if (cflags-compile-code? cflags)
			  (dynamic *compiled-code-optimizers*)
			  (dynamic *interpreted-code-optimizers*))))
		(*initcode-function-name*
		  (string->symbol (format '#f "Initcode for unit ~s"
					  (ucache-ufile c))))
		(*initcode-function*
		  '#f))
    (when (memq 'compiling *printers*)
       (format '#t "~&Compiling  ~s [~A]~%"
	       (ucache-ufile c)
	       (show-symbol-list *optimizers*))
       (force-output))
    (initialize-module-table)
    (dolist (u imported-units)
      (add-modules-to-environment (ucache-modules u)))
    (if (cflags-compile-code? cflags)
	(compile-lisp-files c)
	(load-lisp-files c))
    (load-foreign-files c)
    (multiple-value-bind (mods code)
	(compile-haskell-files (ucache-source-files c))
      ;; General bookkeeping to update module interface in cache.
      (setup-ucache-modules c mods)
      (setf (ucache-code-compiled c) (cflags-compile-code? cflags))
      (setf (ucache-modules c) (get-all-modules))
      (setf (ucache-ifile-loaded c) '#t)
      (setup-unit-date c)
      ;; Write interface file if necessary.
      (let ((interface? (interface-module? (car (ucache-modules c)))))
       (when (cflags-write-interface? cflags)
	(let ((phase-start-time (get-run-time))
	      (icode  (create-dump-code c mods (ucache-load-prelude? c))))
	  (if (dynamic *compile-interface*)
	      (write-compiled-code-file
	        (ucache-cifile c)
		icode
		(dynamic *interface-code-quality*)
		(dynamic *interface-chunk-size*))
	      (write-interpreted-code-file (ucache-sifile c) icode '#f))
	  (when (memq 'phase-time *printers*)
	    (let* ((current-time (get-run-time))
		   (elapsed-time (- current-time phase-start-time)))
	      (format '#t "Interface complete: ~A seconds~%" elapsed-time)
	      (force-output)))))
      ;; Write code file if necessary.
       (when (and (cflags-write-code? cflags)
		  (or (not interface?)
		      (some (function module-stand-alone?) mods)))
	(if (cflags-compile-code? cflags)
	    (write-compiled-code-file
	      (ucache-cfile c)
	      code
	      (if (memq 'lisp (dynamic *optimizers*))
		  (dynamic *optimized-code-quality*)
		  (dynamic *default-code-quality*))
	      (or (ucache-chunk-size c) (dynamic *code-chunk-size*)))
	    (write-interpreted-code-file (ucache-sfile c) code '#t)))
      ;; Load or evaluate code if necessary.
      ;; If we just wrote a compiled code file, load that; otherwise
      ;; do eval or in-core compilation.
      (when (cflags-load-code? cflags)
	(if (and (cflags-write-code? cflags)
		 (cflags-compile-code? cflags)
		 (not interface?))
	    (load (ucache-cfile c))
	    (eval code (cflags-compile-code? cflags)))
	(if (dynamic *initcode-function*)
	    (setf (ucache-initcode c)
		  (combine-initcode imported-units
				    (dynamic *initcode-function*)))
	    (error "No initcode defined in this file!"))
	(mark-conflicting-code c)
	(setf (ucache-code-loaded c) '#t))
      ))))

;;; This marks all code associated with a module that shares a name with a
;;; just loaded module as 'non-loaded'.

(define (mark-conflicting-code c)
  (let ((mods (ucache-modules-defined c)))
    (for-all-cached-units
     (lambda (m1)
       (when (not (eq? m1 c))
         (when (some (lambda (mod1)
		       (memq mod1 mods))
		     (ucache-modules-defined m1))
	    (when (ucache-stable? m1)
              (haskell-warning 'redefinition-of-stable
                "Modules in stable unit ~A may be corrupted"
		(ucache-ufile m1)))
	    (setf (ucache-code-loaded m1) '#f)))))))

;;; This places a timestamp on the unit as determined by the timestamp of
;;; all imported units (which must be loaded & valid at this point)
;;; and all source files.

(define (setup-unit-date u)
  (dolist (file (ucache-lisp-files u))
    (freshen-unit-date u (file-write-date (tuple-2-1 file)))
    (when (file-exists? (tuple-2-2 file))
       (freshen-unit-date u (file-write-date (tuple-2-2 file)))))
  (dolist (file (ucache-source-files u))
    (freshen-unit-date u (file-write-date file)))
  (dolist (i (ucache-imported-units u))
    (let ((iu (lookup-compiled-unit i)))  ; must be in cache by now
      (when (not (ucache-stable? iu))
	 (freshen-unit-date u (ucache-udate iu))))))

;;; The date of a unit is the max of all constituant write dates.
;;; This freshens the date in a unit.

(define (freshen-unit-date u date)
 (when date
  (setf (ucache-udate u) (max (ucache-udate u) date))))



;;;=====================================================================
;;; Filename utilities
;;;=====================================================================

;;; File extensions

(define (source-extension? x)
  (mem-string x *source-file-extensions*))

(define (unit-extension? x)
  (string=? x *unit-file-extension*))

(define (interface-extension? x)
  (mem-string x *interface-file-extensions*))

(define (lisp-extension? x)
  (mem-string x *lisp-file-extensions*))

(define (foreign-extension? x)
  (mem-string x *foreign-object-file-extensions*))


;;; Build file names.

(define (make-cifilename filename)
  (let ((place  (filename-place filename))
	(name   (string-append (filename-name filename) "-hci")))
    (assemble-filename place name binary-file-type)))

(define (make-sifilename filename)
  (let ((place  (filename-place filename))
	(name   (string-append (filename-name filename) "-hci")))
    (assemble-filename place name source-file-type)))

(define (make-cfilename filename)
  (add-extension filename binary-file-type))

(define (make-sfilename filename)
  (add-extension filename source-file-type))


;;; This take a file name (extension ignored) & searches for a source file.

(define (locate-existing-source-file name)
  (locate-extension name *source-file-extensions*))

(define (locate-extension name extensions)
  (if (null? extensions)
      '#f
      (let ((name-1 (add-extension name (car extensions))))
	(if (file-exists? name-1)
	    name-1
	    (locate-extension name (cdr extensions))))))


;;; These globals save the Prelude symbol table to avoid copying it
;;; into all modules which use the Prelude.

;;; Danger!  This assumes that every local symbol in the Prelude is
;;; exported.

(define *prelude-initialized* '#f)

(define (init-prelude-globals)
  (when (not *prelude-initialized*)
    (let ((pmod (locate-module '|Prelude|)))
      (setf *prelude-symbol-table* (module-symbol-table pmod))
      (setf *prelude-fixity-table* (module-fixity-table pmod))
      (when (eq? (module-inverted-symbol-table pmod) '#f)
	(let ((tbl (make-table)))
	  (table-for-each (lambda (name def)
			    (setf (table-entry tbl def) name))
			  *prelude-symbol-table*)
	  (setf (module-inverted-symbol-table pmod) tbl)))
      (setf *prelude-inverted-symbol-table*
	    (module-inverted-symbol-table pmod)))
    (setf *prelude-initialized* '#t)))

;;;=====================================================================
;;; Error utilities
;;;=====================================================================

(define (signal-circular-unit filename)
  (fatal-error 'circular-unit
    "The compilation unit ~a has a circular dependency."
    filename))

(define (signal-unit-not-found filename)
  (fatal-error 'unit-not-found
    "The compilation unit file ~a was not found."
    filename))

(define (signal-extension-needed filename)
  (fatal-error 'extension-needed
    "You must provide an extension on the filename ~a in the .hu file."
     filename))

(define (combine-initcode units code)
  (if (null? units)
      (list code)
      (set-union (ucache-initcode (car units))
		 (combine-initcode (cdr units) code))))
