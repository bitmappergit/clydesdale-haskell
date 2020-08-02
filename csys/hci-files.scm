;;; hci-files.scm -- interface file writer/loader
;;;
;;; author :  John & Sandra
;;; date   :  8 Jul 1992
;;;
;;; This writes binary interface files.  A binary interface file is just
;;; a lisp (mumble) source file which directly builds the ast structure
;;; created by a compilation.  These files could be stored in either
;;; source or binary (compiled lisp) form.

;;; An interface may reference entities defined in other interfaces.
;;; To ensure consistancy between when an interface is written and
;;; when it is read back in, a stamp is assigned to all interface files
;;; which serves as a unique id.  The stamps of all imported units are
;;; saved and examined at load time.



;;;==================================================================
;;; Interface to compilation system
;;;==================================================================


;;; For compiled code, don't actually write out all the source code.
;;; Use a magic macro to memoize the form to be compiled.

(define *form-to-compile* '#f)
(define *magic-file-to-compile* "~/.yale/magic.scm")


;;; The output from compiling the prelude can completely overwhelm
;;; the Lisp compiler.  If this variable is a number, it specifies
;;; a "reasonable" number of top-level forms which can be compiled
;;; and write-compiled-code-file will try to break up the input
;;; code automagically.

(define *magic-chunk-size* '#f)


;;; This is called to write both the code file and the interface file.

(define (write-compiled-code-file filename code code-quality chunk-size)
  (let ((phase-start-time (get-run-time))
        (forms            (flatten-forms code)))
    (dynamic-let ((*magic-chunk-size*
		   (or chunk-size (dynamic *magic-chunk-size*)))
		  (*code-quality*
		   (or code-quality (dynamic *code-quality*))))
      (if (or (not (dynamic *magic-chunk-size*))
	      (<= (the fixnum (length forms))
		  (the fixnum (dynamic *magic-chunk-size*))))
	  (write-compiled-code-file-aux filename `(begin ,@forms))
	  (with-compilation-unit ()
	    (write-compiled-code-file-aux
	      filename
	      `(begin
		 ,@(map (lambda (f) `(load ,f))
			(write-compiled-code-file-split filename forms)))
	      ))))
    (when (memq 'phase-time *printers*)
      (let* ((current-time (get-run-time))
	     (elapsed-time (- current-time phase-start-time)))
	(format '#t "Lisp compilation complete: ~A seconds~%" elapsed-time)))
    ))

(define (write-compiled-code-file-split filename forms)
  (let ((place     (filename-place filename))
	(name      (filename-name filename))
	(type      (filename-type filename))
	(result    '()))
    (do ((i 0 (1+ i)))
	((null? forms))
	(multiple-value-bind (head tail)
	    (split-list forms (dynamic *magic-chunk-size*))
	  (let ((fname
		  (assemble-filename
		    place (format '#f "~a-part~a" name i) type)))
	    (push fname result)
	    (write-compiled-code-file-aux fname `(begin ,@head))
	    (setf forms tail))))
    (nreverse result)))

(define (flatten-forms code)
  (if (and (pair? code) (eq? (car code) 'begin))
      (nreverse (flatten-forms-aux (cdr code) '()))
      (list code)))

(define (flatten-forms-aux forms result)
  (dolist (f forms)
    (if (and (pair? f) (eq? (car f) 'begin))
	(setf result (flatten-forms-aux (cdr f) result))
	(push f result)))
  result)
	

(define (write-compiled-code-file-aux filename code)
  (dynamic-let ((*form-to-compile*  code))
    (compile-file (dynamic *magic-file-to-compile*) filename)))

(define-syntax (magic-form-to-compile)
  (dynamic *form-to-compile*))


;;; Writing source code is good for debugging purposes, but slow.
;;; The *print-circle* and *print-shared* flags have to be set because
;;; the code printed out may contain gensyms, and this will ensure
;;; that the code can be read in again.

(define (write-interpreted-code-file filename code hairy?)
  (dynamic-let ((*print-circle*   '#t)
		(*print-shared*   '#t))
    (call-with-output-file
      filename
      (lambda (port)
	(if hairy?
	    (pprint-flatten code port)
	    (print-flatten code port))))))


;;; This attempts to read a compiled interface for a unit.  This is
;;; done whenever the unit file is newer than the source file.  If
;;; imported units have changed, the load will fail and recompilation
;;; will be attempted.  
;;; The caller is responsible for making sure that the interface file exists
;;; and for making sure that the interface file is up-to-date with
;;; respect to imported modules and that all the imported modules are
;;; known.

;;; These variables are assigned by the code in the dump file.

(define *modules-imported* '())
(define *defs-referenced* '())
(define *types-referenced* '())
(define *writer-version* '())

(define (read-binary-interface unit)
  (dynamic-let ((*modules-loaded*     '())
		(*modules-imported*   '())
		(*defs-referenced*    '())
		(*types-referenced*   '())
		(*writer-version*     '()))
      (load-more-recent-file (ucache-cifile unit) (ucache-sifile unit))
      (setup-ucache-modules unit (vector->list *modules-loaded*))
      (cond ((string=? *writer-version* *haskell-compiler-version*)
	     (add-modules-to-environment (vector->list *modules-loaded*))
	     '#t)
	    (else
	     (signal-incompatible-interface-file (ucache-cifile unit))
	     '#f))))

(define (setup-ucache-modules unit mods)
  (setf (ucache-modules-defined unit) '())
  (setf (ucache-interfaces-defined unit) '())
  (dolist (m mods)
    (if (and (interface-module? m)
	     (not (module-stand-alone? m)))
	(push (module-name m)
	      (ucache-interfaces-defined unit))
	(push (module-name m)
	      (ucache-modules-defined unit)))))

(define (signal-incompatible-interface-file filename)
  (fatal-error 'incompatible-interface-file
    "File ~A~%~
     was written by a different version of the Haskell system.~%~
     You must remove it and recompile."
    filename))


(define (load-more-recent-file cfile sfile)
  (cond ((file-exists? cfile)
	 (if (or (not (file-exists? sfile))
		 (> (file-write-date cfile)
		    (file-write-date sfile)))
	     (load-compiled-interface-file cfile)
	     (load-interpreted-interface-file sfile)))
	((file-exists? sfile)
	 (load-interpreted-interface-file sfile))
	(else
	 (signal-file-not-found cfile))))

(define (load-interpreted-interface-file file)
  (load file)
  (file-write-date file))

(define (load-compiled-interface-file file)
  (load file)
  (file-write-date file))


