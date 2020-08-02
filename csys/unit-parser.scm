;;;=====================================================================
;;; Compilation unit file parsing
;;;=====================================================================

;;; This parses a unit file.  The file simply contains a list of file names.
;;; The files are sorted into two catagories: other compilation units and
;;; source files in the current unit.

;;; The filename will always have a .hu extension at this point.  If an
;;; implicit unit is being used, this will create one if the .hu file does not
;;; exist but a source one does.

(define (parse-compilation-unit filename)
  (if (file-exists? filename)
      (if (interface-extension? (filename-type filename))
	  (create-implicit-interface-unit filename)
	  (parse-compilation-unit-aux
	   filename
	   (call-with-input-file filename (function gather-file-names))))
      (create-implicit-unit filename)))

(define (create-implicit-unit filename)
  (let ((source-file (locate-existing-source-file filename)))
    (if source-file
	(create-ucache filename source-file (list source-file)
		       '() '() '() '#f '#t
		       '#f '() '#f '() '#f
		       (file-write-date source-file))
	(signal-files-not-found filename source-file))))

(define (signal-files-not-found unit-file source-file)
  (fatal-error 'files-not-found
    "Neither the unit file ~a nor source file ~a were found."
    unit-file source-file))
   

;;; Actually parse contents of the unit file.

;;; These are in the command-interface stuff.
(predefine (set-printers args mode))
(predefine (set-optimizers args mode))
(predefine (parse-command-args string start next end))

(define (parse-compilation-unit-aux filename strings)
  (let ((input-defaults   filename)
	(output-defaults  filename)
	(import-defaults  filename)
	(stable?          '#f)
	(load-prelude?    '#t)
	(filenames        '())
	(imports          '())
	(sources          '())
	(lisp-files       '())
	(foreign-files    '())
	(printers         '())
	(printers-set?    '#f)
	(optimizers       '())
	(optimizers-set?  '#f)
	(chunk-size       '#f)
	(temp             '#f)
	(interfaces       '()))
    ;;; First look for magic flags.
    (dolist (s strings)
      (cond ((setf temp (string-match-prefix ":input" s))
	     (setf input-defaults (merge-file-defaults temp filename)))
	    ((setf temp (string-match-prefix ":output" s))
	     (setf output-defaults (merge-file-defaults temp filename)))
	    ((setf temp (string-match-prefix ":import" s))
	     (setf import-defaults (merge-file-defaults temp filename)))
	    ((string=? ":stable" s)
	     (setf stable? '#t))
	    ((string=? ":prelude" s)
	     (setf load-prelude? '#f))
	    ((setf temp (string-match-prefix ":p=" s))
	     (setf printers-set? '#t)
	     (setf printers
		   (set-printers
		      (parse-command-args temp 0 0 (string-length temp))
		      '=)))
	    ((setf temp (string-match-prefix ":o=" s))
	     (setf optimizers-set? '#t)
	     (setf optimizers
		   (set-optimizers
                      (parse-command-args temp 0 0 (string-length temp))
		      '=)))
	    ((setf temp (string-match-prefix ":chunk-size" s))
	     (setf chunk-size (string->number temp)))
	    (else
	     (push s filenames))))
    ;;; Next sort filenames into imports and source files.
    (dolist (s filenames)
      (let ((type    (filename-type s))
	    (fname   '#f))
	(cond ((string=? type "")  ; punt for now on this issue
	       (signal-extension-needed s))
	      ((unit-extension? type)
	       (setf fname  (merge-file-defaults s import-defaults))
	       (if (file-exists? fname)
		   (push fname imports)
		   (signal-unit-not-found fname)))
	      ((source-extension? type)
	       (setf fname  (merge-file-defaults s input-defaults))
	       (if (file-exists? fname)
		   (push fname sources)
		   (signal-unit-not-found fname)))
	      ((interface-extension? type)
	       (setf fname  (merge-file-defaults s input-defaults))
	       (if (file-exists? fname)
		   (push fname interfaces)
		   (signal-unit-not-found fname)))
	      ((lisp-extension? type)
	       (when (string=? (filename-name s) (filename-name filename))
		  (fatal-error 'bad-lisp-file-name
    "Lisp file name ~A in unit ~A can not be the same as the unit name"
                          s filename))
	       (setf fname (merge-file-defaults s input-defaults))
	       (if (file-exists? fname)
		   (push (cons fname
			       (add-extension
			         (merge-file-defaults s output-defaults)
				 binary-file-type))
			 lisp-files)
		   (signal-unit-not-found fname)))
	      ((foreign-extension? type)
	       (setf fname (merge-file-defaults s input-defaults))
	       (if (file-exists? fname)
		   (push fname foreign-files)
		   (signal-unit-not-found fname)))
	      (else
	       (signal-unknown-file-type s)))))
    ;; Add implicit units for .hi files
    (if (and interfaces (null? (cdr interfaces)) (null? sources))
	(setf sources interfaces) ;; Special case
	(dolist (i interfaces)
          (let ((u (create-implicit-interface-unit i)))
	    (push (ucache-ufile u) imports))))
    ;; Finally create the unit object.
    (create-ucache filename output-defaults
		   sources imports lisp-files foreign-files
		   stable? load-prelude?
		   printers-set? printers optimizers-set? optimizers
		   chunk-size (file-write-date filename))))

;;; Create interface-units for implicit interfaces

(define (create-implicit-interface-unit name)
  (let ((u (lookup-compiled-unit name)))
    (or u
	(let ((res (create-ucache name name  ; Should add suffix
				  (list name) '() '() '()
				  '#f '#f
				  '#f '() '#f '()
				  '#f (file-write-date name))))
	  (install-compilation-unit name res)
	  res))))

;;; Helper functions for the above.

;;; This returns a list of strings.  Blank lines and lines starting in -
;;; are ignored.

(define (gather-file-names port)
  (let ((char (peek-char port)))
    (cond ((eof-object? char)
	   '())
	  ((or (char=? char '#\newline) (char=? char '#\-))
	   (read-line port)
	   (gather-file-names port))
	  (else
	   (let ((line (read-line port)))
	     (cons line (gather-file-names port)))))))


;;; This has too many arguments!!

(define (create-ucache filename output-filename
		       source-files imports lisp-files foreign-files
		       stable? load-prelude?
		       printers-set? printers optimizers-set? optimizers
		       chunk-size udate)
  (let* ((cifilename
	  (make-cifilename output-filename))
	 (sifilename
	  (make-sifilename output-filename))
	 (all-imports
	  (if load-prelude?
	      (cons *prelude-unit-filename* imports)
	      imports))
	 (cache-entry
	  (make ucache
		(ufile filename)
		(sifile sifilename)
		(cifile cifilename)
		(sfile (make-sfilename output-filename))
		(cfile (make-cfilename output-filename))
		(udate udate)
		(stable? stable?)
		(load-prelude? load-prelude?)
		(ifile-loaded '#f)
		(code-loaded '#f)
		(code-compiled '#f)
		(source-files source-files)
		(imported-units all-imports)
		(lisp-files lisp-files)
		(foreign-files foreign-files)
		(modules '())
		(printers-set? printers-set?)
		(printers printers)
		(optimizers-set? optimizers-set?)
		(optimizers optimizers)
		(chunk-size chunk-size))))
    (install-compilation-unit filename cache-entry)
    cache-entry))

(define (string-match-prefix prefix s)
  (let ((prefix-length  (string-length prefix))
	(s-length       (string-length s)))
    (if (>= s-length prefix-length)
	(string-match-prefix-aux prefix s prefix-length s-length 0)
	'#f)))

(define (string-match-prefix-aux prefix s prefix-length s-length i)
  (cond ((eqv? i prefix-length)
	 (string-match-prefix-aux-aux s s-length i))
	((not (char=? (string-ref s i) (string-ref prefix i)))
	 '#f)
	(else
	 (string-match-prefix-aux prefix s prefix-length s-length (1+ i)))))

(define (string-match-prefix-aux-aux s s-length i)
  (cond ((eqv? i s-length)
	 "")
	((let ((ch  (string-ref s i)))
	   (or (char=? ch '#\space) (char=? ch #\tab)))
	 (string-match-prefix-aux-aux s s-length (1+ i)))
	(else
	 (substring s i s-length))))

(define (merge-file-defaults filename defaults)
  (let ((place  (filename-place filename))
	(name   (filename-name filename))
	(type   (filename-type filename)))
    (assemble-filename
      (if (string=? place "") defaults place)
      (if (string=? name "") defaults name)
      (if (string=? type "") defaults type))))
    
    
