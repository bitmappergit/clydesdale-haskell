;;; vanilla.scm -- plain vanilla command-line interface
;;;
;;; author :  Sandra Loosemore
;;; date   :  27 Jul 1993
;;;



;;;=========================================================================
;;; Setup
;;;=========================================================================


;;; Input for the command interface just reads from standard input.

(define (vanilla-input-hook)
  (read-line))


;;; Initialize extension buffer, etc. here.

(define *remembered-extension* "")
(define *remembered-previous-extension* "")
(define *remembered-file* "Foo")
(define *remembered-module*  '|Main|)

(define (vanilla-initialize-hook)
  (when (not (find-executable-module/inner *remembered-module*))
    (make-empty-main)))

(define (clear-remembered-extension)
  (setf *remembered-extension* "")
  (setf *remembered-previous-extension* ""))

(define-local-syntax (with-remembered-extension . body)
  (let ((ok  (gensym)))
    `(let ((,ok  '#f))
       (unwind-protect
	   (begin ,@body (setf ,ok '#t))
	 (if ,ok
	     ;; body completed with no errors; save new extension.
	     (setf *remembered-previous-extension* *remembered-extension*)
	     ;; body aborted with errors; undo new extension.
	     (setf *remembered-extension* *remembered-previous-extension*)))
       ,ok)))

(define (make-empty-main)
  (let ((prelude  (compile/load "$PRELUDE/Prelude")))
    (initialize-module-table)
    (add-modules-to-environment (ucache-modules prelude))
    (let ((mods (list (parse-module-body-from-string
		       '|Main|
		       "import Prelude"
		       "None"
		       '#f))))
      (eval (modules->lisp-code mods))
      (setf *current-initcode* (ucache-initcode prelude))
      (setf *implementations-needed* '())
      (setf *modules-available* (append mods (ucache-modules prelude)))
      (setf *remembered-module* '|Main|)
      (clear-remembered-extension))))


(define (remember-mod unit)
  (let ((mods (ucache-modules-defined unit)))
   (when (pair? mods)
    (when (not (memq *remembered-module* mods))
      (setf *remembered-module* (car mods))
      (format '#t "~&Now in module ~A.~%" *remembered-module*)
      (clear-remembered-extension)))))

(define (remember-file file)
  (when (not (equal? file ""))
    (setf *remembered-file* file)))


;;;=========================================================================
;;; Command parsing and dispatch
;;;=========================================================================

;;; Read, parse, and execute a command.

(define (vanilla-command-hook)
  (vanilla-prompt)
  (vanilla-read-and-execute-command))

(define (vanilla-prompt)
  (when (memq 'prompt (dynamic *printers*))
    (format '#t "~%~a> " *remembered-module*))
  (force-output))


(define (vanilla-read-and-execute-command)
  (let ((ch  (peek-char)))
    (cond ((eof-object? ch)
	   ;; Exit haskell on EOF
	   (exit))
	  ((char=? ch #\:)
	   ;; Got a command
	   (read-char)
	   (vanilla-parse-command))
	  ((char=? ch #\newline)
	   ;; Ignore blank lines
	   (read-char)
	   '#f)
	  (else
	   ;; Add to remembered extension.
	   (setf *remembered-extension*
		 (string-append *remembered-extension*
				(read-line)
				(string #\newline))))
	  )))


(define *command-dispatch* '())

(define (vanilla-parse-command)
  (if (char=? (peek-char) #\()
      ;; *** There's a slight problem here.  Some Lisps seem to consume
      ;; *** whitespace after the form being read, and others leave it.
      ;; *** This can cause problems for the next call to (read-line).
      ;; *** I don't know what to do about it.
      (let ((form  (read)))
	(fresh-line)
        (eval form))
      (let* ((string (read-line))
	     (length (string-length string)))
	(multiple-value-bind (word next) (next-word string 0 0 length)
	  (if (not word)
	      (format '#t "~&Huh?~%")
	      (let ((fn (assoc/test (function string-starts?) word
				    *command-dispatch*)))
		(if (not fn)
		    (format '#t "~&~a: unknown command.  Use :? for help.~%"
			    word)
		    (begin
		      (fresh-line)
		      (funcall (cdr fn) (substring string next length))))))))))

;;; Get the next "word" from the command line.

(define (next-word string start next end)
  (declare (type fixnum start next end)
	   (type string string))
  (cond ((eqv? next end)
	 (if (eqv? start next)
	     (values '#f next)
	     (values (substring string start next) next)))
	((char=? (string-ref string next) '#\space)
	 (let ((next-next  (+ next 1)))
	   (if (eqv? start next)
	       (next-word string next-next next-next end)
	       (values (substring string start next) next-next))))
	(else
	 (next-word string start (+ next 1) end))))


;;; This parses the command into a list of substrings.  
;;; Args are separated by spaces.

(define (parse-command-args string start next end)
  (multiple-value-bind (word next-next)
      (next-word string start next end)
    (if word
	(cons word (parse-command-args string next-next next-next end))
	'())))

(define (parse-command-args-top string)
  (parse-command-args string 0 0 (string-length string)))


;;; Call this function to enable this interface.

(define (use-vanilla-interface)
  (setf *haskell-enter-debugger-hook* '#f)
  (setf *haskell-exit-debugger-hook* '#f)
  (setf *haskell-input-hook* (function vanilla-input-hook))
  (setf *haskell-command-hook* (function vanilla-command-hook))
  (setf *haskell-initialize-hook* (function vanilla-initialize-hook))
  (setf *haskell-compilation-error-hook* '#f)
  (vanilla-initialize-hook))


;;;=========================================================================
;;; Handlers for commands
;;;=========================================================================

(define-local-syntax (define-command command name&args . body)
  `(begin
     (define ,name&args ,@body)
     (setf *command-dispatch*
	   (nconc *command-dispatch*
		  (list (cons ',command (function ,(car name&args))))))
     ',(car name&args)))


(define-command "=" (vanilla-eval exp)
  (with-remembered-extension
    (haskell-eval
      exp
      "interactive"
      *remembered-extension*
      *remembered-module*
      '#f)))

(define-command "@" (vanilla-run exp)
  (with-remembered-extension
    (haskell-run
      exp
      "interactive"
      *remembered-extension*
      *remembered-module*
      '#f)))

(define-command ">" (vanilla-run-print exp)
  (with-remembered-extension
    (haskell-run-print
      exp
      "interactive"
      *remembered-extension*
      *remembered-module*
      '#f)))

(define-command ":" (vanilla-report-type exp)
  (with-remembered-extension
    (haskell-report-type
      exp
      "interactive"
      *remembered-extension*
      *remembered-module*
      '#f)))

(define-command "run" (vanilla-run-file string)
  (let* ((file+args  (parse-command-args-top string))
	 (file       (if (null? file+args) "" (car file+args)))
	 (args       (cdr file+args)))
    (remember-file file)
    (remember-mod (compile/run *remembered-file* args))))

(define-command "compile" (vanilla-compile-file file)
  (remember-file file)
  (remember-mod (compile/compile *remembered-file*)))

(define-command "load" (vanilla-load-file file)
  (remember-file file)
  (remember-mod (compile/load *remembered-file*)))

(define-command "clear" (vanilla-clear string)
  (declare (ignore string))
  (clear-remembered-extension))

(define-command "list" (vanilla-list string)
  (declare (ignore string))
  (if (string=? *remembered-extension* "")
      (format '#t "~&The scratch pad is empty.")
      (format '#t "~&Scratch pad:~%~a" *remembered-extension*)))

(define-command "module" (vanilla-module mod)
  (setf mod (string->symbol mod))
  (find-executable-module mod)   ;; checks to make sure it is available
  (clear-remembered-extension)
  (setf *remembered-module* mod))

(define-command "Main" (vanilla-main string)
  (declare (ignore string))
  (make-empty-main))

(define-command "?" (vanilla-help string)
  (declare (ignore string))
  (format '#t
"Commands used by the Y2.0 command interface:

Evaluation Commands.  These are executed in the context of the current module.
:= exp        Evaluate and print exp
:@ exp        Run exp of type IO()
:> exp        Run exp of type IO(a), print result
:: exp        Print the type of exp

Scratch pad commands.  All lines typed that do not start with : are added to
the scratch pad.  Evaluation commands may refer to definitions in the pad.
:clear        Discard the current pad
:list         Print the current pad

Commands to load & run programs.
:load file    Load a file (compilation unit) into the system
:compile file Compile a file to native code and save the binary
:run file     Load a file and run `main'

Commands to control the current module:
:module name  Set the current module
:Main         Switch to an empty module named Main

Other commands:
:cd directory Set the current directory
:?            Print this help file
:quit         Leave Haskell
:p?           Describe available printers
:p= p1 p2 ... Set the printers
:p+ p1 p2 ... Enable selected printers
:p- p1 p2 ... Disable selected printers
:o?           Describe available optimizers
:o= o1 o2 ... Set the optimizers
:o+ o1 o2 ... Enable selected optimizers
:o- o1 o2 ... Disable selected optimizers
:(fn ...)     Evaluate a Lisp expression
:debug on-off Enable/disable the Lisp debugger~%"))

(define-command "quit" (vanilla-quit string)
  (declare (ignore string))
  (exit))

(define-command "p?" (vanilla-printers-list string)
  (declare (ignore string))
  (format '#t
"General messages
      compiling       Printed when the compilation system starts a compilation
      loading         Printed when a previously compiled unit is loaded
      reading         Prints the name of the file being parsed
      pad             Enables printing within scratch pads
      interactive     Print verbose messages in command loop
      prompt          Print prompt in command loop
Timings
      time            Prints the time that it takes to execute a computation
      phase-time      Prints the time of each phase of compilation
Compiler passes
      parse           Prints the program recreated from ast
      import          Lists all symbols imported and exported for each module
      scope           Print the program after scoping and precedence parsing
      depend          Prints entire program in nested let's
      type            Prints signatures during inference
      cfn             Prints entire program after context free normalization
      depend2         Like depend
      flic            Prints entire program as flic code
      optimize        Prints entire program as optimized flic code
      optimize-extra  Prints extra verbose information during optimization
      strictness      Print strictness of all functions and variables
      codegen         Prints generated Lisp code
      codegen-flic    Prints generated Lisp code and associated flic code
      dumper          Prints the code in the interface
      dump-stat       Prints statistics for the interface file~%")
  (show-printers))

(define-command "p=" (vanilla-printers-set string)
  (setf (dynamic *printers*)
	(set-printers (parse-command-args-top string) '=))
  (show-printers))

(define-command "p+" (vanilla-printers-on string)
  (setf (dynamic *printers*)
	(set-printers (parse-command-args-top string) '+))
  (show-printers))

(define-command "p-" (vanilla-printers-off string)
  (setf (dynamic *printers*)
	(set-printers (parse-command-args-top string) '-))
  (show-printers))

(define (show-printers)
 (format '#t "~&Active printers: ~A~%" (show-symbol-list (dynamic *printers*))))

(define-command "o?" (vanilla-optimizers-list string)
  (declare (ignore string))
  (format '#t
"Optimizer switches
      inline         Aggressively inline functions
      constant       Hoist constant expressions to top-level
      foldr          Perform foldr/build deforestation
      lisp           Tell the Lisp compiler to work hard to produce best code
      delays         Try to make delays out-of-line for more compact code~%")
  (show-optimizers))

(define-command "o=" (vanilla-optimizers-set string)
  (setf (dynamic *compiled-code-optimizers*)
	(set-optimizers (parse-command-args-top string) '=))
  (show-optimizers))

(define-command "o+" (vanilla-optimizers-on string)
  (setf (dynamic *compiled-code-optimizers*)
	(set-optimizers (parse-command-args-top string) '+))
  (show-optimizers))

(define-command "o-" (vanilla-optimizers-off string)
  (setf (dynamic *compiled-code-optimizers*)
	(set-optimizers (parse-command-args-top string) '-))
  (show-optimizers))

(define (show-optimizers)
 (format '#t "~&Optimizers: ~A~%"
	 (show-symbol-list (dynamic *compiled-code-optimizers*))))

(define-command "cd" (vanilla-cd d)
  (cd d)
  (format '#t "~&Current directory: ~a~%" (pwd)))

(define-command "debug" (vanilla-debug string)
  (cond ((string-ci=? string "on")
	 (setf *haskell-debug-in-lisp* '#t)
	 (format '#t "~&Lisp debugging enabled.~%"))
	((string-ci=? string "off")
	 (setf *haskell-debug-in-lisp* '#f)
	 (format '#t "~&Lisp debugging disabled.~%"))
	(else
	 (format '#t "~&Huh?~%"))))
