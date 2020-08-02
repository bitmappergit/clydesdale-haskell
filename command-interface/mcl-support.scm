;;; mcl-support.scm -- support functions for MCL-based user interface
;;;
;;; author :  Sandra Loosemore
;;; date   :  01 Sep 1993
;;;
;;;
;;; Note:  This file contains a lot of MCL-specific code.  



;;;=========================================================================
;;; Setup
;;;=========================================================================


;;; The input hook just makes an appropriate tweak to the message area,
;;; and then reads from standard input.

(define *mac-auto-switch-input* '#t)

(define (mac-input-hook)
  (mac-status "input")
  (when *mac-auto-switch-input*
    (ccl:window-select (ccl::current-listener))
    (ccl:ed-beep))
  (let ((result  (read-line)))
    (mac-status "busy")
    result))

;;; Debugger hooks tweak the message area and switch to the MCL menubar
;;; for debugging.  This makes things like the restarts menu available.

(define *mac-haskell-menubar* '#f)   ; initialized later

(define (mac-enter-debugger-hook)
  (ccl:set-menubar ccl:*default-menubar*)
  (mac-status "debug"))

(define (mac-exit-debugger-hook)
  (ccl:set-menubar *mac-haskell-menubar*)
  (mac-status "busy"))


;;; Here's the thing to process commands.  It's basically a polling loop.
;;; ccl:get-next-queued-form reads things that have been queued with
;;; ccl:eval-enqueue (menu items).

(define (mac-command-hook)
  (mac-status "ready")
  (vanilla-prompt)
  (block got-command
    (do () ('#f)
      ;; This lets MCL catch up on other events.
      (dynamic-let ((ccl:*idle*    '#t))
        (ccl:event-dispatch))
      (let ((form  (ccl:get-next-queued-form)))
        (cond (form
               ;; Execute stuff from queue before reading new input.
               (mac-status "busy")
               (terpri)  ;; Newline after prompt
               (return-from got-command (eval form)))
              ((cl:listen)
               ;; There is input available now in the listener window.  
	       ;; Read it and do the normal command processing.
               (mac-status "busy")
               (return-from got-command (vanilla-read-and-execute-command)))
              (else
               ;; Nothing to do yet, poll again.
               '#f))))))


(define (mac-initialize-hook)
  (mac-initialize-menubar)
  (vanilla-initialize-hook))

(define (mac-compilation-error-hook)
  ;; Nothing special needs to be done here.
  '#f)


;;; Helper function to display messages in minibuffer.

(define (mac-status message)
  (ccl:set-mini-buffer (ccl::current-listener) 
                       (format '#f "Haskell: ~a" message)))


;;; Call this function to enable the mcl interface.

(define (use-mac-interface)
  (setf *haskell-enter-debugger-hook* (function mac-enter-debugger-hook))
  (setf *haskell-exit-debugger-hook* (function mac-exit-debugger-hook))
  (setf *haskell-input-hook* (function mac-input-hook))
  (setf *haskell-command-hook* (function mac-command-hook))
  (setf *haskell-initialize-hook* (function mac-initialize-hook))
  (setf *haskell-compilation-error-hook* (function mac-compilation-error-hook))
  (mac-initialize-hook))



;;;=========================================================================
;;; Menubar setup
;;;=========================================================================


;;; Install the haskell menubar.

(define (mac-initialize-menubar)
  (when (not *mac-haskell-menubar*)
    ;; Fix up the Apple menu.
    (let ((apple-menu ccl:*apple-menu*))
      (apply #'ccl:remove-menu-items apple-menu (ccl:menu-items apple-menu))
      (apply #'ccl:add-menu-items
             apple-menu
             (list (mac-make-menu-item 
                     "About Yale HaskellÉ" (function mac-about-haskell) '#f)
                   (mac-make-empty-menu-item))))
    (setf *mac-haskell-menubar*
          (list
            ;; Include the standard MCL file menu.
           (mac-find-standard-menu "File")
           ;; Include the standard MCL edit menu.
           (mac-find-standard-menu "Edit")
           ;; Make our own Haskell menu.
           (mac-make-menu
             "Haskell"
             (mac-make-window-menu-item
              "Eval ExpressionÉ" (function mac-eval-expression) '#f)
             (mac-make-window-menu-item
              "Run DialogueÉ" (function mac-run-dialogue) '#f)
             (mac-make-window-menu-item
              "Type Check ExpressionÉ" (function mac-report-type) '#f)
             (mac-make-empty-menu-item)
             (mac-make-window-menu-item
              "Load FileÉ" (function mac-load-file) '#f)
             (mac-make-window-menu-item
              "Run FileÉ" (function mac-run-file) '#f)
             (mac-make-window-menu-item
              "Compile FileÉ" (function mac-compile-file) '#f)
             (mac-make-empty-menu-item)
             (mac-make-window-menu-item
              "Scratch Pad" (function mac-switch-to-pad) '#f)
             (mac-make-empty-menu-item)
             (mac-make-menu-item
              "PrintersÉ" (function mac-set-printers) '#f)
             (mac-make-menu-item
              "OptimizersÉ" (function mac-set-optimizers) '#f)
             (mac-make-empty-menu-item)
             (mac-make-menu-item
              "Tutorial" (function mac-run-tutorial) '#f)
             (mac-make-empty-menu-item)
             (mac-make-menu-item
              "Abort" (function ccl::interactive-abort) '#\.))
           ;; Include the standard MCL windows menu.
           (mac-find-standard-menu "Windows"))))
  (ccl:set-menubar *mac-haskell-menubar*))


;;; Helper functions for making menus and menu items

(define (mac-make-menu-item title action key)
  (cl:make-instance 'ccl:menu-item 
    :menu-item-title title
    :menu-item-action action
    :command-key key))

(define (mac-make-window-menu-item title action key)
  (cl:make-instance 'ccl:window-menu-item 
    :menu-item-title title
    :menu-item-action action
    :command-key key))

(define (mac-make-empty-menu-item)
  (cl:make-instance 'ccl:menu-item
    :menu-item-title "-"
    :disabled '#t))

(define (mac-make-menu title . items)
  (cl:make-instance 'ccl:menu
    :menu-title title
    :menu-items items))


;;; Helper function to find menus from the standard MCL menubar

(define (mac-find-standard-menu title)
  (mac-find-standard-menu-aux title ccl:*default-menubar*))

(define (mac-find-standard-menu-aux title menus)
  (cond ((null? menus)
         (error "Couldn't find standard menu ~s." title))
        ((equal? title (ccl:menu-title (car menus)))
         (car menus))
        (else
         (mac-find-standard-menu-aux title (cdr menus)))))




;;;=========================================================================
;;; Menu item handlers
;;;=========================================================================


;;; About Yale Haskell

(define (mac-about-haskell)
  (ccl:message-dialog 
    (format '#f "Yale Haskell ~A~A~%~
                 Copyright (c) 1991~%~
                 Yale University CS Dept."
	  *haskell-compiler-version*
	  *haskell-compiler-update*)))


;;; Evaluate expression/Run dialogue
;;; *** Maybe we could do some fancier status messages here.

(cl:defmethod mac-eval-expression ((w ccl:fred-window))
  (let ((exp  (ccl:get-string-from-user "Enter the expression to evaluate:")))
    (mac-save-buffers w)
    (ccl:eval-enqueue
      `(mac-eval
        ',exp
        ',(ccl:window-title w)
        ',(mac-current-extension w)
        ',(mac-current-module w)
        ',(mac-current-filename w)))))

(define (mac-eval exp extension-name extension module-name maybe-file)
  (haskell-eval exp extension-name extension module-name maybe-file)
  (setf *remembered-module* module-name)
  (when maybe-file (setf *remembered-file* maybe-file)))


(cl:defmethod mac-run-dialogue ((w ccl:fred-window))
  (let ((exp  (ccl:get-string-from-user "Enter the dialogue to run:")))
    (mac-save-buffers w)
    (ccl:eval-enqueue
      `(mac-run
        ',exp
        ',(ccl:window-title w)
        ',(mac-current-extension w)
        ',(mac-current-module w)
        ',(mac-current-filename w)))))

(define (mac-run exp extension-name extension module-name maybe-file)
  (haskell-run exp extension-name extension module-name maybe-file)
  (setf *remembered-module* module-name)
  (when maybe-file (setf *remembered-file* maybe-file)))


(cl:defmethod mac-report-type ((w ccl:fred-window))
  (let ((exp  (ccl:get-string-from-user "Enter the expression to type check:")))
    (mac-save-buffers w)
    (ccl:eval-enqueue
      `(mac-report-type-aux
        ',exp
        ',(ccl:window-title w)
        ',(mac-current-extension w)
        ',(mac-current-module w)
        ',(mac-current-filename w)))))

(define (mac-report-type-aux exp extension-name extension module-name maybe-file)
  (haskell-report-type exp extension-name extension module-name maybe-file)
  (setf *remembered-module* module-name)
  (when maybe-file (setf *remembered-file* maybe-file)))



;;; File-related commands
;;; *** Maybe we could do some fancier status messages here.

(cl:defmethod mac-load-file ((w ccl:fred-window))
  (let ((fname  (or (mac-current-filename w)
                    (mac-pathname->namestring 
                     (ccl:choose-file-dialog
                      :mac-file-type '("TEXT")
                      :button-string "Load")))))
   (mac-save-buffers w)
   (ccl:eval-enqueue
    `(compile/load ',fname))))

(cl:defmethod mac-run-file ((w ccl:fred-window))
  (let ((fname  (or (mac-current-filename w)
                    (mac-pathname->namestring 
                     (ccl:choose-file-dialog
                      :mac-file-type '("TEXT")
                      :button-string "Run")))))
   (mac-save-buffers w)
   (ccl:eval-enqueue
    `(compile/run ',fname '()))))

(cl:defmethod mac-compile-file ((w ccl:fred-window))
  (let ((fname  (or (mac-current-filename w)
                    (mac-pathname->namestring 
                     (ccl:choose-file-dialog
                      :mac-file-type '("TEXT")
                      :button-string "Compile")))))
   (mac-save-buffers w)
   (ccl:eval-enqueue
    `(compile/compile ',fname))))


;;; Pads

(cl:defclass mac-pad-window (ccl:fred-window) ())

(cl:defmethod ccl:window-needs-saving-p ((w mac-pad-window))
  (declare (ignore w))
  '#f)

(cl:defmethod mac-switch-to-pad ((w ccl:fred-window))
  (ccl:window-select
     (cond ((is-type? 'ccl:listener w)
            (or (mac-lookup-pad '|Main| '#f)
                (mac-create-pad '|Main| '#f)))
           ((mac-pad-window? w)
            w)
           (else
            (let ((mname  (mac-current-module-aux w)))
              (or (mac-lookup-pad mname w)
                  (mac-create-pad mname w))))
           )))


;;; Printers/optimizers

(define (mac-set-printers)
  (setf *printers*
        (mac-make-checkbox-dialog *all-printers* *printers*)))

(define (mac-set-optimizers)
  (setf *compiled-code-optimizers*
        (mac-make-checkbox-dialog
	  *all-optimizers* *compiled-code-optimizers*)))


;;;=========================================================================
;;; Tutorial 
;;;=========================================================================

(define *tutorial-window* '#f)
(define *tutorial-buffer* '#f)
(define *tutorial-buffer-size* '#f)
(define *tutorial-buffer-page-start* '#f)
(define *tutorial-buffer-page-end* '#f)

(define *tutorial-filename* "$HASKELL/progs/tutorial/mac-tutorial.lhs")
(define *tutorial-window-filename* "$HASKELL/progs/tutorial/temp.lhs")

(cl:defclass mac-tutorial-window (ccl:fred-window) ())

(cl:defmethod ccl:window-needs-saving-p ((w mac-tutorial-window))
  (declare (ignore w))
  '#f)

(define (mac-run-tutorial)
  (mac-initialize-tutorial)
  (ccl:window-select *tutorial-window*))

(define (mac-initialize-tutorial)
  (when (not *tutorial-buffer*)
    ;; Load the tutorial from the file.
    (setf *tutorial-buffer* (ccl:make-buffer))
    (ccl:buffer-insert-file *tutorial-buffer*
      (expand-filename *tutorial-filename*))
    (setf *tutorial-buffer-size* (ccl:buffer-size *tutorial-buffer*)))
  (when (or (not *tutorial-window*) (not (ccl:wptr *tutorial-window*)))
    ;; Reset the master tutorial buffer to the first page.
    (setf *tutorial-buffer-page-start* 0)
    (setf *tutorial-buffer-page-end* (mac-next-tutorial-page 0))
    ;; Create and initialize the Fred window that holds the current page
    ;; of the tutorial
    (setf *tutorial-window* 
	(cl:make-instance 'mac-tutorial-window :window-show '#f))
    (ccl:set-window-filename *tutorial-window* 
      (expand-filename *tutorial-window-filename*))
    (ccl:set-window-title *tutorial-window* "Haskell Tutorial")
    (mac-display-tutorial-page)))

(define (mac-display-tutorial-page)
  (let ((buffer  (ccl:fred-buffer *tutorial-window*)))
    (ccl:buffer-delete buffer 0 (ccl:buffer-size buffer))
    (ccl:buffer-insert 
      buffer
      (ccl:buffer-substring 
        *tutorial-buffer* 
        *tutorial-buffer-page-start*
        *tutorial-buffer-page-end*))
    (ccl:set-mark buffer 0)
    (ccl:fred-update *tutorial-window*)))


;;; Pages in the tutorial are delimited by #\page characters

(define (mac-next-tutorial-page i)
  (or (ccl:buffer-string-pos *tutorial-buffer* (string #\page) 
                             :start i :end *tutorial-buffer-size*)
      *tutorial-buffer-size*))

(define (mac-prev-tutorial-page i)
  (let ((page  (ccl:buffer-string-pos *tutorial-buffer* (string #\page)
                                      :start 0 :end i :from-end '#t)))
    (if page (1+ page) 0)))


(define (mac-goto-next-tutorial-page w)
  (cond ((not (eq? w *tutorial-window*))
         (ccl:ed-beep))
        ((eqv? *tutorial-buffer-page-end* *tutorial-buffer-size*)
         (ccl:ed-beep))
        (else
         (setf *tutorial-buffer-page-start* (1+ *tutorial-buffer-page-end*))
         (setf *tutorial-buffer-page-end* 
               (mac-next-tutorial-page *tutorial-buffer-page-start*))
         (mac-display-tutorial-page))))

(define (mac-goto-prev-tutorial-page w)
  (cond ((not (eq? w *tutorial-window*))
         (ccl:ed-beep))
        ((eqv? *tutorial-buffer-page-start* 0)
         (ccl:ed-beep))
        (else
         (setf *tutorial-buffer-page-end* (1- *tutorial-buffer-page-start*))
         (setf *tutorial-buffer-page-start*
               (mac-prev-tutorial-page *tutorial-buffer-page-end*))
         (mac-display-tutorial-page))))

(define (mac-refresh-tutorial-page w)
  (cond ((not (eq? w *tutorial-window*))
         (ccl:ed-beep))
        (else
         (mac-display-tutorial-page))))



;;;=========================================================================
;;; Fred comtab setup
;;;=========================================================================

(define *mac-control-c-comtab* (ccl:make-comtab (function ccl:ed-beep)))

(ccl:comtab-set-key *mac-control-c-comtab* '(#\e) 
  (function mac-eval-expression)
  "Evaluate Haskell expression")
(ccl:comtab-set-key *mac-control-c-comtab* '(#\r) 
  (function mac-run-dialogue)
  "Run Haskell dialogue")
(ccl:comtab-set-key *mac-control-c-comtab* '(#\t) 
  (function mac-report-type)
  "Type check Haskell expression")
(ccl:comtab-set-key *mac-control-c-comtab* '(#\l) 
  (function mac-load-file)
  "Load Haskell file")
(ccl:comtab-set-key *mac-control-c-comtab* '(:control #\r) 
  (function mac-run-file)
  "Run Haskell file")
(ccl:comtab-set-key *mac-control-c-comtab* '(#\c) 
  (function mac-compile-file)
  "Compile Haskell file")
(ccl:comtab-set-key *mac-control-c-comtab* '(#\p) 
  (function mac-switch-to-pad)
  "Scratch pad")
(ccl:comtab-set-key *mac-control-c-comtab* '(:control #\p) 
  (lambda (w) (declare (ignore w)) (mac-set-printers))
  "Haskell printers menu")
(ccl:comtab-set-key *mac-control-c-comtab* '(:control #\o) 
  (lambda (w) (declare (ignore w)) (mac-set-optimizers))
  "Haskell optimizers menu")
(ccl:comtab-set-key *mac-control-c-comtab* '(#\i) 
  (lambda (w) (declare (ignore w)) (ccl::interactive-abort))
  "Abort")
(ccl:comtab-set-key *mac-control-c-comtab* '(:control #\f)
  (function mac-goto-next-tutorial-page)
  "Go forward to the next tutorial page.")
(ccl:comtab-set-key *mac-control-c-comtab* '(:control #\b)
  (function mac-goto-prev-tutorial-page)
  "Go backward to the previous tutorial page.")
(ccl:comtab-set-key *mac-control-c-comtab* '(:control #\l)
  (function mac-refresh-tutorial-page)
  "Restore the original text of this tutorial page.")

                    

(ccl:comtab-set-key ccl:*comtab* '(:control #\c) *mac-control-c-comtab*)


;;; Mess with the listener comtab to fix bug that prevents it from reading
;;; empty lines.  Basically, if user types return on empty line at end
;;; of buffer, bypass all the fancy listener crap and just treat it like
;;; a literal newline character.

(define (mac-hack-cr w)
  (let* ((buffer  (ccl:fred-buffer w))
         (size    (ccl:buffer-size buffer)))
    (if (and (eqv? (ccl:buffer-position buffer)  size)
             (eqv? (ccl:buffer-char buffer (- size 1)) #\newline))
        (ccl::ed-self-insert w)
        (ccl::ed-enter-command w))))
  

(ccl:comtab-set-key ccl:*listener-comtab* #\CR
   (function mac-hack-cr))


;;; This makes the default type for new files created with Fred be .hs 
;;; instead of .lisp:

(setf ccl:*.lisp-pathname* #p".hs")


;;; This makes the creator for newly created Fred files be Yale Haskell
;;; instead of Macintosh Common Lisp.  Actually, I think it's an MCL
;;; bug that it's got this hardwired in, so I feel justified in messing
;;; with defining a method on something I shouldn't to get this to 
;;; do the right thing.

(cl:defmethod ccl:window-save-as :around ((w ccl:fred-window))
  (let* ((new?    (not (ccl:window-filename w)))
         (result  (cl:call-next-method w)))
    (when new?
      (let ((name  (ccl:window-filename w)))
        (when name
          (ccl:set-mac-file-creator 
            name mumble-implementation::*mac-file-creator*))))
    result))


;;; This is called when the system is started up (see savesys.lisp).
;;; The usual load-init-files stuff is useless on the mac because the
;;; pathnames make no sense.  Load an init file from the same directory
;;; as the executable, and another from the directory you started Haskell
;;; up from.
;;; Also, reset *default-pathname-defaults* and *environment-alist* here.

(define (mac-load-init-files)
  (let ((startup    (ccl::startup-directory))
        (home       (ccl::home-directory)))
    (if home
        ;; was started by double-clicking on document
        (let ((s-init     (cl:merge-pathnames "yhaskell.scm" startup))
              (h-init     (cl:merge-pathnames "yhaskell.scm" home)))
          (setf cl:*default-pathname-defaults* home)
          (setf *environment-alist*
                (list (cons "HOME" (mac-directory->namestring home))
                      (cons "HASKELL" (mac-directory->namestring startup))))
	  (ccl:set-choose-file-default-directory home)
          (when (cl:probe-file s-init) (cl:load s-init))
          (when (cl:probe-file h-init) (cl:load h-init)))
        ;; was started by double-clicking on executable
        (let ((s-init     (cl:merge-pathnames "yhaskell.scm" startup)))
          (setf cl:*default-pathname-defaults* startup)
          (setf *environment-alist*
                (list (cons "HOME" (mac-directory->namestring startup))
                      (cons "HASKELL" (mac-directory->namestring startup))))
	  (ccl:set-choose-file-default-directory startup)
          (when (cl:probe-file s-init) (cl:load s-init))))))



;;;=========================================================================
;;; Printers/optimizers menus
;;;=========================================================================


;;; Here are some support functions.  This is set up so that the check
;;; boxes will get aligned in two columns, with the doit/cancel buttons
;;; appearing at the top of a third column.

(define (mac-make-checkbox text on-p)
  (cl:make-instance 'ccl:check-box-dialog-item
    :view-size #@(125 15)
    :dialog-item-text text
    :check-box-checked-p on-p))

(define (mac-make-button text action default-p)
  (cl:make-instance 'ccl:button-dialog-item
    :view-size #@(72 15)
    :dialog-item-text text
    :dialog-item-action action
    :default-button default-p))

(define (mac-make-dialog items doit-fn)
  (cl:make-instance 'ccl:dialog
    :view-size (ccl:make-point 350 (* 25 (cl:ceiling (length items) 2)))
    :window-type :double-edge-box
    :close-box-p  '#f
    :view-font '("Chicago" 12 :srcor :plain)
    :view-subviews
      (append items
              (list (mac-make-button 
                      "Do it"
                      (lambda (b) 
                        (ccl:return-from-modal-dialog (funcall doit-fn b)))
                      '#t)
                    (mac-make-button 
                      "Cancel"
                      (lambda (b)
                        (declare (ignore b))
                        (ccl:return-from-modal-dialog :cancel))
                      '#f)))))


;;; Here's the function to actually make the menu.  It returns
;;; the list of things that the user has checked off.

(define (mac-make-checkbox-dialog all-items current-items)
  (let* ((flags  (map (lambda (i)
                        (mac-make-checkbox (symbol->string i)
                                           (memq i current-items)))
                      all-items))
         (doit   (lambda (b)
                   (declare (ignore b))
                   (let ((new-items  '()))
                     (for-each
                       (lambda (m i)
                         (when (ccl:check-box-checked-p m)
                           (push i new-items)))
                       flags
                       all-items)
                     new-items))))
    (ccl:modal-dialog (mac-make-dialog flags doit))))




;;;=========================================================================
;;; Fred-related utilities
;;;=========================================================================

;;; Save modified buffers that are associated with files.

(define *mac-ask-before-saving* '#t)

(define (mac-save-buffers current-window)
  (dolist (w (ccl:windows :class 'ccl:fred-window))
    (when (or (and (ccl:window-needs-saving-p w)
                   (or (eq? w current-window)
                       (not *mac-ask-before-saving*)
                       (ccl:y-or-n-dialog
                        (format '#f "Save buffer ~a?" (ccl:window-title w)))))
              (is-type? 'mac-tutorial-window w))
      (ccl:window-save w))))


;;; If w is a Haskell source buffer, return the contents of its associated pad.
;;; If w is a pad, return its contents.
;;; Otherwise return the contents of the last pad used.

(define *mac-last-pad* '#f)

(define (mac-current-extension w)
  (let ((pad  (mac-current-pad w)))
    (if pad
        (let ((buffer  (ccl:fred-buffer w)))
          (ccl:buffer-substring buffer 0 (ccl:buffer-size buffer)))
        "")))

(define (mac-current-pad w)
  (setf *mac-last-pad*
        (cond ((is-type? 'ccl:listener w)
               *mac-last-pad*)
              ((mac-pad-window? w)
               w)
              (else
               (mac-lookup-pad (mac-current-module-aux w) w))
              )))


;;; If w is a Haskell source buffer, find the module definition that the
;;; cursor is in, and return its name.
;;; If w is a pad, return the module name.
;;; Otherwise return the last module name used.
;;; Note that module names are symbols, not strings!

(define *mac-last-module* '|Main|)

(define (mac-current-module w)
  (setf *mac-last-module*
        (cond ((is-type? 'ccl:listener w)
               *mac-last-module*)
              ((mac-pad-window? w)
               (mac-pad-window-module-name w))
              (else
               (mac-current-module-aux w))
              )))



;;; Note, you must save new buffers with .lhs extensions before
;;; literate syntax is recognized here....

(define *module-search-string* "module ")
(define *module-search-string-end* (1- (string-length *module-search-string*)))

(define (mac-current-module-aux w)
  (let* ((buffer    (ccl:fred-buffer w))
	 (pos       (ccl:buffer-position buffer))
	 (literate? (and (ccl:window-filename w)
			 (equal? (cl:pathname-type (ccl:window-filename w))
				 "lhs"))))
    (or (mac-module-search-backward buffer pos literate?)
	(mac-module-search-forward buffer pos literate?)
	'|Main|)))

(define (mac-module-search-backward buffer pos literate?)
  (let ((mstart  (ccl:buffer-string-pos buffer *module-search-string*
					:end pos :from-end '#t)))
    (cond ((not mstart)
	   '#f)
	  ((mac-module-really-matches buffer mstart literate?)
	   (mac-module-extract buffer mstart))
	  ((eqv? mstart 0)
	   '#f)
	  (else
	   (mac-module-search-backward buffer (1- mstart) literate?)))))

(define (mac-module-search-forward buffer pos literate?)
  (let ((mstart  (ccl:buffer-string-pos buffer *module-search-string*
					:start pos)))
    (cond ((not mstart)
	   '#f)
	  ((mac-module-really-matches buffer mstart literate?)
	   (mac-module-extract buffer mstart))
	  (else
	   (mac-module-search-forward buffer (1+ mstart) literate?)))))

(define (mac-module-really-matches buffer mstart literate?)
  (let ((beg  (ccl:buffer-line-start buffer mstart)))
    (if literate?
	(eqv? (ccl:buffer-char buffer beg) '#\>)
	(eqv? beg mstart))))

(define (mac-module-extract buffer mstart)
  (multiple-value-bind (start end)
      (ccl:buffer-word-bounds 
        buffer
	(ccl:buffer-skip-fwd-wsp&comments 
	  buffer
	  (+ mstart *module-search-string-end*)
	  (ccl:buffer-size buffer)))
    (string->symbol (ccl:buffer-substring buffer start end))))


;;; Look up a pad window.

(define *mac-pad-list* '())

(define (mac-lookup-pad module-name file-window)
  (mac-lookup-pad-aux module-name file-window *mac-pad-list*))

(define (mac-lookup-pad-aux module-name file-window pad-list)
  (cond ((null? pad-list)
         '#f)
        ((and (eq? module-name (car (car pad-list)))
              (eq? file-window (cadr (car pad-list)))
              ;; Make sure window has not been closed!
              (ccl:wptr (caddr (car pad-list))))
         (caddr (car pad-list)))
        (else
         (mac-lookup-pad-aux module-name file-window (cdr pad-list)))
        ))


;;; Create a new pad window and enter it into the cache.

(define (mac-create-pad module-name file-window)
  (let ((pad  (cl:make-instance 'mac-pad-window
                :window-title (format '#f "Pad for module ~a" module-name))))
    (push (list module-name file-window pad) *mac-pad-list*)
    pad))


(define (mac-pad-window? w)
  (mac-pad-window-aux w *mac-pad-list*))

(define (mac-pad-window-aux w pad-list)
  (cond ((null? pad-list)
         '#f)
        ((eq? w (caddr (car pad-list)))
         (car pad-list))
        (else
         (mac-pad-window-aux w (cdr pad-list)))))

(define (mac-pad-window-module-name w)
  (car (mac-pad-window? w)))

(define (mac-pad-window-file-window w)
  (cadr (mac-pad-window? w)))


;;; Return the (Unix-style) filename string.
;;; If in a Haskell source buffer, return the name of its compilation unit.
;;; If in a pad, return the name of its compilation unit.
;;; Otherwise, return #f.

(define (mac-current-filename w)
  (cond ((is-type? 'ccl:listener w)
         '#f)
        ((mac-pad-window? w)
         (let ((file-window  (mac-pad-window-file-window w)))
           (if file-window
             (mac-current-filename-aux file-window)
             '#f)))
        (else
         (mac-current-filename-aux w))
        ))


(define (mac-current-filename-aux w)
  (let* ((buffer  (ccl:fred-buffer w))
         (ustart  (ccl:buffer-string-pos buffer "-- unit:" :start 0)))
    (cond (ustart
           ;; The current buffer contains a unit specification
           (let* ((start (ccl:buffer-skip-fwd-wsp&comments 
                          buffer (+ ustart 8) (ccl:buffer-size buffer)))
                  (end   (ccl:buffer-char-pos buffer #\newline :start start)))
             (ccl:buffer-substring buffer start end)))
          ((ccl:window-filename w)
           ;; The current buffer has a filename
           (mac-pathname->namestring (ccl:window-filename w)))
          (else
           ;; This is a new buffer that hasn't been saved to a file yet.
           ;; *** Maybe we should ask for a name to save it with???
           '#f))))


;;; Mess with pathname conversion.

(define (mac-pathname->namestring p)
  (cl:substitute #\/ #\: (cl:namestring p)))

(define (mac-directory->namestring p)
  (let* ((name  (mac-pathname->namestring p))
         (n     (string-length name)))
    (substring name 0 (1- n))))   ; drop trailing /
  
