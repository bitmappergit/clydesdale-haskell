;;; emacs-support.scm -- support for the emacs interface
;;;
;;; author :  Sandra Loosemore
;;; date   :  30 Jul 1993
;;;

;;; The emacs interface is set up to allow the vanilla command interface
;;; to run in a listener window.  The hooks allow some additional
;;; messages to be sent to the emacs process.  And, the emacs interface
;;; sends messages back to the haskell process by using the Lisp escape
;;; feature of the vanilla command interface.

(define (emacs-enter-debugger-hook)
  (emacs-send-status "debug"))

(define (emacs-exit-debugger-hook)
  (emacs-send-status "busy"))

(define (emacs-input-hook)
  (emacs-send-status "input")
  (let ((result (read-line)))
    (emacs-send-status "busy")
    result))

(define (emacs-command-hook)
  (emacs-send-status "ready")
  (vanilla-prompt)
  (peek-char)  ; wait for input to arrive
  (emacs-send-status "busy")
  (vanilla-read-and-execute-command))

(define (emacs-initialize-hook)
  ;; Nothing special needs to be done here.
  (vanilla-initialize-hook))

(define (emacs-compilation-error-hook)
  (emacs-send-error))



;;; Tell emacs to update the process status in the mode line.

(define (emacs-send-status status)
  (format '#t "EMACS:~a~%" status)
  (force-output))


;;; This tells emacs to display a message in the minibuffer area.

(define (emacs-send-message message)
  (format '#t "EMACS:message ~a~%" message)
  (force-output))


;;; Tell emacs there was a compilation error.

(define (emacs-send-error)
  (format '#t "EMACS:error~%")
  (force-output))


;;; Here are some extra functions that the emacs interface uses.

(define (emacs-send-printers)
  (format '#t "EMACS:printers ~s~%" (stringify-syms (dynamic *printers*)))
  (force-output))

(define (emacs-send-optimizers)
  (format '#t "EMACS:optimizers ~s~%"
	  (stringify-syms (dynamic *compiled-code-optimizers*)))
  (force-output))

(define (stringify-syms syms)
  (map (lambda (s) (string-downcase (symbol->string s))) syms))

(define (emacs-set-printers printers)
  (setf (dynamic *printers*) (set-printers printers '=))
  (emacs-send-message "Setting printers ...done.")
  )

(define (emacs-set-optimizers optimizers)
  (setf (dynamic *compiled-code-optimizers*) (set-optimizers optimizers '=))
  (emacs-send-message "Setting optimizers ...done.")
  )

(define (emacs-eval exp extension-name extension module-name maybe-file)
  (emacs-send-message (format '#f "Evaluating: ~a" exp))
  (haskell-eval exp extension-name extension module-name maybe-file)
  (setf *remembered-module* module-name)
  (when maybe-file (setf *remembered-file* maybe-file))
  (emacs-send-message (format '#f "Evaluating: ~a ...done." exp)))

(define (emacs-run exp extension-name extension module-name maybe-file)
  (emacs-send-message (format '#f "Running: ~a" exp))
  (haskell-run exp extension-name extension module-name maybe-file)
  (setf *remembered-module* module-name)
  (when maybe-file (setf *remembered-file* maybe-file))
  (emacs-send-message (format '#f "Running: ~a ...done." exp)))

(define (emacs-report-type exp extension-name extension module-name maybe-file)
  (emacs-send-message (format '#f "Type checking: ~a" exp))
  (haskell-report-type exp extension-name extension module-name maybe-file)
  (setf *remembered-module* module-name)
  (when maybe-file (setf *remembered-file* maybe-file))
  (emacs-send-message (format '#f "Type checking: ~a ...done." exp)))


(define (emacs-run-file filename)
  (emacs-send-message (format '#f "Running file: ~a" filename))
  (vanilla-run-file filename)
  (emacs-send-message (format '#f "Running file: ~a ...done." filename)))

(define (emacs-load-file filename)
  (emacs-send-message (format '#f "Loading file: ~a" filename))
  (vanilla-load-file filename)
  (emacs-send-message (format '#f "Loading file: ~a ...done." filename)))

(define (emacs-compile-file filename)
  (emacs-send-message (format '#f "Compiling file: ~a" filename))
  (vanilla-compile-file filename)
  (emacs-send-message (format '#f "Compiling file: ~a ...done." filename)))


;;; Call this function to enable the emacs interface.

(define (use-emacs-interface)
  (setf *haskell-enter-debugger-hook* (function emacs-enter-debugger-hook))
  (setf *haskell-exit-debugger-hook* (function emacs-exit-debugger-hook))
  (setf *haskell-input-hook* (function emacs-input-hook))
  (setf *haskell-command-hook* (function emacs-command-hook))
  (setf *haskell-initialize-hook* (function emacs-initialize-hook))
  (setf *haskell-compilation-error-hook*
	(function emacs-compilation-error-hook))
  (emacs-initialize-hook))

