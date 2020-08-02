;;; interface-prims.scm -- low-level primitives for user interface
;;;
;;; author :  Sandra Loosemore
;;; date   :  23 Jul 1993
;;;
;;;


;;;=========================================================================
;;; Hooks for customizing the interface
;;;=========================================================================


;;; The savesys code is responsible for establishing a debugger hook
;;; function (e.g., using *debugger-hook* from the CL condition facility).

;;; If this variable is false, don't enter the debugger at all; simply
;;; restart the command loop.

(define *haskell-debug-in-lisp* '#f)


;;; These two functions may be bound to functions of no arguments which
;;; perform some sort of notication to the user.  The debugger hook 
;;; function should invoke them before or after (respectively) invoking
;;; the real debugger.  The return values of the functions are ignored.

(define *haskell-enter-debugger-hook* '#f)
(define *haskell-exit-debugger-hook* '#f)


;;; This function is called to read a line of input (without the trailing
;;; newline) from standard input.  It might want to take some sort of
;;; action to notify the user that they're supposed to type something.

(define *haskell-input-hook*
  (lambda () (error "No interface installed!")))


;;; The command loop calls this function to read and execute a single
;;; command.  It may display prompts, menus, etc. or whatever.  The
;;; function takes no arguments and the return values are ignored.

(define *haskell-command-hook*
  (lambda () (error "No interface installed!")))


;;; When the command loop is started (or restarted, after an error), it
;;; calls this function to perform any initialization specific to the
;;; particular user interface.  It is a function of no arguments and
;;; the return values are ignored.

(define *haskell-initialize-hook*
  (lambda () (error "No interface installed!")))


;;; This variable may be bound to a function that is used to report a
;;; Haskell error (as opposed to an internal Lisp error).  The function
;;; is called whenever a non-recoverable error causes control to be
;;; returned to the command loop.  It takes no arguments and the return
;;; values are ignored.

(define *haskell-compilation-error-hook* '#f)

(define *compile/compile-cflags*
  (make cflags
	(load-code?          '#t)
	(compile-code?       '#t)
	(write-code?         '#t)
	(write-interface?    '#t)))

(define *compile/load-cflags*
  (make cflags
	(load-code?          '#t)
	(compile-code?       '#f)
	(write-code?         '#f)
	(write-interface?    '#f)))


