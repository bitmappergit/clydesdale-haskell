;;; cl-setup.lisp -- set up mumble environment in Common Lisp
;;;
;;; author :  Sandra Loosemore
;;; date   :  10 Oct 1991
;;;
;;; This file must be loaded before either compiling or loading
;;; the cl-definitions file.


;;; The mumble package exports only those symbols that have definitions
;;; in mumble.  Many of these symbols shadow built-in CL definitions.
;;; Programs that use mumble should use the mumble package in place of
;;; (rather than in addition to) the CL package.

;;; Clisp's compiler is hopelessly broken in its treatment of make-package!!!

#-clisp
(unless (find-package "MUMBLE")
  (make-package "MUMBLE" :use nil))

#+clisp
(lisp:in-package "MUMBLE" :use '())


;;; The actual implementation of the mumble compatibility library happens
;;; in the MUMBLE-IMPLEMENTATION package.  We'll explicitly package-qualify
;;; all symbols from the MUMBLE package that it references, and rely
;;; on the definitional macros to arrange to export them from the MUMBLE
;;; package.

#-clisp
(unless (find-package "MUMBLE-IMPLEMENTATION")
  (make-package "MUMBLE-IMPLEMENTATION" :use '("LISP")))

#+clisp
(lisp:in-package "MUMBLE-IMPLEMENTATION" :use '("LISP"))





