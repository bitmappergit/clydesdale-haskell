;;; savesys.lisp -- make a Yale Haskell executable 

;(load "Macintosh HD:haskell:com:mcl:mcl-setup.lisp")
;(setf *load-verbose* t)
;(setf *compile-verbose* t)
;(setf ccl:*record-source-file* nil)
;(proclaim '(optimize (speed 3) (safety 0) (debug 1) (compilation-speed 0)))
;(proclaim '(optimize (speed 1) (safety 3) (debug 3) (compilation-speed 0)))
;(load "Macintosh HD:haskell:cl-support:cl-init.lisp")
(setf *load-verbose* nil)
(setf *compile-verbose* nil)
(in-package :mumble-user)
(setf *printers* '(compiling loading prompt))
(setf *code-chunk-size* 300)
(setf *compile-interface* '#f)
(compile/load *prelude-unit-filename*)
;;; Doing this now will pre-initialize menus instead of having this
;;; done at system startup time.
(setf *modules-loaded* '())
(use-vanilla-interface)
(define (haskell-toplevel)
  (setf cl:*package* (cl:find-package :mumble-user))
  ;(mac-load-init-files)
  (use-vanilla-interface)
  (do () ('#f)
    (cl:with-simple-restart (restart-haskell "Restart Haskell.")
      (heval))))
(define (restart-haskell)
  (cl:invoke-restart 'restart-haskell))
(define (haskell-debugger-hook c f)
  (declare (ignore f))
  (if *haskell-debug-in-lisp*
      (begin
        (when *haskell-enter-debugger-hook*
          (funcall *haskell-enter-debugger-hook*))
        (cl:unwind-protect (cl:invoke-debugger c)
          (when *haskell-exit-debugger-hook*
            (funcall *haskell-exit-debugger-hook*))))
      (begin
        (format '#t "Lisp error:~%~a~%" c)
        (haskell-backtrace)
        (when *haskell-compilation-error-hook*
          (funcall *haskell-compilation-error-hook*))
        (format '#t "Restarting Haskell...~%")
        (restart-haskell))))
(setf cl:*debugger-hook* (function haskell-debugger-hook))
;(setf mumble-implementation::*mac-file-creator* :yhs2)
(ccl:save-application "./bin/yale-ccl"
  ;; Comment out the next line if not making a standalone delivery binary
  :prepend-kernel '#t
  :toplevel-function #'haskell-toplevel)
