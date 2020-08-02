;;; build-prelude.lisp -- build the haskell prelude

(load "Macintosh HD:haskell:com:mcl:mcl-setup.lisp")
(setf *load-verbose* t)
(setf *compile-verbose* t)
(proclaim '(optimize (speed 3) (safety 0) (debug 1) (compilation-speed 0)))
;(proclaim '(optimize (speed 1) (safety 3) (debug 3) (compilation-speed 0)))
(load "Macintosh HD:haskell:cl-support:cl-init.lisp")
(in-package :mumble-user)
(setf *printers* '(phase-time dump-stat))
(setf *code-chunk-size* 300)
(setf *compile-interface* '#f)
(compile/compile *prelude-unit-filename*)
