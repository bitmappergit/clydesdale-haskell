;;; compile.lisp -- compile all the Haskell sources

(load "Macintosh HD:haskell:com:mcl:mcl-setup.lisp")
(setf *load-verbose* t)
(setf *compile-verbose* t)
(proclaim '(optimize (speed 3) (safety 0) (debug 1) (compilation-speed 0)))
;(proclaim '(optimize (speed 1) (safety 3) (debug 3) (compilation-speed 0)))
(load "Macintosh HD:haskell:cl-support:cl-init.lisp")
