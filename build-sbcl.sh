find . -name '*.fasl' | xargs rm
Y2=$PWD HASKELL=$PWD PRELUDE=$PWD/progs/prelude PRELUDEBIN=$PWD/progs/prelude/sbcl sbcl --eval "(progn (if (not (find-package \"MUMBLE-IMPLEMENTATION\")) (make-package \"MUMBLE-IMPLEMENTATION\" :use '(\"COMMON-LISP\")) (in-package \"MUMBLE-IMPLEMENTATION\")))" --load com/mcl/mcl-setup.lisp --load cl-support/cl-init.lisp --load com/mcl/savesys.lisp
