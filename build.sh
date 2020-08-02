find . -name '*.dx64fsl' | xargs rm
Y2=$PWD HASKELL=$PWD PRELUDE=$PWD/progs/prelude PRELUDEBIN=$PWD/progs/prelude/mcl ccl --load com/mcl/mcl-setup.lisp --load cl-support/cl-init.lisp --load com/mcl/savesys.lisp
