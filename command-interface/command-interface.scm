;;; command-interface.scm -- compilation unit definition for user interface

(define-compilation-unit command-interface
  (source-filename "$Y2/command-interface/")
  (require global)
  (unit interface-hooks
    (source-filename "interface-hooks.scm"))
  (unit interface-prims
    (source-filename "interface-prims.scm")
    (require interface-hooks))
  (unit vanilla
    (source-filename "vanilla.scm")
    (require interface-prims))
  (unit emacs-support
    (source-filename "emacs-support.scm")
    (require interface-prims))
#+mcl
  (unit mcl-support
    (source-filename "mcl-support.scm")
    (require interface-prims))
  )
