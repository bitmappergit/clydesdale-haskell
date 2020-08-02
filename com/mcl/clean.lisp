;;; clean.lisp -- delete all binaries

(dolist (f (directory "Macintosh HD:haskell:*:mcl:*.fasl"))
  (delete-file f))

