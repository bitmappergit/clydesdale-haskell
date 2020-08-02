(setf *printers* '())

(define (tst) 
  (initialize-haskell-system)
  (initialize-module-table)
  (compile-haskell-files (list "$Y2/progs/tests/bogus-prelude.hs")))

