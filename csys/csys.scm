;;; csys.scm -- compilation unit definition for the compilation system

(define-compilation-unit csys
  (source-filename "$Y2/csys/")
  (require global runtime flic)
  (unit hci-globals
     (source-filename "hci-globals"))
  (unit cache-structs
    (source-filename "cache-structs.scm"))
  (unit unit-cache
    (require cache-structs)
    (source-filename "unit-cache.scm"))
  (unit unit-parser
    (require cache-structs)
    (source-filename "unit-parser.scm"))
  (unit compiler-driver
    (require cache-structs)
    (source-filename "compiler-driver.scm"))
  (unit hci-files
    (source-filename "hci-files.scm")
    (require hci-globals))
  (unit dump-interface
    (source-filename "dump-interface.scm")
    (require hci-globals))
  (unit dump-flic
    (source-filename "dump-flic.scm")
    (require hci-globals))
  (unit structure-save
    (source-filename "structure-save.scm")
    (require hci-globals))
  )

