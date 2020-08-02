;;; This file contains routines which generate the code for the
;;; dictionaries used in the class system.

(define (make-sel-node size i)
  (**lambda '(x)
     (if (eqv? size 1)
	 (**var 'x)
	 (**sel (tuple-constructor size) (**var 'x) i))))

(define (make-compose f1 f2)
  (**lambda '(x)
      (**app f1 (**app f2 (**var 'x)))))

(define (make-new-var name)  ; name is a string
  (make var (name (string->symbol name))
	    (module *module-name*)
	    (unit *unit*)
	    (toplevel? '#t)
	    (exported? '#t)))

(define (make-runtime-var parent suffix ty)  ; name is a string
  (let ((new-name (string->symbol
		    (string-append
		     (symbol->string (def-name parent))
		     suffix))))
    (make var (name new-name)
	      (module (def-module parent))
	      (unit (def-unit parent))
	      (toplevel? '#t)
	      (exported? '#t)
	      (interface? (interface-module? *module*))
	      (type (**gtype '() (**ntycon ty '()))))))

