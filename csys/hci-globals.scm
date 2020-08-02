
;;; These are the global variables & macros used during the dump process.

;;; Globals

(define *dump-defs* '())
(define *dump-slot-init-code* '())
(define *dump-def-counter* 0)
(define *dump-def-code-table* (make-table))
(define *dump-types* '())
(define *dump-type-counter* 0)
(define *number-vars-dumped* 0)
(define *number-types-dumped* 0)
(define *number-classes-dumped* 0)
(define *dump-file-names* '())


(define-syntax (def-dump-code def)
  `(table-entry *dump-def-code-table* ,def))

(define-syntax (define-dumper-methods types)
  `(begin
     ,@(map (lambda (type) (make-dump-method type)) types)))

(define (make-dump-method type+slots)
  (let ((type (if (pair? type+slots) (car type+slots) type+slots))
	(ignored-slot (cons 'line-number
			    (if (pair? type+slots) (cdr type+slots) '()))))
  `(define-walker-method dump ,type (o)
     o      ; prevent possible unreferenced variable warnings
     (list 'make ',type
	   ,@(concat
	       (map (lambda (slot)
		      (let ((name (sd-name slot)))
			(if (memq name ignored-slot)
			    '()
			    `((list ',name
				    (dump-object
				     (struct-slot ',type ',name o)))))))
		    (td-slots (lookup-type-descriptor type))))))))


