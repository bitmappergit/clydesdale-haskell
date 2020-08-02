;;; foreign.lisp -- A C interface package for Common Lisp
;;;
;;; author :  John Peterson
;;;
;;; You must load cl-setup and cl-support before trying to compile this 
;;; file.

(in-package "MUMBLE-IMPLEMENTATION")

;;; Since the foreign function interfaces are so different among the various
;;; lisps, all definitions are conditionalized at the very top level.

;;; The following functions constitute the C interface:

;;;  (load-foreign-file filename)  -- Loads a .o file

;;; (define-c-function c-name lisp-name r-type type-1 type-2 ... type-n)
;;;   C types are:
;;;     :char
;;;     :short
;;;     :int
;;;     :long
;;;     :unsigned-char
;;;     :unsigned-short
;;;     :unsigned-int
;;;     :unsigned-long
;;;     :float
;;;     :double
;;;     :bool
;;;     :void
;;;     :c-string
;;;     (:* type)
;;;     

(defvar *ff-cache* '())

(define-mumble-function mumble::load-foreign-file/cached (file-name)
  (let ((fname (expand-filename file-name)))
    (if (probe-file fname)
	(let ((wd (file-write-date fname))
	      (old-wd (assoc fname *ff-cache* :test #'string=)))
	  (if (and old-wd (equal wd (cdr old-wd)))
	      ':cached
	      (if old-wd
		  ':error
		  (load-foreign-file-aux fname)))))))

(define-mumble-function mumble::load-foreign-file (file-name)
  (load-foreign-file-aux (expand-filename file-name)))
	        
	         

#+cmu
(progn
  (defun load-foreign-file-aux (file-name)
    (if (probe-file file-name)
	(progn (extensions:load-foreign file-name)
	       (setf *ff-cache* (cons (cons file-name
					    (file-write-date file-name))
				      *ff-cache*))
	       ':loaded)
        ':not-found))

  (defun convert-c-type (ty)
    (cond ((eq ty ':char) 'c-call:char)
	  ((eq ty ':short) 'c-call:short)	   
	  ((eq ty ':int) 'c-call:int)
	  ((eq ty ':long) 'c-call:long)	   
	  ((eq ty ':unsigned-char) 'c-call:unsigned-char)	   
	  ((eq ty ':unsigned-short) 'c-call:unsigned-short)	   
	  ((eq ty ':unsigned-int) 'c-call:unsigned-int)
	  ((eq ty ':unsigned-long) 'c-call:unsigned-long)	   
	  ((eq ty ':float) 'c-call:float)	   
	  ((eq ty ':double) 'c-call:double)
	  ((eq ty ':bool) 'alien:boolean)
	  ((eq ty ':void) 'c-call:void)
	  ((eq ty ':c-string) 'c-call:c-string)
	  ((and (listp ty)
		(eq (car ty) ':*))
	   (list 'alien:* (convert-c-type (cadr ty))))
	  (t (error "C type ~s is not supported in this Lisp." ty))))

  (define-mumble-macro mumble::define-c-function
                         (c-name lisp-name res-type &rest arg-types)
    `(alien:def-alien-routine (,c-name ,lisp-name)
       ,(convert-c-type res-type)
       ,@(mapcar #'(lambda (ty)
		     (list (gensym "a") (convert-c-type ty)))
		 arg-types)))
  )	

#+allegro
(progn
  (defun load-foreign-file-aux (file-name)
    (if (probe-file file-name)
	(progn (load file-name)
	       (setf *ff-cache* (cons (cons file-name
					    (file-write-date file-name))
				      *ff-cache*))
	       ':loaded)
        ':not-found))

  (defun convert-c-type/in (ty)
    (cond ((eq ty ':char) 'character)
;	  ((eq ty ':short) 'c-call:short)	   
	  ((eq ty ':int) 'fixnum)
;	  ((eq ty ':long) 'c-call:long)	   
;	  ((eq ty ':unsigned-char) 'c-call:unsigned-char)	   
;	  ((eq ty ':unsigned-short) 'c-call:unsigned-short)	   
;	  ((eq ty ':unsigned-int) 'c-call:unsigned-int)
;	  ((eq ty ':unsigned-long) 'c-call:unsigned-long)	   
	  ((eq ty ':float) 'float)	   
	  ((eq ty ':double) 'double-float)
;	  ((eq ty ':bool) 'alien:boolean)
	  ((eq ty ':void) ':void)
	  ((eq ty ':c-string) 'string)
;	  ((and (listp ty)
;		(eq (car ty) ':*))
;	   (list 'alien:* (convert-c-type (cadr ty))))
	  (t (error "C type ~s is not supported in this Lisp." ty))))

  (defun convert-c-type/out (ty)
    (cond ((eq ty ':char) :character)
;	  ((eq ty ':short) 'c-call:short)	   
	  ((eq ty ':int) :fixnum)
;	  ((eq ty ':long) 'c-call:long)	   
;	  ((eq ty ':unsigned-char) 'c-call:unsigned-char)	   
;	  ((eq ty ':unsigned-short) 'c-call:unsigned-short)	   
;	  ((eq ty ':unsigned-int) 'c-call:unsigned-int)
;	  ((eq ty ':unsigned-long) 'c-call:unsigned-long)	   
	  ((eq ty ':float) ':single-float)	   
	  ((eq ty ':double) ':double-float)
;	  ((eq ty ':bool) 'alien:boolean)
	  ((eq ty ':void) ':void)
;	  ((eq ty ':c-string) 'string)
;	  ((and (listp ty)
;		(eq (car ty) ':*))
;	   (list 'alien:* (convert-c-type (cadr ty))))
	  (t (error "C type ~s is not supported in this Lisp." ty))))

  (define-mumble-macro mumble::define-c-function
                         (c-name lisp-name res-type &rest arg-types)
    `(ff:defforeign ',lisp-name
       :entry-point ',(concatenate 'string "_" c-name)
       :return-type ',(convert-c-type/out res-type)
       :arguments ',(mapcar #'convert-c-type/in arg-types)
       :arg-checking t
       :language :c))
  )




#+lucid
(progn
  (defun load-foreign-file-aux (file-name)
    (if (probe-file file-name)
	(progn (lcl:load-foreign-files file-name)
	       (setf *ff-cache* (cons (cons file-name
					    (file-write-date file-name))
				      *ff-cache*))
	       ':loaded)
        ':not-found))

  (defun convert-c-type (ty)
    (cond ((eq ty ':char) ':character)
;	  ((eq ty ':short) 'c-call:short)	   
	  ((eq ty ':int) ':signed-32bit)
	  ((eq ty ':long) ':signed-32bit)
;	  ((eq ty ':unsigned-char) 'c-call:unsigned-char)	   
;	  ((eq ty ':unsigned-short) 'c-call:unsigned-short)	   
	  ((eq ty ':unsigned-int) :unsigned-32bit)
	  ((eq ty ':unsigned-long) :unsigned-32bit)
	  ((eq ty ':float) ':single-float)	   
	  ((eq ty ':double) ':double-float)
	  ((eq ty ':bool) ':boolean)
	  ((eq ty ':void) ':null)
	  ((eq ty ':c-string) ':simple-string)
	  ((and (listp ty)
		(eq (car ty) ':*))
	   (list ':pointer (convert-c-type (cadr ty))))
	  (t (error "C type ~s is not supported in this Lisp." ty))))

  (define-mumble-macro mumble::define-c-function
                         (c-name lisp-name res-type &rest arg-types)
    `(lcl:def-foreign-function
        (,lisp-name (:name ',(concatenate 'string "_" c-name))
		    (:return-type ,(convert-c-type res-type)))
	,@(mapcar #'(lambda (ty)
		      (list (gensym "a") (convert-c-type ty)))
		  arg-types)))
  )

	

#-(or cmu allegro lucid)
(progn
  (defun load-foreign-file-aux (file-name)
    (declare (ignore file-name))
    (error "No loader for .o files in this lisp."))
)
