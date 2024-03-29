;;; cl-definitions.lisp -- mumble compatibility package for Common Lisp
;;;
;;; author :  Sandra Loosemore
;;; date   :  11 Oct 1991
;;;
;;; You must load cl-setup and cl-support before trying to compile this 
;;; file.

(in-package "MUMBLE-IMPLEMENTATION")

;;;=====================================================================
;;; Syntax
;;;=====================================================================

(define-mumble-import quote)
(define-mumble-import function)
(define-mumble-import remove)
;;; Lambda lists have to have dot syntax converted to &rest.

(define-mumble-macro mumble::lambda (lambda-list &rest body)
  `(function (lambda ,(mung-lambda-list lambda-list) ,@body)))

(defun mung-lambda-list (lambda-list)
  (cond ((consp lambda-list)
	 (let ((last  (last lambda-list)))
	   (if (null (cdr last))
	       lambda-list
	       `(,@(ldiff lambda-list last) ,(car last) &rest ,(cdr last)))))
	((null lambda-list)
	 '())
	(t
	 `(&rest ,lambda-list))))


;;; We only funcall and apply things that are real functions.


;;; Gag.  Lucid needs to see the procedure declaration to avoid putting
;;; a coerce-to-procedure check in, but there's a compiler bug that causes
;;; it to barf if the function is a lambda form.

#+lucid
(define-mumble-macro mumble::funcall (fn . args)
  (if (and (consp fn) (eq (car fn) 'mumble::lambda))
      `(funcall ,fn ,@args)
      `(funcall (the system::procedure ,fn) ,@args)))

#+(or sbcl cmu allegro akcl lispworks mcl clisp ccl)
(define-mumble-macro mumble::funcall (fn . args)
  `(funcall (the function ,fn) ,@args))

#+wcl
(define-mumble-macro mumble::funcall (fn . args)
  `(funcall (the lisp:procedure ,fn) ,@args))

#-(or sbcl lucid cmu allegro akcl mcl lispworks wcl clisp ccl)
(missing-mumble-definition mumble::funcall)


;;; Could make this declare its fn argument too


(define-mumble-import in-package)

(define-mumble-import apply)

(define-mumble-synonym mumble::map mapcar)
(define-mumble-synonym mumble::for-each mapc)
(define-mumble-import some)
(define-mumble-import every)
(define-mumble-import notany)
(define-mumble-import notevery)
(define-mumble-synonym mumble::procedure? functionp)

(define-mumble-import if)
(define-mumble-import when)
(define-mumble-import unless)


;;; COND and CASE differ from Common Lisp because of using "else" instead 
;;; of "t" as the fall-through case.

(define-mumble-import mumble::else)

(define-mumble-macro mumble::cond (&rest cases)
  (let ((last    (car (last cases))))
    (if (eq (car last) 'mumble::else)
	`(cond ,@(butlast cases) (t ,@(cdr last)))
	`(cond ,@cases))))

(define-mumble-macro mumble::case (data &rest cases)
  (let ((last  (car (last cases))))
    (if (eq (car last) 'mumble::else)
	`(case ,data ,@(butlast cases) (t ,@(cdr last)))
	`(case ,data ,@cases))))


(define-mumble-import and)
(define-mumble-import or)
(define-mumble-import not)

(define-mumble-macro mumble::set! (variable value)
  `(setq ,variable ,value))
(define-mumble-import setf)


;;; AKCL's SETF brokenly tries to macroexpand the place
;;; form before looking for a define-setf-method.  Redefine the
;;; internal function to do the right thing.

#+akcl
(defun system::setf-expand-1 (place newvalue env)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method place env)
    (declare (ignore access-form))
    `(let* ,(mapcar #'list
                    (append vars stores)
                    (append vals (list newvalue)))
       ,store-form)))


;;; Allegro has renamed this stuff as per ANSI CL.

;#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (macro-function 'define-setf-method)
	(macro-function 'define-setf-expander))
  (setf (symbol-function 'get-setf-method)
	(symbol-function 'get-setf-expansion))
  )

(define-mumble-import let)
(define-mumble-import let*)

(define-mumble-macro mumble::letrec (bindings &rest body)
  `(let ,(mapcar #'car bindings)
     ,@(mapcar #'(lambda (b) (cons 'setq b)) bindings)
     (locally ,@body)))

(define-mumble-import flet)
(define-mumble-import labels)

(define-mumble-macro mumble::dynamic-let (bindings &rest body)
  `(let ,bindings
     (declare (special ,@(mapcar #'car bindings)))
     ,@body))

(define-mumble-macro mumble::dynamic (name)
  `(locally (declare (special ,name)) ,name))

(define-setf-method mumble::dynamic (name)
  (let ((store  (gensym)))
    (values nil
	    nil
	    (list store)
	    `(locally (declare (special ,name)) (setf ,name ,store))
	    `(locally (declare (special ,name)) ,name))))


(define-mumble-macro mumble::begin (&rest body)
  `(progn ,@body))

(define-mumble-import block)
(define-mumble-import return-from)

(define-mumble-import do)
(define-mumble-import dolist)
(define-mumble-import dotimes)

(define-mumble-import values)
(define-mumble-import multiple-value-bind)

(define-mumble-macro mumble::let/cc (variable &rest body)
  (let ((tagvar  (gensym)))
    `(let* ((,tagvar   (gensym))
	    (,variable (let/cc-aux ,tagvar)))
	(catch ,tagvar (locally ,@body)))))

(defun let/cc-aux (tag)
  #'(lambda (&rest values)
      (throw tag (values-list values))))


(define-mumble-import unwind-protect)

(define-mumble-import declare)
(define-mumble-import ignore)


;;; IGNORABLE is part of ANSI CL but not implemented by Lucid yet.
;;; IGNORE in Lucid seems to behave like what ANSI CL says IGNORABLE 
;;; should do, but there doesn't seem to be any way to rename it.

#+(or lucid akcl lispworks wcl)
(progn
  (proclaim '(declaration mumble::ignorable))
  (define-mumble-import mumble::ignorable))

#+(or cmu mcl allegro sbcl ccl)
(define-mumble-import cl:ignorable)

#+clisp
(define-mumble-import system::ignorable)

#-(or lucid cmu allegro akcl mcl lispworks wcl clisp sbcl ccl)
(missing-mumble-definition mumble::ignorable)


(define-mumble-import type)



;;;=====================================================================
;;; Definitions
;;;=====================================================================


;;; *** This shouldn't really do a DEFPARAMETER, since that proclaims
;;; *** the variable SPECIAL and makes any LETs of the variable do
;;; *** special binding rather than lexical binding.  But if you just
;;; *** SETF the variable, you'll get a compiler warning about an
;;; *** undeclared free variable on every reference!!!  Argggh.

(define-mumble-macro mumble::define (pattern &rest value)
  (if (consp pattern)
      `(defun ,(car pattern) ,(mung-lambda-list (cdr pattern)) ,@value)
      ;; Arggh!!  Allegro has a broken with-compilation-unit; this is
      ;; a workaround to prevent stupid bogus compilation warnings
      ;; about undeclared variables that aren't.
      ;; Arggh!!  MCL has the same problem!
      #+(or allegro mcl)
      `(progn
	 (mumble::predefine ,pattern)
	 (defparameter ,pattern ,(car value)))
      #-(or allegro mcl)
      `(defparameter ,pattern ,(car value))
      ))

(define-mumble-macro mumble::define-integrable (pattern &rest value)
  (if (consp pattern)
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (proclaim '(inline ,(car pattern))))
	 (defun ,(car pattern) ,(mung-lambda-list (cdr pattern)) ,@value))
      `(defconstant ,pattern ,(car value))))


(define-mumble-macro mumble::define-syntax (pattern . body)
  `(defmacro ,(car pattern) ,(mung-lambda-list (cdr pattern)) ,@body))

(define-mumble-macro mumble::define-local-syntax (pattern . body)
  `(eval-when (:execute :compile-toplevel)
     (defmacro ,(car pattern) ,(mung-lambda-list (cdr pattern)) ,@body)))


(define-mumble-macro mumble::define-setf (getter setter)
  `(define-setf-method ,getter (&rest subforms)
     (define-setf-aux ',setter ',getter subforms)))

(defun define-setf-aux (setter getter subforms)
  (let ((temps    nil)
	(tempvals nil)
	(args     nil)
	(store  (gensym)))
    (dolist (x subforms)
      (if (constantp x)
	  (push x args)
	  (let ((temp  (gensym)))
	    (push temp temps)
	    (push x tempvals)
	    (push temp args))))
    (setq temps (nreverse temps))
    (setq tempvals (nreverse tempvals))
    (setq args (nreverse args))
    (values temps
	    tempvals
	    (list store)
	    `(,setter ,store ,@args)
	    `(,getter ,@args))))


;;; Declaring variables special will make the compiler not proclaim
;;; about references to them.
;;; A proclamation works to disable undefined function warnings in 
;;; most Lisps.  Harlequin seems to offer no way to shut up these warnings.
;;; In allegro, we have to work around a bug in the compiler's handling
;;; of PROCLAIM.

(define-mumble-macro mumble::predefine (pattern)
  `(eval-when (:execute :compile-toplevel)
     #+allegro (let ((excl::*compiler-environment* nil))
		 (do-predefine ',pattern))
     #-allegro (do-predefine ',pattern)
     ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-predefine (pattern)
    (if (consp pattern)
        (proclaim `(ftype (function ,(mung-decl-lambda-list (cdr pattern)) t)
			  ,(car pattern)))
	(proclaim `(special ,pattern))))
  (defun mung-decl-lambda-list (lambda-list)
    (cond ((consp lambda-list)
	   (cons 't (mung-decl-lambda-list (cdr lambda-list))))
	  ((null lambda-list)
	   '())
	  (t
	   '(&rest t))))
  )


;;; CMUCL doesn't complain about function redefinitions, but Lucid does.

#+(or cmu akcl mcl lispworks wcl clisp ccl sbcl)
(define-mumble-macro mumble::redefine (pattern . value)
  `(mumble::define ,pattern ,@value))

#+lucid
(define-mumble-macro mumble::redefine (pattern . value)
  `(let ((lcl:*redefinition-action*  nil))
     (mumble::define ,pattern ,@value)))

#+allegro
(define-mumble-macro mumble::redefine (pattern . value)
  `(let ((excl:*redefinition-warnings*  nil))
     (mumble::define ,pattern ,@value)))

#-(or cmu lucid allegro akcl mcl lispworks wcl clisp ccl sbcl)
(missing-mumble-definition mumble::redefine)


#+(or cmu akcl mcl lispworks wcl clisp ccl sbcl)
(define-mumble-macro mumble::redefine-syntax (pattern . body)
  `(mumble::define-syntax ,pattern ,@body))

#+lucid
(define-mumble-macro mumble::redefine-syntax (pattern . body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((lcl:*redefinition-action*  nil))
       (mumble::define-syntax ,pattern ,@body))))

#+allegro
(define-mumble-macro mumble::redefine-syntax (pattern . body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((excl:*redefinition-warnings*  nil))
       (mumble::define-syntax ,pattern ,@body))))
  
#-(or cmu lucid allegro akcl mcl lispworks wcl clisp ccl sbcl)
(missing-mumble-definition mumble::redefine-syntax)



;;;=====================================================================
;;; Equivalence
;;;=====================================================================

(define-mumble-function-inline mumble::eq? (x y)
  (eq x y))
(define-mumble-function-inline mumble::eqv? (x y)
  (eql x y))

(define-mumble-function mumble::equal? (x1 x2)
  (cond ((eql x1 x2)
	 t)
	((consp x1)
	 (and (consp x2)
	      (mumble::equal? (car x1) (car x2))
	      (mumble::equal? (cdr x1) (cdr x2))))
	((simple-string-p x1)
	 (and (simple-string-p x2)
	      (string= x1 x2)))
	((simple-vector-p x1)
	 (and (simple-vector-p x2)
	      (eql (length (the simple-vector x1))
		   (length (the simple-vector x2)))
	      (every #'mumble::equal?
		     (the simple-vector x1)
		     (the simple-vector x2))))
	(t nil)))


;;;=====================================================================
;;; Lists
;;;=====================================================================

(define-mumble-function-inline mumble::pair? (x)
  (consp x))

(define-mumble-import cons)


;;; Can't import this directly because of type problems.

(define-mumble-synonym mumble::list list)

(define-mumble-function-inline mumble::make-list (length &optional (init nil))
  (the list
       (make-list length :initial-element init)))

(define-mumble-import car)
(define-mumble-import cdr)
(define-mumble-import caar)
(define-mumble-import cadr)
(define-mumble-import cadr)
(define-mumble-import cddr)
(define-mumble-import caaar)
(define-mumble-import caadr)
(define-mumble-import caadr)
(define-mumble-import caddr)
(define-mumble-import cdaar)
(define-mumble-import cdadr)
(define-mumble-import cdadr)
(define-mumble-import cdddr)
(define-mumble-import caaaar)
(define-mumble-import caaadr)
(define-mumble-import caaadr)
(define-mumble-import caaddr)
(define-mumble-import cadaar)
(define-mumble-import cadadr)
(define-mumble-import cadadr)
(define-mumble-import cadddr)
(define-mumble-import cdaaar)
(define-mumble-import cdaadr)
(define-mumble-import cdaadr)
(define-mumble-import cdaddr)
(define-mumble-import cddaar)
(define-mumble-import cddadr)
(define-mumble-import cddadr)
(define-mumble-import cddddr)

(define-mumble-function-inline mumble::null? (x)
  (null x))

(define-mumble-function mumble::list? (x)
  (cond ((null x) t)
	((consp x) (mumble::list? (cdr x)))
	(t nil)))

(define-mumble-function-inline mumble::length (x)
  (the fixnum (length (the list x))))

(define-mumble-import append)
(define-mumble-import nconc)

(define-mumble-function-inline mumble::reverse (x)
  (the list (reverse (the list x))))
(define-mumble-function-inline mumble::nreverse (x)
  (the list (nreverse (the list x))))

(define-mumble-function-inline mumble::list-tail (list n)
  (nthcdr n list))
(define-mumble-function-inline mumble::list-ref (list n)
  (nth n list))

(define-mumble-import last)
(define-mumble-import butlast)

(define-setf-method mumble::list-ref (list n)
  (get-setf-method `(nth ,n ,list)))

(define-mumble-function-inline mumble::memq (object list)
  (member object list :test #'eq))
(define-mumble-function-inline mumble::memv (object list)
  (member object list))
(define-mumble-function-inline mumble::member (object list)
  (member object list :test #'mumble::equal?))

;;; *** The Lucid compiler is not doing anything inline for assq so
;;; *** I'm rewriting this  -- jcp
(define-mumble-function mumble::assq (object list)
  (if (null list)
      nil
      (if (eq object (caar list))
          (car list)
	  (mumble::assq object (cdr list)))))
	
(define-mumble-function-inline mumble::assv (object list)
  (assoc object list))
(define-mumble-function-inline mumble::assoc (object list)
  (assoc object list :test #'mumble::equal?))

(define-mumble-import push)
(define-mumble-import pop)

(define-mumble-synonym mumble::list-copy copy-list)


;;;=====================================================================
;;; Symbols
;;;=====================================================================

(define-mumble-function-inline mumble::symbol? (x)
  (symbolp x))
(define-mumble-synonym mumble::symbol->string symbol-name)

(define-mumble-function-inline mumble::string->symbol (x)
  (intern x))

(define-mumble-synonym mumble::string->gensym make-symbol)


;;; We want a gensym that follows the new ANSI CL gensym-name-stickiness
;;; decision.

#+(or lucid akcl wcl clisp)
(define-mumble-function mumble::gensym (&optional (prefix "G"))
  (gensym prefix))

#+(or cmu allegro mcl lispworks ccl sbcl)
(define-mumble-import gensym)

#-(or lucid akcl wcl cmu allegro mcl lispworks clisp ccl sbcl)
(missing-mumble-definition mumble::gensym)

(define-mumble-function mumble::gensym? (x)
  (and (symbolp x)
       (not (symbol-package x))))

(defun symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))
(define-mumble-import symbol-append)


;;;=====================================================================
;;; Characters
;;;=====================================================================

(define-mumble-function-inline mumble::char? (x)
  (characterp x))

(define-mumble-synonym mumble::char=? char=)
(define-mumble-synonym mumble::char<? char<)
(define-mumble-synonym mumble::char>? char>)
(define-mumble-synonym mumble::char>=? char>=)
(define-mumble-synonym mumble::char<=? char<=)

(define-mumble-synonym mumble::char-ci=? char-equal)
(define-mumble-synonym mumble::char-ci<? char-lessp)
(define-mumble-synonym mumble::char-ci>? char-greaterp)
(define-mumble-synonym mumble::char-ci>=? char-not-lessp)
(define-mumble-synonym mumble::char-ci<=? char-not-greaterp)

(define-mumble-synonym mumble::char-alphabetic? alpha-char-p)
(define-mumble-synonym mumble::char-numeric? digit-char-p)

(define-mumble-function mumble::char-whitespace? (c)
  (member c '(#\space #\tab #\newline #\linefeed #\page #\return)))

(define-mumble-synonym mumble::char-upper-case? upper-case-p)
(define-mumble-synonym mumble::char-lower-case? lower-case-p)


;;; We don't want to use char-code and code-char directly for 
;;; character/integer conversion because we need to force the ASCII
;;; character encoding, which may not be supported by the underlying
;;; Lisp.  Here are the ASCII encodings for the 96 standard Common Lisp
;;; characters:

(defparameter *ascii-code-alist*
  '((32 . #\Space) (33 . #\!) (34 . #\") (35 . #\#)
    (36 . #\$) (37 . #\%) (38 . #\&) (39 . #\')
    (40 . #\() (41 . #\)) (42 . #\*) (43 . #\+)
    (44 . #\,) (45 . #\-) (46 . #\.) (47 . #\/)
    (48 . #\0) (49 . #\1) (50 . #\2) (51 . #\3)
    (52 . #\4) (53 . #\5) (54 . #\6) (55 . #\7)
    (56 . #\8) (57 . #\9) (58 . #\:) (59 . #\;)
    (60 . #\<) (61 . #\=) (62 . #\>) (63 . #\?)
    (64 . #\@) (65 . #\A) (66 . #\B) (67 . #\C)
    (68 . #\D) (69 . #\E) (70 . #\F) (71 . #\G)
    (72 . #\H) (73 . #\I) (74 . #\J) (75 . #\K)
    (76 . #\L) (77 . #\M) (78 . #\N) (79 . #\O) 
    (80 . #\P) (81 . #\Q) (82 . #\R) (83 . #\S)
    (84 . #\T) (85 . #\U) (86 . #\V) (87 . #\W)
    (88 . #\X) (89 . #\Y) (90 . #\Z) (91 . #\[)
    (92 . #\\) (93 . #\]) (94 . #\^) (95 . #\_)
    (96 . #\`) (97 . #\a) (98 . #\b) (99 . #\c)
    (100 . #\d) (101 . #\e) (102 . #\f) (103 . #\g)
    (104 . #\h) (105 . #\i) (106 . #\j) (107 . #\k)
    (108 . #\l) (109 . #\m) (110 . #\n) (111 . #\o)
    (112 . #\p) (113 . #\q) (114 . #\r) (115 . #\s)
    (116 . #\t) (117 . #\u) (118 . #\v) (119 . #\w)
    (120 . #\x) (121 . #\y) (122 . #\z) (123 . #\{)
    (124 . #\|) (125 . #\}) (126 . #\~) (10 . #\Newline)
    ))


;;; Now initialize the tables used by the conversion functions.

(defparameter *int->char-table* (make-array 256 :initial-element nil))
(defparameter *char->int-table* (make-hash-table))

(dolist (e *ascii-code-alist*)
  (let ((i  (car e))
	(c  (cdr e)))
    (setf (svref *int->char-table* i) c)
    (setf (gethash c *char->int-table*) i)))
(dotimes (i 256)
  (let ((c  (svref *int->char-table* i)))
    (when (not c)
      ;; This code isn't defined as one of the standard characters.
      (cond ((not (setf c (code-char i)))
	     ;; It isn't defined as a non-standard character in this
	     ;; implementation either.  Complain!
	     (warn "No character for code ~s found." i))
	    ((gethash c *char->int-table*)
	     ;; The character is defined, but collides with one of the
	     ;; standard characters.  Map this new code onto this character,
	     ;; but leave the character to map onto its original code.
	     (warn "Duplicate character code ~s for ~s." i c)
	     (setf (svref *int->char-table* i) c))
	    (t
	     ;; Let's assume that the character this code maps to actually
	     ;; represents its ASCII equivalent.
	     (setf (svref *int->char-table* i) c)
	     (setf (gethash c *char->int-table*) i))
	    ))))


(define-mumble-function-inline mumble::char->integer (c)
  (gethash c *char->int-table*))
(define-mumble-function-inline mumble::integer->char (i)
  (svref *int->char-table* i))

(define-mumble-import char-upcase)
(define-mumble-import char-downcase)
(define-mumble-import char-name)

(define-mumble-synonym mumble::char->digit digit-char-p)


;;;=====================================================================
;;; Strings
;;;=====================================================================

(define-mumble-function-inline mumble::string? (x)
  (simple-string-p x))

(define-mumble-function-inline mumble::make-string
      (length &optional (init nil init-p))
  (the simple-string
       (if init-p
	   (make-string length :initial-element init)
	   (make-string length))))

(define-mumble-function-inline mumble::string (char &rest more-chars)
  (the simple-string (coerce (cons char more-chars) 'string)))

(define-mumble-function-inline mumble::string-length (string)
  (the fixnum (length (the simple-string string))))

(define-mumble-function-inline mumble::string-ref (x n)
  (the character (schar (the simple-string x) (the fixnum n))))

(define-setf-method mumble::string-ref (string n)
  (get-setf-method `(schar ,string ,n)))

(define-mumble-synonym mumble::string=? string=)
(define-mumble-synonym mumble::string<? string<)
(define-mumble-synonym mumble::string>? string>)
(define-mumble-synonym mumble::string<=? string<=)
(define-mumble-synonym mumble::string>=? string>=)

(define-mumble-synonym mumble::string-ci=? string-equal)
(define-mumble-synonym mumble::string-ci<? string-lessp)
(define-mumble-synonym mumble::string-ci>? string-greaterp)
(define-mumble-synonym mumble::string-ci<=? string-not-greaterp)
(define-mumble-synonym mumble::string-ci>=? string-not-lessp)

(define-mumble-function-inline mumble::substring (string start end)
  (the simple-string (subseq (the simple-string string) start end)))

(define-mumble-function-inline mumble::string-append
      (string &rest more-strings)
  (declare (type simple-string string))
  (the simple-string (apply #'concatenate 'string string more-strings)))

(define-mumble-function-inline mumble::string->list (string)
  (the list (coerce (the simple-string string) 'list)))

(define-mumble-function-inline mumble::list->string (list)
  (the simple-string (coerce (the list list) 'string)))

(define-mumble-function-inline mumble::string-copy (string)
  (the simple-string (copy-seq (the simple-string string))))

(define-mumble-import string-upcase)
(define-mumble-import string-downcase)


;;;=====================================================================
;;; Vectors
;;;=====================================================================

(define-mumble-function-inline mumble::vector? (x)
  (simple-vector-p x))

(define-mumble-function-inline mumble::make-vector
      (length &optional (init nil init-p))
  (declare (type fixnum length))
  (the simple-vector
       (if init-p
	   (make-array length :initial-element init)
	   (make-array length))))


;;; Can't import directly because types are incompatible.

(define-mumble-synonym mumble::vector vector)

(define-mumble-function-inline mumble::vector-length (vector)
  (the fixnum (length (the simple-vector vector))))

(define-mumble-function-inline mumble::vector-ref (x n)
  (svref (the simple-vector x) (the fixnum n)))

(define-setf-method mumble::vector-ref (vector n)
  (get-setf-method `(svref ,vector ,n)))

(define-mumble-function-inline mumble::vector->list (vector)
  (the list (coerce (the simple-vector vector) 'list)))

(define-mumble-function-inline mumble::list->vector (list)
  (the simple-vector (coerce (the list list) 'simple-vector)))

(define-mumble-function-inline mumble::vector-copy (vector)
  (the simple-vector (copy-seq (the simple-vector vector))))


;;;=====================================================================
;;; Numbers
;;;=====================================================================

(define-mumble-synonym mumble::number? numberp)
(define-mumble-synonym mumble::integer? integerp)
(define-mumble-synonym mumble::rational? rationalp)
(define-mumble-synonym mumble::float? floatp)

(define-mumble-function-inline mumble::fixnum? (x)
  (typep x 'fixnum))

(define-mumble-synonym mumble::exact->inexact float)

(define-mumble-import =)
(define-mumble-import <)
(define-mumble-import >)
(define-mumble-import <=)
(define-mumble-import >=)

(define-mumble-synonym mumble::zero? zerop)
(define-mumble-function-inline mumble::positive? (x)
  (> x 0))
(define-mumble-function-inline mumble::negative? (x)
  (< x 0))

(define-mumble-import min)
(define-mumble-import max)

(define-mumble-import +)
(define-mumble-import *)
(define-mumble-import -)
(define-mumble-import /)

(define-mumble-synonym mumble::quotient floor)
(define-mumble-synonym mumble::remainder rem)
(define-mumble-synonym mumble::modulo mod)

(define-mumble-function-inline mumble::floor (x)
  (if (floatp x) (ffloor x) (floor (the rational x))))
(define-mumble-function-inline mumble::ceiling (x)
  (if (floatp x) (fceiling x) (ceiling (the rational x))))
(define-mumble-function-inline mumble::truncate (x)
  (if (floatp x) (ftruncate x) (truncate (the rational x))))
(define-mumble-function-inline mumble::round (x)
  (if (floatp x) (fround x) (round (the rational x))))

(define-mumble-synonym mumble::floor->exact floor)
(define-mumble-synonym mumble::ceiling->exact ceiling)
(define-mumble-synonym mumble::truncate->exact truncate)
(define-mumble-synonym mumble::round->exact round)

(define-mumble-import 1+)
(define-mumble-import 1-)
(define-mumble-import incf)
(define-mumble-import decf)

(define-mumble-function mumble::number->string (number &optional (radix 10))
  (let ((*print-base*  radix))
    (prin1-to-string number)))

(define-mumble-function mumble::string->number (string &optional (radix 10))
  (let ((*read-base* radix))
    (read-from-string string)))

(define-mumble-import expt)



;;;=====================================================================
;;; Tables
;;;=====================================================================

(define-mumble-synonym mumble::table? hash-table-p)

(define-mumble-function-inline mumble::make-table ()
  (make-hash-table :test #'eq))

(define-mumble-function-inline mumble::table-entry (table key)
  (gethash key table))

(define-setf-method mumble::table-entry (table key)
  (get-setf-method `(gethash ,key ,table)))

(define-mumble-synonym mumble::table-for-each maphash)

(define-mumble-function mumble::copy-table (old-table)
  (let ((new-table  (make-hash-table :test #'eq
				     :size (1+ (hash-table-count old-table)))))
    (maphash #'(lambda (key val) (setf (gethash key new-table) val))
	     old-table)
    new-table))


;;;=====================================================================
;;; I/O
;;;=====================================================================

#+mcl
(defparameter *mac-file-creator* :ccl2)

(define-mumble-function-inline mumble::call-with-input-file (string proc)
  (with-open-file (stream (expand-filename string) :direction :input)
    (funcall (the function proc) stream)))

#-mcl
(define-mumble-function-inline mumble::call-with-output-file (string proc)
  (with-open-file (stream (expand-filename string)
			  :direction :output :if-exists :new-version)
    (funcall (the function proc) stream)))
#+mcl
(define-mumble-function-inline mumble::call-with-output-file (string proc)
  (with-open-file (stream (expand-filename string)
			  :direction :output :if-exists :new-version
			  :mac-file-creator *mac-file-creator*)
    (funcall (the function proc) stream)))

(define-mumble-function-inline mumble::call-with-append-file (string proc)
  (with-open-file (stream (expand-filename string)
			  :direction :output :if-exists :append)
    (funcall (the function proc) stream)))

(define-mumble-function-inline mumble::call-with-input-string (string proc)
  (with-input-from-string (stream string)
     (funcall (the function proc) stream)))

(define-mumble-function-inline mumble::call-with-output-string (proc)
  (with-output-to-string (stream)
    (funcall (the function proc) stream)))

(define-mumble-synonym mumble::input-port? input-stream-p)
(define-mumble-synonym mumble::output-port? output-stream-p)

(define-mumble-function-inline mumble::current-input-port ()
  *standard-input*)
(define-mumble-function-inline mumble::current-output-port ()
  *standard-output*)
(define-mumble-function-inline mumble::current-error-port ()
  *error-output*)

(define-mumble-function-inline mumble::open-input-file (string)
  (open (expand-filename string) :direction :input))

#-mcl
(define-mumble-function mumble::open-output-file (string)
  (open (expand-filename string) :direction :output :if-exists :new-version))
#+mcl
(define-mumble-function mumble::open-output-file (string)
  (open (expand-filename string) :direction :output :if-exists :new-version
	:mac-file-creator *mac-file-creator*))

(define-mumble-function mumble::open-append-file (string)
  (open (expand-filename string) :direction :output :if-exists :append))


(define-mumble-synonym mumble::close-input-port close)
(define-mumble-synonym mumble::close-output-port close)

(define-mumble-function-inline mumble::delete-file (string)
  (delete-file (expand-filename string)))


(defvar *eof-object* (make-symbol "EOF"))

(define-mumble-function-inline mumble::read
      (&optional (port *standard-input*))
  (read port nil *eof-object*))

(define-mumble-function-inline mumble::read-char
      (&optional (port *standard-input*))
  (read-char port nil *eof-object*))

(define-mumble-function-inline mumble::peek-char
      (&optional (port *standard-input*))
  (peek-char nil port nil *eof-object*))


;;; CMU CL has some sort of serious bug in their implementation of
;;; read-line, so emulate it using read-char.
#+cmu
(define-mumble-function mumble::read-line (&optional (port *standard-input*))
  (let ((result  '()))
    (do ((ch  (read-char port nil nil) (read-char port nil nil)))
	(nil)
	(cond ((null ch)
	       ;; EOF
	       (if (null result)
		   (return (values *eof-object* t))
		   (return (values (coerce (nreverse result) 'string) t))))
	      ((eql ch #\newline)
	       (return (values (coerce (nreverse result) 'string) nil)))
	      (t
	       (push ch result))))))

#-cmu
(define-mumble-function-inline mumble::read-line
      (&optional (port *standard-input*))
  (read-line port nil *eof-object*))

(define-mumble-function-inline mumble::eof-object? (x)
  (eq x *eof-object*))

(define-mumble-import listen)



;;;=====================================================================
;;; Printer
;;;=====================================================================

(define-mumble-function mumble::internal-write (object port)
  (write object :stream port))
(define-mumble-function-inline mumble::internal-output-width (port)
  (declare (ignore port))
  nil)
(define-mumble-function-inline mumble::internal-output-position (port)
  (declare (ignore port))
  nil)
(define-mumble-synonym mumble::internal-write-char write-char)
(define-mumble-function-inline mumble::internal-write-string
                               (string port start end)
  (write-string string port :start start :end end))
(define-mumble-synonym mumble::internal-newline terpri)
(define-mumble-synonym mumble::internal-fresh-line fresh-line)
(define-mumble-synonym mumble::internal-finish-output finish-output)
(define-mumble-synonym mumble::internal-force-output force-output)
(define-mumble-synonym mumble::internal-clear-output clear-output)

(define-mumble-function mumble::internal-write-to-string (object)
  (write-to-string object))


(define-mumble-function-inline mumble::internal-warning (string)
  (warn "~a" string))

(define-mumble-function-inline mumble::internal-error (string)
  (error "~a" string))


;;; Printer stuff used directly by the pretty printer

(define-mumble-import *print-escape*)
(define-mumble-import *print-circle*)
(define-mumble-import *print-pretty*)
(define-mumble-import *print-level*)
(define-mumble-import *print-length*)
(define-mumble-import *print-base*)
(define-mumble-import *print-radix*)


;;; These functions and variables are all defined with the XP stuff.  But,
;;; let's export all the symbols from the mumble package.

(define-mumble-import mumble::write)
(define-mumble-import mumble::print)
(define-mumble-import mumble::prin1)
(define-mumble-import mumble::princ)
(define-mumble-import mumble::pprint)
(define-mumble-import mumble::prin1-to-string)
(define-mumble-import mumble::princ-to-string)
(define-mumble-import mumble::write-char)
(define-mumble-import mumble::write-string)
(define-mumble-import mumble::write-line)
(define-mumble-import mumble::terpri)
(define-mumble-import mumble::fresh-line)
(define-mumble-import mumble::finish-output)
(define-mumble-import mumble::force-output)
(define-mumble-import mumble::clear-output)
(define-mumble-import mumble::display)
(define-mumble-import mumble::newline)
(define-mumble-import mumble::*print-shared*)
(define-mumble-import mumble::*print-dispatch*)
(define-mumble-import mumble::*print-right-margin*)
(define-mumble-import mumble::*print-miser-width*)
(define-mumble-import mumble::*print-lines*)
(define-mumble-import mumble::*default-right-margin*)
(define-mumble-import mumble::*last-abbreviated-printing*)
(define-mumble-import mumble::*print-structure*)
(define-mumble-import mumble::*print-structure-slots*)
(define-mumble-import mumble::standard-print-dispatch)
(define-mumble-import mumble::pprint-newline)
(define-mumble-import mumble::pprint-logical-block)
(define-mumble-import mumble::pprint-pop)
(define-mumble-import mumble::pprint-exit-if-list-exhausted)
(define-mumble-import mumble::pprint-indent)
(define-mumble-import mumble::pprint-tab)
(define-mumble-import mumble::pprint-fill)
(define-mumble-import mumble::pprint-linear)
(define-mumble-import mumble::pprint-tabular)
(define-mumble-import mumble::format)
(define-mumble-import mumble::warning)
(define-mumble-import mumble::error)


;;; These are keywords for pprint-newline.

(define-mumble-import mumble::linear)
(define-mumble-import mumble::fill)
(define-mumble-import mumble::miser)
(define-mumble-import mumble::mandatory)

;;; These are keywords for pprint-indent

;; (define-mumble-import mumble::block)  ; already imported as special form
(define-mumble-import mumble::current)

;;; These are keywords for pprint-tab

(define-mumble-import mumble::line)
(define-mumble-import mumble::section)
(define-mumble-import mumble::line-relative)
(define-mumble-import mumble::section-relative)


;;;=====================================================================
;;; System Interface
;;;=====================================================================

(define-mumble-import macroexpand-1)
(define-mumble-import macroexpand)


;;; This is a real kludge.  Some lisps have renamed special-form-p
;;; as per ANSI, others have not.  Rather than trying to guess which
;;; Lisp is which, just test to see if it's there.

(defmacro maybe-special-form-p (x)
  (if (fboundp 'special-form-p)
      `(special-form-p ,x)
      `(special-operator-p ,x)))

(define-mumble-function mumble::syntax? (x)
  (or (macro-function x)
      (maybe-special-form-p x)))

(define-mumble-synonym mumble::bound? boundp)
(define-mumble-synonym mumble::fbound? fboundp)


;;; WITH-COMPILATION-UNIT is an ANSI CL feature that isn't yet
;;; supported by all Lisps.

#+lucid
(define-mumble-macro mumble::with-compilation-unit (options &body body)
  (declare (ignore options))
  `(lcl:with-deferred-warnings ,@body))

#+(or cmu mcl allegro lispworks ccl sbcl)
(define-mumble-import with-compilation-unit)

#+(or akcl wcl clisp)
(define-mumble-macro mumble::with-compilation-unit (options &body body)
  (declare (ignore options))
  `(progn ,@body))

#-(or lucid allegro cmu akcl mcl lispworks wcl clisp ccl sbcl)
(missing-mumble-definition mumble::with-compilation-unit)


(define-mumble-function mumble::eval (form &optional compile-p)
  (if compile-p
      #+cmu
      (handler-bind
        ((style-warning #'(lambda (c) (muffle-warning c))))
        (mumble::with-compilation-unit ()
          (eval-compiling-functions form)))
      #-cmu
      (mumble::with-compilation-unit ()
        (eval-compiling-functions form))
      (eval form)))


;;; Simply doing (funcall (compile nil `(lambda () ,form))) would work
;;; except that top-level-ness actions would be lost (causing extraneous
;;; warning messages about global variables whose references are compiled
;;; before a previous predefine is executed, etc).  So make an attempt
;;; to process nested top-level forms in order.  This doesn't look for
;;; all of the common-lispy things that might show up in macro expansions,
;;; but it's close enough.

(defun eval-compiling-functions (form)
  (if (atom form)
      (eval form)
      (let ((fn  (car form)))
	(cond ((or (eq fn 'mumble::begin)
		   (eq fn 'progn))
	       (do ((forms (cdr form) (cdr forms)))
		   ((null (cdr forms)) (eval-compiling-functions (car forms)))
		   (eval-compiling-functions (car forms))))
	      ((eq fn 'mumble::define)
	       (if (consp (cadr form))
		   (compile-define form)
		   (compile-other form)))
	      ((eq fn 'mumble::define-integrable)
	       (if (consp (cadr form))
		   (progn
		     (proclaim `(inline ,(car (cadr form))))
		     (compile-define form))
		   (compile-other form)))
	      ((eq fn 'mumble::predefine)
	       (do-predefine (cadr form)))
	      ((macro-function fn)
	       (eval-compiling-functions (macroexpand-1 form)))
	      (t
	       (compile-other form))))))

(defun compile-define (form)
  (let ((name  (car (cadr form)))
	(args  (mung-lambda-list (cdr (cadr form))))
	(body  (cddr form)))
    (compile name `(lambda ,args ,@body))
    name))

(defun compile-other (form)
  (funcall (compile nil `(lambda () ,form))))


;;; Load and compile-file aren't directly imported from the host
;;; Common Lisp because we want to do our own defaulting of file
;;; name extensions.

(define-mumble-function mumble::load (filename)
  (setq filename (expand-filename filename))
  (if (string= (mumble::filename-type filename) "")
      (let ((source-file  (build-source-filename filename))
	    (binary-file  (build-binary-filename filename)))
	(if (and (probe-file binary-file)
		 (> (file-write-date binary-file)
		    (file-write-date source-file)))
	    (load binary-file)
	    (load source-file)))
      (load filename)))


;;; This is used to control OPTIMIZE declarations in a somewhat more
;;; portable way -- different implementations may need slightly different
;;; combinations.
;;; 0 = do as little as possible when compiling code
;;; 1 = use "default" compiler settings
;;; 2 = omit safety checks and do "easy" speed optimizations.
;;; 3 = do as much as possible; type inference, inlining, etc.  May be slow.
;;; #f = don't mess with optimize settings.

;;; *** The CMU block compilation stuff is commented out for now because
;;; *** we've been having problems with (possibly unrelated) compiler bugs.
;;; *** Should get back to experimenting with this again someday.

(defvar *code-quality* nil)
(define-mumble-import *code-quality*)

(defun code-quality-hack (q)
  (cond ((eql q 0)
;	 #+cmu (setf ext:*block-compile-default* nil)
	 (proclaim '(optimize (speed 1) (safety 3) (compilation-speed 3)
			      #+cmu (ext:debug 1)  
                              #+(or mcl allegro lispworks) (debug 1)
			      )))
	((eql q 1)
;	 #+cmu (setf ext:*block-compile-default* :specified)
	 (proclaim '(optimize (speed 1) (safety 1) (compilation-speed 3)
			      #+cmu (ext:debug 1)
                              #+(or mcl allegro lispworks) (debug 1)
			      )))
	((eql q 2)
;	 #+cmu (setf ext:*block-compile-default* :specified)
         #+cmu (proclaim '(optimize (speed 2) (safety 0) (compilation-speed 1)
				    (ext:debug 1)))
	 #-cmu (proclaim '(optimize (speed 3) (safety 0) (compilation-speed 3)
                                    #+(or mcl allegro lispworks) (debug 0)))
	 )
	((eql q 3)
;	 #+cmu (setf ext:*block-compile-default* t)
	 (proclaim '(optimize (speed 3) (safety 0) (compilation-speed 0)
			      #+cmu (ext:debug 0)
                              #+(or mcl allegro lispworks) (debug 0)
			      )))
	(t
	 (warn "Bogus *code-quality* setting ~s." q))))


;;; If we don't do this, code generated with high code-quality settings
;;; can't be interrupted with ^C.

#+allegro
(setf compiler:generate-interrupt-checks-switch
      #'(lambda (safety space speed debug)
	  (declare (ignore safety space speed debug))
	  t))


;;; This is a bug fix for CMU CL, which brokenly tries to do aggressive
;;; inlining in the interpreter!!!
;;; This bug has been fixed as of version 17e -- sjl
;#+cmu17
;(progn
;  (setf (symbol-function 'broken-maybe-expand-local-inline)
;	(symbol-function 'c::maybe-expand-local-inline))
;  (defun c::maybe-expand-local-inline (fun ref call)
;    (if c::*converting-for-interpreter*
;	fun
;	(broken-maybe-expand-local-inline fun ref call)))
;  )



;;; Note that we expect the binary filename (if supplied) to be
;;; relative to the current directory, not to the source filename.
;;; Lucid and AKCL (and maybe other implementations) merge the :output-file
;;; pathname with the source filename, but the merge by expand-filename
;;; should prevent it from doing anything.

(define-mumble-function mumble::compile-file (filename &optional binary)
  (if *code-quality* (code-quality-hack *code-quality*))
  (setq filename (expand-filename filename))
  (if (string= (mumble::filename-type filename) "")
      (setq filename (build-source-filename filename)))
  (let ((result
	  #+cmu
	  ;; shut up CMU Lisp's overly-verbose messages.
	  (handler-bind
	    ((style-warning #'(lambda (c) (muffle-warning c))))
            (if binary
	        (compile-file filename :output-file (expand-filename binary))
	        (compile-file filename)))
	  #-cmu
          (if binary
	      (compile-file filename :output-file (expand-filename binary))
	      (compile-file filename))
	  ))
    #+mcl (when result (ccl:set-mac-file-creator result *mac-file-creator*))
    result))


;;; We do a lot of calls to inlinable functions, and CMU's internal limit
;;; needs to be raised to something more reasonable.

#+cmu (setf ext:*inline-expansion-limit* 200)


;;; See cl-init.lisp for initialization of *lisp-binary-file-type*.

(defparameter source-file-type ".scm")
(defparameter binary-file-type *lisp-binary-file-type*)
(define-mumble-import source-file-type)
(define-mumble-import binary-file-type)


(defun build-source-filename (filename)
  (mumble::assemble-filename filename filename source-file-type))

(defun build-binary-filename (filename)
  (mumble::assemble-filename filename filename binary-file-type))

(proclaim '(ftype (function (simple-string) simple-string)
		  mumble::filename-place
		  mumble::filename-name
		  mumble::filename-type
		  expand-filename))

(proclaim '(ftype (function (simple-string simple-string simple-string)
			    simple-string)
		  mumble::assemble-filename))

(define-mumble-function mumble::assemble-filename (place name type)
  (concatenate 'string
	       (mumble::filename-place place)
	       (mumble::filename-name name)
	       (mumble::filename-type type)))

(define-mumble-function mumble::filename-place (filename)
  (declare (simple-string filename))
  (let ((slash  (position #\/ filename :from-end t)))
    (if slash
	(subseq filename 0 (1+ slash))
	"")))

(define-mumble-function mumble::filename-name (filename)
  (declare (simple-string filename))
  (let* ((slash  (position #\/ filename :from-end t))
	 (beg    (if slash (1+ slash) 0))
	 (dot    (position #\. filename :start beg)))
    (if (or slash dot)
	(subseq filename beg (or dot (length filename)))
	filename)))

(define-mumble-function mumble::filename-type (filename)
  (declare (simple-string filename))
  (let* ((slash  (position #\/ filename :from-end t))
	 (beg    (if slash (1+ slash) 0))
	 (dot    (position #\. filename :start beg)))
    (if dot
	(subseq filename dot (length filename))
	"")))


;;; This function is called by all functions that pass filenames down
;;; to the operating system.  It does environment variable substitution
;;; and merging with *default-pathname-defaults* (set by the cd function).
;;; Since this function translates mumble's notion of pathnames into
;;; a lower-level representation, this function should never need to
;;; be called outside of this file.

(define-mumble-import expand-filename)
(defun expand-filename (filename)
  (namestring
    (merge-pathnames
      (fix-filename-syntax 
        (expand-environment-vars filename)))))

(defun expand-environment-vars (filename)
  (declare (simple-string filename))
  (if (eql (schar filename 0) #\$)
      (let* ((end    (length filename))
             (slash  (or (position #\/ filename) end))
             (new    (mumble::getenv (subseq filename 1 slash))))
        (if new
            (expand-environment-vars
              (concatenate 'string new (subseq filename slash end)))
            filename))
      filename))


;;; On non-unix machines, may need to change the mumble unix-like filename
;;; syntax to whatever the normal syntax used by the implementation is.

#+mcl
(defun fix-filename-syntax (filename)
  (substitute #\: #\/ filename))

#-mcl
(defun fix-filename-syntax (filename)
  filename)


;;; AKCL's compile-file merges the output pathname against the input
;;; pathname.  If the output pathname doesn't have an explicit directory
;;; but the input pathname does, the wrong thing will happen.  This
;;; hack is so that expand-filename will always put a directory
;;; specification on both pathnames.
;;; Lucid CL does similar merging, but *default-pathname-defaults*
;;; already defaults to the truename of the current directory.

#+akcl
(setf *default-pathname-defaults* (truename "./"))


;;; WCL's *default-pathname-defaults* is OK except that it has a
;;; type of .lisp, which is inappropriate.

#+wcl
(setf *default-pathname-defaults*
      (make-pathname :directory
		     (pathname-directory *default-pathname-defaults*)))

#+(or mcl lispworks)
(setf *default-pathname-defaults*
      (truename *default-pathname-defaults*))


(define-mumble-function mumble::file-exists? (filename)
  (probe-file (expand-filename filename)))

(define-mumble-function mumble::file-write-date (filename)
  (or (file-write-date (expand-filename filename)) 0))

(define-mumble-synonym mumble::current-date get-universal-time)

(define-mumble-function mumble::get-run-time ()
  (/ (get-internal-run-time) (float internal-time-units-per-second)))


;;; Get environment variables

#+lucid
(progn
  (mumble::predefine (mumble::getenv string))
  (define-mumble-synonym mumble::getenv lcl:environment-variable))

#+cmu
(define-mumble-function mumble::getenv (string)
  (let ((symbol  (intern string (find-package "KEYWORD"))))
    (cdr (assoc symbol extensions:*environment-list*))))

#+(or akcl allegro lispworks clisp)
(define-mumble-function mumble::getenv (string)
  (system::getenv string))

#+wcl
(define-mumble-function mumble::getenv (string)
  (lisp:getenv string))

#+ccl
(define-mumble-function mumble::getenv (string)
  (ccl:getenv string))
;;; Hmmm.  The Mac doesn't have environment variables, so we'll have to
;;; roll our own.  *environment-alist* is defvar'ed in a startup script.


#+mcl
(progn
  (define-mumble-import *environment-alist*)
  (define-mumble-function mumble::getenv (string)
    (cdr (assoc string *environment-alist* :test #'string=))))


#+sbcl
(define-mumble-function mumble::getenv (string)
  (sb-posix:getenv string))
#-(or lucid allegro cmu akcl mcl lispworks wcl clisp ccl sbcl)
(missing-mumble-definition mumble::getenv)


;;; Change working directory.
;;; This stores a directory pathname in *default-pathname-defaults*.
;;; See also expand-filename.

(define-mumble-function mumble::cd (filename)
  (if (not (eql (schar filename (1- (length filename))) #\/))
      (setq filename (concatenate 'string filename "/")))
  (setq *default-pathname-defaults* (truename (expand-filename filename))))

(define-mumble-function mumble::pwd ()
  (namestring *default-pathname-defaults*))


;;; Backtrace

#+cmu
(define-mumble-function mumble::backtrace (&optional depth)
  (format t "Backtrace from top of stack:~%")
  (let ((top  (di:top-frame)))  ; ignore current frame
    (do ((frame (di:frame-down top) (di:frame-down frame))
	 (d 0 (1+ d)))
	((or (not frame)
	     (and depth (eql depth d) (format t "  ...~%") t))
	 nil)
	(format t "  ~a~%"
		(di:debug-function-name (di:frame-debug-function frame)))
	)))
      

#+lucid
(define-mumble-function mumble::backtrace (&optional depth)
  (format t "Backtrace from top of stack:~%")
  (let ((e  (sys:stack-frame-environment)))
    (do ((i (sys:top-frame e) (sys:previous-frame i e))
	 (d 0 (1+ d)))
	((or (not i)
	     (and depth (eql depth d) (format t "  ...~%") t))
	 nil)
	(let ((name  (sys:stack-frame-name i e)))
	  (when name (format t "  ~a~%" name)))
	)))

#+allegro
(define-mumble-function mumble::backtrace (&optional depth)
  (format t "Backtrace from top of stack:~%")
  (tpl::zoom-print-stack depth nil))

#+mcl
(define-mumble-function mumble::backtrace (&optional depth)
  (format t "Backtrace from top of stack:~%")
  (do* ((p  (ccl::%get-frame-ptr) (ccl::parent-frame p))
        (d  0 (1+ d))
        (q  (ccl::last-frame-ptr)))
       ((or (null p) (< p q) (and depth (eql d depth))) nil)
    (declare (type fixnum d))
    (format t "  ~a~%" (ccl::cfp-lfun p))))

#-(or cmu lucid allegro mcl ccl sbcl)
(define-mumble-function mumble::backtrace (&optional depth)
  (declare (ignore depth))
  nil)


;;; Catch errors.

(define-mumble-macro mumble::with-error-handler (f &body body)
  (let ((c  (gensym))
	(hb #+lucid     'lcl:handler-bind
	    #+cmu       'lisp:handler-bind
	    #+(or mcl ccl sbcl)       'cl:handler-bind
	    #+allegro   'cl:handler-bind
	    #+lispworks 'lisp:handler-bind
	    #-(or lucid cmu mcl allegro lispworks ccl)
	    	(missing-mumble-definition mumble::with-error-handler)))
    `(,hb
       ((error #'(lambda (,c) (funcall ,f (princ-to-string ,c)))))
       ,@body)))


;;; Leave Lisp

#+lucid
(define-mumble-synonym mumble::exit lcl:quit)

#+allegro
(define-mumble-synonym mumble::exit excl:exit)

#+cmu
(define-mumble-synonym mumble::exit  extensions:quit)

#+akcl
(define-mumble-synonym mumble::exit lisp:bye)

#+(or mcl ccl)
(define-mumble-synonym mumble::exit ccl:quit)

#+lispworks
(define-mumble-synonym mumble::exit lw:bye)

#+wcl
(define-mumble-synonym mumble::exit lisp:quit)

#+clisp
(define-mumble-import exit)

#+sbcl
(define-mumble-synonym mumble::exit sb-ext:quit)

#-(or ccl lucid allegro cmu akcl mcl lispworks wcl clisp sbcl)
(missing-mumble-definition mumble::exit)



;;;=====================================================================
;;; Reader support
;;;=====================================================================


;;; Make the default readtable recognize #f and #t.
;;; CMUCL's loader rebinds *readtable* when loading file, so can't
;;; setq it here; hack the default readtable instead.

#+(or cmu mcl ccl allegro lispworks clisp sbcl)
(defparameter *mumble-readtable* *readtable*)

#+(or lucid akcl wcl)
(progn
  (defparameter *mumble-readtable* (copy-readtable nil))
  (setq *readtable* *mumble-readtable*)
  )

#-(or lucid allegro cmu akcl mcl lispworks wcl clisp ccl sbcl)
(missing-mumble-definition *mumble-readtable*)


;;; Lucid's debugger uses the standard readtable rather than *readtable*
;;; unless you do this magic trick.

#+lucid
(sys:add-debugger-binding '*readtable* *mumble-readtable*)



(set-dispatch-macro-character #\# #\f
    #'(lambda (stream subchar arg)
	(declare (ignore stream subchar arg))
	nil))

(set-dispatch-macro-character #\# #\t
    #'(lambda (stream subchar arg)
	(declare (ignore stream subchar arg))
	t))



;;;=====================================================================
;;; Random stuff
;;;=====================================================================

(defconstant mumble::lisp-implementation-name *lisp-implementation-name*)
(define-mumble-import mumble::lisp-implementation-name)

(define-mumble-function mumble::identify-system ()
  (format nil "~a version ~a on ~a"
	  (or (lisp-implementation-type)
	      "Generic Common Lisp")
	  (or (lisp-implementation-version)
	      "Generic")
	  (or (machine-type)
	      "Generic Machine")))

(defconstant mumble::left-to-right-evaluation t)
(define-mumble-import mumble::left-to-right-evaluation)


#+excl
(define-mumble-function mumble::gc-messages (onoff)
  (setf (sys:gsgc-switch :print) onoff))
#+cmu
(define-mumble-function mumble::gc-messages (onoff)
  (setf extensions:*gc-verbose* onoff))
#+(or lispworks akcl wcl mcl clisp ccl sbcl)
(define-mumble-function mumble::gc-messages (onoff)
  onoff)   ; can't figure out if they have a hook or not
#+lucid
(define-mumble-function mumble::gc-messages (onoff)
  (setf lcl:*gc-silence* (not onoff))
  onoff)


#-(or lucid cmu allegro akcl mcl lispworks wcl clisp ccl sbcl)
(missing-mumble-definition mumble::gc-messages)


(define-mumble-import identity)
