;;; runtime-utils.scm -- basic runtime support
;;;
;;; author :  Sandra Loosemore
;;; date   :  9 Jun 1992
;;;
;;; This file contains definitions (beyond the normal mumble stuff)
;;; that is referenced directly in code built by the code generator.
;;; See backend/codegen.scm.
;;;

;;; (delay form . maybe-name)
;;;   returns a delay object with unevaluated "form".
;;;   if name is  supplied, the thunk is defined as a named function --
;;;   this is strictly for debugging purposes.

(define-syntax (delay form . maybe-name)
  `(cons '#f
	 ,(if (null? maybe-name)
	      `(lambda () ,form)
	      (let ((name  (car maybe-name)))
		`(flet ((,name () ,form)) (function ,name)))
	    )))


;;; (box form)
;;;   returns a delay object with evaluated "form".

(define-syntax (box form)
  (cond ((number? form)
	 `(quote ,(cons '#t form)))
	((and (pair? form) (eq? (car form) 'quote))
	 `(quote ,(cons '#t (cadr form))))
	(else
	 `(cons '#t ,form))))

(define-syntax (unbox form)
  `(cdr ,form))

(define-syntax (forced? form)
  `(car ,form))


;;; (force delay)
;;;   return the value of the delay object.

(define (force delay-object)
  (declare (type pair delay-object))
  (if (car delay-object)
      (cdr delay-object)
      (begin
        (let ((result  (funcall (cdr delay-object))))
	  (setf (car delay-object) '#t)
	  (setf (cdr delay-object) result)))))

;;; Inline version of the above.  Not good to use everywhere because
;;; of code bloat problems, but handy for helper functions.

(define-syntax (force-inline delay-object)
  (let ((temp1  (gensym))
	(temp2  (gensym)))
    `(let ((,temp1  ,delay-object))
       (declare (type pair ,temp1))
       (if (car ,temp1)
	   (cdr ,temp1)
	   (let ((,temp2  (funcall (cdr ,temp1))))
	     (setf (car ,temp1) '#t)
	     (setf (cdr ,temp1) ,temp2))))))


;;; (make-curried-fn opt-fn strictness)
;;; The basic idea is to compare the number of arguments received against
;;; the number expected.
;;; If the same, call the optimized entry point opt-fn.
;;; If more, apply the result of calling the optimized entry to the
;;;   leftover arguments.
;;; If less, make a closure that accepts the additional arguments.

(define (make-curried-fn opt-fn strictness)
  (lambda args
    (curried-fn-body '() args opt-fn strictness)))

(define (curried-fn-body previous-args args opt-fn strictness)
  (multiple-value-bind
      (saturated? actual-args leftover-args leftover-strictness)
      (process-curried-fn-args strictness args '())
    (setf actual-args (append previous-args actual-args))
    (if saturated?
	(if (null? leftover-args)
	    (apply opt-fn actual-args)
	    (apply (apply opt-fn actual-args) leftover-args))
	(lambda more-args
	  (curried-fn-body actual-args more-args opt-fn leftover-strictness)))
    ))

(define (process-curried-fn-args strictness args actual-args)
  (cond ((null? strictness)
	 ;; At least as many arguments as expected.
	 (values '#t (nreverse actual-args) args strictness))
	((null? args)
	 ;; Not enough arguments supplied.
  	 (values '#f (nreverse actual-args) args strictness))
	(else
	 ;; Process the next argument.
	 (if (car strictness)
	     (push (force-inline (car args)) actual-args)
	     (push (car args) actual-args))
 	 (process-curried-fn-args (cdr strictness) (cdr args) actual-args))
	))


;;; Special cases of the above.

(define (make-curried-fn-1-strict opt-fn)
  (lambda (arg1 . moreargs)
    (setf arg1 (force-inline arg1))
    (if (null? moreargs)
	(funcall opt-fn arg1)
	(apply (funcall opt-fn arg1) moreargs))))

(define (make-curried-fn-1-nonstrict opt-fn)
  (lambda (arg1 . moreargs)
    (if (null? moreargs)
	(funcall opt-fn arg1)
	(apply (funcall opt-fn arg1) moreargs))))


;;; Special case for curried version of cons constructor

(define (cons-constructor arg1 . more-args)
  (cond ((null? more-args)
	 (lambda still-more-args
	   (apply (function cons-constructor) arg1 still-more-args)))
	((null? (cdr more-args))
	 (cons arg1 (car more-args)))
	(else
	 (error "Too many arguments to cons constructor!"))))



;;; (make-tuple-constructor arity)
;;; Make uncurried function to build a constructor.

(define (make-tuple-constructor arity strictness)
  (declare (type fixnum arity))
  (cond ((eqv? arity 0)
	 ;; Actually, should never happen -- this is the unit constructor
	 0)
	((eqv? arity 1)
	 (if (car strictness)
	     (make-curried-fn-1-strict (function identity))
	     (make-curried-fn-1-nonstrict (function identity))))
	((eqv? arity 2)
	 (make-curried-fn (function cons) strictness))
	(else
	 (make-curried-fn (function vector) strictness))))


;;; (make-tuple . args)
;;;   first-class version of the above

(define-syntax (make-tuple . args)
  (let ((arity  (length args)))
    (cond ((eqv? arity 0)
	   ;; Actually, should never happen -- this is the unit constructor
	   0)
	  ((eqv? arity 1)
	   (car args))
	  ((eqv? arity 2)
	   `(cons ,@args))
	  (else
	   `(vector ,@args)))))


;;; (make-tagged-data-constructor n arity)
;;;   build a function that makes a data structure with tag "n" and
;;;   "arity" slots.

(define (make-tagged-data-constructor n arity strictness)
  (cond ((eqv? arity 0)
	 (vector n))
	((eqv? arity 1)
	 (if (car strictness)
	     (make-curried-fn-1-strict
	       (lambda (x) (vector n x)))
	     (make-curried-fn-1-nonstrict
	       (lambda (x) (vector n x)))))
	(else
	 (make-curried-fn
	   (lambda args
	     (apply (function vector) n args))
	   strictness))))


;;; (make-tagged-data n . args)
;;;   first-class version of the above

(define-syntax (make-tagged-data n . args)
  `(vector ,n ,@args))


;;; (tuple-select arity i object)
;;;   extract component "i" from untagged "object"

(define-syntax (tuple-select arity i object)
  (cond ((eqv? arity 1)
	 object)
	((eqv? arity 2)
	 (if (eqv? i 0)
	     `(car ,object)
	     `(cdr ,object)))
	(else
	 `(vector-ref (the vector ,object) (the fixnum ,i)))))


;;; (tagged-data-select arity i object)
;;;   extract component "i" from tagged "object"

(define-syntax (tagged-data-select arity i object)
  (declare (ignore arity))
  `(vector-ref (the vector ,object) (the fixnum ,(1+ i))))



;;; Forced equivalents of the above.

(define (force-car x)
  (force-inline (car x)))

(define (force-cdr x)
  (force-inline (cdr x)))

(define (force-vector-ref x i)
  (force-inline (vector-ref (the vector x) (the fixnum i))))

(define-syntax (force-tuple-select arity i object)
  (cond ((eqv? arity 1)
	 `(force ,object))
	((eqv? arity 2)
	 (if (eqv? i 0)
	     `(force-car ,object)
	     `(force-cdr ,object)))
	(else
	 `(force-vector-ref ,object ,i))))

(define-syntax (force-tagged-data-select arity i object)
  (declare (ignore arity))
  `(force-vector-ref ,object ,(1+ i)))


(define (car/force x)
  (car (force-inline x)))

(define (cdr/force x)
  (cdr (force-inline x)))

(define (vector-ref/force x i)
  (vector-ref (the vector (force-inline x)) i))

(define-syntax (tuple-select/force arity i object)
  (cond ((eqv? arity 1)
	 `(force ,object))
	((eqv? arity 2)
	 (if (eqv? i 0)
	     `(car/force ,object)
	     `(cdr/force ,object)))
	(else
	 `(vector-ref/force ,object ,i))))

(define-syntax (tagged-data-select/force arity i object)
  (declare (ignore arity))
  `(vector-ref/force ,object ,(1+ i)))



(define (force-car/force x)
  (force-inline (car (force-inline x))))

(define (force-cdr/force x)
  (force-inline (cdr (force-inline x))))

(define (force-vector-ref/force x i)
  (force-inline (vector-ref (the vector (force-inline x)) i)))

(define-syntax (force-tuple-select/force arity i object)
  (cond ((eqv? arity 1)
	 `(force (force ,object)))
	((eqv? arity 2)
	 (if (eqv? i 0)
	     `(force-car/force ,object)
	     `(force-cdr/force ,object)))
	(else
	 `(force-vector-ref/force ,object ,i))))

(define-syntax (force-tagged-data-select/force arity i object)
  (declare (ignore arity))
  `(force-vector-ref/force ,object ,(1+ i)))





;;; (constructor-number object)
;;;   return the tag from "object"

(define-syntax (constructor-number object)
  `(vector-ref (the vector ,object) 0))



;;; (funcall/force fn . args)
;;;   == (funcall (force fn) . args)

(define-syntax (funcall/force fn . args)
  (let* ((n    (length args))
	 (junk (assv n '((1 . funcall/force-1)
			 (2 . funcall/force-2)
			 (3 . funcall/force-3)
			 (4 . funcall/force-4)))))
    `(,(if junk (cdr junk) 'funcall/force-n) ,fn ,@args)))

(define (funcall/force-1 fn a1)
  (funcall (force-inline fn) a1))
(define (funcall/force-2 fn a1 a2)
  (funcall (force-inline fn) a1 a2))
(define (funcall/force-3 fn a1 a2 a3)
  (funcall (force-inline fn) a1 a2 a3))
(define (funcall/force-4 fn a1 a2 a3 a4)
  (funcall (force-inline fn) a1 a2 a3 a4))
(define-syntax (funcall/force-n fn . args)
  `(funcall (force ,fn) ,@args))


;;; (delay-funcall fn . args)
;;;   == (delay (funcall fn . args))

(define-syntax (delay-funcall fn . args)
  (let* ((n     (length args))
	 (junk  (assv n '((1 . delay-funcall-1)
			  (2 . delay-funcall-2)
			  (3 . delay-funcall-3)
			  (4 . delay-funcall-4)))))
    `(,(if junk (cdr junk) 'delay-funcall-n) ,fn ,@args)))

(define (delay-funcall-1 fn a1)
  (delay (funcall fn a1)))

(define (delay-funcall-2 fn a1 a2)
  (delay (funcall fn a1 a2)))

(define (delay-funcall-3 fn a1 a2 a3)
  (delay (funcall fn a1 a2 a3)))

(define (delay-funcall-4 fn a1 a2 a3 a4)
  (delay (funcall fn a1 a2 a3 a4)))

(define (delay-funcall-n fn . args)
  (delay (apply fn args)))


;;; (delay-funcall/force fn . args)
;;;   == (delay (funcall (force fn) . args))

(define-syntax (delay-funcall/force fn . args)
  (let* ((n     (length args))
	 (junk  (assv n '((1 . delay-funcall/force-1)
			  (2 . delay-funcall/force-2)
			  (3 . delay-funcall/force-3)
			  (4 . delay-funcall/force-4)))))
    `(,(if junk (cdr junk) 'delay-funcall/force-n) ,fn ,@args)))

(define (delay-funcall/force-1 fn a1)
  (delay (funcall (force-inline fn) a1)))

(define (delay-funcall/force-2 fn a1 a2)
  (delay (funcall (force-inline fn) a1 a2)))

(define (delay-funcall/force-3 fn a1 a2 a3)
  (delay (funcall (force-inline fn) a1 a2 a3)))

(define (delay-funcall/force-4 fn a1 a2 a3 a4)
  (delay (funcall (force-inline fn) a1 a2 a3 a4)))

(define (delay-funcall/force-n fn . args)
  (delay (apply (force-inline fn) args)))



;;; (make-haskell-string string)
;;;   Converts a Lisp string lazily to a boxed haskell string (makes
;;;   a delay with a magic function).  Returns an unboxed result.

(define (make-haskell-string string)
  (declare (type string string))
  (let ((index   1)
	(size    (string-length string)))
    (declare (type fixnum index size))
    (cond ((eqv? size 0)
	   '())
	  ((eqv? size 1)
	   (cons (box (char->integer (string-ref string 0)))
		 (box '())))
	  (else
	   (letrec ((next-fn
		      (lambda ()
			(let ((ch  (char->integer (string-ref string index))))
			  (incf index)
			  (cons (box ch)
				(if (eqv? index size)
				    (box '())
				    (cons '#f next-fn)))))))
	     (cons (box (char->integer (string-ref string 0)))
		   (cons '#f next-fn))))
	  )))


;;; Similar, but accepts an arbitrary tail (which must be a delay object)

(define (make-haskell-string-tail string tail-delay)
  (declare (type string string))
  (let ((index   1)
	(size    (string-length string)))
    (declare (type fixnum index size))
    (cond ((eqv? size 0)
	   (force-inline tail-delay))
	  ((eqv? size 1)
	   (cons (box (char->integer (string-ref string 0)))
		 tail-delay))
	  (else
	   (letrec ((next-fn
		      (lambda ()
			(let ((ch  (char->integer (string-ref string index))))
			  (incf index)
			  (cons (box ch)
				(if (eqv? index size)
				    tail-delay
				    (cons '#f next-fn)))))))
	     (cons (box (char->integer (string-ref string 0)))
		   (cons '#f next-fn))))
	  )))


(define (haskell-string->string s)
  (let ((length  0))
    (declare (type fixnum length))
    (do ((s s (force (cdr s))))
	((null? s))
	(setf length (+ length 1)))
    (let ((result  (make-string length)))
      (declare (type string result))
      (do ((s s (unbox (cdr s)))
	   (i 0 (+ i 1)))
	  ((null? s))
	  (declare (type fixnum i))
	  (setf (string-ref result i) (integer->char (force (car s)))))
      result)))


(define (print-haskell-string s port)
   (do ((s1 s (force (cdr s1))))
       ((null? s1))
     (write-char (integer->char (force (car s1))) port)))

;;; This explicates the value returned by a proc (the IO () type).

(define (insert-unit-value x)
  (declare (ignore x))
  0)

;;; These handle list conversions

(define (haskell-list->list fn l)
  (if (null? l)
      '()
      (cons (funcall fn (force (car l))) 
	    (haskell-list->list fn (force (cdr l))))))

(define (list->haskell-list fn l)
  (if (null? l)
      '()
      (cons (box (funcall fn (car l)))
	    (box (list->haskell-list fn (cdr l))))))

(define (haskell-list->list/identity l)
  (if (null? l)
      '()
      (cons (force (car l))
	    (haskell-list->list/identity (force (cdr l))))))

(define (list->haskell-list/identity l)
  (if (null? l)
      '()
      (cons (box (car l))
	    (box (list->haskell-list/identity (cdr l))))))



;;; Not currently needed

(define (eval-haskell-var v)
  (let ((val (eval (fullname v))))
    (if (var-strict? v)
	val
	(force val))))
