
;;; Support for dynamic typing

(define (gather-instances)
 (let ((l '()))
  (table-for-each
   (lambda (mname m)
     (declare (ignore mname))
     (when (eq? (module-type m) 'standard)
	(setf l (append (haskell-list->list/identity
			 (eval-haskell-var (module-instance-var m)))
			l))))
   *modules*)
  l))

(define (prim-all-instances x)
  (declare (ignore x))
  (gather-instances))

;;; This is used to get around the Haskell type system.

(define (prim-apply f x)
  (funcall f (box x)))

(define (prim-apply-list f xs)
  (if xs (apply f (map (lambda (x) (box x)) xs)) f))

(define (prim-from-magic x) x)

(define (prim-to-magic x) x)


;;; Stuff for runtime coercion of overloaded values:

(define (prim-make-vars n)
  (let ((res '()))
    (dotimes (i n)
      (declare (ignorable i))
      (push (gensym "v") res))
    res))

;;;  DANGER   DANGER!   The following functions make assumptions about low
;;;  level representations.  If representations are changed these must be
;;;  updated!

;;; Tuple support

(define (prim-get-tuple-constructor i)
  (let ((s '()))
    (dotimes (j i)
      (declare (ignorable j))
      (push '#f s))
    (make-tuple-constructor i s)))

(define (prim-get-tuple-selector size i)
  (if (eqv? size 2)
      (if (eqv? i 0)
	  (function force-car/force)
	  (function force-cdr/force))
      (lambda (x)
	(force-vector-ref/force x i))))
  
(define (get-tuple-flags v s)
  (if (null? (cdr s))
      (or (car s) (car v))
      (if (pair? v)
	  (list (or (car s) (car (car v))) (or (cadr s) (car (cdr v))))
	  (let ((i -1))
	    (map (lambda (e)
		   (setf i (1+ i))
		   (or e (car (vector-ref v i))))
		 s)))))

(define (get-struct-flags v s)
  (let ((i 0))
    (map (lambda (e)
	   (setf i (1+ i))
	   (or e (car (vector-ref v i))))
	 s)))

;;; Support for manipulation of arbitrary values

(define (d-make-tuple-constr a s)
  (make-tuple-constructor a s))

(define (d-make-constr i a s)
  (make-tagged-data-constructor i a s))

(define (d-make-tuple-sel i a s)
  (cond ((eqv? a 1)
	 (if s
	     (lambda (x) (force x))
	     (lambda (x) (force (force x))))) ; There are two wrappers!!
	((eqv? a 2)
	 (if (eqv? i 0)
	     (if s
		 (lambda (x) (car (force x)))
		 (lambda (x) (force (car (force x)))))
	     (if s
		 (lambda (x) (cdr (force x)))
		 (lambda (x) (force (cdr (force x)))))))
	(else
	 (if s
	     (lambda (x)
	       (vector-ref (the vector (force x)) (the fixnum i)))
	     (lambda (x)
	       (force (vector-ref (the vector (force x)) (the fixnum i))))))))

(define (d-make-sel i a s)
  (declare (ignore a))
  (if s
      (lambda (x)
	(vector-ref (the vector (force x)) (1+ (the fixnum i))))
      (lambda (x)
	(force (vector-ref (the vector (force x)) (1+ (the fixnum i)))))))
  
(define (d-make-enum-constr i)
  i)

(define (d-enum-type-to-int i)
  i)

(define (d-tuple-type-to-int i)
  (declare (ignore i))
  0)

(define (d-type-to-int i)
  (constructor-number i))

