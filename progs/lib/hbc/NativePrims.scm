;;; NativePrims.scm -- Lisp primitives for Native I/O operations
;;;
;;; author :  Sandra Loosemore
;;; date   :  09 Jun 1994
;;;
;;; Note:  this file contains code specific to various Common Lisp
;;; implementations since it isn't possible to do this stuff portably.
;;;
;;;


;;;========================================================================
;;; Shared support code
;;;========================================================================


;;; Constructors for Maybe type

(define-integrable nothing (make-tagged-data 0))
(define-integrable (just x) (make-tagged-data 1 (box x)))


;;; Primitive operations on byte files

(define (prim.open-input-byte-file name)
  (if (file-exists? name)
      (lisp:open name :direction :input :element-type '(lisp:unsigned-byte 8))
      (haskell-runtime-error (format '#f "File ~s not found." name))))

(define (prim.open-output-byte-file name)
  (lisp:open name :direction :output :element-type '(lisp:unsigned-byte 8)))


(define (prim.close-byte-file f)
  (lisp:close f)
  0)

(define (prim.show-byte-file b f)
  (lisp:write-byte b f)
  0)

(define (prim.read-byte-file f)
  (let ((b  (lisp:read-byte f '#f '#f)))
    (if b
	(just b)
        nothing)))



;;; The magic in the interface file makes the character arguments be passed
;;; as small integers, allowing us to mess with them directly as byte objects.

(define (prim.char-show-bytes x list) 
  (cons (box x) list))

(define (prim.char-read-bytes list)
  (if (null? list)
      nothing
      (just (make-tuple (car list) (cdr list)))))

(define (prim.char-show-byte-file x f state)
  (declare (ignore state))
  (prim.show-byte-file x f))

(define (prim.char-read-byte-file f state)
  (declare (ignore state))
  (io-return (prim.read-byte-file f)))



;;;========================================================================
;;; CMU CL stuff
;;;========================================================================
#+cmu
(begin


;;; This is easy; all we have to do is copy the stuff in and out of an
;;; alien union structure.

(define *conversion-buffer*
  (alien:make-alien
    (alien:union lisp:nil
		 (s c-call:short)
		 (i c-call:int)
		 (f c-call:float)
		 (d c-call:double)
		 (b (lisp:array c-call:unsigned-char 8)))))
(define *conversion-bytes* (alien:slot *conversion-buffer* 'b))


(define-integrable short-size (alien:alien-size c-call:short :bytes))
(define-integrable int-size (alien:alien-size c-call:int :bytes))
(define-integrable float-size (alien:alien-size c-call:float :bytes))
(define-integrable double-size (alien:alien-size c-call:double :bytes))

(define-syntax (define-byte-functions size slot
		 show-bytes-fn read-bytes-fn
		 show-byte-file-fn read-byte-file-fn)
  `(begin
     (define (,show-bytes-fn x list)
       (setf (alien:slot *conversion-buffer* ',slot) x)
       (show-bytes-aux ,size list))
     (define (,read-bytes-fn list)
       (multiple-value-bind (success? tail) (read-bytes-aux ,size list)
	 (if success?
	     (just
	       (make-tuple (box (alien:slot *conversion-buffer* ',slot))
			   tail))
	     nothing)))
     (define (,show-byte-file-fn x f state)
       (declare (ignore state))
       (setf (alien:slot *conversion-buffer* ',slot) x)
       (show-byte-file-aux ,size f))
     (define (,read-byte-file-fn f state)
       (declare (ignore state))
       (io-return
	 (if (read-byte-file-aux ,size f)
	     nothing
	     (just (alien:slot *conversion-buffer* ',slot)))))
     ))

(define-byte-functions short-size s
  prim.short-show-bytes prim.short-read-bytes
  prim.short-show-byte-file prim.short-read-byte-file)
(define-byte-functions int-size i
  prim.int-show-bytes prim.int-read-bytes
  prim.int-show-byte-file prim.int-read-byte-file)
(define-byte-functions float-size f
  prim.float-show-bytes prim.float-read-bytes
  prim.float-show-byte-file prim.float-read-byte-file)
(define-byte-functions double-size d
  prim.double-show-bytes prim.double-read-bytes
  prim.double-show-byte-file prim.double-read-byte-file)


;;; Shared helper functions for copying bytes in and out of the buffer.

(define (show-bytes-aux size list)
  (declare (type fixnum size))
  (do ((i  size (1- i)))
      ((eqv? i 0) list)
    (declare (type fixnum i))
    (setf list
	  (cons (box (alien:deref *conversion-bytes* (1- i)))
		(if (eqv? i size) list (box list))))))


(define (read-bytes-aux size list)
  (declare (type fixnum size))
  ;; This is a little bizarre because the list argument is strict, but
  ;; on subsequent iterations we don't force the tail until we have to.
  (if (or (null? list) (eqv? size 0))
      (values (eqv? i size) (box list))
      (read-bytes-aux-aux 0 size list)))

(define (read-bytes-aux-aux i size list)
  (declare (type fixnum i size))
  (setf (alien:deref *conversion-bytes* i) (force (car list)))
  (incf i)
  (if (eqv? i size)
      (values '#t (cdr list))
      (let ((tail  (force (cdr list))))
	(if (null? tail)
	    (values '#f (cdr list))
	    (read-bytes-aux-aux i size tail)))))


(define (show-byte-file-aux size f)
  (dotimes (i size)
    (lisp:write-byte (alien:deref *conversion-bytes* i) f))
  0)

(define (read-byte-file-aux size f)
  (let ((error?  '#f))
    (dotimes (i size)
      (let ((b  (lisp:read-byte f '#f '#f)))
	(if b
	    (setf (alien:deref *conversion-bytes* i) b)
	    (setf error? '#t))))
    error?))


) ; end of cmu-specific code



;;;========================================================================
;;; Other lisps
;;;========================================================================

#-cmu
(lisp:eval-when (lisp:eval lisp:compile lisp:load)
  (error "Sorry, Native IO primitives not implemented in this lisp."))

