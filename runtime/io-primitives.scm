;;; io-primitives.scm -- IO primitives for the prelude
;;;
;;; author :  Sandra Loosemore
;;; date   :  29 Jul 1994
;;;


;;;=======================================================================
;;; IO Monad primitives
;;;=======================================================================


;;; Error data types and printing function.
;;; Represented as (tag . boxed-haskell-string)

(define (prim.read-error? e)   (eq? (car e) 'read-error))
(define (prim.write-error? e)  (eq? (car e) 'write-error))
(define (prim.format-error? e) (eq? (car e) 'format-error))
(define (prim.search-error? e) (eq? (car e) 'search-error))
(define (prim.other-error? e)  (eq? (car e) 'other-error))
(define (prim.eof-error? e)    (eq? (car e) 'eof-error))

(define (prim.make-read-error s)   (cons 'read-error s))
(define (prim.make-write-error s)  (cons 'write-error s))
(define (prim.make-format-error s) (cons 'format-error s))
(define (prim.make-search-error s) (cons 'search-error s))
(define (prim.make-other-error s)  (cons 'other-error s))
(define prim.eof-error             (cons 'eof-error '#f))

(define (prim.read-error-string e)   (force (cdr e)))
(define (prim.write-error-string e)  (force (cdr e)))
(define (prim.format-error-string e) (force (cdr e)))
(define (prim.search-error-string e) (force (cdr e)))
(define (prim.other-error-string e)  (force (cdr e)))

(define (prim.show-error e)
  (let ((str  (if (cdr e)
		  (haskell-string->string (force (cdr e)))
		  '#f)))
    (case (car e)
      (read-error     (string-append "Read Error: " str))
      (write-error    (string-append "Write Error: " str))
      (format-error   (string-append "Format Error: " str))
      (search-error   (string-append "Search Error: " str))
      (other-error    (string-append "Other Error: " str))
      (eof-error      "End of File"))))



;;; Instead of the Either/PrimIO mechanism for handling IO errors described
;;; in the Haskell 1.3 proposal, we handle IO errors using a catch/throw
;;; mechanism.  Here are the low-level guts of this mechanism.

(define *io-error-handler*
  (lambda (e)
    (haskell-runtime-error (prim.show-error e))))

(define-syntax (with-io-error-handler f . body)
  (let ((croak  (gensym))
	(e      (gensym)))
    `(let/cc ,croak
       (dynamic-let ((*io-error-handler*
		       (lambda (,e)
			 (funcall ,croak (funcall ,f ,e)))))
	 ,@body))))

(define-syntax (haskell-io-error e)
  `(funcall (dynamic *io-error-handler*) ,e))


;;; Macros for signaling specific types of errors.

(define-syntax (haskell-read-error s . args)
  `(haskell-io-error
     (prim.make-read-error
       (box (make-haskell-string (format '#f ,s ,@args))))))
(define-syntax (haskell-write-error s . args)
  `(haskell-io-error
     (prim.make-write-error
       (box (make-haskell-string (format '#f ,s ,@args))))))
(define-syntax (haskell-format-error s . args) 
  `(haskell-io-error
     (prim.make-format-error
       (box (make-haskell-string (format '#f ,s ,@args))))))
(define-syntax (haskell-search-error s . args)
  `(haskell-io-error
     (prim.make-search-error
       (box (make-haskell-string (format '#f ,s ,@args))))))
(define-syntax (haskell-other-error s . args) 
  `(haskell-io-error
     (prim.make-other-error
       (box (make-haskell-string (format '#f ,s ,@args))))))
(define-syntax (haskell-eof-error)
  `(haskell-io-error prim.eof-error))


;;; Here are implementations for the primitive IO functions.

(define-integrable (prim.thenio p q s)   ;; strictness S N
  (let ((x  (funcall p s)))
    (funcall (force q) (box x) s)))

(define-integrable (prim.seqio p q s)    ;; strictness S N
  (funcall p s)
  (funcall (force q) s))

(define-integrable (prim.applyio p q)    ;; strictness S N
  (funcall (force q) (box p)))

(define-integrable (prim.failwith x s)   ;; strictness S
  (declare (ignore s))
  (haskell-io-error x))

(define (prim.try p q s)                 ;; strictness S N
  (with-io-error-handler
      (lambda (e) (funcall (force q) (box e) s))
    (funcall p s)))



;;; These macros are provided to support Lisp/Haskell datatype
;;; conversion, to do the (a -> IOResult a) and (IOResult a -> a)
;;; conversions, respectively.

(define-syntax (io-return x) `(box ,x))
(define-syntax (io-result x) `(force ,x))



;;;=======================================================================
;;; Handles
;;;=======================================================================


;;; The handle type incorporates both a pointer to the underlying
;;; stream and various status information for Haskell.
;;; We don't support control over echoing or buffering.

(define-struct prim.handle
  (slots
   (stream (type t))
   (name   (type string))
   (kind   (type (enum :input-only :output-only :input-output)))
   (open   (type (enum :is-open    :is-closed   :is-semi-closed)))
   (file   (type bool))
   (buff   (type (enum :unbuffered :line :block)) (default :block))
   (echo   (type bool) (default '#f))
   ))


(define (prim.print-handle handle stream)
  (format stream "<<Handle for ~a ~s>>"
    (if (prim.handle-file handle) "file" "channel")
    (prim.handle-name handle)))

(define-struct-printer prim.handle prim.print-handle)

(define (prim.show-handle handle)
  (prin1-to-string handle))




;;; Define the standard handles

(define prim.stdin
  (make prim.handle
	(stream (current-input-port))
	(name "stdin")
	(kind ':input-only)
	(open ':is-open)
	(file '#f)))

(define prim.stdout
  (make prim.handle
	(stream (current-output-port))
	(name "stdout")
	(kind ':output-only)
	(open ':is-open)
	(file '#f)))

(define prim.stderr
  (make prim.handle
	(stream (current-error-port))
	(name "stderr")
	(kind ':output-only)
	(open ':is-open)
	(file '#f)))

(define prim.stdnull
  (make prim.handle
	(stream (lisp:make-two-way-stream
		  (lisp:make-concatenated-stream)
		  (lisp:make-broadcast-stream)))
	(name "stdnull")
	(kind ':input-output)
	(open ':is-open)
	(file '#f)))



;;; Input from stdin is read by means of a hook function appropriate to
;;; the user interface.  Input is always line buffered.
;;; *** Maybe the line buffering should be moved into the command interface
;;; *** so that it presents a character-oriented interface to the I/O system.

(predefine *haskell-input-hook*)

(define *haskell-stdin-buffer* "")
(define *haskell-stdin-size* 0)
(define *haskell-stdin-index* 0)

(define (haskell-stdin-buffer-empty?)
  (not (< (the fixnum *haskell-stdin-index*)
	  (the fixnum *haskell-stdin-size*))))

(define (haskell-stdin-read-char)
  (cond ((haskell-stdin-buffer-empty?)
	 (multiple-value-bind (str eof?) (funcall *haskell-input-hook*)
	   (if (eof-object? str)
	       str
	       (begin
		 (setf *haskell-stdin-buffer*
		       (if eof?
			   str
			   (string-append str (string #\newline))))
		 (setf *haskell-stdin-index* 1)
		 (setf *haskell-stdin-size*
		       (string-length *haskell-stdin-buffer*))
		 (string-ref *haskell-stdin-buffer* 0)))))
	(else
	 (let ((ch  (string-ref *haskell-stdin-buffer* *haskell-stdin-index*)))
	   (incf (the fixnum *haskell-stdin-index*))
	   ch))
	))



;;; Keep track of open handles.

(define *open-handles* '())



;;; File positioning.

(define (check-handle-seekable op handle)
  (let ((open    (prim.handle-open handle))
	(file    (prim.handle-file handle)))
    (cond ((not (eq? open ':is-open))
	   (haskell-other-error
	     "~a failed on ~a: handle not open." op handle))
	  ((not file)
	   (haskell-other-error
	     "~a failed on ~a: not a file." op handle))
	  (else
	   '#t))))

(define (prim.handle-size handle)
  (check-handle-seekable "hSize" handle)
  (or (lisp:file-length (prim.handle-stream handle))
      (haskell-other-error
        "hSize failed on ~a: file not seekable." handle)))

(define (prim.handle-posn handle)
  (check-handle-seekable "hPosn" handle)
  (or (lisp:file-position (prim.handle-stream handle))
      (haskell-other-error
        "hPosn failed on ~a: file not seekable." handle)))

(define (prim.seek handle posn)
  (check-handle-seekable "seek" handle)
  (or (with-error-handler
          (lambda (s)
	    (declare (ignore s))
	    (haskell-search-error
	      "seek failed on ~a: Lisp error." handle))
	(lisp:file-position (prim.handle-stream handle) posn))
      (haskell-search-error
        "seek failed on ~a: file not seekable." handle)))


;;; Buffering and echoing control

(define (prim.set-buffering handle mode)
  (unless (eq? mode (prim.handle-buff handle))
    (haskell-write-error
      "setBuffering failed on ~a: not implemented." handle)))

(define (prim.set-echoing handle mode)
  (unless (eq? mode (prim.handle-echo handle))
    (haskell-other-error
     "setEchoing failed on ~a: not implemented." handle)))



  
;;;=======================================================================
;;; Open/close operations
;;;=======================================================================



;;; *** This does not attempt to do any of the locking stuff described in
;;; *** the I/O proposal.  

(define (prim.open-file mode name)
  (let* ((stream
	  (with-error-handler
	      (lambda (s)
	        (declare (ignore s))
	        (haskell-search-error
		  "openFile failed on ~s: Lisp error." name))
	    (case mode
		  ((:read)    (open-input-file name))
		  ((:write)   (open-output-file name))
		  ((:append)  (open-append-file name)))))
	 (handle
	  (make prim.handle
		(stream stream)
		(name name)
		(kind (if (eq? mode ':read) ':input-only ':output-only))
		(open :is-open)
		(file '#t))))
    (push handle (dynamic *open-handles*))
    handle))


;;; *** This is supposed to open a pipe.

(define (prim.open-chan name)
  (haskell-search-error "openChan failed on ~s: not implemented." name))


(define (prim.flush handle)
  (check-handle-writable "flush" handle)
  (force-output (prim.handle-stream handle)))

(define (prim.close handle)
  (let ((open    (prim.handle-open handle))
	(stream  (prim.handle-stream handle)))
    (if (not (eq? open ':is-closed))
	(begin
	   (setf (prim.handle-open handle) ':is-closed)
	   ;; Don't really close standard streams.
	   (unless (or (eq? handle prim.stdin)
		       (eq? handle prim.stdout)
		       (eq? handle prim.stderr)
		       (eq? handle prim.stdnull))
	     (lisp:close stream)))
        (haskell-other-error "close failed on ~a: handle already closed."
			     handle))))



;;;=======================================================================
;;; Input operations on handles
;;;=======================================================================

(define (check-handle-readable op handle)
  (let ((kind    (prim.handle-kind handle))
	(open    (prim.handle-open handle)))
    (cond ((not (or (eq? kind ':input-only) (eq? kind ':input-output)))
	   (haskell-read-error
	     "~a failed on ~a: handle not readable." op handle))
	  ((not (eq? open ':is-open))
	   (haskell-read-error
	     "~a failed on ~a: handle not open." op handle))
	  (else
	   '#t))))

(define (prim.ready handle)
  (check-handle-readable "ready" handle)
  (if (eq? handle prim.stdin)
      (or (not (haskell-stdin-buffer-empty?))
	  (listen (prim.handle-stream handle)))
      (listen (prim.handle-stream handle))))

(define (prim.get-char handle)
  (check-handle-readable "hGetChar" handle)
  (let ((ch  (if (eq? handle prim.stdin)
		 (haskell-stdin-read-char)
	         (read-char (prim.handle-stream handle)))))
    (if (eof-object? ch)
	(haskell-eof-error)
        ch)))


;;; This function has to be declared with noConversion because it returns
;;; a delay rather than a boxed result.

(define (prim.get-contents handle state)
  (declare (ignore state))
  (check-handle-readable "getContents" handle)
  (setf (prim.handle-open handle) ':is-semi-closed)
  (if (and (eq? handle prim.stdin)
	   (not (haskell-stdin-buffer-empty?)))
      (delay (make-haskell-string-tail
	       (substring *haskell-stdin-buffer*
			  *haskell-stdin-index* *haskell-stdin-size*)
	       (delay (get-contents-file handle))))
      (delay (get-contents-file handle))))


;;; If someone is stupid enough to close the handle before 
;;; all of the input has been read, treat it as though EOF has been
;;; reached, but don't signal error.

(define (get-contents-file handle)
  (if (eq? (prim.handle-open handle) ':is-closed)
      '()
      (multiple-value-bind (str eof?)
	  (if (eq? handle prim.stdin)
	      (funcall *haskell-input-hook*)
	      (read-line (prim.handle-stream handle)))
	(if (eof-object? str)
	    '()
	    (make-haskell-string-tail
	      str
	      (if eof?
		  (box '())
		  (box (cons (box (char->integer #\newline))
			     (delay (get-contents-file handle))))))))))



;;;=======================================================================
;;; Output operations on handles
;;;=======================================================================

(define (check-handle-writable op handle)
  (let ((kind    (prim.handle-kind handle))
	(open    (prim.handle-open handle)))
    (cond ((not (or (eq? kind ':output-only) (eq? kind ':input-output)))
	   (haskell-write-error
	     "~a failed on ~a: handle not writable." op handle))
	  ((not (eq? open ':is-open))
	   (haskell-write-error
	     "~a failed on ~a: handle not open." op handle))
	  (else
	   '#t))))

(define (prim.put-char handle ch)
  (check-handle-writable "hPutChar" handle)
  (write-char ch (prim.handle-stream handle)))



;;;=======================================================================
;;; Operating system interaction
;;;=======================================================================

(define (prim.delete-file name)
  (or (with-error-handler
          (lambda (s)
	    (declare (ignore s))
	    (haskell-search-error
	      "deleteFile failed on ~s: Lisp error." name))
	(delete-file name))
      (haskell-search-error "deleteFile failed on ~s." name)))

(define (prim.status-file name)
  (if (file-exists? name)
      "frw"
      (haskell-search-error
        "statusFile failed on ~s: file does not exist." name)))


(predefine *haskell-command-line-args*)   ; in interface-prims.scm
(predefine *haskell-program-name*)        ; in interface-prims.scm

(define (prim.getargs)
  (dynamic *haskell-command-line-args*))

(define (prim.getprogname)
  (dynamic *haskell-program-name*))

(define (prim.getenv name)
  (getenv name))

(define (prim.setenv name value)
  (declare (ignore name value))
  (haskell-other-error "setEnv not implemented"))



;;; Haskell wants time encoded in microseconds since Jan 1, 1970 GMT.

(define-integrable timebase (lisp:encode-universal-time 0 0 0 1 1 1970 0))

(define (prim.getclock)
  (* (the integer (- (the integer (lisp:get-universal-time))
		     (the integer timebase)))
     1000))


(define *initial-cpu-time* 0)

(define-integrable cpu-time-factor
  (/ 1000 lisp:internal-time-units-per-second))

(define (prim.getcputime)
  (let ((current  (lisp:get-internal-run-time)))
    (* (the integer (- (the integer current)
		       (the integer (dynamic *initial-cpu-time*))))
       cpu-time-factor)))



;;; *** do something about this.

(define (prim.run-process progname new-stdin new-stdout new-stderr)
  (declare (ignore progname new-stdin new-stdout new-stderr))
  (haskell-other-error "runProcess failed: not implemented."))

(define (prim.system progname)
  (declare (ignore progname))
  (haskell-other-error "system failed: not implemented."))



;;;=======================================================================
;;; Initialization
;;;=======================================================================


(define-syntax (with-io-system . body)
  `(begin
     ;; Reset the standard handles.
     (setf (prim.handle-open prim.stdin) ':is-open)
     (setf (prim.handle-open prim.stdout) ':is-open)
     (setf (prim.handle-open prim.stderr) ':is-open)
     (setf (prim.handle-open prim.stdnull) ':is-open)
     (setf *haskell-stdin-index* 0)
     (setf *haskell-stdin-size* 0)
     ;; Force output and close any handles left open after executing the body.
     (dynamic-let ((*open-handles*      '())
		   (*initial-cpu-time*  (lisp:get-internal-run-time)))
       (unwind-protect (begin ,@body)
	 (force-output (current-output-port))
	 (force-output (current-error-port))
	 (dolist (h *open-handles*)
	   (unless (eq? (prim.handle-open h) ':is-closed)
	     (lisp:close (prim.handle-stream h)))))))
  )
