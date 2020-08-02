
;;;=====================================================================
;;; Cache manager
;;;=====================================================================

;;; This is the cache manager for compilation units.  We use an alist at
;;; the moment.

(define *unit-cache* '())

(define (reset-unit-cache)
  (setf *unit-cache* '()))


;;; This checks to make sure that the compilation unit it finds
;;; in the cache has not been made out-of-date by updates to the unit file.

(define (lookup-compilation-unit name)
  (let ((r (ass-string name *unit-cache*)))
    (if r
	(let ((c  (cdr r)))
	 (if (ucache-stable? c)
	     c
	     (if (and (file-exists? (ucache-ufile c))
		      (< (ucache-udate c) (file-write-date (ucache-ufile c))))
		 '#f  ; unit file modified
		 c))) ; unit valid
	'#f)))

;;; This is used as an after compilation lookup where no check of the file
;;; updates is needed.

(define (lookup-compiled-unit name)
  (let ((r (ass-string name *unit-cache*)))
    (cdr r)))

(define (install-compilation-unit name c)
  (let ((r (ass-string name *unit-cache*)))
    (if (eq? r '#f)
	(push (cons name c) *unit-cache*)
	(setf (cdr r) c))))

;;; This is used to examine the cache for units to be uncached.

(define (for-all-cached-units p)
  (dolist (c *unit-cache*)
    (funcall p (tuple-2-2 c))))
