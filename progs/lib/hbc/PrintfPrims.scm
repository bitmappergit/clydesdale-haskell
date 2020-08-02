(define (prim.fmte p d)
  (dynamic-let ((lisp:*read-default-float-format* 'lisp:double-float))
    (lisp:format '#f "~,v,3e" p d)))

(define (prim.fmtf p d)
  (dynamic-let ((lisp:*read-default-float-format* 'lisp:double-float))
    (lisp:format '#f "~,vf" p d)))

(define (prim.fmtg p d)
  (dynamic-let ((lisp:*read-default-float-format* 'lisp:double-float))
    (let ((expt  (lisp:log (lisp:abs d) 10)))
      ;; Use the ANSI C rule for deciding which format to use, rather
      ;; than the Common Lisp rule.
      (if (or (< expt -4) (>= expt p))
	  (lisp:format '#f "~,v,3e" p d)
          (lisp:format '#f "~,vf" p d)))))


