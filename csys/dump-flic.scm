;;; dump-flic.scm -- general dump functions for flic structures
;;;
;;; author :  Sandra Loosemore
;;; date   :  24 Feb 1993
;;;
;;;
;;; This stuff is used to write inline expansions to the interface file.
;;; 


(define-flic-walker dump-flic (object var-renamings))

(define (dump-flic-list objects var-renamings)
  (let ((result  '()))
    (dolist (o objects)
      (push (dump-flic o var-renamings) result))
    `(list ,@(nreverse result))))

(define (dump-flic-top object)
  (dump-flic object '()))


(define (make-temp-bindings-for-dump oldvars var-renamings)
  (let ((vars      '())
	(names     '()))
    (dolist (v oldvars)
      (let ((name (symbol->string (def-name v)))
	    (temp (gensym)))
	(push temp vars)
        (push name names)
	(push (cons v temp) var-renamings)))
    (setf names (nreverse names))
    (setf vars (nreverse vars))
    (values vars names var-renamings)))

(define-dump-flic flic-lambda (object var-renamings)
  (multiple-value-bind (vars names var-renamings)
      (make-temp-bindings-for-dump (flic-lambda-vars object) var-renamings)
    `(flic-lambda-hack ,vars ,names 
		       ,(dump-flic (flic-lambda-body object) var-renamings))
    ))

(define-dump-flic flic-let (object var-renamings)
  (multiple-value-bind (vars names var-renamings)
      (make-temp-bindings-for-dump (flic-let-bindings object) var-renamings)
    `(,(if (flic-let-recursive? object) 'flic-letrec-hack 'flic-let*-hack)
      ,vars
      ,names
      ,(map (lambda (v) (dump-flic (var-value v) var-renamings))
	    (flic-let-bindings object))
      ,(dump-flic (flic-let-body object) var-renamings))
    ))

(define-dump-flic flic-app (object var-renamings)
  (let ((fn    (dump-flic (flic-app-fn object) var-renamings))
	(args  (dump-flic-list (flic-app-args object) var-renamings))
	(sat?  (flic-app-saturated? object)))
    ;; Try to produce more compact code.
    (cond ((or (not sat?) (not (pair? fn)))
	   `(make-flic-app ,fn ,args ,sat?))
	  ((eq? (car fn) 'make-flic-ref)
	   `(make-flic-app/ref ,(cadr fn) ,@(cdr args)))
	  ((eq? (car fn) 'make-flic-ref/n)
	   `(make-flic-app/ref/n ,(cadr fn) ,@(cdr args)))
	  ((eq? (car fn) 'make-flic-pack)
	   `(make-flic-app/pack ,(cadr fn) ,@(cdr args)))
	  ((eq? (car fn) 'make-flic-pack/n)
	   `(make-flic-app/pack/n ,(cadr fn) ,@(cdr args)))
	  (else
	   `(make-flic-app ,fn ,args ,sat?)))))

(define-dump-flic flic-ref (object var-renamings)
  (let* ((var    (flic-ref-var object))
	 (entry  (assq var var-renamings)))
    (if entry
	`(make-flic-ref ,(cdr entry))
	(let ((stuff (dump-object var)))
	  (if (def-prelude? var)
	      `(make-flic-ref ,stuff)
	      `(make-flic-ref/n ,(def-dump-index var)))))))

(define-dump-flic flic-const (object var-renamings)
  (declare (ignore var-renamings))
  (let ((val  (flic-const-value object)))
    (if (or (number? val) (string? val))
	`(make-flic-const ,val)   ; self-evaluating
	`(make-flic-const ',val))))

(define-dump-flic flic-pack (object var-renamings)
  (declare (ignore var-renamings))
  (let* ((con   (flic-pack-con object))
	 (stuff (dump-object con)))
    (if (def-prelude? con)
	`(make-flic-pack ,stuff)
	`(make-flic-pack/n ,(def-dump-index con)))))

(define-dump-flic flic-case-block (object var-renamings)
  `(make-flic-case-block
     ',(flic-case-block-block-name object)
     ,(dump-flic-list (flic-case-block-exps object) var-renamings)))

(define-dump-flic flic-return-from (object var-renamings)
  `(make-flic-return-from
     ',(flic-return-from-block-name object)
     ,(dump-flic (flic-return-from-exp object) var-renamings)))

(define-dump-flic flic-and (object var-renamings)
  `(make-flic-and
     ,(dump-flic-list (flic-and-exps object) var-renamings)))

(define-dump-flic flic-if (object var-renamings)
  `(make-flic-if
     ,(dump-flic (flic-if-test-exp object) var-renamings)
     ,(dump-flic (flic-if-then-exp object) var-renamings)
     ,(dump-flic (flic-if-else-exp object) var-renamings)))

(define-dump-flic flic-sel (object var-renamings)
  `(make-flic-sel
     ,(dump-object (flic-sel-con object))
     ,(flic-sel-i object)
     ,(dump-flic (flic-sel-exp object) var-renamings)))

(define-dump-flic flic-is-constructor (object var-renamings)
  `(make-flic-is-constructor
     ,(dump-object (flic-is-constructor-con object))
     ,(dump-flic (flic-is-constructor-exp object) var-renamings)))

(define-dump-flic flic-con-number (object var-renamings)
  `(make-flic-con-number
     ,(dump-object (flic-con-number-type object))
     ,(dump-flic (flic-con-number-exp object) var-renamings)))

(define-dump-flic flic-void (object var-renamings)
  (declare (ignore object var-renamings))
  `(make-flic-void))

(define-dump-flic flic-update (object var-renamings)
  `(make-flic-update
     ,(dump-object (flic-update-con object))
     (list ,@(map (lambda (s)
		    `(cons ,(car s) ,(dump-flic (cdr s) var-renamings)))
		  (flic-update-slots object)))
     ,(dump-flic (flic-update-exp object) var-renamings)))


;;; Runtime helper functions

(define (make-flic-ref/n i)
  (make-flic-ref (def-n i)))

(define (make-flic-pack/n i)
  (make-flic-pack (def-n i)))

(define (make-flic-app/ref def . args)
  (make-flic-app (make-flic-ref def) args '#t))

(define (make-flic-app/ref/n i . args)
  (make-flic-app (make-flic-ref (def-n i)) args '#t))

(define (make-flic-app/pack def . args)
  (make-flic-app (make-flic-pack def) args '#t))

(define (make-flic-app/pack/n i . args)
  (make-flic-app (make-flic-pack (def-n i)) args '#t))


(define-syntax (flic-lambda-hack temps names body)
  `(let ,(map (lambda (temp name) `(,temp (create-temp-var ,name)))
	      temps names)
     (make-flic-lambda (list ,@temps) ,body)))

(define-syntax (flic-let*-hack temps names inits body)
  `(let ,(map (lambda (temp name) `(,temp (create-temp-var ,name)))
	      temps names)
     ,@(map (lambda (temp init) `(setf (var-value ,temp) ,init)) temps inits)
     (make-flic-let (list ,@temps) ,body '#f)))

(define-syntax (flic-letrec-hack temps names inits body)
  `(let ,(map (lambda (temp name) `(,temp (create-temp-var ,name)))
	      temps names)
     ,@(map (lambda (temp init) `(setf (var-value ,temp) ,init)) temps inits)
     (make-flic-let (list ,@temps) ,body '#t)))


