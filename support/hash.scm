
;;; This is a VERY SIMPLE hashing routine for structure printing.

(define *structure-hash-table* '())

(define (structure-hash s)
  (cond ((null? *structure-hash-table*)
	 (setf *structure-hash-table* (list s))
	 0)
	(else
	 (search-structure-hash-table *structure-hash-table* s 0))))

(define (search-structure-hash-table tbl s n)
  (cond ((eq? (car tbl) s) n)
	((null? (cdr tbl))
	 (setf (cdr tbl) (list s))
	 (+ n 1))
	(else (search-structure-hash-table (cdr tbl) s (+ n 1)))))

(define (structure-unhash i)
  (search-structure-table *structure-hash-table* i))

(define (search-structure-table tbl i)
  (if (null? tbl)
      '()
      (if (eqv? i 0)
	  (car tbl)
	  (search-structure-table (cdr tbl) (- i 1)))))


	 
