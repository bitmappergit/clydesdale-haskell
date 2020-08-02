;;; These utilities are specific to the Haskell language.

(define (add-con-prefix str)  ; should be in some utility file
  (string-append ";" str))

(define (remove-con-prefix string)
  (substring string 1 (string-length string)))

(define (has-con-prefix? string)
  (char=? (string-ref string 0) '#\;))

(define (add-con-prefix/symbol sym)
  (string->symbol (add-con-prefix (symbol->string sym))))

(define (remove-con-prefix/symbol sym)
  (string->symbol (remove-con-prefix (symbol->string sym))))

(define (has-con-prefix/symbol? sym)
  (has-con-prefix? (symbol->string sym)))

(define (add-di-prefix name)
  (string->symbol (add-di-prefix/string (symbol->string name))))

(define (add-di-prefix/string name)
 (string-append "di-" name))

(define (remove-di-prefix/string name)
 (substring name 3 (string-length name)))

