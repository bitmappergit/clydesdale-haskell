(SETF *WRITER-VERSION* '"Y2.2")
(SETF *MODULES-IMPORTED* (VECTOR))
(SETF *MODULES-LOADED* (VECTOR (MAKE MODULE (UNIT '|SymbolPrims-interface|) (NAME '|SymbolPrims|) (TYPE 'INTERFACE))))
(SETF *DEFS-REFERENCED* (MAKE-VECTOR 4))
(SET-DEF-N/ALGDATA 0 '|Symbol| '|Symbol|)
(SET-DEF-N/VAR 1 0 '|genSymbol|)
(SET-DEF-N/VAR 2 0 '|eqSymbol|)
(SET-DEF-N/VAR 3 0 '|symbolToString|)
(SETF *TYPES-REFERENCED* (MAKE-VECTOR 6))
(SET-TYPE-N 0 (NTYCON/DEF |*core-String*|))
(SET-TYPE-N 1 (NTYCON/DEF-N 0))
(SET-TYPE-N 2 (ARROW/N 0 1))
(SET-TYPE-N 3 (NTYCON/DEF |*core-Bool*|))
(SET-TYPE-N 4 (ARROW/N 1 1 3))
(SET-TYPE-N 5 (ARROW/N 1 0))
(SETF *DUMP-FILE-NAMES* '("$PRELUDE/PreludeSymbolPrims.hi"))
(SETF (MODULE-STAND-ALONE? (LOOKUP-DEFINED-MOD 0)) T)
(SETF (MODULE-UNRESOLVED-SYMBOLS (LOOKUP-DEFINED-MOD 0)) (LIST (DEF-N 0)))
(SETF (MODULE-INTERFACE-DEFINITIONS (LOOKUP-DEFINED-MOD 0)) (LIST))
(SETF (MODULE-USES-STANDARD-PRELUDE? (LOOKUP-DEFINED-MOD 0)) COMMON-LISP:NIL)
(SETF (MODULE-DEFAULT (LOOKUP-DEFINED-MOD 0)) COMMON-LISP:NIL)
(SETF (MODULE-INSTANCE-DEFS (LOOKUP-DEFINED-MOD 0)) COMMON-LISP:NIL)
(SETF (MODULE-EXPORT-TABLE (LOOKUP-DEFINED-MOD 0)) (LET ((TAB (MAKE-TABLE))) (SET-EXPORT/DEF-N/LIST TAB '(3 2 1)) (SET-EXPORT/DEF TAB |*core-stringToSymbol*|) TAB))
(SETF (AST-NODE-LINE-NUMBER (LOOKUP-DEFINED-MOD 0)) (RESTORE-SOURCE-POINTER 'COMMON-LISP:NIL 'COMMON-LISP:NIL))
(SETF (MODULE-FIXITY-TABLE (LOOKUP-DEFINED-MOD 0)) (MAKE-FIXITY-TABLE 'COMMON-LISP:NIL))
(SETF (MODULE-SYMBOL-TABLE (LOOKUP-DEFINED-MOD 0)) (LET ((TAB (MAKE-TABLE))) (SET-SYMTAB/DEF-N/LIST TAB '(3 2 1 0)) (SET-SYMTAB/DEF TAB |*core-stringToSymbol*|) TAB))
(SET-VAR-COMPLEXITY 3 1)
(INIT-FN-SLOTS 3 T T (GTYPE/NULL 0 5) COMMON-LISP:NIL T 1 (STRICTNESS-N 3) '|SymbolPrims:symbolToString/wrapper| '0 '10)
(SET-VAR-COMPLEXITY 2 1)
(INIT-FN-SLOTS 2 T T (GTYPE/NULL 0 4) COMMON-LISP:NIL T 2 (STRICTNESS-N 7) 'EQ? '0 '12)
(SET-VAR-COMPLEXITY |*core-stringToSymbol*| 1)
(SETF (DEF-MODULE |*core-stringToSymbol*|) '|SymbolPrims|)
(INIT-FN-SLOTS |*core-stringToSymbol*| T T (GTYPE/NULL 0 2) COMMON-LISP:NIL T 1 (STRICTNESS-N 3) '|SymbolPrims:stringToSymbol/wrapper| '0 '11)
(SET-VAR-COMPLEXITY 1 1)
(INIT-FN-SLOTS 1 T T (GTYPE/NULL 0 2) COMMON-LISP:NIL T 1 (STRICTNESS-N 3) '|SymbolPrims:genSymbol/wrapper| '0 '13)
(INIT-ALGDATA-SLOTS 0 COMMON-LISP:NIL 0 COMMON-LISP:NIL COMMON-LISP:NIL COMMON-LISP:NIL COMMON-LISP:NIL COMMON-LISP:NIL COMMON-LISP:NIL COMMON-LISP:NIL COMMON-LISP:NIL COMMON-LISP:NIL COMMON-LISP:NIL 'COMMON-LISP:NIL 'COMMON-LISP:NIL)