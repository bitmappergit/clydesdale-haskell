;;; These are global variables used throughout the compiler.

;;; Configuration stuff

(define *prelude-unit-filename* "$PRELUDE/Prelude.hu")

(define *haskell-compiler-version* "Y2.2")
(define *haskell-compiler-update* "")


;;; Error control

(define *recoverable-error-handler* '())
(define *error-output-port* (current-output-port))

;;; Random stuff

(define *context* '#f)  ; ast node being compiled.
(define *unit* '())
(define *standard-module-default* '())
(define *undefined-def* '())
(define *magic-temp-name* '|t_e_m_p|)

;;; Printers

(define *printers* '(phase-time compiling loading))

(define *all-printers*
  '(phase-time time compiling loading reading pad interactive prompt
    parse import type-decl scope depend
    type cfn depend2
    flic optimize optimize-extra strictness codegen codegen-flic
    dumper dump-stat))


;;; Used by the symbol table routines

(define *modules* '())  ; maps module name -> module structure
(define *interfaces* '())
(define *implementations* '())
(define *modules-being-compiled* '())
(define *module* '())   ; current module
(define *module-name* '())
(define *symbol-table* '())  ; part of the current module
(define *inverted-symbol-table* '())  ; maps def -> localname
(define *fixity-table* '())  ; name -> fixity

;;; Used by the compilation system

(define *implementations-needed* '())
(define *current-initcode* '())
(define *modules-loaded* '())

;;; This is used to stash the Prelude symbol environment

(define *prelude-symbol-table* '())
(define *prelude-fixity-table* '())
(define *prelude-inverted-symbol-table* '())
