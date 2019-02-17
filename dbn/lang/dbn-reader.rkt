#lang racket/base
(require syntax/strip-context)
; require the parser and ast
(require dbn/dbnc/parser
         dbn/dbnc/ast)

; now provide read and read-syntax for the rest of the world
(provide (rename-out [dbn-read read]
                     [dbn-read-syntax read-syntax]))

; define the read function
(define (dbn-read in)
  (syntax->datum
   (dbn-read-syntax #f in)))

; and the read syntax function
(define (dbn-read-syntax path port)
  ; create the parse tree
  (let* ([parse-tree (parse port)]
         ; then build the s-expression (note, ast->sexp does all the transforms)
         [s-exp (ast->sexp parse-tree)]
         ; create a module from this, note that this 
         [module-datum `(module dbn-mod dbn/lang/dbn-expander ,s-exp)])
    (datum->syntax #f module-datum)))



