#lang racket/base

; require the parser and ast
(require dbn/lang/dbn-parser
         dbn/lang/dbn-ast)

(define (read-syntax path port)
  ; create the parse tree
  (let* ([parse-tree (transform-all (parse port))]
         ; then build the s-expression
         [s-exp (ast->sexp parse-tree)]
         ; create a module from this
         [module-datum `(module dbn-mod dbn/lang/expander ,s-exp)])
    (datum->syntax #f module-datum)))

; now provide read syntax for the rest of the world
(provide read-syntax)