#lang racket/base

;; This module contains all the different nodes we use to build the AST
;; syntax of the DBN language
(provide (all-defined-out))


; all nodes have an id, this contains an auto-generated symbol for mapping
(struct ast-node () #:transparent)


; make everything from here inherit from ast-node, this just simplifies
; some of the code below, it's nothing fancy
(define-syntax ast-struct
  (syntax-rules ()
    [(ast-struct name-id (field-id ...))
     (struct name-id ast-node (field-id ...) #:transparent)]
    [(ast-struct name-id (field-id ...) keywords ...)
     (struct name-id ast-node (field-id ...) keywords ...)]))


; represents an entire program, i.e., a list of statements
; and notes attached to the ast
(ast-struct program (statements notes))
(define (make-program statements [notes #f])
  (program statements notes))

;;; these are the expressions for paper, pen and line
(ast-struct paper-expr (value xsize ysize))
(ast-struct pen-expr (value))
(ast-struct line-expr (x1 y1 x2 y2))

;; numeric expressions
(ast-struct numeric-expr (value))

;; var identifiers
(ast-struct var-expr (name))

; paper location ast-structs
(ast-struct set-paper-loc (x y color))
(ast-struct get-paper-loc (x y))
(ast-struct antialias-expr (value))

; used to indicate when a variable is created the first time
(ast-struct create-var-expr (name e1))
; set, which works on variables
(ast-struct assignment-expr (e1 e2))


; iterations
(ast-struct repeat-expr (var start end body))
(ast-struct forever-expr (body))

; predicate expressions
(ast-struct same-expr (e1 e2 body))
(ast-struct not-same-expr (e1 e2 body))
(ast-struct smaller-expr (e1 e2 body))
(ast-struct not-smaller-expr (e1 e2 body))

; structs for defining functions and procedures,
; these have a name a list of parameters and a body
(ast-struct command-fun (name params body))
(ast-struct number-fun (name params body))

; this is the equivalent of a 'return v' in another language
(ast-struct value-expr (value))

; just prints to the standard output
(ast-struct print-expr (value))

; function application, empty for commands
(ast-struct apply-expr (fun-name params))

; things related to the external world, time, mouse, etc
(ast-struct mouse-expr (value))
(ast-struct key-expr (value))
(ast-struct time-expr (value))
(ast-struct bitmap-expr ())

; compound expressions
(ast-struct add-expr (e1 e2))
(ast-struct sub-expr (e1 e2))
(ast-struct mult-expr (e1 e2))
(ast-struct div-expr (e1 e2))

; loading
(ast-struct load-expr (filename))

