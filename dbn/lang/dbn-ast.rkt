#lang racket

(require "dbn-env.rkt")
;; this module contains all the structs needed to create the abstract
;; syntax of the DBN language
(provide (all-defined-out))

;;; these are the expressions for paper, pen and line
(struct paper-expr (value xsize ysize) #:transparent)
(struct pen-expr (value) #:transparent)
(struct line-expr (x1 y1 x2 y2) #:transparent)

;; numeric expressions
(struct numeric-expr (value) #:transparent)

;; var identifiers
(struct var-expr (name) #:transparent)

; paper location structs
(struct set-paper-loc (x y color) #:transparent)
(struct get-paper-loc (x y) #:transparent)
(struct antialias-expr (value) #:transparent)

; used to indicate when a variable is created the first time
(struct create-var-expr (name e1) #:transparent)
; set, which works on variables
(struct assignment-expr (e1 e2) #:transparent)


; iterations
(struct repeat-expr (var start end body) #:transparent)
(struct forever-expr (body) #:transparent)

; predicate expressions
(struct same-expr (e1 e2 body) #:transparent)
(struct not-same-expr (e1 e2 body) #:transparent)
(struct smaller-expr (e1 e2 body) #:transparent)
(struct not-smaller-expr (e1 e2 body) #:transparent)

; structs for defining functions and procedures,
; these have a name a list of parameters and a body
(struct command-fun (name params body) #:transparent)
(struct number-fun (name params body) #:transparent)

; this is the equivalent of a 'return v' in another language
(struct value-expr (value) #:transparent)

; just prints to the standard output
(struct print-expr (value) #:transparent)

; function application, empty for commands
(struct apply-expr (fun-name params) #:transparent)

; things related to the external world, time, mouse, etc
(struct mouse-expr (value) #:transparent)
(struct key-expr (value) #:transparent)
(struct time-expr (value) #:transparent)
(struct bitmap-expr () #:transparent)

; compound expressions
(struct add-expr (e1 e2) #:transparent)
(struct sub-expr (e1 e2) #:transparent)
(struct mult-expr (e1 e2) #:transparent)
(struct div-expr (e1 e2) #:transparent)

; loading
(struct load-expr (filename) #:transparent)

; represents an entire program, i.e., a list of statements
(struct program (statements) #:transparent)

; turn an ast into an s-expression for the reader
(define (ast->sexp ast)
  (match ast
    ; just map the statements into sexpressions
    [(program statements) (cons 'program-command (map ast->sexp statements))]
    ; numbers are numbers (data)
    [(numeric-expr num) num]
    ; symbols are symbols, racket will handle these later
    [(var-expr sym) sym]

    ; turn commands into, well commands that we'll handle with
    ; macros by expanding them into actual racket code
    [(paper-expr val xsize ysize) (list 'paper-command
                                        (ast->sexp val)
                                        (ast->sexp xsize)
                                        (ast->sexp ysize))]
    [(pen-expr val) (list 'pen-command (ast->sexp val))]
    [(line-expr x1 y1 x2 y2) (list 'line-command
                                   (ast->sexp x1)
                                   (ast->sexp y1)
                                   (ast->sexp x2)
                                   (ast->sexp y2))]
    
    ; math expressions
    [(add-expr x y)  (list 'add-expr (ast->sexp x) (ast->sexp y))]
    [(sub-expr x y)  (list 'sub-expr (ast->sexp x) (ast->sexp y))]
    [(mult-expr x y) (list 'mult-expr (ast->sexp x) (ast->sexp y))]
    [(div-expr x y)  (list 'div-expr (ast->sexp x) (ast->sexp y))]
    
    ; we can test stuff using print!
    [(print-expr e)  (list 'print-command (ast->sexp e))]

    ; var expressions become simple lists of the var-expr and its name
    [(var-expr name) (list 'var-expr name)]

    ; sets a point on the screen 
    [(set-paper-loc x y col) (list 'set-pixel-command
                                   (ast->sexp x)
                                   (ast->sexp y)
                                   (ast->sexp col))]

    ; gets a point on the screen
    [(get-paper-loc x y) `(get-pixel-expr ,(ast->sexp x) ,(ast->sexp y))]
    
    ; sets a var to a new value
    [(assignment-expr sym expr) (list 'assignment-command sym (ast->sexp expr))]
    ; creates and sets the variable to a value
    [(create-var-expr sym expr) (list 'create-var-command sym (ast->sexp expr))]

    ; an apply expression, which just creates a list of the apply-expr, the function name,
    ; a mapping of parameters (using ast-sexp) so that the parameters will be translated also
    [(apply-expr fun-name params) (cons 'apply-expr (cons fun-name (map ast->sexp params)))]

    ; a command expression (doesn't return anything), which smashes all of these together
    ; into a single list, but it will have a couple of sublists: the params and the body
    [(command-fun name params body) (list 'command-def name params (map ast->sexp body))]

    ; a number expression (which returns values, aka numbers), and puts all these together
    ; into a single list (though it will consist of a param and body sublist)
    [(number-fun name params body) (list 'number-def name params (map ast->sexp body))]
    [(value-expr val) (list 'value-command (ast->sexp val))]

    ; a load expression which just returns a load-command to be expanded later
    [(load-expr filename) (list 'load-command filename)]

    ; comparison statements, same expression becomes a list of the symbol,
    ; its arguments and a list of the statements composing it
    [(same-expr e1 e2 body) (list 'same-command (ast->sexp e1) (ast->sexp e2)
                                  (map ast->sexp body))]
    ; not same expression becomes a list of the symbol,
    ; its arguments and a list of the statements composing it
    [(not-same-expr e1 e2 body) (list 'not-same-command (ast->sexp e1) (ast->sexp e2)
                                      (map ast->sexp body))]
    ; smaller expression becomes a list of the symbol,
    ; its arguments and a list of the statements composing it
    [(smaller-expr e1 e2 body) (list 'smaller-command (ast->sexp e1) (ast->sexp e2)
                                     (map ast->sexp body))]
    ; not smaller expression becomes a list of the symbol,
    ; its arguments and a list of the statements composing it
    [(not-smaller-expr e1 e2 body) (list 'not-smaller-command (ast->sexp e1) (ast->sexp e2)
                                         (map ast->sexp body))]


    ; turn the repeat struct into an sexp
    [(repeat-expr var start end body) (list 'repeat-command var
                                            (ast->sexp start) (ast->sexp end)
                                            (map ast->sexp body))]

    ; turn the forever struct into a forever sexp to be macro expanded
    [(forever-expr body) (list 'forever-command (map ast->sexp body))]

    ; mouse, key and time expressions are simple to convert into s-expressions
    [(mouse-expr val) (list 'mouse-expr (ast->sexp val))]
    [(key-expr val) (list 'key-expr (ast->sexp val))]
    [(time-expr val) (list 'time-expr (ast->sexp val))]

    ; handle antialias
    [(antialias-expr val) (list 'antialias-command (ast->sexp val))]
    
    ; throw an error so we know we have to fix it
    [_ (error "This expression kind isn't implemented yet in ast->sexp" ast)]
    ))




;;; foldl-and-map simultaneously maps and foldls on a list of statements, note that
;;; just like a map or foldl, however, a function has to decide if it needs to recurse
;;; on its structure since we can't determine how to handle the environment here
(define (foldl-and-map fun acc statements)
  ;; acc is of course the accumulator, but see the note below about the accumulator
  (define (foldl-and-map-helper acc lst)
    (cond
      ; our accumulator will be a pair where the first part is something and the
      ; 2nd part is the list of statements from the mapping part function
      [(eq? lst null) acc]
       
      ; now grab the next statement
      [else (let ([statement (first lst)]
                  [statement-list (cdr acc)])
              ; call fun on the statement, which should give us two values, the new environment
              ; and some list of statements  
              (let-values ([(newenv res-statements) (fun (car acc) statement)])
                ; now if you get a list of statements, then append them to the statement list
                ; and then create a new accumulator from the new environment and this list of statements
                (let ([new-acc (cons newenv (if (list? res-statements)
                                                (append res-statements statement-list)
                                                (cons res-statements statement-list)))])
                  ; recursively continue with this result
                  (foldl-and-map-helper new-acc (rest lst)))))]))
  ; and finally get it all started, so this will return the accumulator in the end, where it's
  ; a pair: the first part is the accumulated value (the environment) and the 2nd part is the list of statements
  (foldl-and-map-helper acc statements))


; this function takes a list of statements and returns a pair where the car of the pair
; is the set of assignments found in the statements, and the cdr of the pair is the set of
; statements without the assignments
(define (split-assignments statements)
  (let ([assignments (filter (λ (statement) (create-var-expr? statement)) statements)]
        [removed-assignments (filter (λ (statement) (not (create-var-expr? statement))) statements)])
    (values assignments removed-assignments)))


; this function looks for var-create-expr inside the body of a block statement, like smaller?,
; and then extends the environment if it finds *new* bindings. The purpose is really to deal with
; the scoping issue that DBN has (blocks are meaningless except for functions)
(define (lift-var-create env body constructor)
  (let* ([acc (foldl-and-map transform-assignment-expression (cons env null) body)]
       ; from the foldl-and-map, we'll get a new set of statements (possibly) for the body, and we need to pull
       ; out any create-var-commands
            [newbody (reverse (cdr acc))])
       (let-values ([(creates without-creates) (split-assignments newbody)])
         ; move the creation of vars up a level before this statement
         (let* ([new-statement (list (constructor without-creates))]
                [new-creates (filter (λ (create-stm)
                                            (match create-stm
                                              [(create-var-expr sym _)
                                               (if (apply-env env sym)
                                                   #f
                                                   #t)])) creates)]
                [new-stms (append new-statement new-creates)]
                [new-env (foldl (λ (var env)
                                  (match var
                                    [(create-var-expr sym v)
                                     (if (apply-env env sym)
                                         env
                                         (extend-env env sym v))]
                                    [_ (error "unexpected kind when lifting create-vars")])) env new-creates)])
         (values new-env new-stms)))))


; find the exported symbols from a module...is that a thing?

; function that transforms an assignment-expression into either a new variable creation
; kind of statement or an actual assignment to the existing variable
(define (transform-assignment-expression env statement)
  (match statement
    ; see if it's an assignment expression
    [(assignment-expr sym expr)
     (cond
       ; if apply-env does not return false, then we just return the current environment and statement,
       ; in other words, it's okay to be a set
       [(apply-env env sym) (values env statement)]
       [else
        ; first time we've seen it, so extend the environment
        (let ([newenv (extend-env env sym #t)])
          ; and now generate a create-var-expr followed by the actual assignment (here we init to 0)
          (values newenv (list statement (create-var-expr sym (numeric-expr 0)))))])]
    ; many of the statements can have blocks, so we need to recursively deal with these
    [(repeat-expr var start end body)
     ; recurse into the structure, but extend the environment with var so it's visible (and we don't redefine it)
     (let ([acc (foldl-and-map transform-assignment-expression (cons (extend-env env var #t) null) body)])
       (values env (repeat-expr var start end (reverse (cdr acc)))))]
    ; recurse into forever
    [(forever-expr body)
     (let ([acc (foldl-and-map transform-assignment-expression (cons env null) body)])
       (values env (forever-expr (reverse (cdr acc)))))]
    ; now the comparisons, same
    [(same-expr e1 e2 body)
     (lift-var-create env body (λ (newbody) (same-expr e1 e2 newbody)))]
    ; then not-same
    [(not-same-expr e1 e2 body)
     (lift-var-create env body (λ (newbody) (not-same-expr e1 e2 newbody)))]
    ; handle smaller? expressions
    [(smaller-expr e1 e2 body)
     (lift-var-create env body (λ (newbody) (smaller-expr e1 e2 newbody)))]
    ; and not-smaller expressions
    [(not-smaller-expr e1 e2 body)
     (lift-var-create env body (λ (newbody) (not-smaller-expr e1 e2 newbody)))]

    ; handle commands, this means extending their environments with parameters when dealing with the bodies
    [(command-fun name params body)
     (let ([acc (foldl-and-map transform-assignment-expression
                               (cons (extend-env-with-pairs (map (λ (param) (cons param #t)) params) env) null)
                               body)])
       ; assignment doesn't really have anything to do with function calls, so we don't need to extend the
       ; returned environment (this would be handled in a different function)
       (values env (command-fun name params (reverse (cdr acc)))))]

    ; handle numbers in a similar fashion
    [(number-fun name params body)
     (let ([acc (foldl-and-map transform-assignment-expression
                               (cons (extend-env-with-pairs (map (λ (param) (cons param #t)) params) env) null)
                               body)])
       (values env (number-fun name params (reverse (cdr acc)))))]
       
    ;[(command-fun name args body) ...]
    [_ (values env statement)]))


; transform-program: fun (with type: statement -> env -> statement(s)) -> program -> program
; transforms the assignment statements in a program by calling
; fun on each statement in the program to transform it as needed.
(define (transform-program fun prog)
  ; get the list of statements
  (let* ([statements (program-statements prog)]
         [transformed-statements
          ; call foldl-and-map to transform the program
          (foldl-and-map fun (cons (empty-env) null) statements)])               
    ; notice that we have to reverse the list at the end
    ; because of the way we're cons'ing up above
    (program (reverse (cdr transformed-statements)))))
                               

; We have a couple of places where a call to several functions with arguments may be mistaken for a single
; function call. This repairs those issues. At this point, we have an AST so we'll know which identifiers
; are either function calls or just variable names and we can split these into multiple lines. 
(define (transform-application-expression env statement)
  (match statement
    ; apply begins with a function name and is followed by args, which must be resolved
    ; before we call this function (eager evaluation)
    [(apply-expr name args)
     (define (apply-splitter fname args)
       ; otherwise, split on closures 
       (let-values ([(first-part tail)
                     (splitf-at args
                                (λ (arg)
                                  ; look for var-exprs and see if one of these is actually a function
                                  (match arg
                                    [(var-expr sym)
                                     ; if it's a var, let's see what kind it is
                                     (let ([res (apply-env env sym)])
                                       ; return true if it's a closure, meaning split here
                                       (not (closure? res)))]
                                    [_ #t])))])
         ; now, create a new apply expression only up to the closure
         (cons (apply-expr fname first-part)
               ; recursively split
               (if (null? tail)
                   null
                   (apply-splitter (var-expr-name (first tail)) (rest tail))))))
     ; and now actually apply it, which should result in a list
     (values env (apply-splitter name args))]

    ; we want to match on functions, so in essence we need to create a closure here for the environment
    [(command-fun name args body)
     ; create a closure based on the current static environment
     (let* ([new-closure (closure name args body env)]
            ; and extend the environment with this closure from this point on
            [newenv (extend-env env name new-closure)]
            ; now make sure we recursively handle the body, which we do with foldl-and-map,
            ; notice that it requires an accumulator that contains the environment and some initial set of
            ; statements, which will be filled by the function
            [new-body
             (reverse (cdr (foldl-and-map transform-application-expression (cons newenv null) body)))])
       (values newenv (command-fun name args new-body)))]

      
    ; default (if we pass it on something not a statement) is to just return the environment and statement
    [_ (values env statement)]))
             

; compose list takes a function list, applies the first function to arg, then the second function
; to the result of arg, and so forth, until we're done with functions
(define (compose-list fun-list arg)
  ; base case, just return the final arg
  (if (null? fun-list)
      arg
      ; recursive case, compose list with the rest of the functions on the result
      ; of the first function applied to the argument
      (compose-list (rest fun-list) ((first fun-list) arg))))



; perform all of the transformations on a given program by applying the functions in fun-list
; to them. fun-list should be a list of functions that take a statement and environment
; to process each statement accordingly. This function then calls transform-program on each
; program using the given function.
(define (transform-ast prog fun-list)
  (let ([transformations
         ; map all the functions to a new function that takes a single argument (the program)
         ; and calls transform-program using that function and passing the program to it
         (map (λ (fun)
                (λ (p) (transform-program fun p))) fun-list)])
    ; now compose all of those transformations till we end up with the final program
    (compose-list transformations prog)))


; just a short-cut to do all the transformations
(define (transform-all prog)
  (transform-ast prog (list
                       transform-assignment-expression
                       transform-application-expression)))