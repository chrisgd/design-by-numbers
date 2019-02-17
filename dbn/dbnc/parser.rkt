#lang racket/base

(require racket/list)

(require parser-tools/yacc
         (prefix-in lex: parser-tools/lex)
         "errors.rkt"
         "lexer.rkt"
         "ast-nodes.rkt"
         "annotations.rkt"
         "env.rkt")

(provide (all-defined-out))

(define DEFAULT-PAPER-SIZE 100)


(define keyword-list
  (list
   (token-PAPER)
   (token-PEN)
   (token-SET)
   (token-LINE)
   (token-REPEAT)
   (token-FOREVER)
   (token-SAME)
   (token-NOTSAME)
   (token-SMALLER)
   (token-NOTSMALLER)
   (token-MOUSE)
   (token-LOAD)
   (token-NUMBER)
   (token-VALUE)
   (token-KEY)
   (token-NET)
   (token-TIME)
   (token-PRINT)
   (token-COMMAND)
   (token-ANTIALIAS)
   (token-NEWLINE)
   (token-BITMAP)))

(define (keyword? tok)
  (ormap (λ (keyword) (eq? tok keyword)) keyword-list))

(define (make-dbn-parser)
  (let* ([notes (make-hasheq)]
         [a-parser
          (parser
           (src-pos)
           (start prog)
           (end EOF)
           (tokens names-and-values end-of-file keywords math-operators parentheses)
           (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                    ; indicate a parsing error occurred
                    (parser-error #t)
                    ; and print an error
                    ; (unless (and (eq? tok-ok? #t) (eq? tok-name 'EOF))
                    (printf "Parsing error at line ~a, col ~a: token: ~a, value: ~a, tok-ok? ~a~n~a~n"
                            (lex:position-line start-pos)
                            (lex:position-col start-pos)
                            tok-name
                            tok-value
                            tok-ok? 
                            (if (keyword? tok-name)
                                (format "You may have used a reserved keyword name, i.e., '~a' for a variable,~n  or you didn't use enough arguments for the previous command." tok-name)
                                ""))))

                      
           ; because our function calls are really ID ID ... ID, we make identifiers right
           ; associative, so that they attach more identifiers to the right by default
           (precs (right IDENTIFIER))
           (expected-SR-conflicts 0)
           (grammar
            (prog
             ; this is needed to handle empty programs
             ;[() (program null)]
             [(maybe-newlines) (make-program null notes)]     
             ; otherwise, a program is just a list of statements
             [(maybe-newlines statements) (make-program (flatten (filter (λ (el) (not (null? el))) $2)) notes)])
            
            ; this is to deal with newlines
            (maybe-newlines
             [() null]
             [(NEWLINE maybe-newlines) $2])
            
            ; this ensures at least one newline is seen
            (at-least-one-newline
             [(NEWLINE) null]
             [(NEWLINE at-least-one-newline) $2])
            
            ; lists of statements
            (statements     
             [(statement maybe-newlines) (list $1)]
             [(statement at-least-one-newline statements) (cons $1 $3)])
            
            (statement
             ; a statement is one of these many things
             [(PAPER expr) (paper-expr $2 (numeric-expr DEFAULT-PAPER-SIZE) (numeric-expr DEFAULT-PAPER-SIZE))]
             [(PAPER expr expr expr) (paper-expr $2 $3 $4)]
             [(PEN expr) (pen-expr $2)]
             [(PRINT expr) (print-expr $2)]
             [(SET IDENTIFIER expr) (assignment-expr $2 $3)]
             [(SET LEFTBRACKET expr expr RIGHTBRACKET expr) (set-paper-loc $3 $4 $6)]
             [(LINE expr expr expr expr) (line-expr $2 $3 $4 $5)]
             [(VALUE expr) (value-expr $2)]
             [(ANTIALIAS expr) (antialias-expr $2)]

             ; statements that have blocks for bodies
             [(REPEAT IDENTIFIER expr expr maybe-newlines block) (repeat-expr $2 $3 $4 $6)]
             [(FOREVER maybe-newlines block) (forever-expr $3)]
     
             ; now for the predicates and comparisons
             [(SAME expr expr maybe-newlines block) (same-expr $2 $3 $5)]
             [(NOTSAME expr expr maybe-newlines block) (not-same-expr $2 $3 $5)]
             [(SMALLER expr expr maybe-newlines block) (smaller-expr $2 $3 $5)]
             [(NOTSMALLER expr expr maybe-newlines block) (not-smaller-expr $2 $3 $5)]

             ; and then functions and command definitions
             [(COMMAND IDENTIFIER maybe-newlines block) (command-fun $2 null $4)]
             [(NUMBER IDENTIFIER maybe-newlines block) (number-fun $2 null $4)]
             [(COMMAND IDENTIFIER parameters maybe-newlines block) (command-fun $2 $3 $5)]
             [(NUMBER IDENTIFIER parameters maybe-newlines block) (number-fun $2 $3 $5)]

             ; loads, but we'll ignore this for now
             [(LOAD FILENAME) (load-expr $2)]
     
             ; function application: note that we can't tell at this phase which
             ; are the functions and which aren't: we do this in a later phase after
             ; building the AST, this way we can just ignore newlines for now
             [(IDENTIFIER exprs) (apply-expr $1 $2)]
             [(IDENTIFIER) (apply-expr $1 null)]

             [(block) $1]
             )
    
            ; keyword list, mainly used for error checking
            (keyword
             [(PAPER) ('Paper)]
             [(PEN) ('Pen)]
             [(LINE) ('Line)]
             [(SET) ('Set)]
             [(REPEAT) ('Repeat)]
             [(FOREVER) ('Forever)]
             [(SAME) ('Same)]
             [(NOTSAME) ('NotSame)]
             [(SMALLER) ('Smaller)]
             [(NOTSMALLER) ('NotSmaller)]
             [(MOUSE) ('Mouse)]
             [(LOAD) ('Load)]
             [(VALUE) ('Value)]
             [(KEY) ('Key)]
             [(NET) ('Net)]
             [(TIME) ('Time)]
             [(PRINT) ('Print)]
             [(COMMAND) ('Command)]
             [(NUMBER) ('Number)]
             [(ANTIALIAS) ('Antialias)]
             [(BITMAP) ('Bitmap)])
    
            ; a block will simply return a list of statements, not a special struct
            (block
             [(LEFTBRACE maybe-newlines statements RIGHTBRACE)
              ;(LEFTBRACE statements RIGHTBRACE)
              (flatten (filter (λ (el) (not (null? el))) $3))])

    
            ; parameters are names used for a function
            (parameters
             [(IDENTIFIER) (list $1)]
             [(IDENTIFIER parameters) (cons $1 $2)])

            ; lists of expressions? Really only useful for fun calls
            (exprs
             [(expr) (list $1)]
             [(expr exprs) (cons $1 $2)])
        
            (expr
             [(expr ADDITION term) (add-expr $1 $3)]
             [(expr SUBTRACTION term) (sub-expr $1 $3)]
             [(term) $1])

            (maybe-exprs
             [() null]
             [(exprs) $1])
    
            (term
             [(term MULTIPLICATION factor) (mult-expr $1 $3)]
             [(term DIVISION factor) (div-expr $1 $3)]
             [(factor) $1])

            (factor
             [(rvalues) $1]
             [(LEFTPARENTHESIS expr RIGHTPARENTHESIS) $2])

            (rvalues
             [(NUMERICVALUE) (numeric-expr $1)]
             [(IDENTIFIER) (begin
                             (let ([e (var-expr $1)])
                               (add-note notes e 'position $1-start-pos)
                               e))]
             [(LEFTBRACKET expr expr RIGHTBRACKET) (get-paper-loc $2 $3)]
             [(LESSTHAN TIME expr GREATERTHAN) (time-expr $3)]
             [(LESSTHAN MOUSE expr GREATERTHAN) (mouse-expr $3)]
             [(LESSTHAN KEY expr GREATERTHAN) (key-expr $3)]
             [(LESSTHAN IDENTIFIER maybe-exprs GREATERTHAN) (apply-expr $2 $3)]
             [(LESSTHAN BITMAP GREATERTHAN) (bitmap-expr)])
    
            ))])
    ; return the parser
    a-parser))

;; this function will parse from any input port and
;; return an ast (a program struct)
(define (parse in)
  (port-count-lines! in)
  ((make-dbn-parser) (get-tokenizer in)))

;; this function parses a string and returns a program struct
(define (parse-str str)
  (let ([in (open-input-string str)])
    (parse in)))

;; this function opens a file, parses it, and returns a program struct
(define (parse-file filename)
  (let ([in (open-input-file filename)])
    (parse in)))

;; tests for the grammar module
(module+ test
  (require rackunit)
  (require rackunit/text-ui)

  (define-simple-check (program-equal? msg p1 p2)
    (test-equal? msg (program-statements p1) (program-statements p2)))
  
  (program-equal? "Grammar test for Paper" (parse-str "Paper 100\n")
               (make-program (list (paper-expr (numeric-expr 100) (numeric-expr 100) (numeric-expr 100)))))
  (program-equal? "Grammar test for Pen" (parse-str "Pen 100\n")
               (make-program (list (pen-expr (numeric-expr 100)))))
  (program-equal? "Grammar test for Line" (parse-str "Line 0 0 100 100\n")
               (make-program (list (line-expr (numeric-expr 0) (numeric-expr 0) (numeric-expr 100) (numeric-expr 100)))))
  (program-equal? "Grammar test for Set" (parse-str "Set X (50 * 25)\n")
               (make-program (list (assignment-expr 'X (mult-expr (numeric-expr 50) (numeric-expr 25))))))
  (program-equal? "Grammar test for Repeat" (parse-str "Repeat X 0 100\n{\nPrint X\n}\n")
               (make-program (list (repeat-expr 'X (numeric-expr 0) (numeric-expr 100) (list (print-expr (var-expr 'X)))))))
  (program-equal? "Grammar test for var expression" (parse-str "Set A X\n")
               (make-program (list (assignment-expr 'A (var-expr 'X))))))



