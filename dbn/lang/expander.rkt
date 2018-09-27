#lang racket
(require (for-syntax syntax/parse)
         "papersim.rkt")


; this defines the macro expander for the module,
; which begins with #%module-begin. We assume
; we just pass in the parse tree, which is really a
; giant syntax object from the parser
(define-syntax-rule (dbn-module-begin PARSE-TREE)
  (#%module-begin
   ; this causes do-setup! to be run prior to the
   ; module, and so we can setup the REPL to work with arith
   (module configure-runtime racket/base
     (require dbn/configure-runtime)
     (configure #f))

   ;(provide #%top #%app (rename-out (dbn-datum #%datum)) (rename-out (dbn-top-interaction #%top-interaction)))
   PARSE-TREE))


; rename dbn-module-begin on the way out to #%module-begin, which racket expects
(provide (rename-out [dbn-module-begin #%module-begin])
         ; and provide #%datum, which is needed for literals, #%top, which is needed for
         ; top-level definitions (and in particular for require to work)
         #%datum #%top)

; This seems to be the way to define #%top-interaction, at least
; according to https://docs.racket-lang.org/reference/__top-interaction.html
; and this is how racket expands it by default (we could change this, for
; example). In our case, this just expands to EXP, and then other macros
; can act on it, so like term, for example
(define-syntax-rule (dbn-top-interaction . EXP)
   EXP)
(provide (rename-out (dbn-top-interaction #%top-interaction)))


; Program-command is a macro to expand an entire DBN program
(define-syntax program-command
  (syntax-rules ()
    ; start with running the paper sim, and end with refreshing
    [(program-command statement ...)
     (begin
       (run-paper-sim)
       statement ...
       (dbn-refresh)
       ; tests whether the window should automatically exit or not (by default it pauses)
       (dbn-maybe-pause))]))
(provide program-command)

; Paper-command is a macro which creates the Racket code to create the blank Paper in DBN.
(define-syntax paper-command
  (syntax-rules ()
    [(paper-command color-expr xsize ysize)
     ; make sure to pass in a dbn color!
     (clear-paper (dbncolor color-expr))]))
(provide paper-command)

; Pen-command is a macro which creates the Racket code to create a Pen with a color in DBN.
(define-syntax pen-command
  (syntax-rules ()
    [(pen-command color-expr)
     ; make sure to pass in a dbcolor!
     (set-pen-color! (dbncolor color-expr))]))
(provide pen-command)

; Line-command is a macro which creates the Racket code to draw a line in DBN.
(define-syntax line-command
  (syntax-rules ()
    [(line-command x1-expr y1-expr x2-expr y2-expr)
                   (draw-line x1-expr y1-expr x2-expr y2-expr)]))
(provide line-command)


; Set-pixel-command is a macro which creates the Racket code to set a location to a color in DBN.
(define-syntax set-pixel-command
  (syntax-rules ()
    [(set-pixel-command x-expr y-expr col-expr)
                   (draw-point x-expr y-expr (dbncolor col-expr))]))
(provide set-pixel-command)

; get-pixel-expr is a macro which creates the Racket code to retrieve a pixel color in DBN
(define-syntax get-pixel-expr
  (syntax-rules ()
    [(get-pixel-expr x-expr y-expr)
     (get-pixel-color x-expr y-expr)]))
(provide get-pixel-expr)

; assignment-command is a macro which creates the Racket code to set a variable to a value
(define-syntax assignment-command
  (syntax-rules ()
    [(assignment-command sym expr) (set! sym expr)]))
(provide assignment-command)


; set-var-command is a macro which creates the Racket code to define a new variable and assign a value to it
(define-syntax create-var-command
  (syntax-rules ()
    [(set-var-command sym expr) (define sym expr)]))
(provide create-var-command)


; add-expr is a macro which creates the Racket code to add two numbers in DBN.
(define-syntax add-expr
  (syntax-rules ()
    [(add-expr x-expr y-expr)
                   (+ x-expr y-expr)]))
(provide add-expr)

; sub-expr is a macro which creates the Racket code to subtract two numbers in DBN.
(define-syntax sub-expr
  (syntax-rules ()
    [(sub-expr x-expr y-expr)
                   (- x-expr y-expr)]))
(provide sub-expr)

; mult-expr is a macro which creates the Racket code to multiply two numbers in DBN.
(define-syntax mult-expr
  (syntax-rules ()
    [(mult-expr x-expr y-expr)
                   (* x-expr y-expr)]))
(provide mult-expr)

; div-expr is a macro which creates the Racket code to divide two numbers in DBN.
(define-syntax div-expr
  (syntax-rules ()
    [(div-expr x-expr y-expr)
                   (/ x-expr y-expr)]))
(provide div-expr)

; print-statement is the Print statement in DBN that calls display on its expression
(define-syntax print-command
  (syntax-rules ()
    [(print-command expr) (display expr)]))
(provide print-command)

; var-expr is a variable expression in DBN--ie, a "read" of the variable and expands to simply a name
(define-syntax var-expr
  (syntax-rules ()
    [(var-expr name) name]))
(provide var-expr)

; apply-expr is an application of a function to some arguments in DBN, and expands to Racket's application
(define-syntax apply-expr
  (syntax-rules ()
    ; look for the apply-expr, this assumes we've done a pass to separate out applications properly
    [(apply-expr fun-name param ...)
     ; then we use Racket's #%app form to call the function with the parameters
     (#%app fun-name param ...)]))
(provide apply-expr)


; defines load-command, which is the Load command from DBN and expands into a require. The cool
; thing about Racket is that it expands the require first before evaluation, so we don't have to
; worry about these being in some weird location (like at the end)--and even if it is, it'll still work
(define-syntax load-command
  (syntax-rules ()
    [(load-command file) (require file)]))
(provide load-command)


; command-def is a definition of a command in DBN, and it shouldn't return anything. This is kinda odd
; in Racket because racket by default uses expressions, so every function can easily return something.
; To make this work so that we don't accidentally get return values, we use (void) at the end of
; a list of the statements that belongs to a command-def
(define-syntax command-def
  (syntax-rules ()
    ; look for command-def
    [(command-def name params (body ...))
     ; then expand this into a define value with a lambda--note params is a sublist whereas we
     ; break apart the body because it has multiple things in it
     (begin (define name (λ params body ... (void)))
            ; also note that we have to do a provide here, to be able to export this name
            ; and use it in other files if we do a Load on it
            (provide name))]))
(provide command-def)

(require racket/stxparam)


; define the parameter
(define-syntax-parameter value-command
  (λ (stx)
    (raise-syntax-error 'Value "Syntax error: You can only use Value within a Number definition.")))
    

; number-def is a definition of a "number" in DBN, which is a function that returns a number value.
; in theory, this ends with "value" which is when the function exits
(define-syntax number-def
  (syntax-rules ()
    ; look for command-def
    [(number-def name params (body ...))
     ; then expand this into a define value with a lambda--note params is a sublist whereas we
     ; break apart the body because it has multiple things in it
     (begin (define name (λ params
                           ; here we use a continuation so we can jump out of this number-def anywhere
                           (call/ec (λ (return-k)
                                      (syntax-parameterize ([value-command
                                                             (syntax-rules ()
                                                               [(_ val) (return-k val)])])
                                        body ...)))))
            ; also note that we have to do a provide here, to be able to export this name
            ; and use it in other files if we do a Load on it
            (provide name))]))
(provide number-def)



; value-command is basically a 'return' in DBN
;(define-syntax value-command
;  (syntax-rules ()
;    ; we expand to what would have been a return continuation,
;    ; note that we're in trouble if we expand to this outside of a number-def
;    [(value-command v) (return-k v)]))
(provide value-command)


; macro to expand the same-command sexp into valid racket code in DBN
(define-syntax same-command
  (syntax-rules ()
    ; look for the same command and expand it to a when, which will execute the body
    ; when the test condition is true, in this case, when (= e1 e2)
    [(same-command e1 e2 (body ...))
     (when (= e1 e2)
       body ...)]))
(provide same-command)
     
; macro to expand the not-same-command sexp into valid racket code in DBN
(define-syntax not-same-command
  (syntax-rules ()
    ; look for the not-same command and expand it to a when, which will execute the body
    ; when the test condition is true, in this case, when (not (= e1 e2))
    [(not-same-command e1 e2 (body ...))
     (when (not (= e1 e2))
       body ...)]))
(provide not-same-command)

; macro to expand the smaller-command sexp into valid racket code in DBN
(define-syntax smaller-command
  (syntax-rules ()
    ; look for the smaller command and expand it to a when, which will execute the body
    ; when the test condition is true, in this case, when (< e1 e2)
    [(smaller-command e1 e2 (body ...))
     (when (< e1 e2)
       body ...)]))
(provide smaller-command)

; macro to expand the not-smaller-command sexp into valid racket code in DBN
(define-syntax not-smaller-command
  (syntax-rules ()
    ; look for the not-smaller command and expand it to a when, which will execute the body
    ; when the test condition is true, in this case, when (not (< e1 e2))
    [(not-smaller-command e1 e2 (body ...))
     (when (not (< e1 e2))
       body ...)]))
(provide not-smaller-command)

; macro that expands antialias into a call to set-antialias
(define-syntax antialias-command
  (syntax-rules ()
    [(antialias-command v)
     (set-antialias v)]))
(provide antialias-command)

#|
(let* ([start (eval-expr env from)] ; evaluate the start value
       [end (eval-expr env to)]     ; then the ending value
       [newenv (extend-env env sym start)]
       [ref (apply-env newenv sym)])
   (letrec ([loop (λ () (cond [(<= (deref ref) end)                                   
                             (eval-statements newenv body slow?)
                                   (setref! ref (add1 (deref ref)))
                                   ;(printf "repeat from ~a to ~a~n" (deref ref) end)
                                   (dbn-refresh)
                                   (loop)]))])
         (loop)
|#

; macro for the repeat command, corresponding to DBN Repeat, which uses a loop in Racket
(define-syntax repeat-command
  (syntax-rules ()
    ; match against the repeat command in this form
    [(repeat-command var s e (body ...))
     ; define the variable to be used in the body, and 
     (let ([var s]
           [end e])
       ; define a loop that simulates the repeat command
       (letrec ([loop (λ ()
                        (cond
                          ; execute the body only if var is less than or equal to the end value
                          [(<= var end)
                           ; execute the statements
                           body ...
                           ; update our counter
                           (set! var (+ var 1))
                           ; to refresh or not to refresh, that is the question,
                           ; as too much refreshing may cause flickering!
                           #;(dbn-refresh)
                           ; recursively call the loop!
                           (loop)]))])
         ; start things off
         (loop)))]))
(provide repeat-command)

; macro to expand forever-command into a DBN Forever command, which
; will be an infinite loop in Racket
(define-syntax forever-command
  (syntax-rules ()
    [(forever-command (body ...))
     ; I think we need at least one refresh here, so we'll loop, execute the instructions
     ; in the body of the loop, and refresh at the end before starting over
     (letrec ([loop (λ () body ... (dbn-refresh) (loop))])
       (loop))]))
(provide forever-command)

; macro to expand the mouse expressions, which corresponds to the DBN Mouse expression
(define-syntax mouse-expr
  (syntax-rules()
    ; mouse command in DBN
    [(mouse-expr v)
     (let ([val v])
       (cond
         ; requires a value that evaluates to 1, 2, or 3, where 1 = x position,
         ; 2 = y position, and 3 is mouse button
         [(= val 1) (get-mouse-x)]
         [(= val 2) (get-mouse-y)]
         [(= val 3) (get-mouse-button)]
         [(error "mouse expects a value of 1, 2, or 3.")]))]))
(provide mouse-expr)

; macro to expand time expressions to work like DBN Time expressions do
(define-syntax time-expr
  (syntax-rules ()
    [(time-expr v)
     (let ([val v])
       (cond [(= val 1) (get-time 'hour)]
             [(= val 2) (get-time 'minutes)]
             [(= val 3) (get-time 'seconds)]
             [(= val 4) (get-time 'milliseconds)]
             [else (error "Expected a Time range from 1 to 4 inclusive, got " val " instead")]))]))
(provide time-expr)

; macro to expand key expressions to work like DBN Key expressions do (which is
; to read a value from 1 to 26 as a keypress)
(define-syntax key-expr
  (syntax-rules ()
    [(key-expr v)
     (let ([val v])
       (cond [(and (>= val 1) (<= val 26)) (get-key val)]
             [else (error "Expected a key range from 1 to 26 inclusive, got " val " instead.")]))]))
(provide key-expr)    
                
