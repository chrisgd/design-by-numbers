#lang racket

(require parser-tools/lex "dbn-lexer.rkt")
(provide dbn-colorer)

; create a color based on our lexer 
(define (dbn-colorer port)
  ; function to catch the lexer errors
  (define (handle-lexer-error err)
    ; grab the source location from the error
    (let ([err-srclocs (exn:fail:read-srclocs err)])
      ; and return an error token
      (position-token (token-ERROR) (first err-srclocs) (first err-srclocs))))
  (define srcloc-tok
    (with-handlers ([exn:fail:read? handle-lexer-error])
      (lex port)))
  (match srcloc-tok
    [(? eof-object?) (values srcloc-tok 'eof #f #f #f)]
    [(position-token tok (position st bline bcol) (position en eline ecol))
     (let-values ([(kind paren)
                   (match (token-name tok)
                     ['NUMERICVALUE (values 'no-color #f)]
                     ['IDENTIFIER (values 'no-color #f)]
                     ['FILENAME (values 'no-color #f)]
                     ['FUNNAME (values 'no-color #f)]
                     ['COMMENT (values 'comment #f)]
                     ['LEFTPARENTHESIS (values 'parenthesis '|(|)]
                     ['RIGHTPARENTHESIS (values 'parenthesis '|)|)]
                     ['LESSTHAN (values 'parenthesis '|(|)]
                     ['GREATERTHAN (values 'parenthesis '|)|)]
                     ['LEFTBRACE (values 'parenthesis '|{|)]
                     ['RIGHTBRACE (values 'parenthesis '|}|)]
                     ['LEFTBRACKET (values 'parenthesis '|[|)]
                     ['RIGHTBRACKET (values 'parenthesis '|]|)]
                     ['ERROR (values 'error #f)]
                     [else (values 'no-color #f)])])
       (values (token-value tok) kind paren st en))]
    [else (values 'no-color #f)]))
       