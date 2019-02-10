#lang racket/base

; the parser
(require dbn/lang/dbn-parser
         syntax/parse
         syntax/strip-context
         dbn/lang/dbn-ast)

; provide out our configure
(provide configure)

; here's where the magic happens: we override the current-read-interaction
; to call our read-one-line function, which in essence calls parse and then
; converts it to an sexp. 
(define (configure data)
  (current-read-interaction read-one-line))

; the repl really reads one line at a time, as far
; as I can tell, so depending on the language, you
; have to parse whatever makes sense in a line. For
; dbn, we would probably deal with a statement at a time
(define (read-one-line origin port)
  (cond
    [(or (not (char-ready? port))
         (eof-object? (peek-char port)))
     eof]
    [else
     ; get the line, then parse it, and turn it into an sexp
     (let* ([line (read-line port 'any)]
            [sexp (ast->sexp (parse-str line))])
       ; otherwise parse using our parse function
       #;(strip-context (datum->syntax #f sexp))
       (datum->syntax #f sexp))]))

