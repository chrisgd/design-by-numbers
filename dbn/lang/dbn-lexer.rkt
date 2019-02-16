#lang racket/base

(require racket/port)

; needed for parsing stuff
(require parser-tools/lex
         
         ; this last gives us prettier names for common regular expression stuff,
         ; and also renames it so they're all prefixed with ':' in their names
         (prefix-in : parser-tools/lex-sre)
         "dbn-errors.rkt")

; just output everything
(provide (all-defined-out))

; now, define the tokens that the parser will use
(define-tokens names-and-values (NUMERICVALUE
                                 IDENTIFIER
                                 FILENAME
                                 FUNNAME
                                 COMMENT))

; this is for the end of file marker
(define-empty-tokens end-of-file (EOF NEWLINE ERROR))


; these are all the keywords
(define-empty-tokens keywords (PAPER
                               PEN
                               LINE
                               SET
                               REPEAT
                               FOREVER
                               SAME
                               NOTSAME
                               SMALLER
                               NOTSMALLER
                               MOUSE
                               LOAD
                               NUMBER
                               VALUE
                               KEY
                               NET
                               TIME
                               PRINT
                               COMMAND
                               ANTIALIAS
                               BITMAP))

; these are the math operators
(define-empty-tokens math-operators (ADDITION
                                     SUBTRACTION
                                     MULTIPLICATION
                                     DIVISION))


; these are the parentheses variations
(define-empty-tokens parentheses (LEFTPARENTHESIS
                                  RIGHTPARENTHESIS
                                  LESSTHAN
                                  GREATERTHAN
                                  LEFTBRACE
                                  RIGHTBRACE
                                  LEFTBRACKET
                                  RIGHTBRACKET))



; and finally the lexer, this returns a function that takes an input port
(define dbnlexer
  (lexer-src-pos
   ; handle comments, need this to be first, so we don't freak out with division
   [#\+                         (token-ADDITION)]
   [#\-                         (token-SUBTRACTION)]
   [#\*                         (token-MULTIPLICATION)]
   [#\/                         (token-DIVISION)]
   [#\(                         (token-LEFTPARENTHESIS)]
   [#\{                         (token-LEFTBRACE)]
   [#\[                         (token-LEFTBRACKET)]
   [#\<                         (token-LESSTHAN)]
   [#\)                         (token-RIGHTPARENTHESIS)]
   [#\}                         (token-RIGHTBRACE)]
   [#\]                         (token-RIGHTBRACKET)]
   [#\>                         (token-GREATERTHAN)]
   ; compress a sequence of newlines into one
   [(:+ #\newline)              (token-NEWLINE)]
   ["PAPER"                     (token-PAPER)]
   ["PEN"                       (token-PEN)]
   ["LINE"                      (token-LINE)]
   ["SET"                       (token-SET)]
   ["REPEAT"                    (token-REPEAT)]
   ["FOREVER"                   (token-FOREVER)]
   ["SAME?"                     (token-SAME)]
   ["NOTSAME?"                  (token-NOTSAME)]
   ["SMALLER?"                  (token-SMALLER)]
   ["NOTSMALLER?"               (token-NOTSMALLER)]
   ["MOUSE"                     (token-MOUSE)]
   ["LOAD"                      (token-LOAD)]
   ["VALUE"                     (token-VALUE)]
   ["KEY"                       (token-KEY)]
   ["NET"                       (token-NET)]
   ["TIME"                      (token-TIME)]
   ["PRINT"                     (token-PRINT)]
   ["COMMAND"                   (token-COMMAND)]
   ["NUMBER"                    (token-NUMBER)]
   ["ANTIALIAS"                 (token-ANTIALIAS)]
   ["BITMAP"                    (token-BITMAP)]
   
   ; literal numbers, these are all ints, make sure it's a good number (this returns #f if it isn't)
   [(:+ numeric)
    (token-NUMERICVALUE
     (let ([num (string->number lexeme 10 'number-or-false)])
       (if num
           num
           (raise-lex-error "invalid number" start-pos lexeme))))]
   
   ; identifiers
   [(:: alphabetic
        (:* (:or numeric alphabetic #\_)))
    (token-IDENTIFIER (string->symbol lexeme))]

   ; valid filenames, not a simple thing, this is a bit general, but must end in .dbn
   [(:: (:+ (:or alphabetic #\_ #\- #\/)) ".dbn")
    (token-FILENAME lexeme)]

   ; handles comments, note that it consumes the newline after the comment
   [(:: "//" (:* (char-complement (:or #\newline #\linefeed)))
        #;(:+ (:or #\newline #\linefeed))) (return-without-pos (dbnlexer input-port))]
   
   ; handle a lang line so we ignore it, which was needed at one time
   ; [(:: "#lang" (:+ (union #\space #\tab)) (union "dbn" "dbn-lang")) (return-without-pos (dbnlexer input-port))]

   ; ignore whitespace
   [whitespace (return-without-pos (dbnlexer input-port))]

   ; good, ole eof
   [(eof) (token-EOF)]

   ; anything else is a syntax error, so report it as such
   [any-char (raise-lex-error "unexpected char" start-pos lexeme)]))

; position -> string -> error
; raises a lexing error
(define (raise-lex-error kind-of-error pos lexeme)
  (let* ([error-msg (format-error-msg (error-kind->msg kind-of-error) pos lexeme)])
    (lexer-error #t)
    (raise-syntax-error 'dbnlexer error-msg)))

(define (error-kind->msg kind-of-error)
  (cond [(eq? kind-of-error "syntax error") "syntax error"]
        [(eq? kind-of-error "invalid number") "invalid number"]
        [(eq? kind-of-error "unrecognized char") "unrecognized character"]
        [else "UNKNOWN ERROR SPECIFIED"]))        

(define (format-error-msg msg pos lexeme)
  (let* ([linenums? (not (eq? (position-line pos) #f))]
         [loc (if linenums? (position-line pos) (position-offset pos))]
         [col (position-col pos)])
    (if linenums? (format "~a at line ~a, col ~a : '~a'" msg loc col lexeme)
                  (format "~a at offset ~a : '~a'" msg loc lexeme))))
                                             


; input port -> thunk
; creates a thunk that when called will return the next token from the input stream
(define (get-tokenizer in)
  (port-count-lines! in)
  (define upper-in (make-uppercase-input-port in))
  (port-count-lines! upper-in)
  (λ () (dbnlexer upper-in)))

; this defines a function that creates an input port from a given
; input port, except that it will turn all the lower-case characters
; into upper-case characters. 
(define (make-uppercase-input-port in)
  (filter-read-input-port in (λ (bytes count)
                               (cond
                                 ; if it's some number of bytes, then
                                 ; modify the bytestring and deal return the count
                                 [(exact-nonnegative-integer? count)
                                  (let* ([up-str (string-upcase
                                                  (bytes->string/utf-8 bytes))]
                                         [conv-bytes (string->bytes/utf-8 up-str)])
                                    (bytes-copy! bytes 0 conv-bytes))
                                  count]
                                 ; otherwise just return the 2nd argument 
                                 [else count]))
                          (λ (bytes pos evt rest)
                            (cond
                              [(exact-nonnegative-integer? rest)
                               (let* ([up-str (string-upcase
                                               (bytes->string/utf-8 bytes))]
                                      [conv-bytes (string->bytes/utf-8 up-str)])
                                 (bytes-copy! bytes 0 conv-bytes)
                                 (bytes-length conv-bytes))]
                              [(procedure? rest)
                               (printf "proc name: ~a~n" rest)
                               rest]
                              [else rest]))
                          #t))

; input port -> list of tokens
; this function takes an input port and returns a list of
; tokens read from it (until it reaches the end of file (eof))
(define (lex in)
  (port-count-lines! in)
  (define filtering-in (make-uppercase-input-port in))
  (port-count-lines! filtering-in)
  (let ([tokenize (get-tokenizer filtering-in)])
    (define (lexfun)
      (let ([tok (tokenize)])
        (cond
          ; test to see if we hit eof as the base case
          [(eq? (position-token-token tok) (token-EOF)) null]
          [else (cons (position-token-token tok) (lexfun))])))
    (lexfun)))


; string -> list of tokens
; this function takes a string and returns a list of
; tokens read from it (until it reaches the end)
(define (lexstr str)
  (lex (open-input-string str)))

; filename -> list of tokens
; this function takes a filename, opens it as an input port,
; and then reads tokens until the end is reached
(define (lexfile filename)
  (lex (open-input-file filename)))

(require rackunit)

(module+ test
  (require rackunit)
  (require rackunit/text-ui)

  ; testing the various things we lex
  (check-equal? (lexstr "Paper") (list (token-PAPER)) "Lexing Paper issue")
  (check-equal? (lexstr "PAPER") (list (token-PAPER)) "Lexing PAPER issue")
  (check-equal? (lexstr "Pen") (list (token-PEN)) "Lexing Pen issue")
  (check-equal? (lexstr "PEN") (list (token-PEN)) "Lexing PEN issue")
  (check-equal? (lexstr "Line") (list (token-LINE)) "Lexing Line issue")
  (check-equal? (lexstr "LINE") (list (token-LINE)) "Lexing LINE issue")
  (check-equal? (lexstr "Set") (list (token-SET)) "Lexing Set issue")
  (check-equal? (lexstr "SET") (list (token-SET)) "Lexing SET issue")
  (check-equal? (lexstr "same?") (list (token-SAME)) "Lexing same? issue")
  (check-equal? (lexstr "notsame?") (list (token-NOTSAME)) "Lexing notsame? issue")
  (check-equal? (lexstr "smaller?") (list (token-SMALLER)) "Lexing smaller? issue")
  (check-equal? (lexstr "notsmaller?") (list (token-NOTSMALLER)) "Lexing notsmaller? issue")
  (check-equal? (lexstr "// this is a comment") null "Lexing comment without newline issue")
  ; this is needed, our comments can't eat up the trailing newline for parsing reasons
  (check-equal? (lexstr "// this is a comment\n") (list (token-NEWLINE)) "Lexing a comment with newline issue")
  (check-equal? (lexstr "1234") (list (token-NUMERICVALUE 1234)) "Lexing numeric value issue")
  (check-equal? (lexstr "{") (list (token-LEFTBRACE)) "Lexing left brace issue")
  (check-equal? (lexstr "}") (list (token-RIGHTBRACE)) "Lexing right brace issue")
  (check-equal? (lexstr "<") (list (token-LESSTHAN)) "Lexing '<' issue")
  (check-equal? (lexstr ">") (list (token-GREATERTHAN)) "Lexing '>' issue")
  (check-equal? (lexstr "[") (list (token-LEFTBRACKET)) "Lexing '[' issue")
  (check-equal? (lexstr "]") (list (token-RIGHTBRACKET)) "Lexing ']' issue")
  (check-equal? (lexstr "(") (list (token-LEFTPARENTHESIS)) "Lexing '(' issue")
  (check-equal? (lexstr ")") (list (token-RIGHTPARENTHESIS)) "Lexing ')' issue")
  (check-equal? (lexstr "*") (list (token-MULTIPLICATION)) "Lexing '*' issue")
  (check-equal? (lexstr "/") (list (token-DIVISION)) "Lexing '/' issue")
  (check-equal? (lexstr "-") (list (token-SUBTRACTION)) "Lexing '-' issue")
  (check-equal? (lexstr "+") (list (token-ADDITION)) "Lexing '+' issue")
  (check-equal? (lexstr "command") (list (token-COMMAND)) "Lexing command keyword issue")
  (check-equal? (lexstr "number") (list (token-NUMBER)) "Lexing number keyword issue")
  (check-equal? (lexstr "antialias") (list (token-ANTIALIAS)) "Lexing antialias issue")
  (check-equal? (lexstr "foo") (list (token-IDENTIFIER 'FOO)) "Lexing identifier issue")
  (check-equal? (lexstr "fo5o1") (list (token-IDENTIFIER 'FO5O1)) "Lexing identifier issue")
  (check-equal? (lexstr "1foo") (list (token-NUMERICVALUE 1) (token-IDENTIFIER 'FOO)) "Lexing identifier issue")
  ; this is fine on Mac, but Windows is case-sensitive (also on some Macs if that's how
  ; the filesystem was defined, so this is something to remember)
  (check-equal? (lexstr "Foo.dbn") (list (token-FILENAME "FOO.DBN")) "checking filename Foo.dbn")
  
  )
