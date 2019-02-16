#lang racket/base

(require parser-tools/lex
         racket/port
         racket/contract
         parser-tools/lex
         syntax-color/lexer-contract
         (prefix-in : parser-tools/lex-sre)
         dbn/lang/dbn-lexer)

; this is critical for writing your lexer which makes sure it's well-behaved, don't skip this!
; it requires the syntax-color/lexer-contract from above!
; See: https://docs.racket-lang.org/syntax-color/index.html
; and then see the start colorer function in: https://docs.racket-lang.org/framework/Color.html,
; which goes into great deal explaining this. Once you have your colorer working, you can probably
; dump this since I suspect it's expensive with the testing it does, haha.
(provide
 (contract-out
  [dbn-colorer lexer/c]))


; simplifying some of the code writing, from syntax-color/racket-lexer
(define (ret lexeme type paren start-pos end-pos)
  (let ([start (position-offset start-pos)]
        [end (position-offset end-pos)])
    ; this will unpack the positions, we calculate the backup distance based on the start and end positions
    (values lexeme type paren start end (- end start))))

(define KEYWORD_COLOR 'symbol)
(define IDENTIFIER_COLOR 'symbol)
(define LITERAL_COLOR 'constant)
(define PAREN_COLOR 'parenthesis)
(define COMMENT_COLOR 'comment)
(define LOAD_COLOR 'other)
(define ERROR_COLOR 'error)
(define EOF_COLOR 'eof)
  

; lexer abbreviations to deal with upper/lowercase, in the regular
; parser I simply just turned the input port into an upper case input
; port, but I was worried about the performance of doing that here
; because it seems like the colorer is called *alot*
(define-lex-abbrev A (:or "A" "a"))
(define-lex-abbrev B (:or "B" "b"))
(define-lex-abbrev C (:or "C" "c"))
(define-lex-abbrev D (:or "D" "d"))
(define-lex-abbrev E (:or "E" "e"))
(define-lex-abbrev F (:or "F" "f"))
(define-lex-abbrev G (:or "G" "g"))
(define-lex-abbrev H (:or "H" "h"))
(define-lex-abbrev I (:or "I" "i"))
(define-lex-abbrev J (:or "J" "j"))
(define-lex-abbrev K (:or "K" "k"))
(define-lex-abbrev L (:or "L" "l"))
(define-lex-abbrev M (:or "M" "m"))
(define-lex-abbrev N (:or "N" "n"))
(define-lex-abbrev O (:or "O" "o"))
(define-lex-abbrev P (:or "P" "p"))
(define-lex-abbrev Q (:or "Q" "q"))
(define-lex-abbrev R (:or "R" "r"))
(define-lex-abbrev S (:or "S" "s"))
(define-lex-abbrev T (:or "T" "t"))
(define-lex-abbrev U (:or "U" "u"))
(define-lex-abbrev V (:or "V" "v"))
(define-lex-abbrev W (:or "W" "w"))
(define-lex-abbrev X (:or "X" "x"))
(define-lex-abbrev Y (:or "Y" "y"))
(define-lex-abbrev Z (:or "Z" "z"))

; now for the keywords
(define-lex-abbrev SET (:: S E T))
(define-lex-abbrev PEN (:: P E N))
(define-lex-abbrev KEY (:: K E Y))
(define-lex-abbrev NET (:: N E T))
(define-lex-abbrev LINE (:: L I N E))
(define-lex-abbrev LOAD (:: L O A D))
(define-lex-abbrev TIME (:: T I M E))
(define-lex-abbrev PAPER (:: P A P E R))
(define-lex-abbrev SAME? (:: S A M E #\?))
(define-lex-abbrev MOUSE (:: M O U S E))
(define-lex-abbrev VALUE (:: V A L U E))
(define-lex-abbrev PRINT (:: P R I N T))
(define-lex-abbrev REPEAT (:: R E P E A T))
(define-lex-abbrev NUMBER (:: N U M B E R))
(define-lex-abbrev BITMAP (:: B I T M A P))
(define-lex-abbrev FOREVER (:: F O R E V E R))
(define-lex-abbrev COMMAND (:: C O M M A N D))
(define-lex-abbrev NOTSAME? (:: N O T S A M E #\?))
(define-lex-abbrev SMALLER? (:: S M A L L E R #\?))
(define-lex-abbrev ANTIALIAS (:: A N T I A L I A S))
(define-lex-abbrev NOTSMALLER? (:: N O T S M A L L E R #\?))

(define dbn-color-lexer
  (lexer
   ; keywords first and other important symbols (like math)
   [(:or SET PEN KEY NET LINE LOAD TIME PAPER SAME? MOUSE VALUE PRINT REPEAT
         NUMBER BITMAP FOREVER COMMAND NOTSAME? SMALLER? ANTIALIAS NOTSMALLER?
         #\+ #\- #\* #\/)
    (ret lexeme KEYWORD_COLOR #f start-pos end-pos)]

   ; parens, brackets and braces
   [(:or #\( #\) #\[ #\] #\{ #\})
    (ret lexeme PAREN_COLOR (string->symbol lexeme) start-pos end-pos)]

   ; special handling for < and >, which are interpreted as ( and )
   [#\< (ret lexeme PAREN_COLOR '|(| start-pos end-pos)]
   [#\> (ret lexeme PAREN_COLOR '|)| start-pos end-pos)]
   
   ; now for literals, like numbers
   ; literal numbers, these are all ints, make sure it's a good number (this returns #f if it isn't)
   [(:+ numeric)
    ; parse it into an actual number
    (let ([num (string->number lexeme 10 'number-or-false)])
      (if num
          (ret lexeme LITERAL_COLOR #f start-pos end-pos)
          (ret lexeme ERROR_COLOR #f start-pos end-pos)))]

   ; identifiers, must start with a letter, followed by zero or more letters, numbers and underscores
   [(:: alphabetic (:* (:or numeric alphabetic  #\_ )))
    (ret lexeme IDENTIFIER_COLOR #f start-pos end-pos)]

   ; valid filenames, not a simple thing, this is a bit general, but must end in .dbn
   [(:: (:+ (:or alphabetic numeric #\_ #\- #\/))
        "." (:: D B N))
    (ret lexeme LOAD_COLOR #f start-pos end-pos)]
   
   ; comments
   [(:: "//" (:* (char-complement (:or #\newline #\linefeed))))
    (ret lexeme COMMENT_COLOR #f start-pos end-pos)]
   
   ; handle whitespace, which we generally ignore
   [(:+ whitespace)
    (ret lexeme 'white-space #f start-pos end-pos)]
   
   ; handle eofs and unrecognized things
   [(eof) (values lexeme EOF_COLOR #f #f #f 0)]
   [any-char (ret lexeme ERROR_COLOR #f start-pos end-pos)]))



; the lexer function that the module-lexer needs--it returns a 7-tuple basically,
; to figure out exactly how to write the above lexer, you should really go to:
; https://docs.racket-lang.org/framework/Color.html, which explains how this function
; is used (and how you can take advantage of it). It also has the invariants to explain
; what things like the backup distance are (hint, don't use 0). 
(define (dbn-colorer in offset mode)
  (let-values ([(lex type paren start end backup) (dbn-color-lexer in)])
    (values lex type paren start end backup mode)))