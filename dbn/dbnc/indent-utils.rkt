#lang racket/base
; Copyright 2019 Chris GauthierDickey
; see LICENSE.txt for details


; Utility functions that help with writing indenters

; needed for these dealing with text% and contracts related to those
(require racket/gui/base
         racket/list
         racket/class
         racket/contract)


; set up tests 
(module+ test
  (require rackunit
           racket/gui/base)

  (define test-text (new text%))
  (send test-text insert "abc\nd f\ng i \n"))


; just to reduce typing, haha
(define natural? exact-nonnegative-integer?)
(define nat-opt/c (or/c #f exact-nonnegative-integer?))
(define string-opt/c (or/c #f string?))
(define text/c (is-a?/c text%))



; get-line: text% natural? -> 
; this returns the line number (starting from 0) that we're currently on. It always
; returns a nonnegative integer, even if you give it a position that's larger than what
(provide (contract-out
          [get-line (text/c natural? . -> . nat-opt/c)]))
(define (get-line text pos)
  (send text position-line pos))

; tests for get-line
(module+ test
  ; checks newline boundaries to make sure it's giving what we expect--
  ; i.e., that a newline is part of the current line, and after that it's the
  ; next line. In addition, anything beyond the last line just gives you the
  ; last line.
  (check-equal? (get-line test-text 0) 0)
  (check-equal? (get-line test-text 3) 0)
  (check-equal? (get-line test-text 4) 1)
  (check-equal? (get-line test-text 7) 1)
  (check-equal? (get-line test-text 8) 2)
  (check-equal? (get-line test-text 12) 2)
  (check-equal? (get-line test-text 13) 3)
  (check-equal? (get-line test-text 25) 3)
  ; test sending negatives
  (check-exn exn:fail? (λ () (get-line test-text -1)))
  ; test sending non-text value
  (check-exn exn:fail? (λ () (get-line #f #f)))
  ; test sending non-number
  (check-exn exn:fail? (λ () (get-line test-text 'foo))))




; prev-line: text% natural? -> (or/c #f natural?
; this returns the line number of the previous line, or #f if the current line is 0
; and so there's no previous line
(provide (contract-out
          [prev-line (text/c natural? . -> . nat-opt/c)]))
(define (prev-line text pos)
  (let ([line-no (get-line text pos)])
    (if (eq? line-no 0)
        #f
        (sub1 line-no))))

; tests for prev-line
(module+ test
  ; tests for prev-line, in particular that #f works
  (check-equal? (prev-line test-text 0) #f)
  (check-equal? (prev-line test-text 3) #f)
  (check-equal? (prev-line test-text 4) 0)
  (check-equal? (prev-line test-text 7) 0)
  (check-equal? (prev-line test-text 8) 1)
  (check-equal? (prev-line test-text 13) 2)
  (check-equal? (prev-line test-text 25) 2)
  ; test sending negatives
  (check-exn exn:fail? (λ () (prev-line test-text -1)))
  ; test sending non-text value
  (check-exn exn:fail? (λ () (prev-line #f #f)))
  ; test sending non-number
  (check-exn exn:fail? (λ () (prev-line test-text 'foo))))




; get-char: text% pos . -> . char
; get-char takes a text% object and a position, and returns the char at that position
(provide (contract-out
          [get-character (text/c natural? . -> . (or/c char? #\nul))]))
(define (get-character text pos)
  ; simply re-call the object's get-character method
  (let ([ch (send text get-character pos)])
    ; just convert the #\nul to #f to simplify some things
    (if (eq? ch #\nul) #f ch)))

; tests for get-character
(module+ test
  (check-equal? (get-character test-text 0) #\a)
  (check-equal? (get-character test-text 1) #\b)
  (check-equal? (get-character test-text 2) #\c)
  (check-equal? (get-character test-text 3) #\newline)
  (check-equal? (get-character test-text 12) #\newline)
  (check-equal? (get-character test-text 13) #f))




; get-line-str: text/c natural? . -> . string?
; get-line-str returns the string representation of a given line number
(provide (contract-out (get-line-str [text/c natural? . -> . string?])))
(define (get-line-str text line-no)
  (let ([start (send text line-start-position line-no)]
        [end (send text line-end-position line-no)])
    ; note, text always returns a string even if it's empty
    (send text get-text start end)))

; tests for get-line-str
(module+ test
  (check-equal? (get-line-str test-text 0) "abc")
  (check-equal? (get-line-str test-text 1) "d f")
  (check-equal? (get-line-str test-text 2) "g i "))




; get-start-of-line-pos: text/c natural? . -> . (values (or/c natural? #f) (or/c string? #f))
; get-start-of-line-pos returns *2* values, the first is the position of the first non-whitespace character,
; and the second is the string that was matched (this was obtained during a call to get-line-str, and is often
; useful, so instead of throwing it out, we return it too
(provide (contract-out (get-start-of-line-pos [string? . -> . nat-opt/c])))
(define (get-start-of-line-pos str)
  ; now str has the contents of a line, so let's find where the first non-whitespace char is
  (let ([position (regexp-match-positions #rx"[^ \t]" str)])
    (cond
      ; need to add the offset from the start of the line to the position found by the regex
      [position (car (first position))]
      [else #f])))
  
; tests for get-start-of-line-pos
(module+ test
  (check-equal? (get-start-of-line-pos (get-line-str test-text 0)) 0)
  (check-equal? (get-start-of-line-pos (get-line-str test-text 1)) 0)
  (check-equal? (get-start-of-line-pos (get-line-str test-text 2)) 0)
  (check-equal? (get-start-of-line-pos "") #f)
  )





; get-end-of-line-pos text/c natural? . -> . (values (or/c natural? #f) (or/c string? #f))
; get-end-of-line-pos returns *2* values, the first is the position of the first non-whitespace character,
; and the second is the string that was matched (this was obtained during a call to get-line-str, and is often
; useful, so instead of throwing it out, we return it too
(provide (contract-out (get-end-of-line-pos [string? . -> . nat-opt/c])))
(define (get-end-of-line-pos str)
  ; now str has the contents of a line, so let's find where the first non-whitespace char is
  ; here we use regexp-match-positions* to get *all* matches
  (let ([position (regexp-match-positions* #rx"[^ \t]+" str)])
    (cond
      ; need to add the offset of the start of the line to its position the regex found
      [(not (empty? position)) (sub1 (cdr (last position)))]
      [else #f])))
  
; tests for get-start-of-line-pos
(module+ test
  (check-equal? (get-end-of-line-pos (get-line-str test-text 0)) 2)
  (check-equal? (get-end-of-line-pos (get-line-str test-text 1)) 2)
  (check-equal? (get-end-of-line-pos (get-line-str test-text 2)) 2)
  (check-equal? (get-end-of-line-pos "") #f))




; get-start-of-line-char text/c (natural?) string? . -> . (or/c char? #f)
; get-start-of-line-char takes a text% object, a line number, and a string, and
; searches the line for that character, returning its position
(provide (contract-out (get-start-of-line-char [text/c natural? . -> . (or/c char? #f)])))
(define (get-start-of-line-char text line-no)
  (let* ([str (get-line-str text line-no)]
         [position (get-start-of-line-pos str)])
    (cond
      [(and position (string-ref str position))]
      [else #f])))

; tests for get-start-of-line-char
(module+ test
  (check-equal? (get-start-of-line-char test-text 0) #\a)
  (check-equal? (get-start-of-line-char test-text 1) #\d)
  (check-equal? (get-start-of-line-char test-text 2) #\g)
  (check-equal? (get-start-of-line-char test-text 25) #f))





; get-end-of-line-char text/c (natural?) string? . -> . (or/c char? #f)
; get-end-of-line-char takes a text% object, a line number, and a string, and
; searches the line for the last character, returning the character
(provide (contract-out (get-end-of-line-char [text/c natural? . -> . (or/c char? #f)])))
(define (get-end-of-line-char text line-no)
  ; get the string for this line
  (let* ([str (get-line-str text line-no)]
         ; and its position
         [position (get-end-of-line-pos str)])
    (cond
      [(and position (string-ref str position))]
      [else #f])))

; tests for get-end-of-line-char
(module+ test
  (check-equal? (get-end-of-line-char test-text 0) #\c)
  (check-equal? (get-end-of-line-char test-text 1) #\f)
  (check-equal? (get-end-of-line-char test-text 2) #\i)
  (check-equal? (get-end-of-line-char test-text 25) #f))







; line-starts-with?: text/c natural? char? . -> . boolean?
; line-starts-with? takes a text% object, a line number, and a character, and returns #t or #f if the line
; starts with that character, excluding tabs and spaces
(provide (contract-out (line-starts-with? [text/c natural? char? . -> . boolean?])))
(define (line-starts-with? text line-no char)
  (eq? char (get-start-of-line-char text line-no)))

(module+ test
  (send test-text insert "    {\n")
  (send test-text insert "  }\n")
  (check-equal? (get-start-of-line-char test-text 3) #\{)
  (check-equal? (get-start-of-line-char test-text 4) #\}))




; foldl-text-lines: ((natural? any/c text/c . -> . any/c) natural? any/c text/c . -> . any/c)
; foldl-text-lines: basically is a foldl on the text buffer, except that our function must take 3, yes 3, arguments.
; the function we pass takes in the current-line number we're working on, an accumulator, and the text object,
; this way we can run pretty much anything on text to get our accumulator
(provide (contract-out (foldl-text-lines [(natural? any/c text/c . -> . any/c) natural? any/c text/c
                                          . -> . any/c])))
(define (foldl-text-lines proc line-no init text)
  ; store of the last line so we don't have to do it repeatedly
  (let* ([last-line (add1 (send text last-line))]
         ; make a helper to call recursively
         [helper (λ (proc line-no init text)
                   ; if we're less than the last line, we can recurse
                   (if (< line-no last-line)
                       (let ([acc (proc line-no init text)])
                         (foldl-text-lines proc (add1 line-no) acc text))
                       init))])
    (helper proc line-no init text)))
; tests for foldl-text-lines
(module+ test
  (check-equal? (foldl-text-lines (λ (line-no acc text)
                                    (+ acc
                                       (- (send text line-end-position line-no)
                                          (send text line-start-position line-no))))
                                  0 0 test-text) 18))



; text%->strlist: text/c . -> . (listof string?)
; text%->strlist takes a text buffer object and extracts each line in the buffer
; into a list of those lines, one string per line
(provide (contract-out (text%->strlist [text/c . -> . (listof string?)])))
(define (text%->strlist text)
  (reverse (foldl-text-lines
            (λ (line-no acc text)
              (cons (get-line-str text line-no) acc))
            0 '() text)))

; tests for text%->strlist
(module+ test
  (check-equal? (text%->strlist test-text)
                '("abc" "d f" "g i " "    {" "  }" "")))




; get-line-start-positions: text/c . -> . (listof natural?)
; get-line-start-positions returns a list of all of the starting positions of the current buffer
(provide (contract-out (get-line-start-positions [text/c . -> . (listof natural?)])))
(define (get-line-start-positions text)
  (reverse (foldl-text-lines
            (λ (line-no acc text)
              (let* ([str (get-line-str text line-no)]
                     [position (get-start-of-line-pos str)])
                (cons position acc)))
            0 '() text)))

; test for get-line-start-positions
(module+ test
  (check-equal? (get-line-start-positions test-text) '(0 0 0 4 2 #f)))





; apply indenter applies an indenting function, which takes a text and a position, to a given text buffer
(provide (contract-out
          (apply-indenter [(text/c exact-nonnegative-integer? . -> . exact-nonnegative-integer?)
                           text/c . -> . text/c])))
(define (apply-indenter proc text)
  (foldl-text-lines
   (λ (line-no acc text)
     ; get the start of the line
     (let* ([line-start (send text line-start-position line-no)]
            [indent-val (proc text line-start)])
       ; now insert the spaces into this line
       (send text insert (make-string indent-val #\space) line-start)
       ; and return the text, so it's in the accumulator
       text))
   0 #f text))

(module+ test
  ; indent everything by 2
  (check-equal? (text%->strlist (apply-indenter (λ (text pos) 2) test-text))
                '("  abc"
                  "  d f"
                  "  g i "
                  "      {"
                  "    }"
                  "  ")))
   