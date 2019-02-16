#lang racket/base

; general requirements
(require racket/contract
         racket/gui/base
         racket/class)

; help deal with indentations
(require dbn/lang/dbn-indent-utils)

; how far we want to indent
(define indent-width (make-parameter 4))
; I suppose, 
(provide indent-width)


(define (is-open-paren? ch)
  (ormap (λ (p) (eq? p ch)) '(#\{)))

(module+ test
  
  (require rackunit)
  (check-true (is-open-paren? #\{))
  (check-false (is-open-paren? #\())
  (check-false (is-open-paren? #\[))
  (check-false (is-open-paren? #\<))
  (check-false (is-open-paren? #\}))
  (check-false (is-open-paren? #\)))
  (check-false (is-open-paren? #\]))
  (check-false (is-open-paren? #\>))
  (check-false (is-open-paren? #\a)))


(define (is-close-paren? ch)
  (ormap (λ (p) (eq? p ch)) '(#\})))

(module+ test
  (require rackunit)
  (check-false (is-close-paren? #\{))
  (check-false (is-close-paren? #\())
  (check-false (is-close-paren? #\[))
  (check-false (is-close-paren? #\<))
  (check-true (is-close-paren? #\}))
  (check-false (is-close-paren? #\)))
  (check-false (is-close-paren? #\]))
  (check-false (is-close-paren? #\>))
  (check-false (is-close-paren? #\a)))



;(provide (contract-out (indent-dbn
;                       [(is-a?/c text%) exact-nonnegative-integer? . -> . exact-nonnegative-integer?])))
(define (indent-dbn text [pos 0])
  ; when we're indenting, we really only know that we're indenting the
  ; current line, so we can try to look at context and hope that we've
  ; recursively done the right thing, so, maybe we've type'd a { to end
  ; the previous line, well if we did, the indent should have been
  ; increased for this line
  (let ([prev-line (prev-line text pos)]
        [curr-line (get-line text pos)])
    ; prev-line can return #f, meaning we're at the top of the buffer,
    ; and if that's the case, we can't really indent, i.e., indent should
    ; just be 0
    (cond
      ; now we know there was a previous line, so this should return
      ; a position for the last character on it, and a string representing
      ; the whole line, which can be used for inspection
      [prev-line
       (let ([result
              (let* ([last-str (get-line-str text prev-line)]
                     [last-char-loc (get-end-of-line-pos last-str)]
                     [last-indent (if last-char-loc (get-start-of-line-pos last-str) 0)]
                     [curr-str (get-line-str text curr-line)]
                     [curr-char-loc (get-end-of-line-pos curr-str)])
                (cond 
                  ; if the final character of the last line was an open brace,
                  ; and the last char of our current line isn't a close brace
                  ; then we definitely want to indent this line by whatever it was
                  ; indented to plus our indent space
                  [(and last-char-loc curr-char-loc (is-open-paren? (string-ref last-str last-char-loc))
                        (not (is-close-paren? (string-ref curr-str curr-char-loc))))
                   (+ (indent-width) last-indent)]
                  ; now if the last line is something else other than open paren
                  ; and we're closing that up on this line, then unindent it
                  [(and last-char-loc curr-char-loc
                        ; make sure the last line doesn't have an open indent,
                        (not (is-open-paren? (string-ref last-str last-char-loc)))
                        ; and make sure that this line doesn't start with an open paren
                        (not (is-open-paren? (string-ref curr-str (get-start-of-line-pos curr-str))))
                        ; and finally that this line ends with a close paren
                        (is-close-paren? (string-ref curr-str curr-char-loc))) 
                   (- last-indent (indent-width))]
                  [else last-indent]))])
         ; ensure overall that we get a positive value or zero, so we don't have contract issues
         (if (>= result 0) result 0))]

      ; in this case there was no previous line, so we shouldn't indent
      [else 0])))


(module+ test
  (require rackunit
           racket/gui/base)

  (define test-text (new text%))
  ; put some stuff that should be indented
  (send test-text insert
        "
Paper 0 200 300
Pen 100

Set a 5
Set a 6
Line a 200 200 a

Set lastx <Mouse 1>
Set lasty <Mouse 2>
Forever {
Set newx <Mouse 1>
{
Set newy <Mouse 2>
Line lastx lasty newx newy
{ Set [newx newy] 100 }
Forever {
Print 5
}
}
Set lasty newy
}

Command Foo X {
Line 0 X 100 X
}

Foo 50
Foo 25
")
  ; print this out to see how it's working
  (for-each (λ (line) (displayln line)) (text%->strlist (apply-indenter indent-dbn test-text))))