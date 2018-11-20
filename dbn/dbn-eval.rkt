#lang racket/base

(require "lang/dbn-parser.rkt"
         "lang/dbn-errors.rkt"
         "lang/papersim.rkt"
         (prefix-in ast: "lang/dbn-ast.rkt")
         "lang/expander.rkt"
         racket/string
         racket/cmdline)

(provide (all-defined-out))

;; this evaluates dbn from an input port, or should anyways
(define (eval-in port)
  ; get the ast (this is from dbn-parser.rkt)
  (let ([ast (ast:transform-all (parse port))])
    (parser-error #f)
    (if (parser-error)
        (error "Error parsing dbn file")
        ; convert it to an s-expression (from dbn-ast.rkt)
        (let ([sexp (ast:ast->sexp ast)])
          ;(parameterize ([current-namespace (make-base-namespace)])
            ; then turn it into a syntax object, macro expand it, and eval it
            #;(require "lang/expander.rkt")
            (eval (expand (datum->syntax #f sexp)))))))

;;; evaluates a file by opening it and calling eval-in on the port
(define (eval-file filename)
  (let ([in (open-input-file filename)])
    (eval-in in)))

;;; evaluates a string by opening it as an input port
(define (eval-str str)
  (let ([in (open-input-string str)])
    (eval-in in)))

;; sets up the list of files in case we execute this
(define filenames (make-parameter #f))
(filenames (command-line #:program "dbn-papersim"
                         #:once-each
                         [("-c" "--close") "Automatically close after drawing to the window (useful for testing)"
                                           (automatically-close #t)]
                         #:args files
                         files))

;; Main entry point, executed when run with the `racket` executable or DrRacket.
;; this will look at the command line arguments, and if you give it a file
;; name, it will just execute that particular file
;(require "lang/expander.rkt")
(unless (= (length (filenames)) 0)
  (for ([file (filenames)])
    ; now, with each file, eval it!
    (printf "Executing ~a~n" file)
    (eval-file file)))

;; loads all the files in a directory, one at a time, and executes eval-file on them
(define (eval-dir dir)
  (for ([file (in-directory dir)])
    (if (string-suffix? (path->string file) ".dbn")
        (begin
          (printf "Evaluating ~a~n" file)
          (eval-file file))
        (printf "Skipping ~a~n" file))))


(module test racket/base
  (require rackunit)
  (require rackunit/text-ui)
  )