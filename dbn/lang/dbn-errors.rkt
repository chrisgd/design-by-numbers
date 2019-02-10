#lang racket/base

; provide all the symbols from here
(provide (all-defined-out))

; these basically represent global-like parameters so we know if there was an issue when compiling
(define lexer-error (make-parameter #t))
(define parser-error (make-parameter #t))
(define eval-error (make-parameter #t))

; reset all the errors
(define (reset-errors!)
  (lexer-error #f)
  (parser-error #f)
  (eval-error #f))

