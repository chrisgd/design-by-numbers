#lang info
; name of this package, dbn
(define collection "dbn")
; dependencies
(define deps '("base" "rackunit-lib" "parser-tools-lib" "gui-lib" "wxme-lib"))
; build dependencies, we need scribble, docs and rackunit for testing
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
; location of documentation
(define scribblings '(("scribblings/dbn-manual.scrbl" (multi-page) (language))))
; things to skip tests
(define test-omit-paths '("scribblings/dbn-manual.scrbl")) 
; first release of design by numbers
(define pkg-desc "Implementation of DBN (Design by Numbers) language")
; version number
(define version "1.0.2")
; the folks who created this (us!)
(define pkg-authors '(jedgingt chrisg))
