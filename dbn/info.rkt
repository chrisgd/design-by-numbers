#lang info
; name of this package, dbn
(define collection "dbn")
; dependencies
(define deps '("base"))
; build dependencies, we need scribble, docs and rackunit for testing
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
; location of documentation
(define scribblings '(("scribblings/manual.scrbl" (multi-page) (language))))
; first release of design by numbers
(define pkg-desc "Implementation of DBN (Design by Numbers) language")
; version number
(define version "1.0.1")
; the folks who created this (us!)
(define pkg-authors '(jedgingt chrisg))
