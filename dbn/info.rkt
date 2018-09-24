#lang info
(define collection "dbn")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/manual.scrbl" (multi-page))))
(define pkg-desc "Implementation of DBN (Design by Numbers) language")
(define version "0.1")
(define pkg-authors '(jedgingt chrisg))
