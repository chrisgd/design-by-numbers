#lang racket/base

; in order for a #lang to work, you need to have a reader submodule of main.rkt
; see https://docs.racket-lang.org/guide/language-collection.html for details, thus
; as long as we require the right reader module here, we will be good because it
; will export the read and read-syntax functions properly. Doing this prevents "conflicts"
; on the package manager because you can name your reader whatever you'd like
; (otherwise it seems to want to find <pkg>/lang/reader.rkt, which conflicts with
; every other #lang package created like this
(module reader racket/base
  (require "lang/dbn-reader.rkt")
  (require "lang-info.rkt")
  (provide read read-syntax get-info))

; now we have to provide read-syntax from main so the reader works properly
(require 'reader)
(provide read-syntax read get-info)

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.  
  )

