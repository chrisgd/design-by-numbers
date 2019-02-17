#lang racket/base

; when the racket system queries our module, we'll handle
; the configure-runtime key by executing our configure-runtime
; module
(define (get-info in mod line col pos)
  (λ (key default)
    (case key
      [(configure-runtime)
       '(#(dbn/configure-runtime configure #f))]
      ; handle coloring
      [(color-lexer)
       (dynamic-require 'dbn/dbnc/colorer 'dbn-colorer)]
      ; indentation
      [(drracket:indentation)
       (dynamic-require 'dbn/dbnc/indenter 'indent-dbn)]
      ; in theory these affect saving and such
      [(drracket:default-extension) "dbn"]
      [(drracket:default-filters) '(("DBN files" "*.dbn"))]
     
      [else
       default])))

(provide get-info)

