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
       (dynamic-require 'lang/dbn-colorer 'dbn-colorer)]
      #;[(drracket:indentation) ...]
      #;[(drracket:toolbar-buttons) ...]
      [else
       default])))

(provide get-info)