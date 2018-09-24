#lang racket/base

; when the racket system queries our module, we'll handle
; the configure-runtime key by executing our configure-runtime
; module
(define (get-info data)
  (λ (key default)
    (case key
      [(configure-runtime)
       '(#(dbn/configure-runtime configure #f))]
      [else
       default])))

(provide get-info)