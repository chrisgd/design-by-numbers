#lang racket/base

(require racket/contract
         racket/hash
         "ast-nodes.rkt")

; build up a set of annotations on the nodes, this separates out
; this info so that we can more easily write check-expects on the
; correct structure of the AST--note we use a hasheq instead of a
; hash because we need to know just exactly if two objects are eq?
; not equal? (the later would be structure equality, and we
; need to map information per object). eq? will give us a kind of
; referential equality.
(define annotations? hash-eq?)

(module+ test
  (require rackunit)
  ; for testing
  (define notes (make-hasheq)))


(provide (contract-out (make-annotations [-> annotations?])))
(define (make-annotations)
  (make-hasheq))

; we can add notes to a node with a symbol and a value
; generally, the sym should be something like 'position, or 'type
(provide (contract-out (add-note [annotations? ast-node? symbol? any/c . -> . void?])))
(define (add-note annotations node sym item)
  (let ([notes (if (hash-has-key? annotations node)
                (hash-ref annotations node)
                (let ([newhash (make-hash)])
                  (hash-set! annotations node newhash)
                  newhash))])
    (hash-set! notes sym item)))



; this retrieves a note on a node from the annotations
(provide (contract-out (get-note [annotations? ast-node? symbol? . -> . any/c])))
(define (get-note node annotations sym)
  (if (hash-has-key? annotations node)
      (let ([node-ht (hash-ref annotations node)])
        (if (hash-has-key? node-ht sym)
            (hash-ref node-ht sym)
            (let ([errorstr (open-output-string)])
              (fprintf errorstr "Unable to find ~a as an annotation for ~a~n"
                       sym node)
              (error (get-output-string errorstr)))))
        (let ([errorstr (open-output-string)])
          (fprintf errorstr "Unable to find any (including ~a) annotations for ~a~n"
                   sym node)
          (error (get-output-string errorstr)))))




; returns true if a node has a note, or false otherwise
(provide (contract-out (has-note? [annotations? ast-node? symbol? . -> . any/c])))
(define (has-note? annotations node sym)
  (if (hash-has-key? annotations node)
      (let ([node-ht (hash-ref annotations node)])
        (hash-has-key? node-ht sym))
      #f))