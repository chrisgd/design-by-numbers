#lang racket

(provide (all-defined-out))

; simple environment using references in our symbol table
(struct memref (sym [value #:mutable]) #:transparent)

; dereferences the reference
(define (deref ref)
  (memref-value ref))

; sets the reference
(define (setref! ref val)
  (set-memref-value! ref val))

; extends the environment with a key/value pair,
; and returns the new environment
(define (extend-env env k v)
  (match v
    [(closure _ _ _ _) (cons (cons k v) env)]
    [_ (let ([ref (memref k v)])
         (cons (cons k ref) env))]))

; apply environment looks for var in the env
(define (apply-env env var)
  (let ([res (assoc var env)])
    (if res
        (cdr res)
        #f)))

; extend the environment with this list of pairs (key/value), this
; is just so if we use this, the underlying representation can be changed
; if we so desire without breaking code
(define (extend-env-with-pairs env pairs)
  (append pairs env))

; creates an emtpy environment
(define (empty-env) '())

;;; closures for functions
;;; we can store closures in addition to values in the environment,
;;; note, it's useful to store the name (sym) with the closure for errors and such
(struct closure (sym params body env) #:transparent)


