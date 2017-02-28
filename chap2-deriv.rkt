(load "number.rkt")

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define a '(1 2 3))
(define b '(1 2 3))

