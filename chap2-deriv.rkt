(load "number.rkt")

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product a1 a2) (list '* a1 a2))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (deriv ex var)
  (cond ((number? ex) 0)
        ((variable? ex) (if (same-variable? ex var) 1
                             0))
        ((sum? ex) (make-sum (deriv (addend ex) var)
                              (deriv (augend ex) var)))
        ((product? ex) (make-sum (make-product (multiplier ex)
                                               (deriv (multiplicand ex) var))
                                 (make-product (deriv (multiplier ex) var)
                                               (multiplicand ex))))
        (else (error "unknown expression type --DERIV" ex))))


(define a '(+ x 1))
(define b '(1 2 3))
(define c (lambda (x) (eval (c x))))

