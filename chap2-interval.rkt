;(2.7)
;problem related Alyssa P. Hacker
(define (make-interval a b) (cons a b))
(define (greater-one a b) (if (< a b) b a))
(define (lesser-one a b) (if (> a b) b a))
(define (upper-bound interval) (greater-one (car interval) (cdr interval)))
(define (lower-bound interval) (lesser-one (car interval) (cdr interval)))
