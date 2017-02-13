;(2.2)
(define (make-point x y)
  (cons x y))
(define (x-coodinate point)
  (car point))
(define (y-coodinate point)
  (cdr point))

(define (make-segment startpoint endpoint)
  (cons startpoint endpoint))
(define (start-point segment) (car segment))
(define (end-point segment) (cdr segment))

(define (midpoint-segment segment)
  (let ((sp (start-point segment))
        (ep (end-point segment)))
    (make-segment (average (x-coodinate sp) (x-coodinate ep))
                  (average (y-coodinate sp) (y-coodinate ep)))))

(define point-alpha (make-point 1 3))
(define point-beta (make-point 3 7))
(define segA (make-segment point-alpha point-beta))

