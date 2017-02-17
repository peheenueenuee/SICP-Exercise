;(2.17)
(define (last-pair-of-xs xs)
  (if (null? (cdr xs)) (car xs) (last-pair-of-xs (cdr xs))))
;(2.18)
(define (reversing xs)
  (define (iter normal revers)
    (if (null? normal) revers
        (iter (cdr normal) (cons (car normal) revers))))
  (iter xs null))
