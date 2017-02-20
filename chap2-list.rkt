(define (dec n) (- n 1))
(define (inc n) (+ n 1))
;(2.17)
(define (last-pair-of-xs xs)
  (if (null? (cdr xs)) (car xs) (last-pair-of-xs (cdr xs))))
;(2.18)
(define (reversing xs)
  (define (iter normal revers)
    (if (null? normal) revers
        (iter (cdr normal) (cons (car normal) revers))))
  (iter xs null))

;(2.19)
(define us-coin (list 50 25 1 5 10))
(define uk-coin (list 0.5 1 2 5 10 20 50 100))
(define jp-coin (list 1 5 10 50 100 500))

(define (cc remain-amount kinds-of-coin)
  (cond ((= remain-amount 0) 1)
        ((or (< remain-amount 0) (null? kinds-of-coin)) 0)
        (else (+ (cc remain-amount (cdr kinds-of-coin))
                 (cc (- remain-amount (car kinds-of-coin)) kinds-of-coin)))))

;(2.20)
(define (same-parity x . xs)
  (define (parity-filtered-list fil? ls)
    (if (null? ls) null
        (if (fil? (car ls))
            (cons (car ls) (parity-filtered-list  fil? (cdr ls)))
            (parity-filtered-list fil? (cdr ls)))))
  (if (even? x) (cons x (parity-filtered-list even? xs))
      (cons x (parity-filtered-list odd? xs))))

;(2.21)
(define (square-list1 items)
  (if (null? items) null
      (cons ((lambda (x) (* x x)) (car items))
            (square-list1 (cdr items)))))
(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

;(2.22)
; by Louis Reasoner
(define (square-list-rev items)
  (define (iter things answer)
    (if (null? things) answer
        (iter (cdr things)
              (cons ((lambda (x) (* x x)) (car things)) answer))))
  (iter items null))

(define (square-list-recur items)
  (define (iter things answer)
    (if (null? things) answer
        (iter (cdr things)
              (cons answer
                    ((lambda (x) (* x x)) (car things))))))
  (iter items null))

;(2.23)
(define (my-for-each proc xs)
  (cond ((null? xs) null)
        (else (proc (car xs))
              (my-for-each proc (cdr xs)))))

;(2.24)
(define list224 (list 1 (list 2 (list 3 4))))
;(2.26)
(define list226x (list 1 2 3))
(define list226y (list 4 5 6))
