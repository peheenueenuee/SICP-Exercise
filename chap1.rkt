#lang racket
;(1.1)
10
(+ 5 3 4)
(- 9 1)
(+ (* 2 4) (- 4 6))
(define a 5)
(define b (+ a 1))
(+ a b (* a b))
(if (and (< a b) (< b (* a b)))
    b
    a)
(cond ((= a 5) 6)
      ((= b 5) (+ b a))
      (else 25))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))



;(1.2)
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;(1.3)
(define (sum-of-square a b)
  (+ (square a) (square b)))
(define (square a)
  (* a a))
(define (pick-Big-Two a b c)
  (cond ((and (> a c) (> b c)) (sum-of-square a b))
        ((and (> b a) (> c a)) (sum-of-square b c))
        (else (sum-of-square c a))))

;(1.4)
(define (a-plus-abs-b a b) 
  ((if (> b 0) + -) a b))

;(1.5)
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;(chap.1.1.7)
(define (sqrt-Iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-Iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average a b)
  (/ (+ a b) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (mysqrt x)
  (sqrt-Iter 1.0 x))

;(1.6)
;Alyssa P. Hacker, Eva Lu Ator
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;(1.7)
(define (good-enough?-2 guess pre-guess x)
  (< (abs (- pre-guess guess)) 0.01))
(define (sqrt-Iter-2 guess pre-guess x)
  (if (good-enough?-2 guess pre-guess x)
      guess
      (sqrt-Iter (improve guess x) guess x)))
(define (mysqrt-2 x)
  (sqrt-Iter-2 1.0 0.0 x))

;(1.8)
(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
(define (cbrt-Iter guess pre-guess x)
  (display guess)
  (display "   ")
  (if (good-enough?-2 guess pre-guess x)
      guess
      (cbrt-Iter (improve-cube guess x) guess x)))
(define (cbrt x)
  (cbrt-Iter (improve-cube 1.0 x) 1.0 x))

;(1.10)
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
(define (pow a n)
  (cond ((or (< n 1) (= n 1)) a)
        (else (* a (pow a (- n 1))))))

;(1.11)
(define (f11-recur n)
  (cond ((< n 3) n)
        (else (+ (f11-recur (- n 1)) 
                 (* 2 (f11-recur (- n 2))) 
                 (* 3 (f11-recur (- n 3)))))))
;(1.12)
(define (=< a b )
  (or (= a b) (< a b)))
(define (dec a)
  (- a 1))
(define (pascal-triangle-node i n)
  (cond ((or (= n 1) (= n i)) 1)
        ((or (< n 0) (> n i)) 0)
        (else (+ (pascal-triangle-node (dec i) n)
                 (pascal-triangle-node (dec i) (dec n))))))

;(1.13)
(define (fib n)
  (cond ((=< n 2) 1)
        (else (+ (fib (dec n)) (fib (dec (dec n)))))))
; phi = (1 + sqrt5)/2
; phi^2 = phi + 1
; phi^n = (fib n)*phi + (fib (dec n))
; psi = (1 - sqrt5)/2
; psi^n = (fib n)*psi + (fib (dec n))
; phi - psi = sqrt5
; (phi^n - psi^n) / sqrt5 = (fib n)

;(1.14)
(define (count-change amount)
  (define (cc remain-amount kinds-of-coin)
    (define (first-denomination koc)
      (cond ((= koc 1) 1)
            ((= koc 2) 5)
            ((= koc 3) 10)
            ((= koc 4) 50)
            ((= koc 5) 25)))
            ;((= koc 6) 500)))
    (cond ((= remain-amount 0) 1)
          ((or (< remain-amount 0) (= kinds-of-coin 0)) 0)
          (else (+ (cc remain-amount (dec kinds-of-coin))
                   (cc (- remain-amount 
                          (first-denomination kinds-of-coin)) 
                       kinds-of-coin)))))
  (cc amount 5))
; Steps and Calculate-Space-Size order, how big?
; (no idea)

;(1.15)
(define (cube x) (* x x x))
(define (my-sin angle)
  (define (p x) 
    (- (* 3.0 x) (* 4.0 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (my-sin (/ angle 3.0)))))

;(1.15a) 
;> (/ 12.15 3.0)
;4.05
;> (/ 4.05 3.0)
;1.3499999999999999
;> (/ 1.35 3.0)
;0.45
;> (/ 0.45 3.0)
;0.15
;> (/ 0.15 3.0)
;0.049999999999999996

;(1.15b)
; Step : Linear( 1/30 * a ) -> Log3(a) --http://sicp.iijlab.net/solution/ex1.2.html
; Space : 1

;(1.16)
; recursion
(define (fast-expt a n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt a (/ n 2))))
        (else (* a (fast-expt a (dec n))))))
; loop
(define (fast-expt2 base power)
  (define (fast-expt-loop a n odd-parts)
    (cond ((= n 0) 1)
        ((= n 1) (* a odd-parts))
        ((even? n) (fast-expt-loop (square a) (/ n 2) odd-parts))
        (else (fast-expt-loop a (dec n) (* a odd-parts)))))
  (fast-expt-loop base power 1))

 ;(1.17),(1.18)
(define (double a) (+ a a))
(define (halve a)
  (cond ((even? a) (/ a 2))
        (else (halve (dec a)))))
(define (multiply left right)
  (define (multiply-loop a b odd-parts)
    (cond ((= b 0) 0)
          ((= b 1) (+ a odd-parts))
          ((even? b) (multiply-loop (double a) (halve b) odd-parts))
          (else (multiply-loop a (dec b) (+ a odd-parts)))))
  (multiply-loop left right 0))

;(1,19)
(define (fast-fib n)
  (fib-iter2 1 0 0 1 n))
(define (fib-iter2 a b p q count)
    (cond ((= count 0) b)
          ((even? count) (fib-iter2 a 
                                    b 
                                    (+ (square p) (square q)) 
                                    (+ (square q) (* 2 p q)) 
                                    (halve count)))
          (else (fib-iter2 (+ (* b q) (* a q) (* a p)) 
                          (+ (* b p) (* a q)) 
                          p q (dec count)))))

; (1.20)
(define (mygcd a b)
  (define (gcd-loop a b k)
    (if (= b 0) (cons a k)
      (gcd-loop b (remainder a b) (+ 1 k))))
  (gcd-loop a b 1))

; (1.21)
(define (inc a) (+ a 1))
(define (divides? a b) (= 0 (remainder a b)))
(define (smallest-divisor n) 
  (define (find-divisor test-divisor)
                (cond ((> (square test-divisor) n) n)
                      ((divides? n test-divisor) test-divisor)
                      (else (find-divisor (inc test-divisor)))))
  (find-divisor 2))
