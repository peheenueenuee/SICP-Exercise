#lang racket

(define (id x) x)
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (dec n) (- n 1))
(define (inc n) (+ n 1))
(define (average a b) (/ (+ a b) 2))

;(1.29)
;(1.30)
;(1.32)
(define (accumulate conbiner id-value term a next b)
  (define (iter x result)
	(if (> x b)
	  result
	  (iter (next x) (conbiner result (term x)))))
  (iter a id-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (product term a next b)
  (accumulate * 1 term a next b))

(define (pi-sum a b)
  (define (pi-term x)
	(/ 1.0 (* x (+ x 2))))
  (define (next n)
	(+ n 4))
  (sum pi-term a next b))
(define (integral f a b dx)
  (define (add-dx x)
	(+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k)
	(define y-term (f (+ a (* k h))))
	(cond ((or (= k 0) (= k n)) y-term)
		  ((even? k) (* 2.0 y-term))
		  (else (* 4.0 y-term))))
  (define sum-y (sum y 0 inc n))
  (* sum-y (/ h 3.0)))

;(1.31)
(define (factorial n)
  (product id 1 inc n))
(define (pi-product n)
  (define (pi-term x)
	(define xt (* 2 x))
	(/ (square xt) (square (inc xt))))
  (/ (* 2.0 2.0 n (product pi-term 2.0 inc n)) 9.0))

;(1.33)
(define (filterd-accumulate conbiner id-value filterf term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (if (filterf x) (iter (next x) (conbiner result (term x)))
            (iter (next x) result))))
  (iter a id-value))

(define (expmod-mr base expe m)
  (cond ((= expe 0) 1)
        ((even? expe)
         (remainder
          (square (expmod-mr base (/ expe 2) m))
          m))
        (else
         (remainder
          (* base (expmod-mr base (dec expe) m)) 
          m))))
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-mr a (dec n) n) 1))
  (try-it (inc (random (dec n)))))
(define (fast-prime-mr? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime-mr? n (dec times)))
        (else false)))
(define (prime? n)
  (fast-prime-mr? n 20))

(define (mygcd a b)
  (define (gcd-loop gtn ltn k)
    (if (= ltn 0) gtn
        (gcd-loop ltn (remainder gtn ltn) (+ 1 k))))
  (if (> a b) (gcd-loop a b 1)
      (gcd-loop b a 1)))

(define (coprime? a b)
  (= 1 (mygcd a b)))

(define (products-lt-coprime n)
  (define (coprime-with-n? x) (coprime? n x))
  (filterd-accumulate * 1 coprime-with-n? id 2 inc n))

;(1.34)
;(1.36)
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (try guess)
   (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
   (newline)
   (display guess)
   (let ((next (average (f guess) guess)))
   (if (close-enough? guess next)
       next
       (try next))))
  (try first-guess))

;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) ;phi
(/ (+ 1 (sqrt 5)) 2) ;phi

;(1.35)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 30.0)

;(1.37)
(define (cont-frac n d k)
  (if (= k 1) (/ n d)
      (/ n (+ d (cont-frac n d (dec k))))))
(define (cont-frac-iter n d k)
  (define (iter result times)
    (let ((next (/ n (+ d result))))
      (if (= times 1) next
          (iter next (dec times)))))
  (iter (/ n d) k))

(cont-frac 1.0 1.0 100)
(cont-frac-iter 1.0 1.0 100)
