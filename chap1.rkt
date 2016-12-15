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
