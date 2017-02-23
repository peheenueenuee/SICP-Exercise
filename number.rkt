(define (square x) (* x x))
(define (inc a) (+ a 1))
(define (dec a) (- a 1))

(define (ennumerate-interval a b)
  (if (> a b) null
      (cons a (ennumerate-interval (inc a) b))))

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
