(define (fizz n)
  (= 0 (modulo n 3)))

(define (buzz n)
  (= 0 (modulo n 5)))

(define (fibu n)
  (and (fizz n)
       (buzz n)))

(define (fizzbuzz n)
  (cond ((fibu n) "fizzbuzz")
	((fizz n) "fizz")
	((buzz n) "buzz")
	(else n)))

(display (map fizzbuzz (iota 100)))
