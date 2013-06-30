
(define (fb n)
  (let ((fizz (= 0 (modulo n 3)))
	(buzz (= 0 (modulo n 5))))
    (cond ((and fizz buzz) "fizzbuzz")
	  (fizz "fizz")
	  (buzz "buzz")
	  (else n))))

(define (fizzbuzz n)
  (map fb (cdr (iota (+ 1 n)))))

(display (fizzbuzz 20))
