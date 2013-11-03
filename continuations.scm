
(define the-continuation #f)

(define (next n)
    (call-with-current-continuation (lambda (k) (set! the-continuation k)))
    (set! n (+ n 1))
    n)

(define next-fib #f)

(define (init-fib)
  (let* ((x 1) 
	 (y 1) 
	 (tmp #f))
    (call/cc (lambda (k) (set! next-fib k)))
    (set! tmp x)
    (set! x y)
    (set! y (+ x tmp))
    x))
  
(define (some-function)
  (the-continuation)
  "foo")

(define (*& x y k)
  (k (* x y)))

(define (+& x y k)
  (k (+ x y)))

(define (sqrt& x k)
  (k (sqrt x)))

(define (pyth x y)
 (sqrt (+ (* x x) (* y y))))

(define (pyth& x y k)
  (*& x x (lambda (x2)
	    (*& y y (lambda (y2)
		      (+& x2 y2 (lambda (x2py2)
				  (sqrt& x2py2 k))))))))

(define (foo x y z)
  (* x (+ y z)))

(foo 2 3 4) ; => 14

(define (foo-k x y z k)
  (+& y z (lambda (ypz)
	    (*& x ypz k))))

(foo-k 2 3 4 (lambda (x) x)) ; => 14

(define (baz x y z)
  (+ (* x x) (* y z)))

(define (baz-k x y z k)
  (*& y z (lambda (yz)
	    (*& x x (lambda (xx)
		      (+& xx yz k))))))

(define (add-double& x y k)
  (+& x y (lambda (xy)
	    (*& 2 xy k))))


(define inc
  (lambda (n)
    (+ n 1)))

(define inc&
  (lambda (n k)
  (k (+ n 1))))



(define empty-k
  (lambda (x) x))


(define (hypo a b)
  (define (square x) (* x x))

  (sqrt (+ (square a)
	   (square b))))




(define (hypo/k a b k)
  (define (square/k x k)
    (k (* x x)))
  
  (square/k a 
            (lambda (a^2)
              (square/k b
                       (lambda (b^2)
                         (+& a^2 b^2
                                (lambda (a^2+b^2)
                                  (k (sqrt a^2+b^2)))))))))


(define */cps 
  (lambda (x y k)
    (k (* x y))))
