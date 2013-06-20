; Implementing streams from scratch, as an exercise.
; For proper Scheme streams, see http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Streams.html

(define the-empty-stream '())

; Basic stream operations

(define-syntax cons-s
  (syntax-rules ()
    ((cons-stream x y) (cons x (delay y)))))

(define car-s car)
(define (cdr-s stream) (force (cdr stream)))
(define null-s? null?)

(define (map-s fn stream)
  (cons-s (fn (car-s stream))
	  (map-s fn (cdr-s stream))))

(define (filter-s fn stream)
  (let ((first (car-s stream))
	(rest (cdr-s stream)))
    (if (fn first)
	(cons-s first (filter-s fn rest))
	(filter-s fn rest))))

(define (list->s lst)
  (if (null? lst)
      the-empty-stream
      (cons-s (car lst) (list->s (cdr lst)))))

(define (s->list stream)
  (if (null-s? stream)
      '()
      (cons (car-s stream) 
	    (s->list (cdr-s stream)))))

; Additional stream functions

(define (nth-s n stream)
  (if (zero? n)
      (car-s stream)
      (nth-s (- n 1) (cdr-s stream))))

(define (append-s stream1 stream2)
  (if (null-s? stream1)
      stream2
      (cons-s (car-s stream1)
	      (append-s (cdr-s stream1) stream2))))

(define (take-s n stream)
  (if (zero? n)
      '()
      (cons (car-s stream)
	    (take-s (- n 1) (cdr-s stream)))))

(define (drop-s n stream)
  (if (zero? n)
      stream
      (drop-s (- n 1) (cdr-s stream))))

(define (take-while-s fn stream)
  (if (fn (car-s stream))
      (cons (car-s stream)
	    (take-while-s fn (cdr-s stream)))
      '()))

(define (drop-while-s fn stream)
  (if (fn (car-s stream))
      (drop-while-s fn (cdr-s stream))
      stream))

(define (zip-with-s fn stream1 stream2)
  (let ((e1 (car-s stream1))
	(e2 (car-s stream2)))
    (cons-s (fn e1 e2) 
	  (zip-with-s fn (cdr-s stream1) (cdr-s stream2)))))

(define (repeat x)
  (cons-s x (repeat x)))

(define (cycle lst)
  (letrec ((nth-mod (lambda (n)
		      (list-ref lst (remainder n (length lst)))))
	   (cycle (lambda (n) 
		    (cons-s (nth-mod n)
			    (cycle (+ 1 n))))))
    (cycle 0)))

(define (iterate x fn)
  (cons-s x (iterate (fn x) fn)))

; Some example streams

(define zeros (repeat 0))
(define ones (map-s (lambda (x) (+ 1 x)) zeros))
(define twos (zip-with-s + ones ones))

(define abc (cycle '(a b c)))

(define (natural-numbers-from n)
  (cons-s n (natural-numbers-from (+ 1 n))))

(define natural-numbers
  (natural-numbers-from 0))

(define even-numbers
  (filter-s even? natural-numbers))

(define fibs
  (cons-s 0 (cons-s 1 (zip-with-s + (cdr-s fibs) fibs))))

(define fizzbuzz 
  (zip-with-s (lambda (n fb) (if (string-null? fb) n fb))
	      (natural-numbers-from 1)
	      (zip-with-s string-append 
			  (cycle (list "" "" "fizz"))
			  (cycle (list "" "" "" "" "buzz")))))

(define (newton-sqrt n) 
  ; Newtons method for finding square roots
  ; with an initial "guess" of 1.0
  (iterate n (lambda (x) (/ (+ x 
			     (/ 2.0 x)) 
			  2))))

(define (sieve stream)
  ; Sieve of Eratosthenes
  (cons-s (car-s stream)
	  (sieve (filter-s 
		  (lambda (x) (not (= 0 (remainder x (car-s stream)))))
		  (cdr-s stream)))))

(define primes 
  (sieve (natural-numbers-from 2)))


