; Implementing streams from scratch, as an exercise.
; For the real implementation, see http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Streams.html

; Stream operations

(define-syntax cons-s
  (syntax-rules ()
    ((cons-stream x y) (cons x (delay y)))))

(define car-s car)
(define (cdr-s stream) (force (cdr stream)))
(define null-s? null?)

(define (take-s n stream)
  (if (zero? n)
      '()
      (cons (car-s stream)
	    (take-s (- n 1) (cdr-s stream)))))

(define (take-all-s stream)
  (if (null-s? stream)
      '()
      (cons (car-s stream)
	    (take-all-s (cdr-s stream)))))

(define (zip-with-s fn stream1 stream2)
  (let ((e1 (car-s stream1))
	(e2 (car-s stream2)))
    (cons-s (fn e1 e2) 
	  (zip-with-s fn (cdr-s stream1) (cdr-s stream2)))))

(define (map-s fn stream)
  (cons-s (fn (car-s stream))
	  (map-s fn (cdr-s stream))))

(define (filter-s fn stream)
  (let ((first (car-s stream))
	(rest (cdr-s stream)))
    (if (fn first)
	(cons-s first (filter-s fn rest))
	(filter-s fn rest))))

(define (repeat x)
  (cons-s x (repeat x)))

(define (list->s lst)
  (if (null? lst)
      '()
      (cons-s (car lst) (list->s (cdr lst)))))

(define (append-s stream1 stream2)
  (if (null-s? stream1)
      stream2
      (cons-s (car-s stream1)
	      (append-s (cdr-s stream1) stream2))))

(define (cycle lst)
  (letrec ((nth-mod (lambda (n)
		      (list-ref lst (remainder n (length lst)))))
	   (cycle (lambda (n) 
		    (cons-s (nth-mod n)
			    (cycle (+ 1 n))))))
    (cycle 0)))

; Example streams

(define the-empty-stream '())
(define zeros (cons-s 0 zeros))
(define ones (cons-s 1 ones))
(define twos (zip-with-s + ones ones))

(define (natural-numbers-from n)
  (cons-s n (natural-numbers-from (+ 1 n))))

(define natural-numbers
  (natural-numbers-from 0))

(define fibs
  (cons-s 0
	  (cons-s 1
		  (zip-with-s + (cdr-s fibs) fibs))))

