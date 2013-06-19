; Basic stream operations

(define-syntax cons-s
  (syntax-rules ()
    ((cons-stream x y) (cons x (delay y)))))

(define car-s car)
(define (cdr-s stream) (force (cdr stream)))
(define empty-s? null?)

; More advanced operations

(define (take-s n stream)
  (if (zero? n)
      '()
      (cons (car-s stream)
	    (take-s (- n 1) (cdr-s stream)))))

(define (zip-with-s fn stream1 stream2)
  (let ((e1 (car-s stream1))
	(e2 (car-s stream2)))
    (cons-s (fn e1 e2) 
	  (zip-with-s fn (cdr-s stream1) (cdr-s stream2)))))

; Some simple streams

(define the-empty-stream '())
(define zeros (cons-s 0 zeros))
(define ones (cons-s 1 ones))

(define twos (zip-with-s + ones ones))

(take-s 10 twos)
