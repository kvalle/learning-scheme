; Function composition
; Implemented mostly to explore variadic functions.

(define (compose f g)
  (define (fn . args)
    (f (apply g args)))
  fn)

; And with lambda!
(define (compose f g) 
  (lambda args
    (f (apply g args))))

; Examples

(define <> 
  (compose not =))

(<> 1 2) ; => #t
(<> 1 1) ; => #f

(define non-zero?
  (compose not zero?))

(non-zero? 0) ; => #f
(non-zero? 4) ; => #t

