; Define within defines

(define (inc a)
  (define (inner b)
    (+ b 1))
  (inner a))

; Var-args

(define (varargs arg1 . rest)
  (begin
    (display "I am the first argument: ")
    (display arg1)
    (newline)
    (display "And here are the rest: ")
    (display rest)
    (newline)))

(varargs 'a 'b 'c 'd)
(apply varargs (list 'a 'b 'c 'c))

; Structs

(define-structure animal type name)

(define-structure person name age pet)

(let* ((d (make-animal 'dog "lassie"))
       (p (make-person "timmy" 12 d)))
  (newline)
  (display (person-name p))
  (newline)
  (display (person-age p))
  (newline)
  (display (animal-name (person-pet p)))
  (newline))

; Records
; http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/define_002drecord_002dtype-_0028SRFI-9_0029.html#define_002drecord_002dtype-_0028SRFI-9_0029

(define-record-type pare
       (kons x y)
       pare?
       (x kar set-kar!)
       (y kdr))

