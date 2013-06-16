(define questions (load "questions.scm"))

(define (nth lst n)
  (if (> n 0)
      (nth (cdr lst) (- n 1))
      (car lst)))

(define (correct? q index)
  (car (nth (cdr q) index)))

(define (present-question q)
  (let ((question (car q))
	(answers (map cdr (cdr q))))
    (newline)
    (display question)
    (newline)
    (present-answers answers 0)))

(define (present-answers as n)
  (display n)
  (display ") ")
  (display (car as))
  (newline)
  (if (not (null? (cdr as)))
      (present-answers (cdr as) (+ n 1))))

(define (read-number)
  (let ((a (string->number (read-line))))
    (if (number? a)
	a
	(begin
	  (display "Sorry, that's not a number. Try again... ")
	  (read-number)))))  

(define (ask q)
  (begin
    (present-question q)
    (display "? ")
    (let ((a (read-number)))
      (if (correct? q a)
	  (display "Correctomundo!")
	  (display "Sorry, wrong answer...")))
    (newline)))

(define (quiz qs points)
  (if (null? qs)
      (string-append "You got " (number->string points) " answers correct")
      (begin
	(ask (car qs))
	(quiz (cdr qs) (+ 1 points)))))

(quiz questions 0)
