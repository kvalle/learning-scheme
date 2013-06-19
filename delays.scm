(define delayed 
  (delay (begin
	   (display "ohhai")
	   (newline)
	   (display "sorry I'm late"))))

(begin
  (newline)
  (display "bah, mr. late is late")
  (newline)
  (force delayed))

