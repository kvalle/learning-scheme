(define-syntax unless
  (syntax-rules ()
    ((unless pred e1) (if (not pred) e1))
    ((unless pred e1 e2) (if (not pred) e1 e2))))

(begin
  (newline)
  (display
   (unless #t "NOT PRINTED" "printed\n"))
  (display
   (unless #f "printed too\n" "NOT PRINTED")))
