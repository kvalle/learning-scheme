":"; exec mzscheme -r $0 "$@"

(begin
  (display "You can run me like a shell script\n")
  (display "Just run ./run.scm\n")
  (newline)
  (display "Only works (?) with mzscheme, which is really Racket, though.\n"))