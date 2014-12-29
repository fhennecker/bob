(define (do-nothing) (values))

(define (display-characteristic i c) 
  (fill-rectangle! 0 (* 10 i) c 10 #xF00)
  (fill-rectangle! c (* 10 i) (- 130 c) 10 #xFFF))

(define (wait-and-listen n)
  (context 'update-buttons)
  (bob 'update context)
  (if (eq? n 0) '() (wait-and-listen (- n 1))))