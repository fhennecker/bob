(define (do-nothing) (values))

(define (clear-screen) (fill-rectangle! 0 0 130 130 #xFFF))
(define (color-screen color) (fill-rectangle! 0 0 130 130 color))

(define (display-characteristic i c) 
  (fill-rectangle! 0 (* 10 i) c 10 #xF00)
  (fill-rectangle! c (* 10 i) (- 130 c) 10 #xFFF))

(define (wait-and-listen n)
  (context 'update-buttons)
  (if (not (bob 'is-dead?))
  	(bob 'update context)
  	(do-nothing))
  (if (eq? n 0) '() (wait-and-listen (- n 1))))