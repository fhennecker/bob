(define background-color #xFFF)

(define (do-nothing) (values))

(define (clear-screen) (fill-rectangle! 0 0 130 130 #xFFF))
(define (color-screen color) (fill-rectangle! 0 0 130 130 color))

(define (display-characteristic i c) 
  (fill-rectangle! 0 (* 10 i) c 10 #xF00)
  (fill-rectangle! c (* 10 i) (- 130 c) 10 background-color))

(define (wait-and-listen n)
  (context 'update-buttons)
  (display-characteristic 11 (* n 10))
  (if (not (bob 'is-dead?))
  	(bob 'update context)
  	(do-nothing))
  (if (eq? n 0) '() (wait-and-listen (- n 1))))

; all leds flashing one after the other 5 times
(define (led-animation)
	(define (blinkloop i)
		(context 'led-on i)
		(wait 2000)
		(context 'led-off i)
		(if (eq? i 0) (do-nothing) (blinkloop (- i 1))))
	(define (looploop l)
		(blinkloop 2)
		(if (eq? l 0) (do-nothing) (looploop (- l 1))))
	(looploop 4))

; single led l blinking i times
(define (blinkled l i)
	(context 'led-on l)
	(wait 2000)
	(context 'led-off l)
	(wait 3000)
	(if (eq? i 0) (do-nothing) (blinkled l (- i 1))))