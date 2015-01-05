(define bob-color #xF00)
(define bob-eyes-color #xFFF)
(define background-color #xFFF)

; doesn't color the whole screen, leaves bob's face untouched
(define (color-background color) 
	(fill-rectangle! 0 0 130 10 color) ; top background
	(fill-rectangle! 0 10 30 70 color) ; left background
	(fill-rectangle! 100 10 30 70 color) ; right background
	(fill-rectangle! 0 120 130 10 color)) ; bottom background

(define (set-background-color! color)
	(set! background-color color)
	(color-background color)
	(draw-bob-feet))

(define (drawline x y slope width color) ; line starting in (x, y) with a certain slope to the right
	(fill-rectangle! x (inexact->exact (round y)) 1 1 color)
	(if (eq? width 0) 
		(do-nothing) 
		(drawline (+ x 1) (+ y slope) slope (- width 1) color)))

(define (draw-bob-head)
	(fill-rectangle! 30 10 70 70 bob-color))

(define (draw-bob-feet)
	(fill-rectangle! 55 120 5 10 bob-color)
	(fill-rectangle! 70 120 5 10 bob-color))

(define (draw-bob-eyes height)
	(fill-rectangle! 40 20 20 (- 20 height) bob-color) ; left eye cover
	(fill-rectangle! 40 (- 40 height) 20 height bob-eyes-color) ; left eye
	(fill-rectangle! 70 20 20 (- 20 height) bob-color) ; right eye cover
	(fill-rectangle! 70 (- 40 height) 20 height bob-eyes-color)) ; right eye

(define (draw-bob-belly width)
	(fill-rectangle! 10 80 (- 55 width) 40 background-color) ; left of left part
	(fill-rectangle! (- 65 width) 80 width 40 bob-color) ; left part
	(fill-rectangle! 65 80 width 40 bob-color) ; right part
	(fill-rectangle! (+ 65 width) 80 (- 65 width) 40 background-color)) ; right of right part

(define (draw-bob-nice-eyebrows)
	(fill-rectangle! 42 10 18 10 bob-color) ; left cover
	(drawline 44 18 (- 0.5) 12 #xFFF) ; left eyebrow
	(fill-rectangle! 70 10 18 10 bob-color) ; right cover
	(drawline 74 12 0.5 12 #xFFF)) ; right eyebrow

(define (draw-bob-mean-eyebrows)
	(fill-rectangle! 42 10 18 10 bob-color) ; left cover
	(drawline 44 12 0.5 12 #x000) ; left eyebrow
	(fill-rectangle! 70 10 18 10 bob-color) ; right cover
	(drawline 74 18 (- 0.5) 12 #x000)) ; right eyebrow

(define (draw-bob-smile amount)
	(fill-rectangle! 55 50 21 21 bob-color); clearing previous smile
	(define (compute-position x width)
		(fill-rectangle! (+ 65 x) (inexact->exact (round (- 60 (/ (* x x amount) 100)))) 1 1 #xFFF)
		(if (eq? width 0) (do-nothing) (compute-position (+ x 1) (- width 1))))
	(compute-position (- 10) 20))

(define (draw-poop-warning)
	(fill-rectangle! 15 10 4 10 #xF00)
	(fill-rectangle! 15 23 4 4 #xF00))

(define (erase-poop-warning)
	(fill-rectangle! 15 10 4 17 background-color))

(define (draw-poops-loop number)
	(fill-rectangle! 0 (- 130 (* number 11)) 10 10 #x630)
	(if (eq? number 0) (do-nothing) (draw-poops-loop (- number 1))))

(define (draw-poops number)
	(if (or (< number 0) (> number 11))
		(do-nothing)
		(begin (fill-rectangle! 0 0 10 130 background-color) (draw-poops-loop number))))
