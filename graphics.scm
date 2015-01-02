(define bob-color #xF00)
(define bob-eyes-color #xFFF)
(define background-color #xFFF)

(define (color-background color)
	(fill-rectangle! 0 0 130 10 color) ; top background
	(fill-rectangle! 0 10 30 70 color) ; left background
	(fill-rectangle! 100 10 30 70 color) ; right background
	(fill-rectangle! 0 120 130 10 color)) ; bottom background

(define (set-background-color! color)
	(set! background-color color)
	(color-background color)
	(draw-bob-feet))

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
	(fill-rectangle! 0 80 (- 65 width) 40 background-color) ; left of left part
	(fill-rectangle! (- 65 width) 80 width 40 bob-color) ; left part
	(fill-rectangle! 65 80 width 40 bob-color) ; right part
	(fill-rectangle! (+ 65 width) 80 (- 65 width) 40 background-color)) ; right of right part
