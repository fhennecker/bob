(load "lib")

;;; Initialisations
(define ax (pin 16)) ; Accelerometer x
(define ay (pin 17)) ; Accelerometer y

(define l1 (pin 18)) ; green led
(define l2 (pin 10)) ; orange led
(define l3 (pin 11)) ; red led

(set-output-pin! l1)
(set-output-pin! l2)
(set-output-pin! l3)

(define b1 (pin 23)) ; button 1 (far left)
(define b2 (pin 24)) ; button 2 (middle)
(define b3 (pin 25)) ; button 3 (far right)

(set-input-pin! b1)
(set-input-pin! b2)
(set-input-pin! b3)

(define w 10) ; width
(define px 0) ; previous x
(define py 0) ; previous y

(define color #xFFF)

;;; Utility
(define (clear-screen) (fill-rectangle! 0 0 130 130 #xFFF))
(define (limit val) (quotient (- val 13000) 100))
(define fr fill-rectangle!)

(define (update-screen)
	(begin	(fill-rectangle! py px w w color) ; clearing previous square
			(set! px (limit (pulse_in ax)))
			(set! py (- 110 (limit (pulse_in ay))))
			(fill-rectangle! py px w w #xF00)
			(wait 1000)
			(if (is-pin-set? b1)
				(begin (set-pin! l1) (set! color #x0F0))
				(clear-pin! l1))
			(if (is-pin-set? b2)
				(begin (set-pin! l2) (set! color #xFF0))
				(clear-pin! l2))
			(if (is-pin-set? b3)
				(begin (set-pin! l3) (set! color #xF00))
				(clear-pin! l3))
			(update-screen)
		))

(define (print-doppler)
	(begin	(clear-screen)
			(fr -8 0 8 130 #x000) (fr 8 0 7 130 #x000) (fr 22 0 6 130 #x000) (fr 34 0 5 130 #x000)
			(fr 44 0 4 130 #x000) (fr 52 0 3 130 #x000) (fr 58 0 2 130 #x000) (fr 62 0 1 130 #x000)
			(fr 64 0 2 130 #x000) (fr 68 0 3 130 #x000) (fr 74 0 4 130 #x000) (fr 82 0 5 130 #x000)
			(fr 92 0 6 130 #x000) (fr 104 0 7 130 #x000) (fr 118 0 8 130 #x000) (fr 134 0 9 130 #x000)
			(fr 152 0 10 130 #x000) (fr 172 0 9 130 #x000) (fr 190 0 8 130 #x000) (fr 206 0 7 130 #x000)
			;Horizontal bars
			(fr 0 -8 130 8 #x000) (fr 0 8 130 7 #x000) (fr 0 22 130 6 #x000) (fr 0 34 130 5 #x000) 
			(fr 0 44 130 4 #x000) (fr 0 52 130 3 #x000) (fr 0 58 130 2 #x000) (fr 0 62 130 1 #x000) 
			(fr 0 64 130 2 #x000) (fr 0 68 130 3 #x000) (fr 0 74 130 4 #x000) (fr 0 82 130 5 #x000) 
			(fr 0 92 130 6 #x000) (fr 0 104 130 7 #x000) (fr 0 118 130 8 #x000) (fr 0 134 130 9 #x000) 
			(fr 0 152 130 10 #x000) (fr 0 172 130 9 #x000) (fr 0 190 130 8 #x000) (fr 0 206 130 7 #x000)
		))

(clear-screen)
;(update-screen)
(print-doppler)
(update-screen)
