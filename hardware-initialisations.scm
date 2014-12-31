(define ax (pin 16)) ; Accelerometer x
(define ay (pin 17)) ; Accelerometer y

(define l0 (pin 18)) ; green led
(define l1 (pin 10)) ; orange led
(define l2 (pin 11)) ; red led

(set-output-pin! l0)
(set-output-pin! l1)
(set-output-pin! l2)

(define b0 (pin 23)) ; button 1 (far left)
(define b1 (pin 24)) ; button 2 (middle)
(define b2 (pin 25)) ; button 3 (far right)

(set-input-pin! b0)
(set-input-pin! b1)
(set-input-pin! b2)