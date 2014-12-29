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