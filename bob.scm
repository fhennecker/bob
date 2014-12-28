(load "lib.scm")

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

(define (display-characteristic i c) 
  (fill-rectangle! 0 (* 10 i) c 10 #xF00)
  (fill-rectangle! c (* 10 i) (- 130 c) 10 #xFFF))

(define (wait-and-listen n)
  (if (is-pin-set? b1) (fill-rectangle! 30 30 30 30 #x0F0) (fill-rectangle! 30 30 30 30 #x000))
  (context 'update-buttons)
  (if (context 'button1-pressed?)
      (context 'update-general-state -1)
      (do-nothing))
  (if (eq? n 0) '() (wait-and-listen (- n 1))))

(define (get-input context input-name)
  (assoc input-name context))
(define input-value cdr)
(define (button-pressed? button)
  (eq? button 'pressed))

;;; Creating standard context structure
(define (make-context time general-state)
  (let ((button1-current-value (is-pin-set? b1))
        (button1-previous-value (is-pin-set? b1)))
  (define (update-general-state value)
    (set! general-state (+ general-state value)))
  (define (update-time value)
    (set! time (+ time value)))
  (define (update-buttons) 
    (set! button1-previous-value button1-current-value)
    (set! button1-current-value (is-pin-set? b1)))
  (define (button1-pressed?) (and (eq? #f button1-previous-value) (eq? #t button1-current-value)))
  (lambda (msg . args)
    (case msg
      ('time time)
      ('general-state general-state)
      ('update-general-state (apply update-general-state args))
      ('update-time (apply update-time args))
      ('update-buttons (update-buttons))
      ('button1-pressed? (button1-pressed?))
      (else #f))))) ; no such context variable


(define (make-finite-state-machine start-state)
  (let ((current-state start-state)
        (current-transitions (start-state 'transitions)))

  ;;; get the name of all input channels needed by the current state's transitions
  (define (get-transition-inputs)
    (map transition-input-name current-transitions))

  ;;; feed the FSM with current context
  (define (feed-context context general-state)
    ;; will trigger all transitions that satisfy their predicate with given context
    (for-each (lambda (transition)
		  (let ((input (context (transition-input-name transition))))
		    (if (and input
			           ((transition-predicate transition) input))
			      (change-state (transition-state transition))
            (do-nothing))))
		  current-transitions))

  (define (change-state new-state)
;      (if (not (equal? new-state current-state)) ; check for transition to itself
	  (begin 
	    (current-state 'exit-action)
	    (new-state 'entry-action)
	    (set! current-transitions (new-state 'transitions))
	    (set! current-state new-state)))

  (lambda (msg . args)
    (case msg
      ('feed-context (apply feed-context args))
      ('get-inputs (get-transition-inputs))
      (else (error "Msg not understood: " msg))))))

(define (make-transition input-name predicate new-state) 
  (list input-name predicate new-state))
(define transition-input-name car)
(define transition-predicate cadr)
(define transition-state caddr)

(define (do-nothing) (values))

;;; State takes at least 2 arguments
;;;  entry-action: zero argument procedure (thunk)
;;;  exit-action:   idem
(define (make-state entry-action exit-action . transitions)
  (define (add-transition transition)
    (set! transitions (cons transition
			    transitions)))
  (lambda (msg . args)
    (case msg
      ('entry-action (entry-action))
      ('exit-action (exit-action))
      ('transitions transitions)
      ('add-transition (apply add-transition args))
      (else (error "Msg not understood: " msg)))))

;;; Bob is our character. It's an agglomerate of FSMs (characteristics).
;;; The more characteristics are in a bad state, the quicker its general state 
;;; goes down. 
(define (make-bob characteristics)

  ; defining general state of Bob
  (define general-state-value 1000)
  (define (update-general-state-value value)
    (set! general-state-value (+ general-state-value value)))
  (define general-state
    (lambda (msg . args) 
      (case msg 
        ('value general-state-value)
        ('update (apply update-general-state-value args)))))

  ; what to do when context updates
  (define (update context)
    (map (lambda(fsm) (fsm 'feed-context context general-state)) characteristics))

  (lambda (msg . args)
    (case msg
      ('update (apply update args))
      (else (error "Message not understood: " msg)))))

;;;;;;;;;;;;;   
;;;; tests
;;;;;;;;;;;;;

(define testing #f)

(define (fail testid) (begin
  (display "\n\033[31mTEST ")
  (display testid)
  (display " FAILED ===================================================\033[0m")))
(define (succeed testid) (begin
  (display "\n\033[32mTEST ")
  (display testid)
  (display " SUCCEEDED ================================================\033[0m")))
(define (test testname predicate)
  (if testing 
      (if predicate (succeed testname) (fail testname))
      (do-nothing)))

(define test-context
  (make-context 123 10))
(test "CTX01" (equal? (test-context 'time) 123))
(test "CTX02" (equal? (test-context 'general-state) 10))
(test-context 'update-general-state -3)
(test "CTX03" (equal? (test-context 'general-state) 7))
(test-context 'update-time 4)
(test "CTX04" (equal? (test-context 'time) 127))

(define sad-state
  (make-state (lambda () 
        				(display "You ought to know I'm feeling very depressed")
        				(newline))
			        (lambda ()
        				(display "I'm feeling a bit better")
        				(newline))))

(define happy-state
 (let* ((maximum-happiness 1000)
	 	(happiness-counter maximum-happiness) 
	 	(happy-state 
      (make-state (lambda () 
				    		    (set! happiness-counter maximum-happiness)
				    		    (display "I am happy now!")
				    		    (newline))
				  		    (lambda () 
				    		    (display "I am no longer happy :(")
				    		    (newline)))))
    ;;; this transition uses and sets the 'happiness-counter' lexical variable
    ;;; which is why we make and add the transition here already
    (happy-state 'add-transition
		  (make-transition 
        'time
			  (let ((prev-time #f))  ; this is a pretty complex predicate, basically updates and checks a timer
			    (lambda (time)
			      (let ((delta-time (if prev-time (- time prev-time) 0)))  ; time difference
  				    (set! happiness-counter (- happiness-counter delta-time)) ; decrement happiness
  				    (set! prev-time time)  ; remember the current time value
  				    (display-characteristic 0 (round (/ happiness-counter 10)))
    					(if (< happiness-counter 0)
    					    #t       ;; I am no longer happy now
    					    #f))))
			  sad-state))
    (happy-state 'add-transition
      (make-transition
        'general-state
        (lambda (state)
          (set! happiness-counter (+ happiness-counter state))
          (display-characteristic 0 (round (/ happiness-counter 10)))
          (if (< happiness-counter 0)
            #t
            #f))
        sad-state))
    happy-state)) ; return the happy state as result

(define play-transition
 (make-transition 
    'play-button
		(lambda (button)
		  (if (button-pressed? button)
			  #t
			  #f))
		happy-state))

(happy-state 'add-transition play-transition)
(sad-state 'add-transition play-transition)
(define happy-fsm (make-finite-state-machine happy-state))


(define context (make-context 0 0))
(define bob (make-bob (list happy-fsm)))

(define (run bob context) 
    (bob 'update context)
    (wait-and-listen 100)
    (context 'update-time 1)
    ;(context 'update-general-state -1)
    (run bob context))

(run bob context)



