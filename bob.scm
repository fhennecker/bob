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
  (context 'update-buttons)
  (bob 'update context)
  (if (eq? n 0) '() (wait-and-listen (- n 1))))

(define (get-input context input-name)
  (assoc input-name context))
(define input-value cdr)
(define (button-pressed? button)
  (eq? button 'pressed))

;;; Creating standard context structure
(define (make-context time general-state)
  (let ((button1-current-value (is-pin-set? b1))
        (button1-previous-value (is-pin-set? b1))
        (timestep 0))
  (define (update-general-state value)
    (set! general-state (+ general-state value)))
  (define (update-time value)
    (set! time (+ time value))
    (set! timestep value)) ; remembering the last timestep
  (define (update-buttons) 
    (set! button1-previous-value button1-current-value)
    (set! button1-current-value (is-pin-set? b1)))
  (define (button1-pressed?) (and (eq? #f button1-previous-value) (eq? #t button1-current-value)))
  (lambda (msg . args)
    (case msg
      ('time time)
      ('timestep timestep)
      ('general-state general-state)
      ('update-general-state (apply update-general-state args))
      ('update-time (apply update-time args))
      ('update-buttons (update-buttons))
      ('button1-pressed? (button1-pressed?))
      (else #f))))) ; no such context variable


(define (make-finite-state-machine start-state)
  (let ((current-state start-state)
        (current-transitions (start-state 'transitions))
        (measure-value 1000)) ; default value measuring the state of one characteristic

  (define (update-value value)
    (set! measure-value (max 0 (+ measure-value value))))
  (define (measure msg . args)
    (case msg
      ('value measure-value)
      ('update (apply update-value args))))

  ;;; get the name of all input channels needed by the current state's transitions
  (define (get-transition-inputs)
    (map transition-input-name current-transitions))

  ;;; feed the FSM with current context
  (define (feed-context context)
    ;; will trigger all transitions that satisfy their predicate with given context
    (for-each (lambda (transition)
		  (let ((input (context (transition-input-name transition))))
		    (if (and input
			           ((transition-predicate transition) measure input))
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

  ; ; defining general state of Bob
  ; (define general-state-value 1000)
  ; (define (update-general-state-value value)
  ;   (set! general-state-value (+ general-state-value value)))
  ; (define general-state
  ;   (lambda (msg . args) 
  ;     (case msg 
  ;       ('value general-state-value)
  ;       ('update (apply update-general-state-value args)))))

  ; feeding context to all characteristics FSMs of bob
  (define (update context)
    (map (lambda(fsm) (fsm 'feed-context context)) characteristics))

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
        				(newline)(display "You ought to know I'm feeling very depressed"))
			        (lambda ()
        				(newline)(display "I'm feeling a bit better"))))
(define happy-state
  (make-state (lambda () 
		    		    (newline) (display "I am happy now!"))
		  		    (lambda () 
		    		    (newline)(display "I am no longer happy :("))))
		    		    

(happy-state 'add-transition
  (make-transition 
    'timestep
    (lambda (measure timestep)
	    (measure 'update (- timestep)) ; decrement happiness
	    (display-characteristic 0 (round (/ (measure 'value) 10)))
			(if (< (measure 'value) 0)
			  #t       ;; I am no longer happy now
			  #f))
	  sad-state))
(happy-state 'add-transition
  (make-transition
    'general-state
    (lambda (measure state)
      (measure 'update state)
      (display-characteristic 0 (round (/ (measure 'value) 10)))
      (if (< (measure 'value) 0)
        #t
        #f))
    sad-state))

(define play-transition
 (make-transition 
    'play-button
		(lambda (measure button)
		  (if (button-pressed? button)
			  #t
			  #f))
		happy-state))

(happy-state 'add-transition play-transition)
(sad-state 'add-transition play-transition)
(define happy-fsm (make-finite-state-machine happy-state))

(define fed-state 
  (make-state
    (lambda ()
      (fill-rectangle!  60 60 30 30 #x0F0))
    (lambda ()
      (fill-rectangle! 60 60 30 30 #xF00))))
(define unfed-state
  (make-state
    (lambda ()
      (fill-rectangle!  60 60 30 30 #xF00))
    (lambda ()
      (fill-rectangle! 60 60 30 30 #x0F0))))

(fed-state 'add-transition
  (make-transition
    'timestep
    (lambda (measure timestep)
      (measure 'update (- timestep))
      (display-characteristic 1 (round (/ (measure 'value) 10)))
        (if (< (measure 'value) 500)
          #t
          #f))
    unfed-state))
(fed-state 'add-transition
  (make-transition
    'button1-pressed?
    (lambda (measure button1-pressed?)
      (if button1-pressed? (measure 'update 100) (do-nothing))
      (display-characteristic 1 (round (/ (measure 'value) 10)))
        (if (> (measure 'value) 500)
          #t
          #f))
    fed-state))
(unfed-state 'add-transition
  (make-transition
    'timestep
    (lambda (measure timestep)
      (measure 'update (- timestep))
      (display-characteristic 1 (round (/ (measure 'value) 10)))
        (if (< (measure 'value) 500)
          #f
          #t))
    fed-state))
(unfed-state 'add-transition
  (make-transition
    'button1-pressed?
    (lambda (measure button1-pressed?)
      (if button1-pressed? (measure 'update 100) (do-nothing))
      (display-characteristic 1 (round (/ (measure 'value) 10)))
        (if (> (measure 'value) 500)
          #t
          #f))
    fed-state))

(define hunger-fsm (make-finite-state-machine fed-state))


(define context (make-context 0 0))
(define bob (make-bob (list happy-fsm hunger-fsm)))

(define (run bob context) 
    (bob 'update context)
    (wait-and-listen 100)
    (context 'update-time 1)
    ;(context 'update-general-state -1)
    (run bob context))

(run bob context)



