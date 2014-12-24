(define (get-input context input-name)
  (assoc input-name context))
(define input-value cdr)
(define (button-pressed? button)
  (eq? button 'pressed))


(define (make-finite-state-machine start-state)
  (let ((current-state start-state)
        (current-transitions (start-state 'transitions)))

  ;;; get the name of all input channels needed by the current state's transitions
  (define (get-transition-inputs)
    (map transition-input-name current-transitions))

  ;;; feed the FSM with some sensor information (context)
  (define (feed-context context)
    ;; will trigger all transitions that satisfy their predicate with given context
    (for-each (lambda (transition)
		  (let ((input (get-input context (transition-input-name transition))))
		    (when (and input
			             ((transition-predicate transition) (input-value input)))
			        (change-state (transition-state transition)))))
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

(define (do-nothing) '())

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

;;;;;;;;;;;;;   
;;;; tests
;;;;;;;;;;;;;
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
        'timer
			  (let ((prev-timer #f))  ; this is a pretty complex predicate, basically updates and checks a timer
			    (lambda (timer)
            (newline)
			      (display "The time is: ") (display timer) (newline)
			      (let ((delta-time (if prev-timer (- timer prev-timer) 0)))  ; time difference
				    (set! happiness-counter (- happiness-counter delta-time)) ; decrement happiness
				    (set! prev-timer timer)  ; remember the current timer value
				    (display "I am this happy: ") (display happiness-counter) (newline)
  					(if (< happiness-counter 0)
  					    #t       ;; I am no longer happy now
  					    #f))))
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
(define fsm (make-finite-state-machine happy-state))

(fsm 'feed-context (list (cons 'timer 0) (cons 'play-button 'unpressed)))
(fsm 'feed-context (list (cons 'timer 10) (cons 'play-button 'unpressed)))
(fsm 'feed-context (list (cons 'timer 100) (cons 'play-button 'unpressed)))
(fsm 'feed-context (list (cons 'timer 1000) (cons 'play-button 'unpressed)))
(fsm 'feed-context (list (cons 'timer 1010) (cons 'play-button 'unpressed)))
(fsm 'feed-context (list (cons 'timer 1020) (cons 'play-button 'pressed)))
(fsm 'feed-context (list (cons 'timer 1030) (cons 'play-button 'unpressed)))
(fsm 'feed-context (list (cons 'timer 1500) (cons 'play-button 'pressed)))
(fsm 'feed-context (list (cons 'timer 1520) (cons 'play-button 'unpressed)))

