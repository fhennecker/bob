(define (get-input context input-name)
  (assoc input-name context))
(define input-value cdr)
(define (button-pressed? button)
  (eq? button 'pressed))

;;; Creating standard context structure
(define (make-context time general-state)
  (define (update-general-state value)
    (set! general-state (+ general-state value)))
  (define (update-time value)
    (set! time (+ time value)))
  (lambda (msg . args)
    (case msg
      ('time time)
      ('general-state general-state)
      ('update-general-state (apply update-general-state args))
      ('update-time (apply update-time args))
      (else #f)))) ; no such context variable


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
      ('update (apply update args)))))

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
(test-context 'update-time +4)
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
  				    (newline) (display "I am this happy (time) : ") (display happiness-counter)
    					(if (< happiness-counter 0)
    					    #t       ;; I am no longer happy now
    					    #f))))
			  sad-state))
    (happy-state 'add-transition
      (make-transition
        'general-state
        (let ((prev-state #f))
          (lambda (state)
            (let ((delta-state (if prev-state (- state prev-state) 0)))
              (set! happiness-counter (+ happiness-counter delta-state))
              (set! prev-state state)
              (newline) (display "I am this happy (state) : ") (display happiness-counter)
              (if (< happiness-counter 0)
                #t
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
(define happy-fsm (make-finite-state-machine happy-state))


(define context (make-context 0 1000))
(define bob (make-bob (list happy-fsm)))
(bob 'update context)
(context 'update-time 10)
(bob 'update context)
(context 'update-time 100)
(context 'update-general-state -3)
(bob 'update context)



