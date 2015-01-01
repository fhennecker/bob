(load "lib.scm")

;;; Initialisations
;;; This line initializes : ax, ay (accelerometer values);
;;; l1 (green led), l2 (orange led), l3 (red led)
;;; b1, b2, b3 (all the buttons)
(load "hardware-initialisations.scm")

;;; Utilities
(load "utilities.scm")

(clear-screen)

(define (get-input context input-name)
  (assoc input-name context))
(define input-value cdr)

(define (make-button object)
  (let ((prev (is-pin-set? object))
        (current (is-pin-set? object)))
    (define (update)
      (set! prev current)
      (set! current (is-pin-set? object)))
    (lambda (msg . args)
      (case msg
        ('prev prev)
        ('is-on? current)
        ('update (update))
        (else (error))))
  ))

;;; ===== Context =====
;;; Creating standard context structure
(define (make-context time general-state)
  (let ((buttons (list (make-button b0) (make-button b1) (make-button b2)))
        (timestep 1))
  (define (update-general-state value)
    (set! general-state (+ general-state value)))
  (define (update-time value)
    (set! time (+ time value))
    (set! timestep value)) ; remembering the last timestep
  (define (update-buttons) 
    (map (lambda(b) (b 'update)) buttons))
  (define (button-pressed? button) 
    (and (eq? #f ((list-ref buttons button) 'prev)) ((list-ref buttons button) 'is-on?)))
  (lambda (msg . args)
    (case msg
      ('time time)
      ('timestep timestep)
      ('general-state general-state)
      ('update-general-state (apply update-general-state args))
      ('update-time (apply update-time args))
      ('update-buttons (update-buttons))
      ('button0-pressed? (button-pressed? 0))
      ('button1-pressed? (button-pressed? 1))
      ('button2-pressed? (button-pressed? 2))
      ('button-pressed? (apply button-pressed? args))
      ('ax (pulse_in ax))
      ('ay (pulse_in ay))
      (else #f))))) ; no such context variable


;;; ===== Finite State Machine ===== 
(define (make-finite-state-machine start-state deathly)
  (let ((current-state start-state)
        (current-transitions (start-state 'transitions))
        (measure-value 1000)) ; default value measuring the state of one characteristic

    (define (update-value value)
      (set! measure-value (min 1300 (max 0 (+ measure-value value))))
      (if (and deathly (eq? measure-value 0)) (bob 'die) (do-nothing)))
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
  			           ((transition-predicate transition) measure))
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

    (define (kickstart)
      (current-state 'entry-action))

    (lambda (msg . args)
      (case msg
        ('feed-context (apply feed-context args))
        ('get-inputs (get-transition-inputs))
        ('kickstart (kickstart))
        (else (error "Msg not understood: " msg))))))


;;; ===== Transitions =====
(define (make-transition input-name predicate new-state) 
  (list input-name predicate new-state))
(define transition-input-name car)
(define transition-predicate cadr)
(define transition-state caddr)


;;; ===== States =====
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


;;; ===== Bob =====
;;; Bob is our character. It's an agglomerate of FSMs (characteristics).
;;; The more characteristics are in a bad state, the quicker its general state 
;;; goes down. 
(define (make-bob characteristics)
  (let ((is-dead? #f))
    ; feeding context to all characteristics FSMs of bob
    (define (update context)
      (map (lambda(fsm) (fsm 'feed-context context)) characteristics))

    ; Performing entry action of all fsm start states
    (define (kickstart)
      (map (lambda(fsm) (fsm 'kickstart)) characteristics))

    (define (die) 
      (set! is-dead? #t)
      (color-screen #x000)
      (fill-rectangle! 60 20 10 90 #xFFF)
      (fill-rectangle! 30 50 70 10 #xFFF))

    (lambda (msg . args)
      (case msg
        ('update (apply update args))
        ('kickstart (kickstart))
        ('die (die))
        ('is-dead? is-dead?)
        (else (error "Message not understood: " msg))))))


;;; Creating all FSMs, states and transitions
(load "characteristics.scm")


;;; ===== Tests =====
(define testing #f)

(if testing
    (load "tests.scm")
    (do-nothing))


;;; ===== Main loop and run =====
(define context (make-context 0 0))
(define bob (make-bob (list happiness-fsm hunger-fsm exhaustion-fsm health-fsm)))
(bob 'kickstart)

(define (run bob context iteration) 
  (wait-and-listen 10)
  (context 'update-time 1)
  (display-characteristic 12 iteration)
  (if (not (bob 'is-dead?)) 
      (run bob context (+ 1 iteration)) 
      (do-nothing)))

(run bob context 0)
(bob 'die)
