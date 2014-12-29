;;; ============================================================================
;;;                               HAPPINESS
;;; ============================================================================

;;; ===== Happiness states =====
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
                
;;; ===== Happy state transitions
(happy-state 'add-transition
  (make-transition 
    'timestep
    (lambda (measure timestep)
      ;(measure 'update (- timestep)) ; decrement happiness
      (measure 'update (* timestep (context 'general-state)))
      (display-characteristic 0 (round (/ (measure 'value) 10)))
      (if (< (measure 'value) 0) #t #f))
    sad-state))

(define play-transition
 (make-transition 
    'play-button
    (lambda (measure button)
      (if (button-pressed? button) #t #f))
    happy-state))

(happy-state 'add-transition play-transition)
(sad-state 'add-transition play-transition)
(define happy-fsm (make-finite-state-machine happy-state))


;;; ============================================================================
;;;                                  HUNGER
;;; ============================================================================

(define hunger-fsm
  (let ((HUNGER_SPEED 3)
        (FED_TO_HUNGRY_THRESHOLD 800)
        (HUNGRY_TO_STARVING_THRESHOLD 300))
    ;;; ===== Hunger states =====
    (define fed-state 
      (make-state (lambda ()
                    (context 'update-general-state 2))
                  (lambda ()
                    (context 'update-general-state -2))))
    (define hungry-state
      (make-state (lambda ()
                    (context 'update-general-state -2))
                  (lambda ()
                    (context 'update-general-state 2))))
    (define starving-state
      (make-state (lambda ()
                    (context 'update-general-state -5))
                  (lambda ()
                    (context 'update-general-state 5))))

    ;;; ===== Fed state transitions =====
    ;;; Getting hungry over time
    (fed-state 'add-transition
      (make-transition
        'timestep
        (lambda (measure timestep)
          (measure 'update (* (- HUNGER_SPEED) timestep))
          (display-characteristic 1 (round (/ (measure 'value) 10)))
            (if (< (measure 'value) FED_TO_HUNGRY_THRESHOLD) #t #f))
        hungry-state))

    ;;; Getting fed 
    (fed-state 'add-transition
      (make-transition
        'button1-pressed?
        (lambda (measure button1-pressed?)
          (if button1-pressed? (measure 'update 100) (do-nothing))
          (display-characteristic 1 (round (/ (measure 'value) 10)))
          #f) ; no transition possible since we're going up from the highest state
        fed-state))

    ;;; ===== Hungry state transitions =====
    ;;; Getting hungry over time
    (hungry-state 'add-transition
      (make-transition
        'timestep
        (lambda (measure timestep)
          (measure 'update (* (- HUNGER_SPEED) timestep))
          (display-characteristic 1 (round (/ (measure 'value) 10)))
            (if (< (measure 'value) HUNGRY_TO_STARVING_THRESHOLD) #t #f))
        starving-state))

    ;;; Getting fed
    (hungry-state 'add-transition
      (make-transition
        'button1-pressed?
        (lambda (measure button1-pressed?)
          (if button1-pressed? (measure 'update 100) (do-nothing))
          (display-characteristic 1 (round (/ (measure 'value) 10)))
            (if (> (measure 'value) FED_TO_HUNGRY_THRESHOLD) #t #f))
        fed-state))

    ;;; ===== Unfed state transitions =====
    ;;; Getting hungry over time
    (starving-state 'add-transition
      (make-transition
        'timestep
        (lambda (measure timestep)
          (measure 'update (* (- HUNGER_SPEED) timestep))
          (display-characteristic 1 (round (/ (measure 'value) 10)))
          #f) ; getting hungry while starving will never transition to something 
              ; else because starving is the worst state possible
        starving-state))

    ;;; Getting fed
    (starving-state 'add-transition
      (make-transition
        'button1-pressed?
        (lambda (measure button1-pressed?)
          (if button1-pressed? (measure 'update 100) (do-nothing))
          (display-characteristic 1 (round (/ (measure 'value) 10)))
            (if (> (measure 'value) HUNGRY_TO_STARVING_THRESHOLD) #t #f))
        hungry-state))

    ;;; Returning the hunger FSM
    (make-finite-state-machine fed-state)))
