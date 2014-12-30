;;; ============================================================================
;;;                               HAPPINESS
;;; ============================================================================

(define happiness-fsm
  (let ((HAPPY_TO_NORMAL_THRESHOLD 1000)
        (NORMAL_TO_SAD_THRESHOLD 400)
        (SAD_TO_DEPRESSED_THRESHOLD 200))

    ;;; ===== Happiness states =====
    (define happy-state
      (make-state (lambda () 
                    (fill-rectangle! 0 60 20 20 #x00F))
                  (lambda () (display "leaving the land of happy people"))))
    (define normal-state
      (make-state (lambda () 
                    (fill-rectangle! 0 60 20 20 #x0F0))
                  (lambda () 
                    (newline)(display "I am no longer happy "))))
    (define sad-state
      (make-state (lambda () 
                    (fill-rectangle! 0 60 20 20 #xFC0))
                  (lambda ()
                    (newline)(display "I'm feeling a bit better"))))
    (define depressed-state
      (make-state (lambda () 
                    (fill-rectangle! 0 60 20 20 #xF00))
                  (lambda () 
                    (newline)(display "I am no longer happy"))))

    ;;; ===== Happy state transitions =====
    ; This is a generator of transitions based on time
    (define (make-time-evolution-transition for-value to-state real-state)
      (make-transition 
        'timestep
        (lambda (measure timestep)
          ;(measure 'update (- timestep)) ; decrement happiness
          (measure 'update (* timestep (context 'general-state)))
          (display-characteristic 0 (round (/ (measure 'value) 10)))
          (if ((if (eq? to-state 'up) > <) (measure 'value) for-value) #t #f))
        ; defining which state we need to transition to 
        real-state))

    ;;; transitions from a state to the state below 
    (happy-state 'add-transition (make-time-evolution-transition HAPPY_TO_NORMAL_THRESHOLD 'down normal-state))
    (normal-state 'add-transition (make-time-evolution-transition NORMAL_TO_SAD_THRESHOLD 'down sad-state))
    (sad-state 'add-transition (make-time-evolution-transition SAD_TO_DEPRESSED_THRESHOLD 'down depressed-state))
    ; we set the threshold to -1 because whe should never get out of depression by waiting
    (depressed-state 'add-transition (make-time-evolution-transition -1 'down depressed-state))

    ;;; transitions from a state to the state above
    ; we set the threshold to 1301 because 
    (happy-state 'add-transition (make-time-evolution-transition 1301 'up happy-state))
    (normal-state 'add-transition (make-time-evolution-transition HAPPY_TO_NORMAL_THRESHOLD 'up happy-state))
    (sad-state 'add-transition (make-time-evolution-transition NORMAL_TO_SAD_THRESHOLD 'up normal-state))
    (depressed-state 'add-transition (make-time-evolution-transition SAD_TO_DEPRESSED_THRESHOLD 'up sad-state))

    ; (define play-transition
    ;  (make-transition 
    ;     'play-button
    ;     (lambda (measure button)
    ;       (if (button-pressed? button) #t #f))
    ;     happy-state))

    ; (happy-state 'add-transition play-transition)
    ; (sad-state 'add-transition play-transition)

    (make-finite-state-machine happy-state)))


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

    ;;; ===== Starving state transitions =====
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
