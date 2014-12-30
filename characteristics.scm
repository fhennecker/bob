;;; ============================================================================
;;;                               HAPPINESS
;;; ============================================================================

(define happiness-fsm
  (let ((HAPPY_NORMAL_THR 1000)
        (NORMAL_SAD_THR 400)
        (SAD_DEPRESSED_THR 200))

    ;;; ===== Happiness states =====

    (define happy-state
      (make-state (lambda () (fill-rectangle! 0 60 20 20 #x00F))
                  (lambda () (display "leaving the land of happy people"))))
    (define normal-state
      (make-state (lambda () (fill-rectangle! 0 60 20 20 #x0F0))
                  (lambda () (newline)(display "I am no longer happy "))))
    (define sad-state
      (make-state (lambda () (fill-rectangle! 0 60 20 20 #xFC0))
                  (lambda () (newline)(display "I'm feeling a bit better"))))
    (define depressed-state
      (make-state (lambda () (fill-rectangle! 0 60 20 20 #xF00))
                  (lambda () (newline)(display "I am no longer happy"))))

    ;;; ===== Happiness transitions =====

    (define (make-time-evolution-transition predicate threshold to-state)
      (make-transition 
            'timestep
            (lambda (measure timestep)
              (measure 'update (* timestep (context 'general-state)))
              (display-characteristic 0 (round (/ (measure 'value) 10)))
              (if (predicate (measure 'value) threshold) #t #f))
            to-state))

    ;;; transitions from a state to the state below 
    (happy-state 'add-transition (make-time-evolution-transition < HAPPY_NORMAL_THR normal-state))
    (normal-state 'add-transition (make-time-evolution-transition < NORMAL_SAD_THR sad-state))
    (sad-state 'add-transition (make-time-evolution-transition < SAD_DEPRESSED_THR depressed-state))
    ; we set the threshold to -1 because whe should never get out of depression by waiting
    (depressed-state 'add-transition (make-time-evolution-transition < -1 depressed-state))

    ;;; transitions from a state to the state above
    ; we set the threshold to 1301 because 
    (happy-state 'add-transition (make-time-evolution-transition > 1301 happy-state 0))
    (normal-state 'add-transition (make-time-evolution-transition > HAPPY_NORMAL_THR happy-state))
    (sad-state 'add-transition (make-time-evolution-transition > NORMAL_SAD_THR normal-state))
    (depressed-state 'add-transition (make-time-evolution-transition > SAD_DEPRESSED_THR sad-state))

    ; (define play-transition
    ;  (make-transition 
    ;     'play-button
    ;     (lambda (measure button)
    ;       (if (button-pressed? button) #t #f))
    ;     happy-state))

    ; (happy-state 'add-transition play-transition)
    ; (sad-state 'add-transition play-transition)

    (make-finite-state-machine happy-state #f)))


;;; ============================================================================
;;;                                  HUNGER
;;; ============================================================================

(define hunger-fsm
  (let ((HUNGER_SPEED 3)
        (FED_HUNGRY_THR 800)
        (HUNGRY_STARVING_THR 300))

    ;;; ===== Hunger states =====

    (define fed-state 
      (make-state (lambda ()
                    (fill-rectangle! 20 60 20 20 #x0F0)
                    (context 'update-general-state 2))
                  (lambda () (context 'update-general-state -2))))
    (define hungry-state
      (make-state (lambda ()
                    (fill-rectangle! 20 60 20 20 #xFC0)
                    (context 'update-general-state -2))
                  (lambda () (context 'update-general-state 2))))
    (define starving-state
      (make-state (lambda ()
                    (fill-rectangle! 20 60 20 20 #xF00)
                    (context 'update-general-state -5))
                  (lambda () (context 'update-general-state 5))))

    ;;; ===== Hunger transitions =====

    (define (make-time-evolution-transition predicate threshold to-state)
      (make-transition 
            'timestep
            (lambda (measure timestep)
              (measure 'update (* timestep (- HUNGER_SPEED)))
              (display-characteristic 1 (round (/ (measure 'value) 10)))
              (if (predicate (measure 'value) threshold) #t #f))
            to-state))

    (define (make-feeding-transition threshold to-state)
      (make-transition
        'button1-pressed?
        (lambda (measure button1-pressed?)
          (if button1-pressed? (measure 'update 100) (do-nothing))
          (display-characteristic 1 (round (/ (measure 'value) 10)))
          (if (> (measure 'value) threshold) #t #f))
        to-state))

    (fed-state 'add-transition (make-time-evolution-transition < FED_HUNGRY_THR hungry-state))
    (fed-state 'add-transition (make-feeding-transition 1301 fed-state))

    (hungry-state 'add-transition (make-time-evolution-transition < HUNGRY_STARVING_THR starving-state))
    (hungry-state 'add-transition (make-feeding-transition FED_HUNGRY_THR fed-state))

    (starving-state 'add-transition (make-time-evolution-transition < -1 starving-state))
    (starving-state 'add-transition (make-feeding-transition HUNGRY_STARVING_THR hungry-state))

    ;;; Returning the hunger FSM
    (make-finite-state-machine fed-state #t)))

