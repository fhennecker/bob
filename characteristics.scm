;;; ============================================================================
;;;                                HAPPINESS
;;; ============================================================================

(define happiness-fsm
  (let ((HAPPY_NORMAL_THR 1000)
        (NORMAL_SAD_THR 400)
        (SAD_DEPRESSED_THR 200))

    ;;; ===== Happiness states =====

    (define happy-state
      (make-state (lambda () (fill-rectangle! 0 60 20 20 #x00F))
                  (lambda () (do-nothing))))
    (define normal-state
      (make-state (lambda () (fill-rectangle! 0 60 20 20 #x0F0))
                  (lambda () (do-nothing))))
    (define sad-state
      (make-state (lambda () (fill-rectangle! 0 60 20 20 #xFC0))
                  (lambda () (do-nothing))))
    (define depressed-state
      (make-state (lambda () (fill-rectangle! 0 60 20 20 #xF00))
                  (lambda () (do-nothing))))

    ;;; ===== Happiness transitions =====

    (define (make-time-evolution-transition predicate threshold to-state)
      (make-transition 
            'timestep
            (lambda (measure)
              (measure 'update (* (context 'timestep) (context 'general-state)))
              (display-characteristic 0 (round (/ (measure 'value) 10)))
              (if (predicate (measure 'value) threshold) #t #f))
            to-state))

    (define (make-play-transition predicate threshold to-state)
      (make-transition
        'button1-pressed? ; play button triggered
        (lambda (measure)
          (if (eq? (exhaustion-fsm 'state) 'asleep)
            (do-nothing)
            (begin
              (context 'update-buttons) ; unpressing the play button
              (led-animation)
              (let ((picked-led (modulo (context 'ax) 3))) ; picking "random" value
                (define (win)
                  (blinkled picked-led 2) (context 'update-buttons) (measure 'update 200))
                (define (lose)
                  (blinkled picked-led 2) (context 'update-buttons) (measure 'update (- 40)))
                (define (wait-for-button) ; listening for any button press
                  (context 'update-buttons)
                  (cond ((or (and (eq? picked-led 0) (context 'button-pressed? 0))
                             (and (eq? picked-led 1) (context 'button-pressed? 1))
                             (and (eq? picked-led 2) (context 'button-pressed? 2)))
                          (win))
                        ; pushed on the wrong button
                        ((or (context 'button-pressed? 0) (context 'button-pressed? 1) (context 'button-pressed? 2))
                          (lose))
                        ; no button pressed
                        (else (wait-for-button))))
                (wait-for-button))))
          (if (predicate (measure 'value) threshold) #t #f))
        to-state))

    ;;; transitions from a state to the state below 
    (happy-state 'add-transition (make-time-evolution-transition < HAPPY_NORMAL_THR normal-state))
    (normal-state 'add-transition (make-time-evolution-transition < NORMAL_SAD_THR sad-state))
    (sad-state 'add-transition (make-time-evolution-transition < SAD_DEPRESSED_THR depressed-state))

    ;;; transitions from a state to the state above
    (normal-state 'add-transition (make-time-evolution-transition > HAPPY_NORMAL_THR happy-state))
    (sad-state 'add-transition (make-time-evolution-transition > NORMAL_SAD_THR normal-state))
    (depressed-state 'add-transition (make-time-evolution-transition > SAD_DEPRESSED_THR sad-state))

    ;;; play transitions
    (happy-state 'add-transition (make-play-transition < HAPPY_NORMAL_THR normal-state))
    (normal-state 'add-transition (make-play-transition < NORMAL_SAD_THR sad-state))
    (sad-state 'add-transition (make-play-transition < SAD_DEPRESSED_THR depressed-state))
  
    (normal-state 'add-transition (make-play-transition > HAPPY_NORMAL_THR happy-state))
    (sad-state 'add-transition (make-play-transition > NORMAL_SAD_THR normal-state))    
    (depressed-state 'add-transition (make-play-transition > SAD_DEPRESSED_THR sad-state))

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
            (lambda (measure)
              (measure 'update (* (context 'timestep) (- HUNGER_SPEED)))
              (display-characteristic 1 (round (/ (measure 'value) 10)))
              (if (predicate (measure 'value) threshold) #t #f))
            to-state))

    (define (make-feeding-transition threshold to-state)
      (make-transition
        'button0-pressed?
        (lambda (measure)
          (define (choose-meal)
            (context 'update-buttons) ; "unpress" the button which took us to this transition
            ; wait for another button press, then "unpress" that button again so 
            ; it doesn't transition to some other state in another fsm
            (cond ((context 'button-pressed? 0) (measure 'update 50) (context 'update-buttons))
                  ((context 'button-pressed? 1) (measure 'update 100) (context 'update-buttons))
                  ((context 'button-pressed? 2) 
                    (measure 'update 300) 
                    (health-fsm 'update-measure (- 50))
                    (context 'update-buttons))
                  (else (choose-meal))))
          (if (eq? (exhaustion-fsm 'state) 'asleep)
              (do-nothing)
              (choose-meal))
          (display-characteristic 1 (round (/ (measure 'value) 10)))
          (if (> (measure 'value) threshold) #t #f))
        to-state))

    (fed-state 'add-transition (make-time-evolution-transition < FED_HUNGRY_THR hungry-state))
    ; the change-state predicate will never be true since feeding only transitions to
    ; upper states and fed-state is the highest state.
    (fed-state 'add-transition (make-feeding-transition 1301 fed-state))

    (hungry-state 'add-transition (make-time-evolution-transition < HUNGRY_STARVING_THR starving-state))
    (hungry-state 'add-transition (make-feeding-transition FED_HUNGRY_THR fed-state))

    (starving-state 'add-transition (make-time-evolution-transition < -1 starving-state))
    (starving-state 'add-transition (make-feeding-transition HUNGRY_STARVING_THR hungry-state))

    ;;; Returning the hunger FSM
    (make-finite-state-machine fed-state #t)))


;;; ============================================================================
;;;                                 EXHAUSTION
;;; ============================================================================

(define exhaustion-fsm
  (let ((EXHAUSTION_SPEED 5)
        (AWAKE_SLEEPY_THR 500)
        (SLEEPY_EXHAUSTED_THR 200))

    ;;; ===== Exhaustion states =====

    (define awake-state 
      (make-state (lambda ()
                    (fill-rectangle! 40 60 20 20 #x0F0)
                    (context 'update-general-state 1))
                  (lambda () (context 'update-general-state -1))
                  'awake))
    (define sleepy-state
      (make-state (lambda ()
                    (fill-rectangle! 40 60 20 20 #xFC0)
                    (context 'update-general-state -2))
                  (lambda () (context 'update-general-state 2))
                  'sleepy))
    (define exhausted-state
      (make-state (lambda ()
                    (fill-rectangle! 40 60 20 20 #xF00)
                    (context 'update-general-state -5))
                  (lambda () (context 'update-general-state 5))
                  'exhausted))
    (define asleep-state
      (make-state (lambda () (bob 'sleep))
                  (lambda () (bob 'wake-up))
                  'asleep))

    ;;; ===== Exhaustion transitions =====
    (define (make-time-evolution-transition predicate threshold to-state)
      (make-transition 
            'timestep
            (lambda (measure)
              (measure 'update (* (context 'timestep) (- EXHAUSTION_SPEED)))
              (display-characteristic 2 (round (/ (measure 'value) 10)))
              (if (predicate (measure 'value) threshold) #t #f))
            to-state))

    (define (make-sleep-transition)
      (make-transition
        'ax
        (lambda (measure)
          (if (< (context 'ax) 18200) #t #f))
        asleep-state))
    (define (make-wake-up-transition low-threshold high-threshold to-state)
      (make-transition
        'ax
        (lambda (measure)
          (if (> (context 'ax) 18200) 
            (if (and (>= (measure 'value) low-threshold) (< (measure 'value) high-threshold))
                #t #f) 
            #f))
        to-state))
    (define (make-recovery-transition) ; recovering energy when sleeping
      (make-transition
        'timestep
        (lambda (measure)
          (measure 'update (* (context 'timestep) EXHAUSTION_SPEED))
          (display-characteristic 2 (round (/ (measure 'value) 10)))
          (if (eq? (measure 'value) 1300) #t #f))
        asleep-state))

    (awake-state 'add-transition (make-time-evolution-transition < AWAKE_SLEEPY_THR sleepy-state))
    (sleepy-state 'add-transition (make-time-evolution-transition < SLEEPY_EXHAUSTED_THR exhausted-state))

    (sleepy-state 'add-transition (make-time-evolution-transition > AWAKE_SLEEPY_THR awake-state))
    (exhausted-state 'add-transition (make-time-evolution-transition > SLEEPY_EXHAUSTED_THR sleepy-state))

    ; to sleep transition
    (awake-state 'add-transition (make-sleep-transition))
    (sleepy-state 'add-transition (make-sleep-transition))
    (exhausted-state 'add-transition (make-sleep-transition))

    (asleep-state 'add-transition (make-wake-up-transition AWAKE_SLEEPY_THR 1301 awake-state))
    (asleep-state 'add-transition (make-wake-up-transition SLEEPY_EXHAUSTED_THR AWAKE_SLEEPY_THR sleepy-state))
    (asleep-state 'add-transition (make-wake-up-transition 0 SLEEPY_EXHAUSTED_THR exhausted-state))
    (asleep-state 'add-transition (make-recovery-transition))

  (make-finite-state-machine awake-state #t)))

;;; ============================================================================
;;;                                  HEALTH
;;; ============================================================================

(define health-fsm
  (let ((HEALTHY_ILL_THR 400)
        (CURE_VALUE 200)
        (MEDICATION_OVERLOAD_THR 600)
        (MEDICATION_WEAROUT_SPEED 3)
        (medication-amount 0)) ; taking too much medication in not enough time is dangerous!

    ;;; ===== Health states =====

    (define healthy-state 
      (make-state (lambda ()
                    (fill-rectangle! 60 60 20 20 #x0F0)
                    (context 'update-general-state 1))
                  (lambda () (context 'update-general-state -1))))
    (define ill-state
      (make-state (lambda ()
                    (fill-rectangle! 60 60 20 20 #xF00)
                    (context 'update-general-state -5))
                  (lambda () (context 'update-general-state 5))))

    ;;; ===== Health transitions =====

    ; This transition doesn't update measure itself, but it checks if we didn't
    ; give too much medication to the pet
    (define (make-time-evolution-transition)
      (make-transition 
            'timestep
            (lambda (measure)
              (set! medication-amount (min 1300 (max 0 (- medication-amount (* MEDICATION_WEAROUT_SPEED (context 'timestep))))))
              (display-characteristic 4 (round (/ medication-amount 10)))
              (display-characteristic 3 (round (/ (measure 'value) 10)))
              #f)
            healthy-state)) ; dummy state

    (define (make-cure-transition predicate threshold to-state)
      (make-transition
        'button2-pressed?
        (lambda (measure)
          (if (eq? (exhaustion-fsm 'state) 'asleep)
            (do-nothing)
            (begin
              (if (context 'button-pressed? 2) 
                  (begin  (measure 'update 200)
                          (set! medication-amount (+ medication-amount 200))) 
                  (do-nothing))
              (if (> medication-amount MEDICATION_OVERLOAD_THR) (measure 'update (- 400)) (do-nothing))))
          (display-characteristic 3 (round (/ (measure 'value) 10)))
          if (predicate (measure 'value) threshold #t #f))
        to-state))

    (healthy-state 'add-transition (make-time-evolution-transition))
    (ill-state 'add-transition (make-time-evolution-transition))

    (healthy-state 'add-transition (make-cure-transition < HEALTHY_ILL_THR ill-state))
    (ill-state 'add-transition (make-cure-transition > HEALTHY_ILL_THR healthy-state))

  (make-finite-state-machine healthy-state #t)))

;;; ============================================================================
;;;                                 COMPLIANCE
;;; ============================================================================

(define compliance-fsm
  (let ((SUBMISSIVE_REBELLIOUS_THR 500))

    ;;; ===== Compliance states =====

    (define submissive-state 
      (make-state (lambda ()
                    (fill-rectangle! 80 60 20 20 #x0F0))
                  (lambda () (do-nothing))))
    (define rebellious-state
      (make-state (lambda ()
                    (fill-rectangle! 80 60 20 20 #xF00))
                  (lambda () (do-nothing))))

    ;;; ===== Compliance transitions =====

    (define (make-time-evolution-transition predicate threshold to-state)
      (make-transition 
            'timestep
            (lambda (measure)
              (measure 'set (+ 800 (* 100 (context 'general-state))))
              (display-characteristic 5 (round (/ (measure 'value) 10)))
              (if (predicate (measure 'value) threshold) #t #f))
            to-state))

    (submissive-state 'add-transition (make-time-evolution-transition < SUBMISSIVE_REBELLIOUS_THR rebellious-state))
    (rebellious-state 'add-transition (make-time-evolution-transition > SUBMISSIVE_REBELLIOUS_THR submissive-state))
  
  (make-finite-state-machine submissive-state #f)))
