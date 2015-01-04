;;; ============================================================================
;;;                                HAPPINESS
;;; ============================================================================

(define happiness-fsm
  (let ((HAPPY_NORMAL_THR 1000)
        (NORMAL_SAD_THR 400)
        (SAD_DEPRESSED_THR 200))

    ;;; ===== Happiness states =====

    (define happy-state
      (make-state (lambda () (do-nothing))
                  (lambda () (do-nothing))
                  'happy 
                  (lambda () (do-nothing))))
    (define normal-state
      (make-state (lambda () (do-nothing))
                  (lambda () (do-nothing))
                  'normal 
                  (lambda () (do-nothing))))
    (define sad-state
      (make-state (lambda () (do-nothing))
                  (lambda () (do-nothing))
                  'sad 
                  (lambda () (do-nothing))))
    (define depressed-state
      (make-state (lambda () (blinkled 2 3))
                  (lambda () (do-nothing))
                  'depressed 
                  (lambda () (do-nothing))))

    ;;; ===== Happiness transitions =====

    (define (make-time-evolution-transition predicate threshold to-state)
      (make-transition 
            'timestep
            (lambda (measure)
              (measure 'update (* (context 'timestep) (context 'general-state)))
              (draw-bob-smile (- (scale-value (measure 'value) 1300 20) 10))
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
  (let ((HUNGER_SPEED 5)
        (FED_HUNGRY_THR 800)
        (HUNGRY_STARVING_THR 300))

    ;;; ===== Hunger states =====

    (define fed-state 
      (make-state (lambda () (context 'update-general-state 2))
                  (lambda () (context 'update-general-state -2))
                  'fed
                  (lambda () (do-nothing))))
    (define hungry-state
      (make-state (lambda () (context 'update-general-state -2)
                              (if (eq? (compliance-fsm 'state) 'rebellious) (blinkled 2 2)))
                  (lambda () (context 'update-general-state 2))
                  'hungry
                  (lambda () (do-nothing))))
    (define starving-state
      (make-state (lambda () 
                      (blinkled 2 3)
                      (context 'update-general-state -5))
                  (lambda () (context 'update-general-state 5))
                  'starving
                  (lambda () (do-nothing))))

    ;;; ===== Hunger transitions =====

    (define (make-time-evolution-transition predicate threshold to-state)
      (make-transition 
            'timestep
            (lambda (measure)
              (measure 'update (* (context 'timestep) (- HUNGER_SPEED)))
              (draw-bob-belly (scale-value (measure 'value) 1300 50))
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
            (cond ((context 'button-pressed? 0) 
                      ; will only eat a big snack if he is rebellious, and will only eat
                      ; small food when ill
                      (if (and (eq? (compliance-fsm 'state) 'rebellious) (not (eq? (health-fsm 'state) 'ill))) 
                        (do-nothing)
                        (measure 'update 50)) 
                      (context 'update-buttons))
                  ((context 'button-pressed? 1) 
                      (if (eq? (compliance-fsm 'state) 'rebellious) 
                        (do-nothing)
                        (measure 'update 100)) 
                      (context 'update-buttons))
                  ((context 'button-pressed? 2) 
                    (if (eq? (health-fsm 'state) 'ill)
                      (do-nothing)
                      (begin (measure 'update 300) 
                        (health-fsm 'update-measure (- 50))))
                    (context 'update-buttons))
                  (else (choose-meal))))
          (if (eq? (exhaustion-fsm 'state) 'asleep)
              (do-nothing)
              (choose-meal))
          (draw-bob-belly (scale-value (measure 'value) 1300 50))
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
  (let ((EXHAUSTION_SPEED 3)
        (AWAKE_SLEEPY_THR 500)
        (SLEEPY_EXHAUSTED_THR 200))

    ;;; ===== Exhaustion states =====

    (define awake-state 
      (make-state (lambda () (context 'update-general-state 1))
                  (lambda () (context 'update-general-state -1))
                  'awake
                  (lambda () (do-nothing))))
    (define sleepy-state
      (make-state (lambda () (context 'update-general-state -2)
                              (if (eq? (compliance-fsm 'state) 'rebellious) (blinkled 2 2) (do-nothing)))
                  (lambda () (context 'update-general-state 2))
                  'sleepy
                  (lambda () (do-nothing))))
    (define exhausted-state
      (make-state (lambda () 
                        (blinkled 2 3)
                        (context 'update-general-state -5))
                  (lambda () (context 'update-general-state 5))
                  'exhausted
                  (lambda () (do-nothing))))
    (define asleep-state
      (make-state (lambda ()  (set-background-color! #x000)
                              (bob 'update-graphics))
                  (lambda ()  (set-background-color! #xFFF)
                              (bob 'update-graphics))
                  'asleep
                  (lambda () (draw-bob-eyes 1))))

    ;;; ===== Exhaustion transitions =====
    (define (make-time-evolution-transition predicate threshold to-state)
      (make-transition 
            'timestep
            (lambda (measure)
              (measure 'update (* (context 'timestep) (- EXHAUSTION_SPEED)))
              (draw-bob-eyes (scale-value (measure 'value) 1300 20))
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
      (make-state (lambda ()  (context 'update-general-state 1)
                              (set! bob-eyes-color #xFFF))
                  (lambda () (context 'update-general-state -1))
                  'healthy
                  (lambda () (do-nothing))))
    (define ill-state
      (make-state (lambda ()  (context 'update-general-state -5)
                              (set! bob-eyes-color #x0F0))
                  (lambda () (context 'update-general-state 5))
                  'ill
                  (lambda () (do-nothing))))

    ;;; ===== Health transitions =====

    ; This transition doesn't update measure itself, but it checks if we didn't
    ; give too much medication to the pet as well as checking if the value hasn't
    ; been affected by other FSMs (poop for example)
    (define (make-time-evolution-transition threshold to-state)
      (make-transition 
            'timestep
            (lambda (measure)
              (set! medication-amount (min 1300 (max 0 (- medication-amount (* MEDICATION_WEAROUT_SPEED (context 'timestep))))))
              (if (< (measure 'value) threshold) #t #f))
            to-state)) ; dummy state

    (define (make-cure-transition predicate threshold to-state)
      (make-transition
        'button2-pressed?
        (lambda (measure)
          (if (eq? (exhaustion-fsm 'state) 'asleep)
            (do-nothing)
            (begin
              (if (context 'button-pressed? 2) 
                (begin 
                  (define (choose-cure) ; medicine, poop/clean the room
                    (context 'update-buttons)
                    (cond ((context 'button-pressed? 0) 
                              (measure 'update 200)
                              (set! medication-amount (+ medication-amount 200)) 
                              (context 'update-buttons))
                          ((context 'button-pressed? 1)
                              (poop-fsm 'update-measure 1000) ; makes him poop and clean the room
                              (context 'update-buttons))
                          ((context 'button-pressed? 2) (context 'update-buttons)) ; doing nothing
                          (else (choose-cure))))
                  (choose-cure)
                  ) 
                  (do-nothing))
              (if (> medication-amount MEDICATION_OVERLOAD_THR) (measure 'update (- 400)) (do-nothing))))
          if (predicate (measure 'value) threshold #t #f))
        to-state))

    (healthy-state 'add-transition (make-time-evolution-transition HEALTHY_ILL_THR ill-state))
    (ill-state 'add-transition (make-time-evolution-transition -1 ill-state))

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
      (make-state (lambda () (do-nothing))
                  (lambda () (do-nothing))
                  'submissive
                  (lambda () (draw-bob-nice-eyebrows))))
    (define rebellious-state
      (make-state (lambda () (do-nothing))
                  (lambda () (do-nothing))
                  'rebellious
                  (lambda () (draw-bob-mean-eyebrows))))

    ;;; ===== Compliance transitions =====

    (define (make-time-evolution-transition predicate threshold to-state)
      (make-transition 
            'timestep
            (lambda (measure)
              (measure 'set (+ 800 (* 100 (context 'general-state))))
              (if (predicate (measure 'value) threshold) #t #f))
            to-state))

    (submissive-state 'add-transition (make-time-evolution-transition < SUBMISSIVE_REBELLIOUS_THR rebellious-state))
    (rebellious-state 'add-transition (make-time-evolution-transition > SUBMISSIVE_REBELLIOUS_THR submissive-state))
  
  (make-finite-state-machine submissive-state #f)))

;;; ============================================================================
;;;                                   POOP
;;; ============================================================================

(define poop
  (let ((DIGESTION_SPEED 3)
        (poops-number 0)
        (digested 0)
        (to-digest 0))
    (define (fill-stomach value)
      (set! to-digest (+ to-digest value)))
    (define (timestep)
      (set! to-digest (- to-digest (* DIGESTION_SPEED (context 'timestep))))
      (set! digested (+ digested (* DIGESTION_SPEED (context 'timestep)))))
    (lambda (msg . args)
      (case msg
        ('fill-stomach (apply fill-stomach args))
        ('timestep)))
  ))


(define poop-fsm
  (let ((TOILET_NEED_THR 300)
        (POOP_THR 10)
        (DIGESTION_SPEED 4)
        (number-of-poops 0)
        (digesting 0))

    ;;; ===== Poop states =====

    (define digesting-state
      (make-state (lambda () (set! number-of-poops 0))
                  (lambda () (do-nothing))
                  'digesting
                  (lambda ()  (draw-poops number-of-poops)
                              (erase-poop-warning))))
    (define toilet-need-state
      (make-state (lambda () (blinkled 2 2))
                  (lambda () (do-nothing))
                  'toilet-need
                  (lambda () (draw-poop-warning) )))
    (define poop-state
      (make-state (lambda () (do-nothing))
                  (lambda () (do-nothing))
                  'poop
                  (lambda () (draw-poops number-of-poops))))

    ;;; ===== Poop transitions =====

    (define (make-time-evolution-transition threshold to-state)
      (make-transition 
            'timestep
            (lambda (measure)
              (measure 'update (- (* DIGESTION_SPEED (context 'timestep))))
              (if (< (measure 'value) threshold) #t #f))
            to-state))

    (define (make-pooping-transition) ; pooping regularly when in poop-state
      (make-transition
        'timestep
        (lambda (measure)
          (set! digesting (+ digesting (* DIGESTION_SPEED (context 'timestep))))
          (health-fsm 'update-measure (* (context 'timestep) number-of-poops -2))
          (if (> digesting 50)
            (begin (set! number-of-poops (+ number-of-poops 1))
              (set! digesting 0)
              (draw-poops number-of-poops))
            (do-nothing))
          (if (> (measure 'value) POOP_THR) 
            (begin (measure 'set 1000) #t) #f))
        digesting-state))

    (digesting-state 'add-transition (make-time-evolution-transition TOILET_NEED_THR toilet-need-state))
    (toilet-need-state 'add-transition (make-time-evolution-transition POOP_THR poop-state))
    (poop-state 'add-transition (make-pooping-transition))

  (make-finite-state-machine digesting-state #f)))

