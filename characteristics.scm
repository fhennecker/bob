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

;;; ===== Hunger states =====
(define fed-state 
  (make-state (lambda ()
                (fill-rectangle!  60 60 30 30 #x0F0)
                (context 'update-general-state 2))
              (lambda ()
                (fill-rectangle! 60 60 30 30 #x080)
                (context 'update-general-state -2))))
(define unfed-state
  (make-state (lambda ()
                (fill-rectangle!  60 60 30 30 #xF00)
                (context 'update-general-state -2))
              (lambda ()
                (fill-rectangle! 60 60 30 30 #x800)
                (context 'update-general-state 2))))

;;; ===== Fed state transitions =====
;;; Getting hungry over time
(fed-state 'add-transition
  (make-transition
    'timestep
    (lambda (measure timestep)
      (measure 'update (* -5 timestep))
      (display-characteristic 1 (round (/ (measure 'value) 10)))
        (if (< (measure 'value) 500) #t #f))
    unfed-state))

;;; Getting fed 
(fed-state 'add-transition
  (make-transition
    'button1-pressed?
    (lambda (measure button1-pressed?)
      (if button1-pressed? (measure 'update 100) (do-nothing))
      (display-characteristic 1 (round (/ (measure 'value) 10)))
        (if (> (measure 'value) 500) #t #f))
    fed-state))

;;; ===== Unfed state transitions =====
;;; Getting hungry over time
(unfed-state 'add-transition
  (make-transition
    'timestep
    (lambda (measure timestep)
      (measure 'update (* -5 timestep))
      (display-characteristic 1 (round (/ (measure 'value) 10)))
        (if (< (measure 'value) 500) #f #t))
    fed-state))

;;; Getting fed
(unfed-state 'add-transition
  (make-transition
    'button1-pressed?
    (lambda (measure button1-pressed?)
      (if button1-pressed? (measure 'update 100) (do-nothing))
      (display-characteristic 1 (round (/ (measure 'value) 10)))
        (if (> (measure 'value) 500) #t #f))
    fed-state))

(define hunger-fsm (make-finite-state-machine fed-state))