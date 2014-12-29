;;; ===== Mini test suite =====
(define (fail testid) (begin
  (display "\n\033[31mTEST ")
  (display testid)
  (display " FAILED ===================================================\033[0m")))
(define (succeed testid) (begin
  (display "\n\033[32mTEST ")
  (display testid)
  (display " SUCCEEDED ================================================\033[0m")))
(define (test testname predicate)
    (if predicate (succeed testname) (fail testname)))

;;; ===== Tests =====
(define test-context
  (make-context 123 10))
(test "CTX01" (equal? (test-context 'time) 123))
(test "CTX02" (equal? (test-context 'general-state) 10))
(test-context 'update-general-state -3)
(test "CTX03" (equal? (test-context 'general-state) 7))
(test-context 'update-time 4)
(test "CTX04" (equal? (test-context 'time) 127))