(declaim (optimize (speed 0) (safety 3) (debug 3)))

;; (ql:quickload :basic-morphenator/tests)
;; (in-package #:basic-morphenator-test)
;; (fiveam:run-all-tests)
;; (run! 'basic-equalities)

;; (setf 5am:*on-error* :debug)

;;; =========================== fixtures =======================================
(def-fixture prepare-variables (n)
    (let ((a n)
          (b 2)
          (c 3)
          (expected 6))
      ;; body
      (&body)
      ;; teardown code
      (setf a nil
            b nil
            c nil)))

;;; ======================= tests ==============================================
(fiveam:test basic-equalities
      "basic equalities"
      (with-fixture prepare-variables (1)
                    (is (eq a 1))
                    (is (eq b 2))
                    (is (eq c 3))
                    (is (eq (+ a b c) expected))))
