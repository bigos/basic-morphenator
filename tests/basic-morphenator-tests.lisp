(declaim (optimize (speed 0) (safety 3) (debug 3)))

;; (ql:quickload :basic-morphenator/tests)
;; (in-package #:basic-morphenator-test)
;; (fiveam:run-all-tests)
;; (run! 'basic-equalities)

;;; =========================== fixtures =======================================
(def-fixture prepare-varables (n)
    (let ((a n)
          (b 2)
          (c 3))
      ;; body
      (&body)
      ;; teardown code
      ))

;;; ======================= tests ==============================================
(test basic-equalities
      "basic equalities"
      (with-fixture prepare-varables ((1))
                    (is (eq a 1))
                    (is (eq b 2))
                    (is (eq c 3))
                    (is (eq (+ a b c) 6))))
