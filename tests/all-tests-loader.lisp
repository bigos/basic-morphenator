;;; all rests loader

;;; in this folder, run with:
;; cd ~/Programming/Lisp/basic-morphenator/tests/
;; sbcl --load ./all-tests-loader.lisp


;;; all-tests-loader.lisp
(ql:quickload :basic-morphenator/tests)
(ql:quickload :demo-morphenator/tests)

(5am:run! 'basic-morphenator-test:basic-morphenator-suite)
(5am:run! 'demo-morphenator-test:demo-morphenator-suite)

(defun in-repl? ()
  (equalp (package-name (symbol-package (slot-value *standard-output* 'symbol)))
          "SWANK"))

(if (in-repl?)
    (warn "finished running the tests in REPL")
    (progn (warn "finished running the tests in the terminal, quitting")
           (sb-ext:quit)))
