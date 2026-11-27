;;; all rests loader

;;; in this folder, run with:
;; cd ~/Programming/Lisp/basic-morphenator/tests/
;; sbcl --load ./all-tests-loader.lisp

(ql:quickload :basic-morphenator/tests)
(in-package #:basic-morphenator-test)

(5am:run! 'basic-morphenator-suite)

(defun in-repl? ()
  (equalp (package-name
           (symbol-package
            (slot-value *standard-output* 'symbol)))
          "SWANK")) ; terminal package name is SB-SYS

(if (in-repl?)
    (progn
      (warn "finished running the tests in REPL"))
    (progn
      (warn "finished running the test in the terminal, quitting")
      (sb-ext:quit)))
