(defpackage basic-morphenator-system
  (:use :common-lisp :asdf))
;;; some say it is a good practice to use a separate package for system declaration
(in-package :basic-morphenator-system)

(defsystem "basic-morphenator"
    :version "0.0.1"
    :author "https://github.com/bigos"
    :license "PUBLIC DOMAIN"
    :depends-on (#:clops-gui
                 #:sycamore
                 #:serapeum
                 #:cl-gtk4 #:cl-gdk4 #:cl-glib #:cl-cairo2
                 #:defclass-std
                 )
    :pathname "src/"
  :components ((:file "basic-morphenator")
               (:file "classes")
               (:file "methods")
               (:file "main")
               )
    :description "basic morphenator for GUI for clops in separate system")

(defsystem "basic-morphenator/tests"
    :depends-on ("basic-morphenator" "fiveam")
    :pathname "tests/"
    :serial T
    :components ((:file "packages")
                 (:file "basic-morphenator-tests"))
    :perform (test-op (op c)
                      (uiop:symbol-call :fiveam :run-all-tests)))
