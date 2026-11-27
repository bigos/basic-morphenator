(defsystem "basic-morphenator"
  :version "0.0.1"
  :author "https://github.com/bigos"
  :license "PUBLIC DOMAIN"
  :depends-on (#:clops-gui
               #:sycamore
               #:closer-mop
               #:cl-gtk4 #:cl-gdk4 #:cl-glib #:cl-cairo2
               #:defclass-std
               )
  :pathname "src/"
  :serial T
  :components ((:file "packages")
               (:file "basic-morphenator")
               (:file "classes")
               (:file "gui-color")
               (:file "geometry")
               (:file "damage")
               (:file "protocols")
               (:file "text")
               (:file "drawing")
               (:file "helpers")
               (:file "methods")
               (:file "gui-experiments")
               (:file "main"))
  :in-order-to ((test-op (test-op "basic-morphenator/tests")))
  :description "basic morphenator for GUI for clops in separate system")

;; (ql:quickload :basic-morphenator)
;; (asdf:test-system "basic-morphenator")
(defsystem "basic-morphenator/tests"
  :depends-on (#:basic-morphenator  #:fiveam)
    :pathname "tests/"
    :serial T
    :components ((:file "test-packages")
                 (:file "basic-morphenator-tests"))
    :perform (test-op (op c)
                      (uiop:symbol-call :fiveam
                                        :run!
                                        (uiop:find-symbol*
                                         :basic-morphenator-suite
                                         :basic-morphenator-test))))
