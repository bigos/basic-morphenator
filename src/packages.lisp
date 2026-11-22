(defpackage #:basic-morphenator
  (:use #:cl)
  (:import-from :serapeum
                ~>
                ->)
  (:import-from :defclass-std
                defclass/std)
  (:import-from :boxes
                width
                height
                )
  ;; (:local-nicknames (#:sy #:sycamore))
  (:export
   main
   draw-window
   process-event
   menu-bar
   ;; open-file
   ;; cancel-open-file
   ;; save-file
   ;; cancel-save-file
   ))
