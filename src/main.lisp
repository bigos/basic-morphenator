(declaim (optimize (speed 2) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defmethod handle-key-pressed (entered key-name key-code mods lisp-window)
  (cond ((equal mods '(:ALT))
         (cond ((equal entered "r")
                (warn "refresh the init-ui")
                (init-gui (the-model)))
               ((equal entered "g")
                (greet-me (the-model)))
               ((equal entered "c")
                (warn "clear by shortcut")
                (setf (greeted (the-model)) ""
                      (entered (the-model)) ""))
               (T (warn "unknown ALT shortcut"))))
        (T
         (setf (entered (the-model))
               (format nil "~A~A" (entered (the-model)) entered)))))

(defun find-the-morph (x y)
  (morph-at (world (the-model)) x y))

(defmethod process-event ((lisp-window basic-morphenator-window) event &rest args)
  (unless (member event '(:timeout :motion))
    (unless (eq *environment* :testing) (warn "!!! event ~S ~S" event args))
    )
  (case event
    (:timeout
     ;; do nothing yet
     )
    ((:motion :motion-enter)
     ;; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       (handle-event-motion (find-the-morph x y)
                            event x y)))
    (:motion-leave
     (handle-event-leave (last-movement-morph (world (the-model))))
     (gui-app:mouse-motion-leave))
    (:focus-enter)
    (:focus-leave)
    (:pressed
     (destructuring-bind ((button x y)) args
       (handle-event-mouse-button (find-the-morph x y)
                                  event x y)))
    (:released
     (destructuring-bind ((button x y)) args
       (handle-event-mouse-button (find-the-morph x y)
                                  event x y)))
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
              (gui-window:window-resize w h lisp-window)
       (setf (width lisp-window) w
             (height lisp-window) h
             *window-size* (cons w h))
       (resize (world (the-model)) w h)))
    (:key-pressed
     (destructuring-bind ((entered key-name key-code mods)) args
                   ;; example of accessing gtk window object
       (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
       (handle-key-pressed entered key-name key-code mods lisp-window)))
    (:menu-simple
     (destructuring-bind ((action)) args
                   (cond
                     ;; File
                     ((equalp action "new")
                      (format T "menu selected new~%")
                      (new-file (the-model)))
                     ;; ((equalp action "open")
                     ;;  (format T "menu selected open~%")
                     ;;  (gui-window-gtk:present-file-open-dialog))
                     ;; ((equalp action "save-as")
                     ;;  (format T "menu selected save-as~%")
                     ;;  (file-save-selector))
                     ((equalp action "quit")
                      (format T "menu selected quit~%")
                      (gui-window-gtk:close-all-windows-and-quit))
                     ;; View
                     ;; ((equalp action "toggle-line-numbers")
                     ;;  (line-numbers-toggle (the-model)))
                     ;; Help
                     ((equalp action "about")
                      (format T "menu selected about~%")
                      (gui-window-gtk:present-about-dialog (about-dialog)))
                     (T
                      (format T "unhandled menu action ~S~%" action)))

       ;; remember to steal menu focus
       (gui-window:steal-focus lisp-window)))
    (otherwise
     (unless (eq event  :key-released)
       (warn "not handled event ~S ~S" event args))))

  ;; redrawing ------------------------------
  (gui-window:redraw-canvas lisp-window (format  nil "EVENT_~A" event)))

(defmethod draw-window ((window basic-morphenator-window))
  ;; TODO review drawing window
  (declare (ignore window))
  (render-world (world (the-model))))

;;; main =======================================================================
(defun menu-bar (app lisp-window)
  (let ((menu (gio:make-menu)))

    (gui-menu:build-menu
     menu
     (gui-menu:prepare-submenu
      "File"
      ;; (gui-menu:prepare-section
      ;;  nil
      ;;  (gui-menu:build-items
      ;;   (gui-menu:prepare-item-simple lisp-window app menu "New" "new"))
      ;;  )
      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple lisp-window app menu "Quit" "quit"))))

     (gui-menu:prepare-submenu
      "Help"
      ;; for now I plan to have only the About menu item
      (gui-menu:prepare-section
       nil
       (gui-menu:build-items
        (gui-menu:prepare-item-simple lisp-window app menu "About" "about")))))

    (values menu)))

(defun about-dialog ()
  (list :authors (list "Jacek Podkanski")
        :website      "https://github.com/bigos/basic-morphenator"
        :program-name "Basic Morphenator"
        :comments     (format nil "~A~%~A"
                       "Basic Morphenator"
                       "A sample morphenator experiment written in SBCL Common Lisp")
        :license      "Public Domain"
        :system-information (format nil "~A~%~A~%~A~%~A~%~A~%"
                                    (lisp-implementation-type)
                                    (lisp-implementation-version)
                                    (uiop/os:detect-os)
                                    (uiop/os:architecture)
                                    (uiop/os:implementation-identifier))
        ;; icon names to try
        ;; https://specifications.freedesktop.org/icon-naming-spec/latest/#names
        :logo-icon-name  "applications-development"))

(defun main (&key (testing nil))
  (setf
   gui-drawing:*client-fn-draw-objects* 'basic-morphenator:draw-window
   gui-window-gtk:*client-fn-menu-bar*  'basic-morphenator:menu-bar
   gui-events:*client-fn-process-event* 'basic-morphenator:process-event
   gui-window-gtk:*initial-window-width*    600
   gui-window-gtk:*initial-window-height*   400
   gui-window-gtk:*initial-title*           "Basic-Morphenator"
   ;; gui-window-gtk:*client-fn-menu-bar* 'basic-morphenator::menu-bar
   ;; gui-window-gtk:*client-fn-open-file* 'basic-morphenator:open-file
   ;; gui-window-gtk:*client-fn-cancel-open-file* 'basic-morphenator:cancel-open-file
   ;; gui-window-gtk:*client-fn-save-file* 'basic-morphenator:save-file
   ;; gui-window-gtk:*client-fn-cancel-save-file* 'basic-morphenator:cancel-save-file

   *basic-morphenator-model* (make-instance 'basic-morphenator-model)

   boxes:*model* (the-model))

  (init-gui (the-model))

  (if testing
      ;; then
      (let ((experimental-window (make-instance 'basic-morphenator-window)))
        (setf *environment* :testing)
        (setf (gui-window::gir-window experimental-window) :testing)
        (setf gui-app:*lisp-app* (gui-app:make-lisp-app nil))
        (gui-window-gtk:window-creation-from-simulation :testing experimental-window) experimental-window)
      ;; else
      (progn
        (setf *environment* :development)
        (gui-window-gtk:window-main (make-instance 'basic-morphenator-window)))))

;; (main)
;;; type annotations
(-> experiment () null)
(defun experiment ()
  (let ((ew (main :testing T)))
    (process-event ew :resize '(400 500))
    (process-event ew :motion-enter '(0 0))
    (process-event ew :motion '(10 10))
    nil))
