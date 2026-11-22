(declaim (optimize (speed 2) (safety 3) (debug 3)))

(in-package #:basic-morphenator)


(defun printable-input-p (entered ctrl-p)
  (and (not ctrl-p)
       entered
       (stringp entered)
       (> (length entered) 0)
       (not (member (char entered 0)
                    '(#\Escape #\Return #\Newline #\Rubout)
                    :test #'char=))))

(defun handle-text-morph-key (text-morph entered key-name shift-p ctrl-p)
  "Handle keys specific to an active text-morph. Returns T if handled, NIL otherwise."
  (cond
    ((and ctrl-p (string-equal entered "a"))
     (setf (selection-start text-morph) 0
           (cursor-pos text-morph) (length (widget-text text-morph)))
     t)
    ((string-equal key-name "Left")
     (move-text-cursor text-morph (1- (cursor-pos text-morph)) shift-p)
     t)
    ((string-equal key-name "Right")
     (move-text-cursor text-morph (1+ (cursor-pos text-morph)) shift-p)
     t)
    ((string-equal key-name "Home")
     (move-text-cursor text-morph 0 shift-p)
     t)
    ((string-equal key-name "End")
     (move-text-cursor text-morph (length (widget-text text-morph)) shift-p)
     t)
    ((string-equal key-name "Delete")
     (unless (delete-selection text-morph)
       (clamp-cursor text-morph)
       (let* ((txt (widget-text text-morph))
              (pos (cursor-pos text-morph)))
         (when (< pos (length txt))
           (setf (widget-text text-morph)
                 (concatenate 'string (subseq txt 0 pos) (subseq txt (1+ pos)))))))
     t)
    ((backspace-key-p key-name)
     (delete-before-cursor text-morph)
     t)
    ((printable-input-p entered ctrl-p)
     (insert-at-cursor text-morph entered)
     t)
    (t nil)))

(defmethod handle-key-pressed (entered key-name key-code mods lisp-window)
  (declare (ignore key-code lisp-window))
  (let ((model (the-model))
        (text-morph (active-text-morph))
        (shift-p (and (listp mods) (member :SHIFT mods)))
        (ctrl-p  (and (listp mods) (member :CONTROL mods)))
        (alt-p   (and (listp mods) (member :ALT mods))))
    (cond
      (alt-p
       (cond ((equal entered "r")
              (warn "refresh the init-ui")
              (init-gui model))
             ((equal entered "g")
              (greet-me model))
             ((equal entered "c")
              (warn "clear by shortcut")
              (when model
                (setf (greeted model) ""))
              (if text-morph
                  (progn
                    (setf (widget-text text-morph) ""
                          (cursor-pos text-morph) 0
                          (scroll-x text-morph) 0)
                    (clear-selection text-morph))
                  (when model
                    (setf (entered model) ""))))
             (t (warn "unknown ALT shortcut"))))
      ((and text-morph
            (handle-text-morph-key text-morph entered key-name shift-p ctrl-p)))
      ((backspace-key-p key-name)
       (when model
         (setf (entered model)
               (delete-last-char (entered model)))))
      ((return-key-p key-name entered)
       (when model
         (greet-me model)))
      ((printable-input-p entered ctrl-p)
       (when model
         (setf (entered model)
               (format nil "~A~A" (entered model) entered)))))))

(defun find-the-morph (x y)
  (car (last (morphs-at (current-world) x y))))

(defmethod process-event ((lisp-window basic-morphenator-window) event &rest args)
  (unless (member event '(:timeout :motion))
    (unless (eq *environment* :testing) (warn "!!! event ~S ~S" event args))
    )
  (case event
    (:timeout
     ;; do nothing yet
     )
    ((:motion :motion-enter)
     (destructuring-bind ((x y)) args
       (update-text-drag-selection x)
       (process-mouse-over (the-handler) (morphs-at (current-world) x y))))
    (:motion-leave
     (process-mouse-over (the-handler) nil)
     (gui-app:mouse-motion-leave))
    (:focus-enter)
    (:focus-leave)
    (:pressed
     (destructuring-bind ((button x y)) args
       (handle-event-mouse-button (find-the-morph x y)
                                  event button x y)))
    (:released
     (let* ((w (current-world))
            (fm (and w (focused-morph w))))
       (finish-text-selection fm))
     (destructuring-bind ((button x y)) args
       (handle-event-mouse-button (find-the-morph x y)
                                  event button x y)))
    (:scroll)
    (:resize
     (destructuring-bind ((w h)) args
       (gui-window:window-resize w h lisp-window)
       (setf (width lisp-window) w
             (height lisp-window) h
             *window-size* (cons w h))
       (resize (current-world) w h)))
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

       (gui-window:steal-focus lisp-window)))
    (otherwise
     (unless (eq event  :key-released)
       (warn "not handled event ~S ~S" event args))))

  ;; redrawing ------------------------------
  (gui-window:redraw-canvas lisp-window (format  nil "EVENT_~A" event)))

(defmethod draw-window ((window basic-morphenator-window))
  ;; TODO review drawing window
  (declare (ignore window))
  (render-world (current-world)))

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
