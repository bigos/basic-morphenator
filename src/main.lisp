(declaim (optimize (speed 1) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defun handle-alt-shortcut (entered model text-morph)
  "Run a registered ALT shortcut for ENTERED. Return T if handled, NIL otherwise."
  (cond ((equal entered "r")
         (warn "refresh the init-ui")
         (when model
           (activate-world model (init-gui model)))
         t)

        ((equal entered "g")
         (greet-me model)
         t)
        ((equal entered "c")
         (warn "clear by shortcut")
         (when model
           (setf (greeted model) ""))
         (if text-morph
             (clear-text-morph text-morph)
             (when model
               (setf (entered model) "")))
         t)
        (t nil)))

(defun handle-text-morph-key (text-morph entered key-name shift-p ctrl-p alt-p)
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
     (delete-char-at-cursor text-morph)
     t)
    ((backspace-key-p key-name)
     (delete-before-cursor text-morph)
     t)
    ;; Don't insert printable characters while ALT is held — they're app shortcuts.
    ((and (not alt-p)
          (printable-input-p entered ctrl-p))
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
      ((and alt-p (handle-alt-shortcut entered model text-morph))
       (when text-morph (record-morph-damage text-morph))
       (mark-needs-redraw)
       t)
      ((and text-morph
            (handle-text-morph-key text-morph entered key-name shift-p ctrl-p alt-p))
       (record-morph-damage text-morph)
       t)
      ((return-key-p key-name entered)
       (when model
         (greet-me model)
         (mark-needs-redraw)
         t))
      (t nil))))

(defun handle-menu-action (action)
  (cond
    ((equalp action "new")
     (format t "menu selected new~%")
     (new-file (the-model)))
    ;; ((equalp action "open")
    ;;  (format t "menu selected open~%")
    ;;  (gui-window-gtk:present-file-open-dialog))
    ;; ((equalp action "save-as")
    ;;  (format t "menu selected save-as~%")
    ;;  (file-save-selector))
    ((equalp action "quit")
     (format t "menu selected quit~%")
     (gui-window-gtk:close-all-windows-and-quit))
    ;; ((equalp action "toggle-line-numbers")
    ;;  (line-numbers-toggle (the-model)))
    ((equalp action "about")
     (format t "menu selected about~%")
     (gui-window-gtk:present-about-dialog (about-dialog)))
    (t
     (format t "unhandled menu action ~S~%" action))))

(defun handle-window-resize (lisp-window w h)
  (gui-window:window-resize w h lisp-window)
  (when (fboundp '(setf width))
    (ignore-errors (setf (width lisp-window) w)))
  (when (fboundp '(setf height))
    (ignore-errors (setf (height lisp-window) h)))

  (setf *window-size* (cons w h))
  (when (current-world)
    (resize (current-world) w h)
    (record-damage-rect (current-world) 0 0 w h)))

(defun dispatch-mouse-button (event args)
  (destructuring-bind ((button x y)) args
    (let ((target (or (find-the-morph x y)
                      (current-world))))
      (when target
        (handle-event-mouse-button target event button x y)))))

(defmethod process-event ((lisp-window basic-morphenator-window) event &rest args)
  (unless (or (member event '(:timeout :motion))
              (eq *environment* :testing))
    (warn "!!! event ~S ~S" event args))

  (let ((changed-p nil))
    (case event
      ((:timeout :focus-enter :focus-leave :scroll)
       nil)
      ((:motion :motion-enter)
       (destructuring-bind ((x y)) args
         (let ((drag-changed (update-text-drag-selection x))
               (hover-changed (and (current-world)
                                   (process-mouse-over (the-handler)
                                                       (morphs-at (current-world) x y)))))
           (when (or drag-changed hover-changed)
             (mark-needs-redraw)
             (setf changed-p t)))))
      (:motion-leave
       (when (process-mouse-over (the-handler) nil)
         (mark-needs-redraw)
         (setf changed-p t))
       (gui-app:mouse-motion-leave))
      (:pressed
       (dispatch-mouse-button event args)
       (setf changed-p (world-needs-redraw-p)))
      (:released
       (let* ((w (current-world))
              (fm (and w (world-keyboard-focus w))))
         (when (and fm (typep fm 'text-model-morph) (is-selecting fm))
           (finish-text-selection fm)
           (record-morph-damage fm)))
       (dispatch-mouse-button event args)
       (setf changed-p (world-needs-redraw-p)))
      (:resize
       (destructuring-bind ((w h)) args
         (handle-window-resize lisp-window w h)
         (setf changed-p t)))
      (:key-pressed
       (destructuring-bind ((entered key-name key-code mods)) args
         (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
         (when (handle-key-pressed entered key-name key-code mods lisp-window)
           (setf changed-p t))))
      (:menu-simple
       (destructuring-bind ((action)) args
         (handle-menu-action action)
         (gui-window:steal-focus lisp-window)
         (mark-needs-redraw)
         (setf changed-p t)))
      (otherwise
       (unless (eq event :key-released)
         (warn "not handled event ~S ~S" event args))))

    (when (or changed-p (world-needs-redraw-p))
      (gui-window:redraw-canvas lisp-window (format nil "EVENT_~A" event)))))
;;; ========================
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

;;; ------------- main GUI ------------------------------------------------------
(defun configure-gui-hooks ()
  (setf gui-drawing:*client-fn-draw-objects* 'basic-morphenator:draw-window
        gui-window-gtk:*client-fn-menu-bar*  'basic-morphenator:menu-bar
        gui-events:*client-fn-process-event* 'basic-morphenator:process-event
        gui-window-gtk:*initial-window-width*  600
        gui-window-gtk:*initial-window-height* 400
        gui-window-gtk:*initial-title*         "Basic-Morphenator"))

(defun initialize-application-state ()
  (let ((model (make-instance 'basic-morphenator-model)))
    (setf *basic-morphenator-model* model
          boxes:*model* model)
    (activate-world model (init-gui model))))

(defun start-testing-window ()
  (let ((experimental-window (make-instance 'basic-morphenator-window)))
    (setf *environment* :testing
          (gui-window::gir-window experimental-window) :testing
          gui-app:*lisp-app* (gui-app:make-lisp-app nil))
    (gui-window-gtk:window-creation-from-simulation :testing experimental-window)
    experimental-window))

(defun main (&key (testing nil))
  (configure-gui-hooks)
  (initialize-application-state)
  (if testing
      (start-testing-window)
      (progn
        (setf *environment* :development)
        (gui-window-gtk:window-main (make-instance 'basic-morphenator-window)))))

(defun experiment ()
  (let ((ew (main :testing t)))
    (process-event ew :resize '(400 500))
    (process-event ew :motion-enter '(0 0))
    (process-event ew :motion '(10 10))
    nil))
