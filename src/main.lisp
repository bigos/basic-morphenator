(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defmethod process-event ((lisp-window basic-morphenator-window) event &rest args)
  (unless (member event '(:timeout :motion))
    ;; (unless (eq *environment* :testing) (warn "event ~S ~S" event args))
    )
  (case event
    (:timeout
     ;; do nothing yet
     )
    ((:motion :motion-enter)
     ;; we use simple case with one window so we ignore the window argument
     (destructuring-bind ((x y)) args
       ;; (print-object-inner gui-app::*lisp-app* T)
       (gui-app:mouse-motion-enter lisp-window x y)))
    (:motion-leave
     (gui-app:mouse-motion-leave))
    (:focus-enter)
    (:focus-leave)
    (:pressed
     (destructuring-bind ((button x y)) args
       ;; (gui-app:mouse-button-pressed button)
       ;; (warn "mouse state ~S ~S" (gui-app:mouse-button gui-app:*lisp-app*) (list button x y))
       ;; (let*
       ;;     ((children (~> (the-container *basic-morphenator-model*)
       ;;                    boxes:children))
       ;;      (first-child-found
       ;;        (car (loop for c in children
       ;;                   when (boxes:mouse-over-p c)
       ;;                     collect c)))
       ;;      (char-child-found nil))
       ;;   (warn "model world children under mouse ~S"
       ;;         first-child-found)
       ;;   ;; TODO we need overhaul of finding widgets

       ;;   (if (and children
       ;;            (null first-child-found))
       ;;                                  ;then
       ;;    (let* ((grandchildren (children (first children)))
       ;;           (first-grandchild-found
       ;;             (car (loop for c in grandchildren
       ;;                        when (boxes:mouse-over-p c)
       ;;                          collect c))))
       ;;      (setf char-child-found first-grandchild-found))
       ;;                                  ;else
       ;;    (progn
       ;;      (setf char-child-found first-child-found)))

       ;;   (when (and char-child-found
       ;;              (typep char-child-found 'basic-morphenator-character))
       ;;     (warn "clicked ready to move cursor ~S" char-child-found)
       ;;     (move-cursor-to *basic-morphenator-model*
       ;;                     (row char-child-found)
       ;;                     (col char-child-found))))
       ))
    (:released
               (destructuring-bind ((button x y)) args
                 ;; (gui-app:mouse-button-released button)
                 ;; (warn "mouse state released ~S ~S" (gui-app:mouse-button gui-app:*lisp-app*) (list button x y))
                 ))
    (:scroll)
    (:resize
          ;; on resize move cursor to corresponding file position
     (destructuring-bind ((w h)) args
       ;; (gui-window:window-resize w h lisp-window)
       ;; (setf (width lisp-window) w
       ;;       (height lisp-window) h)
       ;; (let ((model *basic-morphenator-model*))
       ;;   (when (world model)
       ;;     (let ((bwidth (calculate-bwidth model)))
       ;;       (when (> bwidth 0)
       ;;         (setf (wrap-at-column model)
       ;;               (floor
       ;;                (/
       ;;                 (width (the-container model))
       ;;                 bwidth)))
       ;;         (reload-text-structure model)))))
       ))
    (:key--pressed
          (destructuring-bind ((entered key-name key-code mods)) args
            ;; example of accessing gtk window object
            (format t "~&>>> key pressed ~S~%" (list entered key-name key-code mods))
            ;; (handle-key-pressed entered key-name key-code mods lisp-window)
            ))
    (:menu-simple
          (destructuring-bind ((action)) args
            (cond
              ;; File
              ((equalp action "new")
               (format T "menu selected new~%")
               (new-file *basic-morphenator-model*))
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
              ;;  (line-numbers-toggle *basic-morphenator-model*))
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

  ;; moving widgets -------------------------
  ;; (warn "may implement moving widgets in response to actions)
  ;; redrawing ------------------------------
  (gui-window:redraw-canvas lisp-window (format  nil "EVENT_~A" event)))

(defmethod draw-window ((window basic-morphenator-window))
  ;; paint background
  (let ((cv 0.95)) (cairo:set-source-rgb  cv cv cv))
  (cairo:paint))

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
   gui-window-gtk:*client-fn-open-file* 'basic-morphenator:open-file
   gui-window-gtk:*client-fn-cancel-open-file* 'basic-morphenator:cancel-open-file
   gui-window-gtk:*client-fn-save-file* 'basic-morphenator:save-file
   gui-window-gtk:*client-fn-cancel-save-file* 'basic-morphenator:cancel-save-file

   *basic-morphenator-model* (make-instance 'basic-morphenator-model)
   boxes:*model* *basic-morphenator-model*)

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
