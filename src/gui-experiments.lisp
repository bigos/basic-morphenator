(declaim (optimize (speed 1) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defun activate-world (model world)
  "Attach WORLD to MODEL when both are non-NIL, returning WORLD."
  (when (and model world)
    (setf (world model) world))
  world)

;;; ================= init gui =================================================
(defparameter *scene* :greeting
  "Scene built by INIT-GUI: :greeting or :row.")

(defmethod init-gui ((model basic-morphenator-model))
  (ecase *scene*
    (:greeting (build-greeting-scene model))
    (:row      (build-row-scene model))))

;;; https://drcuis.github.io/DesignGUI/Quick-Start.html
(defun build-greeting-scene (model)
  (let* ((world (make-instance 'world-morph))
         (view (make-instance 'layout-morph
                              :color "pink"
                              :direction :vertical
                              :extent (make-instance 'extent :x 600 :y 400)
                              :layout-spec (make-layout-spec :proportional-width 1.0
                                                             :proportional-height 1.0)))
         (row (make-instance 'layout-morph
                             :color "orange"
                             :direction :horizontal
                             :extent (make-instance 'extent :x 600 :y 30)
                             :layout-spec (make-layout-spec :proportional-width 1.0
                                                            :fixed-height 30)))
         (label (make-instance 'label-morph
                               :color "brown"
                               :extent (make-instance 'extent :x 60 :y 30)
                               :contents "Name:"))
         (input (make-instance 'text-model-morph
                               :color "white"
                               :extent (make-instance 'extent :x 50 :y 30)
                               :layout-spec (make-layout-spec :proportional-width 1.0)
                               :model model))
         (button (make-instance 'pluggable-button-morph
                                :action-selector (lambda () (greet-me (the-model)))
                                :color "green"
                                :mouse-over-color "lime"
                                :extent (make-instance 'extent :x 60 :y 30)
                                :label "Greet"))
         ;; TODO resize the label to reflect new content and center it after resizing
         (greeting-label (make-instance 'label-morph
                                        :color "blue"
                                        :extent (make-instance 'extent :x 50 :y 20)
                                        :contents (lambda () (or (greeted model) "Hmm...")))))
    (with-deferred-layout ()
      (add-morph world view)
      (add-morph view row)
      (add-morph row label)
      (add-morph row input)
      (add-morph row button)
      (add-morph view greeting-label))
    ;; NEW: measure-and-relayout every label now that the tree is assembled,
    ;; so the greeting-label (whose contents is a closure over MODEL) is
    ;; sized to its actual rendered text rather than its constructed 50×20.
    (refresh-world-labels world)
    world))

;; https://drcuis.github.io/DesignGUI/To-row-or-not-to-row.html
(defun build-row-scene (model)
  (declare (ignore model))
  (let* ((world (make-instance 'world-morph))
         (view (make-instance 'layout-morph
                              :color "pink"
                              :direction :vertical
                              :padding-x 10
                              :padding-y 10
                              :separation 10
                              :extent (make-instance 'extent :x 600 :y 400)
                              :layout-spec (make-layout-spec :proportional-width 1.0
                                                             :proportional-height 1.0)))
         ;; TODO children of the orange layout use default gap,
         (row (make-instance 'layout-morph
                             :color "orange"
                             :direction :horizontal
                             :separation 10
                             :padding-x 10
                             :padding-y 10
                             :extent (make-instance 'extent :x 580 :y 30)
                             :layout-spec (make-layout-spec :proportional-width 1.0)))
         (box1 (make-instance 'boxed-morph
                              :color "red"
                              :extent (make-instance 'extent :x 50 :y 50)))
         (box2 (make-instance 'boxed-morph
                              :color "green"
                              :extent (make-instance 'extent :x 75 :y 75)))
         (box3 (make-instance 'boxed-morph
                              :color "blue"
                              :extent (make-instance 'extent :x 100 :y 100))))
    (with-deferred-layout ()
      (add-morph world view)
      (add-morph view row)
      (add-morph row box1)
      (add-morph row box2)
      (add-morph row box3))
    world))
