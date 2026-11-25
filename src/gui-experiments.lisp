(declaim (optimize (speed 2) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defun activate-world (model world)
  "Attach WORLD to MODEL when both are non-NIL, returning WORLD."
  (when (and model world)
    (setf (world model) world))
  world)

;;; ================= init gui =================================================

(defmethod init-gui ((model basic-morphenator-model))
  (let ((selector 1))
    ;; use selector to select the expected scene
    (case selector
      (0 (build-greeting-scene model))
      (1 (build-row-scene))
      (otherwise (error "selector ~s did not match any scene" selector)))))

;;; try to implement: https://drcuis.github.io/DesignGUI/Quick-Start.html
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
    world))

;; https://drcuis.github.io/DesignGUI/To-row-or-not-to-row.html
(defun build-row-scene (model)
  ;; TODO add another scene
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
