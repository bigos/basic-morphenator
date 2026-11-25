(declaim (optimize (speed 2) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

;;; ================= init gui =================================================
;;; try to implement: https://drcuis.github.io/DesignGUI/Quick-Start.html

(defmethod init-gui ((model basic-morphenator-model))
  (let
      ((world (make-instance 'world-morph))

       (view (make-instance 'layout-morph
                            :color "pink"
                            :direction :vertical
                            :extent (make-instance 'extent :x 600 :y 400)
                            :layout-spec (make-layout-spec :proportional-width 1.0
                                                           :proportional-height 1.0)))
       (row  (make-instance 'layout-morph
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

    (progn
      (setf (world model) world)
      (add-morph (world model) view)
      (add-morph view row)
      (add-morph row label)
      (add-morph row input)
      (add-morph row button)
      (add-morph view greeting-label))))
