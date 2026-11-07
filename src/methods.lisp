(declaim (optimize (speed 2) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

;;; try to implement: https://drcuis.github.io/DesignGUI/Quick-Start.html
(defmethod init-gui ((model basic-morphenator-model))
  (let* ((view (make-instance 'layout-morph
                              :color "pink"
                              :direction :vertical
                              :location (make-instance 'location :x 25 :y 25)
                              :extent (make-instance 'extent :x 200 :y 100) ))
         (row  (make-instance 'layout-morph
                              :color "orange"
                              :direction :horizontal
                              :location (make-instance 'location :x 5 :y 5)
                              :extent (make-instance 'extent :x 180 :y 50)))
         (label (make-instance 'label-morph
                               :color "brown"
                               :location (make-instance 'location :x 80 :y 15)
                               :extent (make-instance 'extent :x 60 :y 10)))
         (input (make-instance 'text-model-morph
                               :color "white"
                               :location (make-instance 'location :x 15 :y 15)
                               :extent (make-instance 'extent :x 50 :y 10)))
         (button (make-instance 'pluggable-button-morph
                                :color "green"
                                :location (make-instance 'location :x 155 :y 15)
                                :extent (make-instance 'extent :x 20 :y 10)))
         (greeting-label (make-instance 'label-morph
                                        :color "blue"
                                        :location (make-instance 'location :x 85 :y 65)
                                        :extent (make-instance 'extent :x 50 :y 10))))

    (add-morph (world model) view)
    (add-morph view row)
    (add-morph row label)
    (add-morph row input)
    (add-morph row button)
    (add-morph view greeting-label)


    ))

;; (defmethod init-gui ((model basic-morphenator-model))
;;   (let ((world (world model))
;;         (first-box (make-instance 'morph :location (build-location 10 20)))
;;         (second-box (make-instance 'morph :location (build-location 50 50))))
;;     (add-submorph world first-box)
;;     (add-submorph first-box second-box)))

(defun build-location (x y)
  (make-instance 'location
                 :x x
                 :y y))

(defun world-morphp (morph)
  (typep morph 'world-morph))

;;; ============= location and add submorph ====================================

(defmethod refresh-location ((morph morph))
  (warn "doing refresh location")
  (unless (world-morphp morph)
    (progn
      (assert (not (null (owner morph))))
      (let ((owner-morph-location (~> morph owner location)))
        (assert (not (null owner-morph-location)))
        (setf (location morph)
              (make-instance 'location
                             :absolute-x (+ (~> owner-morph-location absolute-x) (~> morph location x))
                             :absolute-y (+ (~> owner-morph-location absolute-y) (~> morph location y))
                             :x (~> morph location x)
                             :y (~> morph location y)))))))

(defmethod refresh-locations ((morph morph))
  "Refresh LOCATION and children."
  (refresh-location morph))
(defmethod refresh-locations :after ((morph morph))
  (loop for m in (submorphs morph)
        do (refresh-locations m)))

(defmethod add-morph ((morph morph) (submorph morph))
  (setf (owner submorph) morph)
  (setf (submorphs morph) (append (submorphs morph) (list submorph))))
(defmethod add-morph :after ((morph morph) (submorph morph) )
  (warn "after add morph")
  (refresh-location morph))

(defmethod add-morph :after ((morph layout-morph) (submorph morph))
  (ecase (direction morph)
    (:vertical)
    (:horizontal))
  (refresh-locations morph))

;;; ============= render =======================================================

(defmethod render :before ((morph morph))
  ;; (refresh-location morph)
  )
(defmethod render ((morph placed-morph))
  (gui-color:set-rgba (if (color morph)
                          (color morph)
                          "yellow"))
  (cairo:rectangle
   (~> morph location absolute-x)
   (~> morph location absolute-y)
   (~> morph extent x)
   (~> morph extent y))
  (cairo:fill-path))
(defmethod render :after ((morph morph))
  (loop for m in (submorphs morph)
        do (render m)))

;;; ------------ world ---------------------------------------------------------

(defmethod initialize-instance :after ((morph world-morph) &key)
  (setf (owner morph) nil
        (scale-factor morph) 1.0
        (location morph) (make-instance 'location
                                        :absolute-x 0
                                        :absolute-y 0
                                        :x 0
                                        :y 0)))

(defmethod render-world   ((morph world-morph) (window basic-morphenator-window))
  ;; actual morph
  (gui-color:set-rgba "red")
  (cairo:rectangle   0
                     0
                     (width window)
                     (height window))
  (cairo:fill-path)
  ;; render children
  (loop for m in (submorphs morph) do (render m)))
