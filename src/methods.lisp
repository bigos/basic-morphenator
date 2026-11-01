(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

;;; try to implement: https://drcuis.github.io/DesignGUI/Quick-Start.html
(defmethod init-gui ((model basic-morphenator-model))
  (let ((world (world model))
        (first-box (make-instance 'morph :location (build-location 10 20)))
        (second-box (make-instance 'morph :location (build-location 50 50))))
    (add-submorph world first-box)
    (add-submorph first-box second-box)))

(defun build-location (x y)
  (make-instance 'location
                 :x x
                 :y y))

(defun world-morphp (morph)
  (typep morph 'world-morph))

;;; ============= location and add submorph ====================================

(defmethod refresh-location ((morph morph))
  (unless (world-morphp morph)
    (setf (location morph)
          (let ((owner-morph-location (~> morph owner location)))
            (make-instance 'location
                           :absolute-x (+ (~> owner-morph-location absolute-x) (~> morph location x))
                           :absolute-y (+ (~> owner-morph-location absolute-y) (~> morph location y))
                           :x (~> morph location x)
                           :y (~> morph location y))))))

(defmethod refresh-locations ((morph morph))
  "Refresh LOCATION and children."
  (refresh-location morph))
(defmethod refresh-locations :after ((morph morph))
  (loop for m in (submorphs morph)
        do (refresh-location m)))

(defmethod add-submorph ((morph morph) (submorph morph) )
  (setf (owner submorph) morph)
  (setf (submorphs morph) (append (submorphs morph) (list submorph))))
(defmethod add-submorph :after ((morph morph) (submorph morph) )
  (refresh-location submorph))

;;; ============= render =======================================================

(defmethod render :before ((morph morph))
  (refresh-location morph))
(defmethod render ((morph morph))
  (gui-color:set-rgba "yellow")
  (cairo:rectangle   (~> morph location absolute-x)
                     (~> morph location absolute-y)
                     50
                     30)
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
