(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defmethod init-gui ((model basic-morphenator-model))
  (let ((model-world (world model))
        (first-box (make-instance 'morph :location (build-location 10 20)))
        (second-box (make-instance 'morph :location (build-location 50 50))))
    (add-submorph model-world first-box)
    (add-submorph first-box second-box)))

(defun build-location (x y)
  (make-instance 'location
                 :x x
                 :y y))

(defun world-morphp (morph)
  (typep morph 'world-morph))

(defmethod refresh-location ((morph morph))
  (break "examine morph ~s and owner ~s" morph (owner morph))

  (setf (location morph)
        (let ((owner-morph-location-absolute-x (if (world-morphp morph) 0
                                                   (~> morph owner location absolute-x)))
              (owner-morph-location-absolute-y (if (world-morphp morph) 0
                                                   (~> morph owner location absolute-y))))
          (make-instance 'location
                         :absolute-x (+ owner-morph-location-absolute-x (~> morph location x))
                         :absolute-y (+ owner-morph-location-absolute-y (~> morph location y))
                         :x (~> morph location x)
                         :y (~> morph location y)))))

(defmethod refresh-location :after ((morph morph))
  (loop for m in (submorphs morph)
        do (refresh-location m)))

(defmethod add-submorph ((morph morph) (submorph morph) )
  (setf (owner submorph) morph)
  (setf (submorphs morph) (if (submorphs morph)
                              ;; then add morph to submorph collection
                              (append (submorphs morph) (list submorph))
                              ;; else create a collection with one submorph
                              (list submorph))))

(defmethod add-submorph :after ((morph morph) (submorph morph) )
  (refresh-location submorph)
  ;; (setf (location submorph)
  ;;       (let ((morph-location-absolute-x (if (world-morphp morph) 0 (~> morph location absolute-x)))
  ;;             (morph-location-absolute-y (if (world-morphp morph) 0 (~> morph location absolute-y))))
  ;;         (make-instance 'location
  ;;                        :absolute-x (+ morph-location-absolute-x (~> submorph location x))
  ;;                        :absolute-y (+ morph-location-absolute-y (~> submorph location y))
  ;;                        :x (~> submorph location x)
  ;;                        :y (~> submorph location y))))
  )

;;; ============= render =======================================================

(defmethod render :before ((morph morph))
  ;;   (absolute-coordinates morph)
  (unless (world-morphp morph)
    (let ((owner-location (~> morph owner location)))
      (when owner-location
        (let ((owner-absolute-x (~> owner-location absolute-x))
              (owner-absolute-y (~> owner-location absolute-y)))

          (setf (~> morph location absolute-x) (+ owner-absolute-x  (~> morph location x)))
          (setf (~> morph location absolute-y) (+ owner-absolute-y  (~> morph location y))))))))

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

(defmethod initialize-instance :after ((morph world-morph))
  (warn "setting the world data")
  (setf (owner morph) nil
        (scale-factor morph) 1.0
        (location morph) (make-instance 'location
                                        :absolute-x 0
                                        :absolute-y 0
                                        :x 0
                                        :y 0)))
