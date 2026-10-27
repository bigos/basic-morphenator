(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defmethod init-gui ((model basic-morphenator-model))
  (let ((model-world (world model))
        (first-box (make-instance 'morph))
        (second-box (make-instance 'placed-morph :location (cons 50 50))))
    (add-submorph model-world first-box)
    (add-submorph first-box second-box)))

(defun build-location (x y)
  (make-instance 'location
                 :x x
                 :y y))

(defmethod add-submorph ((morph morph) (submorph morph) )
  (setf (owner submorph) morph)
  (setf (submorphs morph) (if (submorphs morph)
                              ;; then add morph to submorph collection
                              (append (submorphs morph) (list submorph))
                              ;; else create a collection with one submorph
                              (list submorph)))
  (setf (location submorph)
        (make-instance 'location
                       :absolute-x (+ (~> morph location absolute-x) (~> submorph location x))
                       :absolute-y (+ (~> morph location absolute-y) (~> submorph location y))
                       :x (~> submorph location x)
                       :y (~> submorph location y))))

;;; ============= render =======================================================
;; (defmethod render :before ((morph morph))
;;   (absolute-coordinates morph))

(defmethod render ((morph morph))

  (gui-color:set-rgba "yellow")
  (cairo:rectangle   10
                     10
                     50
                     30)
  (cairo:fill-path))

(defmethod render :after ((morph morph))
  (loop for m in (submorphs morph)
        do (render m)))

(defmethod render ((morph placed-morph))
  (gui-color:set-rgba "lime")
  (cairo:rectangle   10
                     10
                     20
                     20)
  (cairo:fill-path))

;;; ------------ world ---------------------------------------------------------
(defmethod render-world ((morph world-morph) (window basic-morphenator-window))
  ;; absolute coordinates

  ;; actual morph
  (gui-color:set-rgba "red")
  (cairo:rectangle   0
                     0
                     (width window)
                     (height window))
  (cairo:fill-path)
  ;; render children
  (loop for m in (submorphs morph) do (render m)))
