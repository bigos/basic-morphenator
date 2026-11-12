(declaim (optimize (speed 2) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defmethod print-object ((obj location) stream)
  (print-object-inner obj stream))

(defmethod print-object ((obj morph) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s" (list
                         :id
                         (id obj)))))

;;; try to implement: https://drcuis.github.io/DesignGUI/Quick-Start.html
(defmethod init-gui ((model basic-morphenator-model))
  (let ((view (make-instance 'layout-morph
                             :color "pink"
                             :direction :vertical
                             :extent (make-instance 'extent :x 200 :y 50) ))
        (row  (make-instance 'layout-morph
                             :color "orange"
                             :direction :horizontal
                             :extent (make-instance 'extent :x 180 :y 30)))
        (label (make-instance 'label-morph
                              :color "brown"
                              :extent (make-instance 'extent :x 60 :y 10)))
        (input (make-instance 'text-model-morph
                              :color "white"
                              :extent (make-instance 'extent :x 50 :y 10)))
        (button (make-instance 'pluggable-button-morph
                               :color "green"
                               :extent (make-instance 'extent :x 20 :y 10)))
        (greeting-label (make-instance 'label-morph
                                       :color "blue"
                                       :extent (make-instance 'extent :x 50 :y 10))))

    (add-morph (world model) view)
    (add-morph view row)
    (add-morph row label)
    (add-morph row input)
    (add-morph row button)
    (add-morph view greeting-label)))

(defun build-location (x y)
  (make-instance 'location
                 :x x
                 :y y))

(defmethod initialize-instance :after ((morph morph) &key)
  (let ((last-id (if (or (null (last-morph-id morph))
                         (typep morph 'world-morph))
                     0                  ;make sure we have a number on the first morph
                     (last-morph-id morph))))
    (setf (id morph) (1+ last-id)))
  (setf (last-morph-id morph) (id morph)))

;;; =============== add morph ==================================================
(defmethod add-morph ((morph morph) (submorph morph))
  (setf (owner submorph) morph)
  (setf (submorphs morph) (append (submorphs morph) (list submorph))))
(defmethod add-morph :after ((morph morph) (submorph morph) ))

(defmethod add-morph :after ((morph layout-morph) (submorph morph))
  (add-morph-inner morph (direction morph)))
(defmethod add-morph-inner ((morph layout-morph) (direction (eql :horizontal)))
  (warn "adding layout element ~s" direction))
(defmethod add-morph-inner ((morph layout-morph) (direction (eql :vertical)))
  (warn "adding layout element ~s" direction))

;;; ================== location ================================================
(defmethod morph-location-x ((morph morph))
  (if (location morph)
      (x (location morph))
      0))
(defmethod morph-location-y ((morph morph))
  (if (location morph)
      (y (location morph))
      0))

(defmethod prev-sibling-widths-sum ((morph morph))
  (let ((gap 0))
    (loop
      for m in (submorphs (owner morph))
      until (eq (id m)
                (id morph))
      sum (+
           (~> m extent x)
           (~> m location x)
           gap))))
(defmethod prev-sibling-heights-sum ((morph morph))
  (let ((gap 0))
    (loop
      for m in (submorphs (owner morph))
      until (eq (id m)
                (id morph))
      sum (+
           (~> m extent y)
           (~> m location y)
           gap))))
(defmethod nth-sibling ((morph morph))
  (loop
    for nth = 0 then (1+ nth)
    for m in (submorphs (owner morph))
    until (eq (id m)
              (id morph))
    finally (return nth)))

(defmethod refresh-location ((morph morph))
  (when (owner morph)
    (refresh-location-with-owner morph (owner morph))))

(defmethod refresh-location-with-owner ((morph morph) (owner-morph layout-morph))
  (let ((owner-morph-location (~> owner-morph location)))
    (refresh-location-with-owner-direction morph owner-morph (direction owner-morph))))

(defmethod refresh-location-with-owner-direction ((morph morph) (owner-morph layout-morph) (direction (eql :vertical)))
  (let ((owner-morph-location (~> owner-morph location)))
    (setf (location morph)
          (make-instance 'location
                         :absolute-x (+ (~> owner-morph-location absolute-x) (~> morph morph-location-x))
                         :absolute-y (+ (prev-sibling-heights-sum morph)
                                        (~> owner-morph-location absolute-y)
                                        0)
                         :x (~> morph morph-location-x)
                         :y (~> morph morph-location-y)))))

(defmethod refresh-location-with-owner-direction ((morph morph) (owner-morph layout-morph) (direction (eql :horizontal)))
  (let ((owner-morph-location (~> owner-morph location)))
    (setf (location morph)
          (make-instance 'location
                         :absolute-x (+ (prev-sibling-widths-sum morph)
                                        (~> owner-morph-location absolute-x)
                                        0)
                         :absolute-y (+ (~> owner-morph-location absolute-y) (~> morph morph-location-y))
                         :x (~> morph morph-location-x)
                         :y (~> morph morph-location-y)))))

(defmethod refresh-location-with-owner ((morph morph) (owner-morph morph))
  (let ((owner-morph-location (~> owner-morph location)))
    (setf (location morph)
          (make-instance 'location
                         :absolute-x (+ (~> owner-morph-location absolute-x) (~> morph morph-location-x))
                         :absolute-y (+ (~> owner-morph-location absolute-y) (~> morph morph-location-y))
                         :x (~> morph morph-location-x)
                         :y (~> morph morph-location-y)))))

;;; ============= render =======================================================
(defmethod render :before ((morph morph))
  (refresh-location morph))
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
(defparameter *the-world* nil)
(defmethod initialize-instance :after ((morph world-morph) &key)
  (setf *the-world* morph)

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
