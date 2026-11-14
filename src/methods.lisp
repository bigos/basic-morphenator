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
  (let
      ((view (make-instance 'layout-morph
                            :color "pink"
                            :direction :vertical
                            :extent (make-instance 'extent :x 180 :y 50) ))
       (row  (make-instance 'layout-morph
                            :color "orange"
                            :direction :horizontal
                            :extent (make-instance 'extent :x 180 :y 20)))
       (label (make-instance 'label-morph
                             :color "brown"
                             :extent (make-instance 'extent :x 60 :y 20)
                             :contents "Name:"))
       (input (make-instance 'text-model-morph
                             :color "white"
                             :extent (make-instance 'extent :x 50 :y 20)
                             :model model))
       (button (make-instance 'pluggable-button-morph
                              :color "green"
                              :extent (make-instance 'extent :x 50 :y 20)
                              :label "Greet"))
       (greeting-label (make-instance 'label-morph
                                      :color "blue"
                                      :extent (make-instance 'extent :x 50 :y 20)
                                      :contents "Hmm...")))

    (progn
      (setf (submorphs (world model)) nil)
      (add-morph (world model) view)
      (add-morph view row)
      (add-morph row label)
      (add-morph row input)
      (add-morph row button)
      (add-morph view greeting-label))))

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
  (add-morph-after% morph (direction morph)))
(defmethod add-morph-after% ((morph layout-morph) (direction (eql :horizontal)))
  (warn "adding layout element ~s" direction))
(defmethod add-morph-after% ((morph layout-morph) (direction (eql :vertical)))
  (warn "adding layout element ~s" direction))

;;; ================== location ================================================
(defun morph-location-axis (morph axis-fn)
  (if (location morph)
      (funcall axis-fn (location morph))
      0))
(defmethod morph-location-x ((morph morph))
  (morph-location-axis morph #'x))
(defmethod morph-location-y ((morph morph))
  (morph-location-axis morph #'y))

(defmethod prev-sibling-axis-sum ((morph morph) axis-fn)
  (let ((gap 10))
    (loop
      for m in (submorphs (owner morph))
      until (eq (id m)
                (id morph))
      sum (+
           (~> m extent (funcall axis-fn _))
           (~> m location (funcall axis-fn _))
           gap))))
(defmethod prev-sibling-widths-sum ((morph morph))
  (prev-sibling-axis-sum morph #'x))
(defmethod prev-sibling-heights-sum ((morph morph))
  (prev-sibling-axis-sum morph #'y))
;;; UNUSED
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
    (let ((new-location
            (make-instance 'location
                           :absolute-x (+ (~> owner-morph-location absolute-x) (~> morph morph-location-x))
                           :absolute-y (+ (prev-sibling-heights-sum morph)
                                          (~> owner-morph-location absolute-y)
                                          0)
                           :x (~> morph morph-location-x)
                           :y (~> morph morph-location-y))))
      (setf (location morph) new-location))))
(defmethod refresh-location-with-owner-direction ((morph morph) (owner-morph layout-morph) (direction (eql :horizontal)))
  (let ((owner-morph-location (~> owner-morph location)))
    (let ((new-location
            (make-instance 'location
                           :absolute-x (+ (prev-sibling-widths-sum morph)
                                          (~> owner-morph-location absolute-x)
                                          0)
                           :absolute-y (+ (~> owner-morph-location absolute-y) (~> morph morph-location-y))
                           :x (~> morph morph-location-x)
                           :y (~> morph morph-location-y))))
      (setf (location morph) new-location))))

(defmethod refresh-location-with-owner ((morph morph) (owner-morph morph))
  (let ((owner-morph-location (~> owner-morph location)))
    (setf (location morph)
          (make-instance 'location
                         :absolute-x (+ (~> owner-morph-location absolute-x) (~> morph morph-location-x))
                         :absolute-y (+ (~> owner-morph-location absolute-y) (~> morph morph-location-y))
                         :x (~> morph morph-location-x)
                         :y (~> morph morph-location-y)))))

(defmethod refresh-location-with-owner :after ((morph morph) (owner-morph layout-morph))
  ;; warn when absolute coordinates and extent are outside owner
  (let ((owner-morph-location (location owner-morph))
        (owner-morph-extent   (extent owner-morph))
        (morph-location (location morph) )
        (morph-extent   (extent morph)))
    (when (or (< (~> morph-location absolute-x) (~> owner-morph-location absolute-x))
              (< (~> morph-location absolute-y) (~> owner-morph-location absolute-y))
              (> (+ (~> morph-location absolute-x) (~> morph-extent x))
                 (+ (~> owner-morph-location absolute-x) (~> owner-morph-extent x)))
              (> (+ (~> morph-location absolute-y) (~> morph-extent y))
                 (+ (~> owner-morph-location absolute-y) (~> owner-morph-extent y))))
      (warn "morph ~s is has elements outside its owner" morph)
      )))

;;; ============= render =======================================================
(defmethod render-prepare-to-show-text ((morph placed-morph))
  ;;"Advaita Mono"   ;; "Liberation Mono"
  (cairo:select-font-face "Ubuntu Mono" :normal :normal)
  (cairo:set-font-size 15)
  (cairo:move-to (~> morph location absolute-x)
                 (~> morph location absolute-y (+ _ 15))))

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

(defmethod render ((morph label-morph))
  (call-next-method)

  (render-prepare-to-show-text morph)
  (gui-color:set-rgba "white")

  (cairo:show-text (render-text-for-color morph)))

(defmethod render-text-for-color ((morph label-morph))
  "Allow render to select text based on widget color"
  (if (equal (color morph) "blue")
      ;; then - greeted label
      (or                               ; or avoid repeating test and then
       (greeted *basic-morphenator-model*)
       (contents morph))
      ;; else - normal widget
      (contents morph)))

(defmethod render ((morph pluggable-button-morph))
  (call-next-method)

  (render-prepare-to-show-text morph)
  (gui-color:set-rgba "white")
  (cairo:show-text (label morph)))

(defmethod render ((morph text-model-morph))
  (call-next-method)

  (render-prepare-to-show-text morph)
  (gui-color:set-rgba "red")
  (cairo:show-text (format nil "~A"  (entered *basic-morphenator-model*))))
;;; ------------ world ---------------------------------------------------------
(defmethod initialize-instance :after ((morph world-morph) &key)
  (setf (owner morph) nil
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
