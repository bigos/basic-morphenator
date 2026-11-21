(declaim (optimize (speed 2) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defmethod print-object ((obj location) stream)
  (print-object-inner obj stream))

(defmethod print-object ((obj morph) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s" (list
                         :id
                         (id obj)
                         :color
                         (color obj)))))

(defmethod greet-me ((model basic-morphenator-model))
  (setf (greeted (the-model)) (entered (the-model))))

;;; try to implement: https://drcuis.github.io/DesignGUI/Quick-Start.html
(defmethod init-gui ((model basic-morphenator-model))
  (let
      ((world (make-instance 'world-morph))
       (view (make-instance 'layout-morph
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
                              :action-selector (lambda () (greet-me (the-model)))
                              :color "green"
                              :mouse-over-color "lime"
                              :extent (make-instance 'extent :x 50 :y 20)
                              :label "Greet"))
       (greeting-label (make-instance 'label-morph
                                      :color "blue"
                                      :extent (make-instance 'extent :x 50 :y 20)
                                      :contents "Hmm...")))

    (progn
      (setf (world model) world)
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
  (unless (typep morph 'world-morph)
    (setf (id morph) (1+ (last-morph-id morph)))
    (setf (last-morph-id morph) (id morph))))

;;; =============== add morph ==================================================
(defmethod add-morph ((morph morph) (submorph morph))
  (setf (owner submorph) morph)
  (setf (submorphs morph) (append (submorphs morph) (list submorph))))

(defmethod add-morph :after ((morph layout-morph) (submorph morph))
  (add-morph-after-aux morph (direction morph)))
(defmethod add-morph-after-aux ((morph layout-morph) (direction (eql :horizontal)))
  (warn "adding layout element ~s" direction))
(defmethod add-morph-after-aux ((morph layout-morph) (direction (eql :vertical)))
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

;;; in cuis layout children get their locations overwritten by owner
(defmethod refresh-location ((morph morph))
  (when (owner morph)
    (refresh-location-with-owner morph (owner morph))))

(defmethod refresh-location-with-owner ((morph morph) (owner-morph layout-morph))
  (refresh-location-with-owner-direction-aux morph owner-morph (direction owner-morph)))
(defmethod refresh-location-with-owner-direction-aux ((morph morph) (owner-morph layout-morph) (direction (eql :vertical)))
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
(defmethod refresh-location-with-owner-direction-aux ((morph morph) (owner-morph layout-morph) (direction (eql :horizontal)))
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
      (warn "morph ~s it has elements outside its owner" morph)
      )))

;;; ---------------- mouse location --------------------------------------------
(defmethod contains-point-p ((morph placek-morph) x y)
  (if (and (location morph) (extent morph))
    (let* ((x-top-left (absolute-x (location morph)))
           (y-top-left (absolute-y (location morph)))
           (x-bottom-right (+ x-top-left (x (extent morph))))
           (y-bottom-right (+ y-top-left (y (extent morph)))))
      (and (<= x-top-left x x-bottom-right)
           (<= y-top-left y y-bottom-right)))
    (error "missing location or extent for ~s" morph)))

(defmethod morph-at ((morph placed-morph) x y)
  (loop
    for the-morph in (submorphs morph)
    for found = (morph-at the-morph x y) ; recursively descend over submorphs
    when found return found             ; TODO it my go to deep later, so I may need handlesMouseDown / ignoresMouseDown
      finally (return (when (contains-point-p morph x y) ; return the deepest found morph
                        morph))))

;;; ================= events ===================================================
(defmethod handle-event-motion ((morph null) (event symbol) x y)
  (let ((world-morph (world (the-model))))
    (let ((last-morph (last-movement-morph world-morph)))
      (progn ;; else
        ;; (warn "else ~S~S" last-morph world-morph)
        (when last-morph
          (handle-event-motion-do-leave last-morph event x y))
        (handle-event-motion-do-enter world-morph event x y)

        ;; set the last-movement-morph
        (setf (last-movement-morph world-morph) nil)))))
(defmethod handle-event-motion ((morph morph) (event symbol) x y)
  (let ((last-morph (last-movement-morph morph)))
    (if (equal last-morph morph)
        (progn ;; then
          ;; (warn "then ~S~S" last-morph morph)
          (handle-event-motion-do-motion morph event x y))
        (progn ;; else
          ;; (warn "else ~S~S" last-morph morph)
          (when last-morph
            (handle-event-motion-do-leave last-morph event x y))
          (handle-event-motion-do-enter morph event x y)

          ;; set the last-movement-morph
          (setf (last-movement-morph morph) morph)))))

  ;; if over the green button id 6 then change button color for enter leave
(defmethod handle-event-motion-do-enter  ((morph morph) (event symbol) x y)
  (mouse-over morph)
  (warn "enter ~s ~s" event morph))
(defmethod handle-event-motion-do-motion ((morph morph) (event symbol) x y)
  (typecase morph
    (world-morph)
    (otherwise
     (warn "motion ~s ~s" event morph))))
(defmethod handle-event-motion-do-leave  ((morph morph) (event symbol) x y)
  (mouse-out morph)
  (warn "leave ~s ~s" event (last-movement-morph morph)))

(defmethod handle-event-motion-do-enter  ((morph pluggable-button-morph) (event symbol) x y)
  (mouse-over morph)
  (setf (mouse-is-over morph) T)
  (warn "enter ~s ~s" event morph))
(defmethod handle-event-motion-do-motion ((morph pluggable-button-morph) (event symbol) x y)
  (setf (mouse-is-over morph) T)
  (warn "motion ~s ~s" event morph))
(defmethod handle-event-motion-do-leave  ((morph pluggable-button-morph) (event symbol) x y)
  (mouse-out morph)
  (setf (mouse-is-over morph) nil)
  (warn "leave ~s ~s" event (last-movement-morph morph)))

(defmethod mouse-over ((morph morph))
  (if (eq (id morph) 6)
      (setf (mouse-over morph) T)))
(defmethod mouse-out ((morph morph))
  (if (eq (id morph) 6)
      (setf (mouse-over morph) nil)))

(defmethod handle-event-mouse-button ((morph null) (event symbol) button x y)
  (warn "UNHANDLED mouse button event ~s at ~a for ~s" event (format nil "~,2f x ~,2f" x y) morph)
  (setf morph (world (the-model)))
  (warn "pressed is ~S" (pressed-morph morph))
  (when (and (eq event :released)
             (pressed-morph morph))
    (warn "releasing the button")
    (setf (is-pressed (pressed-morph morph)) nil)
    (setf (pressed-morph morph) nil)
    (warn "after releasing ~s" (pressed-morph morph))))
(defmethod handle-event-mouse-button ((morph morph) (event symbol) button x y)
  (warn "UNHANDLED mouse button event ~s at ~a for ~s" event (format nil "~,2f x ~,2f" x y) morph)
  (warn "pressed is ~S" (pressed-morph morph))
  (when (and (eq event :released)
             (pressed-morph morph))
    (warn "releasing the button")
    (setf (is-pressed (pressed-morph morph)) nil)
    (setf (pressed-morph morph) nil)
    (warn "after releasing ~s" (pressed-morph morph))))
(defmethod handle-event-mouse-button ((morph pluggable-button-morph) (event symbol) button x y)
  (case event
    (:pressed
     (setf (is-pressed morph) T)
     (setf (pressed-morph morph) morph)
     (when (and (contains-point-p morph x y)
                (action-selector morph))
       (funcall (action-selector morph))))
    (:released
     (when (pressed-morph morph)
       (setf (is-pressed (pressed-morph morph)) nil)
       (setf (pressed-morph morph) nil))
     (when (and (contains-point-p morph x y)
                (secondary-action-selector morph))
       (funcall (secondary-action-selector morph))))))

(defmethod handle-event-leave ((morph null))
  (warn "leaving before moving over any morph"))
(defmethod handle-event-leave ((morph morph))
  (warn "mouse left the window ~s -last morph is ~s" morph (last-movement-morph morph))
  (typecase morph
    (pluggable-button-morph
     (setf (mouse-is-over morph) nil)
     (setf (last-movement-morph morph) nil))
    (otherwise
     (warn "morph without mouse over"))))

;;; ============= render =======================================================
(defmethod render-prepare-to-show-text ((morph placed-morph))
  ;;"Advaita Mono"   ;; "Liberation Mono"
  (cairo:select-font-face "Ubuntu Mono" :normal :normal)
  (cairo:set-font-size 15)
  (cairo:move-to (~> morph location absolute-x)
                 (~> morph location absolute-y (+ _ 15))))

;;; apparently tere is function cairo:traslate tx ty
;;; that may allow to get rid of absolute-x absolute-y
;; https://www.cairographics.org/manual-1.17.2/cairo-Transformations.html#cairo-translate

(defmethod render :before ((morph morph))
  (refresh-location morph))             ;TODO move refreshing locations to layout morphs, with ad-morph/remove-morph etc
(defmethod render ((morph placed-morph))
  (gui-color:set-rgba (or (color morph)
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
  (if (equal (id morph) 7)
      ;; then - greeted label
      (or                               ; or avoid repeating test and then
       (greeted (the-model))
       (contents morph))
      ;; else - normal widget
      (contents morph)))

(defmethod render ((morph pluggable-button-morph))
  (gui-color:set-rgba (cond
                        ((is-pressed morph) "red")
                        ((mouse-is-over morph) (mouse-over-color morph))
                        ((color morph) (color morph))
                        (T "yellow")))
  (cairo:rectangle
   (~> morph location absolute-x)
   (~> morph location absolute-y)
   (~> morph extent x)
   (~> morph extent y))
  (cairo:fill-path)

  (render-prepare-to-show-text morph)
  (gui-color:set-rgba "white")
  (cairo:show-text (label morph)))

(defmethod render ((morph text-model-morph))
  (call-next-method)

  (render-prepare-to-show-text morph)
  (gui-color:set-rgba "red")
  (cairo:show-text (format nil "~A"  (entered (the-model)))))
;;; ------------ world ---------------------------------------------------------
(defmethod initialize-instance :after ((morph world-morph) &key)
  (setf (last-morph-id morph) 1
        (id morph) 1)
  (setf (owner morph) nil
        (last-movement-morph morph) nil
        (pressed-morph morph) nil
        (location morph) (make-instance 'location
                                        :absolute-x 0
                                        :absolute-y 0
                                        :x 0
                                        :y 0)
        (extent morph) (make-instance 'extent :x (car *window-size*) :y (cdr *window-size*))))

(defmethod render-world   ((morph world-morph))
  ;; actual morph
  (gui-color:set-rgba "red")
  (cairo:rectangle   0
                     0
                     (~> morph extent x)
                     (~> morph extent y))
  (cairo:fill-path)
  ;; render children
  (loop for m in (submorphs morph) do (render m)))

(defmethod resize ((morph world-morph) w h)
  "Resize the world on window resize"
  (if (extent morph)
      (progn
        (setf (x (extent morph)) w)
        (setf (y (extent morph)) h))
      (progn
        (setf (extent morph) (make-instance 'extent :x w :y h)))))
