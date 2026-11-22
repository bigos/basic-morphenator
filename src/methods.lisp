(declaim (optimize (speed 2) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defmethod print-object ((obj location) stream)
  (print-object-inner obj stream))

(defmethod print-object ((obj boxed-morph) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s" (list
                         :id
                         (id obj)
                         :color
                         (color obj)))))

;;; ================= helpers ==================================================
(defmethod greet-me ((model basic-morphenator-model))
  (setf (greeted model) (entered model)))

(defun draw-box-rect (x y w h color)
  (gui-color:set-rgba (or color "yellow"))
  (cairo:rectangle x y w h)
  (cairo:fill-path))

(defun draw-morph-box (morph &optional color)
  (let ((loc (location morph))
        (ext (extent morph)))
    (draw-box-rect (absolute-x loc)
                   (absolute-y loc)
                   (x ext)
                   (y ext)
                   (or color (color morph)))))

(defun draw-morph-label (morph text &key (color "white"))
  (render-prepare-to-show-text morph)
  (gui-color:set-rgba color)
  (cairo:show-text (or text "")))

;;; ================= init gui =================================================
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
                                      :contents (lambda () (or (greeted model) "Hmm...")))))

    (progn
      (setf (world model) world)
      (add-morph (world model) view)
      (add-morph view row)
      (add-morph row label)
      (add-morph row input)
      (add-morph row button)
      (add-morph view greeting-label))
    (setf (active-hand world)
          (make-instance 'hand-morph
                         :mouse-over-handler (make-instance 'mouse-over-handler)))))

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
(defmethod contains-point-p ((morph morph) x y)
  (declare (ignore x y))
  nil)
(defmethod contains-point-p ((morph boxed-morph) x y)
  (let ((loc (location morph))
        (ext (extent morph)))
    (when (and loc ext
               (absolute-x loc) (absolute-y loc)
               (x ext) (y ext))
      (let ((left (absolute-x loc))
            (top  (absolute-y loc)))
        (and (<= left x) (< x (+ left (x ext)))
             (<= top  y) (< y (+ top  (y ext))))))))

(defgeneric morphs-at (morph x y)
  (:documentation "Morphs containing the point, outermost first.")
  (:method ((morph morph) x y) nil)
  (:method ((morph boxed-morph) x y)
    (when (contains-point-p morph x y)
      (cons morph
            (loop for sub in (reverse (submorphs morph))
                  for chain = (morphs-at sub x y)
                  when chain return chain)))))

(defun process-mouse-over (handler new-chain)
  (let* ((old (mouse-over-morphs handler))
         (left    (remove-if (lambda (m) (member m new-chain)) old))
         (entered (remove-if (lambda (m) (member m old)) new-chain)))
    (dolist (m (reverse left))
      (when (handles-mouse-over-p m) (mouse-leave m)))
    (dolist (m entered)
      (when (handles-mouse-over-p m) (mouse-enter m)))
    (setf (mouse-over-morphs handler) new-chain)))

;;; ================= events ===================================================
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

(defgeneric handles-mouse-over-p (morph)
  (:method ((morph morph)) nil))
(defmethod handles-mouse-over-p ((morph pluggable-button-morph)) t)

(defgeneric mouse-enter (morph))
(defgeneric mouse-leave (morph))
(defmethod mouse-enter ((morph pluggable-button-morph))
  (setf (mouse-is-over morph) t))
(defmethod mouse-leave ((morph pluggable-button-morph))
  (setf (mouse-is-over morph) nil))

;;; =============== content ====================================================
;; label-morph contents may be a string or a function
(defmethod label-text ((morph label-morph))
  (let ((c (contents morph)))
    (if (functionp c) (funcall c) c)))

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
  (draw-morph-box morph))
(defmethod render :after ((morph morph))
  (loop for m in (submorphs morph)
        do (render m)))

(defmethod render ((morph label-morph))
  (call-next-method)
  (draw-morph-label morph (render-text-for-color morph)))

(defmethod render-text-for-color ((morph label-morph))
  "Allow render to select text based on widget color"
  (if (equal (id morph) 7)
      ;; then - greeted label
                (or                 ; or avoid repeating test and then
                 (greeted (the-model))
                 (contents morph))
                ;; else - normal widget
                (contents morph)))

(defmethod render ((morph pluggable-button-morph))
  (let ((fill (cond
                ((is-pressed morph) "red")
                ((mouse-is-over morph) (mouse-over-color morph))
                (t (color morph)))))
    (draw-morph-box morph fill))
  (draw-morph-label morph (label morph)))

(defmethod render ((morph text-model-morph))
  (call-next-method)
  (draw-morph-label morph
                    (format nil "~A" (entered (the-model)))
                    :color "red"))
;;; ------------ world ---------------------------------------------------------
(defmethod initialize-instance :after ((morph world-morph) &key)
  (setf (last-morph-id morph) 1
        (id morph) 1)
  (setf (owner morph) nil
        (pressed-morph morph) nil
        (location morph) (make-instance 'location
                                        :absolute-x 0
                                        :absolute-y 0
                                        :x 0
                                        :y 0)
        (extent morph) (make-instance 'extent :x (car *window-size*) :y (cdr *window-size*))))

(defmethod render-world ((morph world-morph))
  (let ((ext (extent morph)))
    (draw-box-rect 0 0 (x ext) (y ext) "red"))
  (loop for m in (submorphs morph) do (render m)))

(defmethod resize ((morph world-morph) w h)
  "Resize the world on window resize"
  (if (extent morph)
      (progn
        (setf (x (extent morph)) w)
        (setf (y (extent morph)) h))
      (progn
        (setf (extent morph) (make-instance 'extent :x w :y h)))))
