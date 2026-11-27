(declaim (optimize (speed 1) (safety 3) (debug 3)))

(in-package #:basic-morphenator)

(defun activate-world (model world)
  "Attach WORLD to MODEL when both are non-NIL, returning WORLD."
  (when (and model world)
    (setf (world model) world))
  world)

(defun set-layout-alignment (morph &key horizontal vertical)
  "Store alignment preferences (:left/:center for HORIZONTAL, :top/:center for VERTICAL)
  in MORPH's properties plist."
  (when morph
    (let ((props (properties morph)))
      (when horizontal
        (setf (getf props :h-align) horizontal))
      (when vertical
        (setf (getf props :v-align) vertical))
      (setf (properties morph) props)))
  morph)

(defun mark-fill-width (morph &optional (fill-p t))
  "Mark MORPH in its properties plist to absorb remaining horizontal row width."
  (when morph
    (let ((props (properties morph)))
      (setf (getf props :fill-width) fill-p)
      (setf (properties morph) props)))
  morph)

(defun mark-proportional-gaps (morph &optional (prop-p t))
  "Mark horizontal row MORPH in its properties plist to keep outer left/right
  padding fixed while splitting remaining width evenly across inner gaps."
  (when morph
    (let ((props (properties morph)))
      (setf (getf props :proportional-gaps) prop-p)
      (setf (properties morph) props)))
  morph)

(defun fill-width-child-p (morph)
  (or (getf (properties morph) :fill-width)
      (and (slot-boundp morph 'layout-spec)
           (layout-spec morph)
           (slot-exists-p (layout-spec morph) 'proportional-width)
           (let ((pw (slot-value (layout-spec morph) 'proportional-width)))
             (and pw (> pw 0))))))

(defun set-morph-width (morph new-w)
  (let* ((ext (ensure-extent morph))
         (h (y ext)))
    (setf (extent morph) (make-instance 'extent :x (max 0 (round new-w)) :y h))))

(defun vertical-y-for-child (sub v-align pad-y inner-h)
  (let ((sub-h (y (ensure-extent sub))))
    (ecase v-align
      (:top    pad-y)
      (:center (+ pad-y (max 0 (round (- inner-h sub-h) 2)))))))

(defun align-row-proportional-gaps (row v-align)
  "Keep outer left/right padding fixed and split remaining inner width evenly
  between adjacent children of ROW."
  (let* ((subs (submorphs row))
         (n (length subs))
         (pad-x (layout-padding-x row))
         (pad-y (layout-padding-y row))
         (row-ext (ensure-extent row))
         (inner-w (max 0 (- (x row-ext) (* 2 pad-x))))
         (inner-h (max 0 (- (y row-ext) (* 2 pad-y))))
         (children-w (loop for sub in subs
                           sum (x (ensure-extent sub))))
         (num-gaps (max 0 (1- n)))
         (gap (if (plusp num-gaps)
                  (max 0 (/ (- inner-w children-w) num-gaps))
                  0))
         (cur-x (float pad-x 1.0d0)))
    (dolist (sub subs)
      (let ((sub-w (x (ensure-extent sub)))
            (y-pos (vertical-y-for-child sub v-align pad-y inner-h)))
        (set-location-xy sub (round cur-x) y-pos)
        (incf cur-x (+ sub-w gap))))))

(defun align-horizontal-row-children (row &key horizontal vertical)
  "Position children of ROW along its horizontal axis and cross axis according to
  HORIZONTAL (:left or :center) and VERTICAL (:top or :center), using values stored
  on ROW when keyword arguments are omitted."
  (let* ((h-align (or horizontal (getf (properties row) :h-align) :left))
         (v-align (or vertical   (getf (properties row) :v-align) :center))
         (subs (submorphs row))
         (gap (layout-gap row))
         (pad-x (layout-padding-x row))
         (pad-y (layout-padding-y row))
         (row-ext (ensure-extent row))
         (inner-w (max 0 (- (x row-ext) (* 2 pad-x))))
         (inner-h (max 0 (- (y row-ext) (* 2 pad-y)))))
    (if (getf (properties row) :proportional-gaps)
        (align-row-proportional-gaps row v-align)
        (let* ((flex-subs (remove-if-not #'fill-width-child-p subs))
               (fixed-subs (remove-if #'fill-width-child-p subs))
               (total-gaps (if (> (length subs) 1)
                               (* gap (1- (length subs)))
                               0))
               (fixed-w (loop for sub in fixed-subs
                              sum (x (ensure-extent sub))))
               (flex-w (if (plusp (length flex-subs))
                           (max 0 (round (- inner-w fixed-w total-gaps) (length flex-subs)))
                           0)))
          (when (plusp (length flex-subs))
            (dolist (fsub flex-subs)
              (set-morph-width fsub flex-w)))
          (let* ((children-w (loop for sub in subs
                                   for idx from 0
                                   sum (+ (x (ensure-extent sub))
                                          (if (> idx 0) gap 0))))
                 (cur-x (ecase h-align
                          (:left   pad-x)
                          (:center (+ pad-x (max 0 (round (- inner-w children-w) 2)))))))
            (dolist (sub subs)
              (let ((sub-ext (ensure-extent sub))
                    (y-pos (vertical-y-for-child sub v-align pad-y inner-h)))
                (set-location-xy sub cur-x y-pos)
                (incf cur-x (+ (x sub-ext) gap)))))))))

(defmethod layout-submorphs :after ((morph layout-morph))
  "Re-apply custom row alignment after standard layout passes (such as window resize)."
  (when (and (eq (direction morph) :horizontal)
             (or (getf (properties morph) :h-align)
                 (getf (properties morph) :v-align)
                 (getf (properties morph) :proportional-gaps)
                 (some #'fill-width-child-p (submorphs morph)))
             (> (length (submorphs morph)) 0))
    (align-horizontal-row-children morph)))

;;; ================= init gui =================================================
(defparameter *scene* :nesting
  "Scene built by INIT-GUI:
  :greeting, :row, :row-left-center, :row-center-center, :row-center-top,
  :row-proportional-fill, :row-proportional-gaps, or :nesting.")

(defmethod init-gui ((model basic-morphenator-model))
  (ecase *scene*
    (:greeting              (build-greeting-scene model))
    (:row                   (build-row-default-scene model))
    (:row-left-center       (build-row-left-center-scene model))
    (:row-center-center     (build-row-center-center-scene model))
    (:row-center-top        (build-row-center-top-scene model))
    (:row-proportional-fill (build-row-proportional-fill-scene model))
    (:row-proportional-gaps (build-row-proportional-gaps-scene model))
    (:nesting               (build-nesting-scene model))))

;;; https://drcuis.github.io/DesignGUI/Quick-Start.html
(defun build-greeting-scene (model)
  (let* ((world (make-instance 'world-morph))
         (view (make-instance 'layout-morph
                              :color "pink"
                              :direction :vertical
                              :separation 10
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
                               :color nil
                               :font '(:family "Ubuntu Mono" :size 15 :color "black")
                               :layout-spec (make-layout-spec :fixed-width 80)
                               :contents "Name:"))
         (input (make-instance 'text-model-morph
                               :color "white"
                               :extent (make-instance 'extent :x 50 :y 30)
                               :layout-spec (make-layout-spec :proportional-width 1.0)
                               :model model))
         (button (make-instance 'pluggable-button-morph
                                :action-selector (lambda () (greet-me (the-model)))
                                :color "green"
                                :font '(:family "Ubuntu Mono" :size 15 :color "yellow")
                                :mouse-over-color "lime"
                                :extent (make-instance 'extent :x 60 :y 30)
                                :label "Greet"))
         (greeting-label (make-instance 'label-morph
                                        :color "yellow"
                                        :font '(:family "Ubuntu Mono" :size 15 :color "black")
                                        :contents (lambda ()
                                                    (let ((g (greeted model)))
                                                      (if (and g (plusp (length g)))
                                                          g
                                                          "Hmm..."))))))
    (with-deferred-layout ()
      (add-morph world view)
      (add-morph view row)
      (add-morph row label)
      (add-morph row input)
      (add-morph row button)
      (add-morph view greeting-label))
    (refresh-world-labels world)
    world))

;;; ------------------------- rows ---------------------------------------------
;; https://drcuis.github.io/DesignGUI/To-row-or-not-to-row.html
(defun build-row-default-scene (model)
  (declare (ignore model))
  (let* ((world (make-instance 'world-morph))
         (view (make-instance 'layout-morph
                              :color "pink"
                              :direction :vertical
                              :padding-x 10
                              :padding-y 10
                              :separation 10
                              :extent (make-instance 'extent :x 600 :y 400)
                              :layout-spec (make-layout-spec :proportional-width 1.0
                                                             :proportional-height 1.0)))
         ;; TODO children of the orange layout use default gap,
         (row (make-instance 'layout-morph
                             :color "orange"
                             :direction :horizontal
                             :separation 10
                             :padding-x 10
                             :padding-y 10
                             :extent (make-instance 'extent :x 580 :y 30)
                             :layout-spec (make-layout-spec :proportional-width 1.0)))
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

(defun build-row-left-center-scene (model)
  (declare (ignore model))
  (let* ((world (make-instance 'world-morph))
         (view (make-instance 'layout-morph
                              :color "pink"
                              :direction :vertical
                              :padding-x 10
                              :padding-y 10
                              :separation 10
                              :extent (make-instance 'extent :x 600 :y 400)
                              :layout-spec (make-layout-spec :proportional-width 1.0
                                                             :proportional-height 1.0)))
         (row (make-instance 'layout-morph
                             :color "orange"
                             :direction :horizontal
                             :separation 10
                             :padding-x 10
                             :padding-y 10
                             :extent (make-instance 'extent :x 580 :y 30)
                             :layout-spec (make-layout-spec :proportional-width 1.0)))
         (box1 (make-instance 'boxed-morph
                              :color "red"
                              :extent (make-instance 'extent :x 50 :y 50)))
         (box2 (make-instance 'boxed-morph
                              :color "green"
                              :extent (make-instance 'extent :x 75 :y 75)))
         (box3 (make-instance 'boxed-morph
                              :color "blue"
                              :extent (make-instance 'extent :x 100 :y 100))))
    (set-layout-alignment row :horizontal :left :vertical :center)
    (with-deferred-layout ()
      (add-morph world view)
      (add-morph view row)
      (add-morph row box1)
      (add-morph row box2)
      (add-morph row box3))
    world))

(defun build-row-center-center-scene (model)
  (declare (ignore model))
  (let* ((world (make-instance 'world-morph))
         (view (make-instance 'layout-morph
                              :color "pink"
                              :direction :vertical
                              :padding-x 10
                              :padding-y 10
                              :separation 10
                              :extent (make-instance 'extent :x 600 :y 400)
                              :layout-spec (make-layout-spec :proportional-width 1.0
                                                             :proportional-height 1.0)))
         (row (make-instance 'layout-morph
                             :color "orange"
                             :direction :horizontal
                             :separation 10
                             :padding-x 10
                             :padding-y 10
                             :extent (make-instance 'extent :x 580 :y 30)
                             :layout-spec (make-layout-spec :proportional-width 1.0)))
         (box1 (make-instance 'boxed-morph
                              :color "red"
                              :extent (make-instance 'extent :x 50 :y 50)))
         (box2 (make-instance 'boxed-morph
                              :color "green"
                              :extent (make-instance 'extent :x 75 :y 75)))
         (box3 (make-instance 'boxed-morph
                              :color "blue"
                              :extent (make-instance 'extent :x 100 :y 100))))
    (set-layout-alignment row :horizontal :center :vertical :center)
    (with-deferred-layout ()
      (add-morph world view)
      (add-morph view row)
      (add-morph row box1)
      (add-morph row box2)
      (add-morph row box3))
    world))

(defun build-row-center-top-scene (model)
  ;; Horizontal center, vertical top
  (declare (ignore model))
  (let* ((world (make-instance 'world-morph))
         (view (make-instance 'layout-morph
                              :color "pink"
                              :direction :vertical
                              :padding-x 10
                              :padding-y 10
                              :separation 10
                              :extent (make-instance 'extent :x 600 :y 400)
                              :layout-spec (make-layout-spec :proportional-width 1.0
                                                             :proportional-height 1.0)))
         (row (make-instance 'layout-morph
                             :color "orange"
                             :direction :horizontal
                             :separation 10
                             :padding-x 10
                             :padding-y 10
                             :extent (make-instance 'extent :x 580 :y 30)
                             :layout-spec (make-layout-spec :proportional-width 1.0)))
         (box1 (make-instance 'boxed-morph
                              :color "red"
                              :extent (make-instance 'extent :x 50 :y 50)))
         (box2 (make-instance 'boxed-morph
                              :color "green"
                              :extent (make-instance 'extent :x 75 :y 75)))
         (box3 (make-instance 'boxed-morph
                              :color "blue"
                              :extent (make-instance 'extent :x 100 :y 100))))
    (set-layout-alignment row :horizontal :center :vertical :top)
    (with-deferred-layout ()
      (add-morph world view)
      (add-morph view row)
      (add-morph row box1)
      (add-morph row box2)
      (add-morph row box3))
    world))

;;; see https://drcuis.github.io/DesignGUI/Proportion.html
;;; First proportional scene: green box (box2) takes all remaining width after
;;; red box (box1), blue box (box3), row padding, and gaps, updating on resize.
(defun build-row-proportional-fill-scene (model)
  (declare (ignore model))
  (let* ((world (make-instance 'world-morph))
         (view (make-instance 'layout-morph
                              :color "pink"
                              :direction :vertical
                              :padding-x 10
                              :padding-y 10
                              :separation 10
                              :extent (make-instance 'extent :x 600 :y 400)
                              :layout-spec (make-layout-spec :proportional-width 1.0
                                                             :proportional-height 1.0)))
         (row (make-instance 'layout-morph
                             :color "orange"
                             :direction :horizontal
                             :separation 10
                             :padding-x 10
                             :padding-y 10
                             :extent (make-instance 'extent :x 580 :y 30)
                             :layout-spec (make-layout-spec :proportional-width 1.0)))
         (box1 (make-instance 'boxed-morph
                              :color "red"
                              :extent (make-instance 'extent :x 50 :y 50)))
         (box2 (make-instance 'boxed-morph
                              :color "green"
                              :extent (make-instance 'extent :x 75 :y 75)))
         (box3 (make-instance 'boxed-morph
                              :color "blue"
                              :extent (make-instance 'extent :x 100 :y 100))))
    (mark-fill-width box2 t)
    (set-layout-alignment row :horizontal :left :vertical :center)
    (with-deferred-layout ()
      (add-morph world view)
      (add-morph view row)
      (add-morph row box1)
      (add-morph row box2)
      (add-morph row box3))
    world))

;;; Second proportional scene: all boxes stay square (50x50, 75x75, 100x100) and
;;; vertically centered, outer left/right padding stays fixed, and the gaps
;;; around the green square (box2) adjust evenly on horizontal resize.
(defun build-row-proportional-gaps-scene (model)
  (declare (ignore model))
  (let* ((world (make-instance 'world-morph))
         (view (make-instance 'layout-morph
                              :color "pink"
                              :direction :vertical
                              :padding-x 10
                              :padding-y 10
                              :separation 10
                              :extent (make-instance 'extent :x 600 :y 400)
                              :layout-spec (make-layout-spec :proportional-width 1.0
                                                             :proportional-height 1.0)))
         (row (make-instance 'layout-morph
                             :color "orange"
                             :direction :horizontal
                             :separation 10
                             :padding-x 10
                             :padding-y 10
                             :extent (make-instance 'extent :x 580 :y 30)
                             :layout-spec (make-layout-spec :proportional-width 1.0)))
         (box1 (make-instance 'boxed-morph
                              :color "red"
                              :extent (make-instance 'extent :x 50 :y 50)))
         (box2 (make-instance 'boxed-morph
                              :color "green"
                              :extent (make-instance 'extent :x 75 :y 75)))
         (box3 (make-instance 'boxed-morph
                              :color "blue"
                              :extent (make-instance 'extent :x 100 :y 100))))
    (mark-proportional-gaps row t)
    (set-layout-alignment row :horizontal :left :vertical :center)
    (with-deferred-layout ()
      (add-morph world view)
      (add-morph view row)
      (add-morph row box1)
      (add-morph row box2)
      (add-morph row box3))
    world))

;;; ---------------------------- nesting ---------------------------------------
;;; see https://drcuis.github.io/DesignGUI/Nesting-layouts.html
;;; A horizontal row containing three vertical columns of labels.
(defun build-nesting-scene (model)
  (declare (ignore model))
  (let* ((world (make-instance 'world-morph))
         (view (make-instance 'layout-morph
                              :color "pink"
                              :direction :vertical
                              :padding-x 10
                              :padding-y 10
                              :separation 10
                              :extent (make-instance 'extent :x 600 :y 400)
                              :layout-spec (make-layout-spec :proportional-width 1.0
                                                             :proportional-height 1.0)))
         (row (make-instance 'layout-morph
                             :color "orange"
                             :direction :horizontal
                             :separation 10
                             :padding-x 10
                             :padding-y 10
                             :extent (make-instance 'extent :x 580 :y 150)
                             :layout-spec (make-layout-spec :proportional-width 1.0)))
         (col1 (make-instance 'layout-morph
                              :color nil
                              :direction :vertical
                              :separation 6
                              :padding-x 8
                              :padding-y 8
                              :extent (make-instance 'extent :x 140 :y 120)))
         (col2 (make-instance 'layout-morph
                              :color nil
                              :direction :vertical
                              :separation 6
                              :padding-x 8
                              :padding-y 8
                              :extent (make-instance 'extent :x 240 :y 120)))
         (col3 (make-instance 'layout-morph
                              :color nil
                              :direction :vertical
                              :separation 6
                              :padding-x 8
                              :padding-y 8
                              :extent (make-instance 'extent :x 140 :y 120)))
         (fruit-labels (mapcar (lambda (text)
                                 (make-instance 'label-morph
                                                :color nil
                                                :font '(:family "Ubuntu Mono" :size 15 :color "black")
                                                :contents text))
                               '("Apple" "Banana" "Cherry")))
         (question-label (make-instance 'label-morph
                                        :color "yellow"
                                        :font '(:family "Ubuntu Mono" :size 15 :color "black")
                                        :font '(:family "Ubuntu Mono" :size 15 :color "black")
                                        :contents "What are your favourites?"))
         (season-labels (mapcar (lambda (text)
                                  (make-instance 'label-morph
                                                 :color nil
                                                 :font '(:family "Ubuntu Mono" :size 15 :color "black")
                                                 :contents text))
                                '("Spring" "Winter" "Summer" "Fall"))))
    (with-deferred-layout ()
      (add-morph world view)
      (add-morph view row)
      (add-morph row col1)
      (add-morph row col2)
      (add-morph row col3)
      (dolist (lbl fruit-labels)
        (add-morph col1 lbl))
      (add-morph col2 question-label)
      (dolist (lbl season-labels)
        (add-morph col3 lbl)))
    (refresh-world-labels world)
    world))
