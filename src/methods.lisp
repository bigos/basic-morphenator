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

(defun build-location (x y)
  (make-instance 'location
                 :x x
                 :y y))

(defmethod initialize-instance :after ((morph morph) &key)
  (unless (typep morph 'world-morph)
    (setf (id morph) (next-morph-id))))
;;; =============== add morph ==================================================
(defgeneric layout-submorphs (morph)
  (:method ((morph morph)) nil))

(defmethod add-morph ((morph morph) (submorph morph))
  ;; 1. Cycle: submorph is morph, or an ancestor of morph.
  (when (morph-ancestor-p morph submorph)
    (error "Cannot add ~S to ~S: would create a cycle." submorph morph))

  ;; 2. Duplicate: already a direct child of morph.
  (when (member submorph (submorphs morph) :test #'eq)
    (error "~S is already a submorph of ~S." submorph morph))

  ;; 3. Detach from old owner (remember it for re-layout).
  (let ((old-owner (owner submorph)))
    (when old-owner
      (setf (submorphs old-owner)
            (remove submorph (submorphs old-owner) :test #'eq))
      (setf (owner submorph) nil))

    ;; 4. Attach to new parent.
    (setf (owner submorph) morph)
    (setf (submorphs morph) (append (submorphs morph) (list submorph)))
    (refresh-location submorph)
    (layout-submorphs morph)

    ;; 5. Re-layout the old owner so its remaining siblings fill the gap.
    (when old-owner
      (layout-submorphs old-owner)))

  submorph)

;;; ================== location ================================================
;;; Ensure every placed-morph starts with a valid (0, 0) location
(defmethod initialize-instance :after ((morph placed-morph) &key)
  (unless (location morph)
    (setf (location morph) (build-location 0 0))))

(defun morph-location-axis (morph axis-fn)
  (if (location morph)
      (or (funcall axis-fn (location morph)) 0)
      0))

(defmethod morph-location-x ((morph morph))
  (morph-location-axis morph #'x))

(defmethod morph-location-y ((morph morph))
  (morph-location-axis morph #'y))

(defun layout-gap (morph)
  (if (and morph (typep morph 'layout-morph))
      (or (separation morph) 10)
      10))

(defmethod prev-sibling-axis-sum ((morph morph) axis-fn)
  (let* ((own (owner morph))
         (gap (layout-gap own)))
    (loop for m in (submorphs own)
          for idx from 0
          until (eq m morph)
          sum (+ (funcall axis-fn (extent m))
                 (if (> idx 0) gap 0)))))

(defmethod prev-sibling-widths-sum ((morph morph))
  (prev-sibling-axis-sum morph #'x))

(defmethod prev-sibling-heights-sum ((morph morph))
  (prev-sibling-axis-sum morph #'y))

;;; in cuis layout children get their locations overwritten by owner
(defmethod refresh-location ((morph morph))
  (when (owner morph)
    (refresh-location-with-owner morph (owner morph))))

(defmethod refresh-location-with-owner ((morph morph) (owner-morph layout-morph))
  (ecase (direction owner-morph)
    (:vertical
     (setf (location morph)
           (build-location (morph-location-x morph)
                           (prev-sibling-heights-sum morph))))
    (:horizontal
     (setf (location morph)
           (build-location (prev-sibling-widths-sum morph)
                           (morph-location-y morph))))))

(defmethod refresh-location-with-owner ((morph morph) (owner-morph morph))
  ;; plain (non-layout) owner: child keeps its own local offset;
  ;; nesting is handled by the cairo translate, nothing to recompute
  (unless (location morph)
    (setf (location morph) (build-location 0 0))))

(defmethod layout-submorphs ((morph layout-morph))
  (let* ((subs (submorphs morph))
         (n (length subs)))
    (when (> n 0)
      (let* ((gap (layout-gap morph))
             (total-gap (* gap (max 0 (1- n))))
             (dir (direction morph))
             (owner-w (x (extent morph)))
             (owner-h (y (extent morph)))
             (avail-w (max 0 (- owner-w (if (eq dir :horizontal) total-gap 0))))
             (avail-h (max 0 (- owner-h (if (eq dir :vertical) total-gap 0))))
             (fixed-sum 0)
             (prop-sum 0.0))
        ;; 1. Tally fixed sizes and proportional weights along the main axis
        (dolist (sub subs)
          (let ((spec (layout-spec sub)))
            (if (eq dir :horizontal)
                (cond
                  ((and spec (proportional-width spec))
                   (incf prop-sum (proportional-width spec)))
                  ((and spec (fixed-width spec))
                   (incf fixed-sum (fixed-width spec)))
                  (t
                   (incf fixed-sum (x (extent sub)))))
                (cond
                  ((and spec (proportional-height spec))
                   (incf prop-sum (proportional-height spec)))
                  ((and spec (fixed-height spec))
                   (incf fixed-sum (fixed-height spec)))
                  (t
                   (incf fixed-sum (y (extent sub))))))))
        ;; 2. Compute sizes and place children cleanly with cursor tracking
        (let ((rem (max 0 (- (if (eq dir :horizontal) avail-w avail-h) fixed-sum)))
              (cursor 0))
          (dolist (sub subs)
            (let* ((spec (layout-spec sub))
                   (new-w (cond
                            ((and (eq dir :horizontal) spec (proportional-width spec) (> prop-sum 0))
                             (round (* rem (/ (proportional-width spec) prop-sum))))
                            ((and spec (fixed-width spec))
                             (fixed-width spec))
                            ((and (eq dir :vertical) spec (proportional-width spec))
                             (round (* avail-w (proportional-width spec))))
                            (t (x (extent sub)))))
                   (new-h (cond
                            ((and (eq dir :vertical) spec (proportional-height spec) (> prop-sum 0))
                             (round (* rem (/ (proportional-height spec) prop-sum))))
                            ((and spec (fixed-height spec))
                             (fixed-height spec))
                            ((and (eq dir :horizontal) spec (proportional-height spec))
                             (round (* avail-h (proportional-height spec))))
                            (t (y (extent sub))))))
              (setf (x (extent sub)) new-w
                    (y (extent sub)) new-h)
              (ecase dir
                (:horizontal
                 (setf (location sub) (build-location cursor (morph-location-y sub)))
                 (incf cursor (+ new-w gap)))
                (:vertical
                 (setf (location sub) (build-location (morph-location-x sub) cursor))
                 (incf cursor (+ new-h gap))))
              (layout-submorphs sub))))))))

;;; ---------------- mouse location --------------------------------------------
(defun morph-global-offset (morph)
  (values
   (loop for m = morph then (owner m) while m
         sum (if (and (location m) (x (location m))) (x (location m)) 0))
   (loop for m = morph then (owner m) while m
         sum (if (and (location m) (y (location m))) (y (location m)) 0))))

(defmethod contains-point-p ((morph morph) x y)
  (declare (ignore x y))
  nil)
(defmethod contains-point-p ((morph boxed-morph) x y)
  (let ((ext (extent morph)))
    (when ext
      (multiple-value-bind (dx dy) (morph-global-offset morph)
        (let ((lx (- x dx)) (ly (- y dy)))
          (and (<= 0 lx) (< lx (x ext))
               (<= 0 ly) (< ly (y ext))))))))

(defgeneric morphs-at (morph x y)
  (:documentation "Morphs containing the point (X, Y), ordered outermost to innermost.")
  (:method ((morph morph) x y)
    (declare (ignore x y))
    nil)
  (:method ((morph placed-morph) x y)
    (when (contains-point-p morph x y)
      (cons morph
            (loop for sub in (reverse (submorphs morph))
                  for sub-chain = (morphs-at sub x y)
                  when sub-chain append sub-chain
                    when sub-chain return sub-chain)))))

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
(defun release-world-pressed-morph (world)
  (when (and world (pressed-morph world))
    (setf (is-pressed (pressed-morph world)) nil
          (pressed-morph world) nil)))

(defun handle-unhandled-mouse-button (morph event x y world)
  (declare (ignore morph x y))
  (when (and world (eq event :released))
    (release-world-pressed-morph world)))

(defmethod handle-event-mouse-button ((morph morph) (event symbol) button x y)
  (declare (ignore button))
  (when (eq event :pressed)
    (let ((w (morph-target-world morph)))
      (when w
        (setf (world-keyboard-focus w) nil))))
  (handle-unhandled-mouse-button morph event x y (morph-target-world morph)))

(defmethod handle-event-mouse-button ((morph pluggable-button-morph) (event symbol) button x y)
  (declare (ignore button))
  (let ((w (morph-target-world morph)))
    (case event
      (:pressed
       (setf (is-pressed morph) t)
       (when w
         (setf (pressed-morph w) morph))
       (when (and (contains-point-p morph x y)
                  (action-selector morph))
         (funcall (action-selector morph)))
       (when w
         (setf (world-keyboard-focus w) nil)))
      (:released
       (release-world-pressed-morph w)
       (when (and (contains-point-p morph x y)
                  (secondary-action-selector morph))
         (funcall (secondary-action-selector morph)))))))

(defmethod handle-event-mouse-button ((morph text-model-morph) (event symbol) button x y)
  (declare (ignore button y))
  (let ((w (morph-target-world morph)))
    (case event
      (:pressed
       (when w
         (setf (world-keyboard-focus w) morph))
       (multiple-value-bind (dx dy) (morph-global-offset morph)
         (declare (ignore dy))
         (let ((idx (cursor-index-at-local-x morph (- x dx))))
           (setf (cursor-pos morph) idx
                 (selection-start morph) idx
                 (is-selecting morph) t))))
      (:released
       (finish-text-selection morph)))))

(defun update-text-drag-selection (x)
  (let ((fm (let ((w (current-world))) (and w (world-keyboard-focus w)))))
    (when (and (typep fm 'text-model-morph) (is-selecting fm))
      (multiple-value-bind (dx dy) (morph-global-offset fm)
        (declare (ignore dy))
        (setf (cursor-pos fm) (cursor-index-at-local-x fm (- x dx)))))))

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
(defmethod label-text ((morph label-morph))
  (let ((c (contents morph)))
    (if (functionp c)
        (funcall c)
        c)))

;;; ============= render =======================================================
(defgeneric render-prepare-to-show-text (morph)
  (:method ((morph t))
    (when (and (boundp 'cairo:*context*) cairo:*context*)
      (cairo:select-font-face "Ubuntu Mono" :normal :normal)
      (cairo:set-font-size 15)
      (cairo:move-to 0 15))))

(defmethod render-prepare-to-show-text ((morph placed-morph))
  (cairo:select-font-face "Ubuntu Mono" :normal :normal)
  (cairo:set-font-size 15)
  (cairo:move-to 0 15))   ; baseline 15px below local origin

(defgeneric render (morph))

(defmethod render :around ((morph placed-morph))
  (cairo:save)                                    ; match your binding's style
  (cairo:translate (x (location morph)) (y (location morph)))
  (unwind-protect (call-next-method)
    (cairo:restore)))

(defmethod render ((morph placed-morph))
  (draw-morph-box morph))
(defmethod render :after ((morph morph))
  (loop for m in (submorphs morph)
        do (render m)))

(defmethod render ((morph label-morph))
  (call-next-method)
  (draw-morph-label morph (render-text-for-color morph)))

(defmethod render-text-for-color ((morph label-morph))
  (or (label-text morph) ""))

(defmethod render ((morph pluggable-button-morph))
  (let ((fill (cond
                ((is-pressed morph) "red")
                ((mouse-is-over morph) (mouse-over-color morph))
                (t (color morph)))))
    (draw-morph-box morph fill))
  (draw-morph-label morph (label morph)))

(defgeneric widget-text (morph)
  (:documentation "Return the current text displayed by MORPH."))

(defgeneric (setf widget-text) (value morph)
  (:documentation "Update the text stored or bound by MORPH."))

(defmethod widget-text ((morph text-model-morph))
  (cond
    ((text-getter morph)
     (funcall (text-getter morph) (model morph)))
    ((and (model morph) (typep (model morph) 'basic-morphenator-model))
     (or (entered (model morph)) ""))
    (t
     (or (text morph) ""))))

(defmethod (setf widget-text) (value (morph text-model-morph))
  (let ((str (or value "")))
    (cond
      ((text-setter morph)
       (funcall (text-setter morph) str (model morph)))
      ((and (model morph) (typep (model morph) 'basic-morphenator-model))
       (setf (entered (model morph)) str))
      (t
       (setf (text morph) str)))
    str))

(defmethod render ((morph text-model-morph))
  (draw-morph-box morph)
  (let* ((ext (extent morph))
         (w (x ext))
         (h (y ext))
         (pad 4)
         (txt (format nil "~A" (widget-text morph))))
    (clamp-cursor morph)
    (cairo:save)
    (cairo:rectangle pad 0 (max 0 (- w (* 2 pad))) h)
    (cairo:clip)
    (render-prepare-to-show-text morph)
    (let* ((prefix (subseq txt 0 (cursor-pos morph)))
           (cursor-x (safe-text-advance-width morph prefix))
           (inner-w (max 0 (- w (* 2 pad)))))
      (when (> (- cursor-x (scroll-x morph)) inner-w)
        (setf (scroll-x morph) (- cursor-x inner-w)))
      (when (< (- cursor-x (scroll-x morph)) 0)
        (setf (scroll-x morph) cursor-x))
      (let ((draw-x (- pad (scroll-x morph))))
        (multiple-value-bind (sel-s sel-e) (selection-bounds morph)
          (when sel-s
            (let* ((x1 (+ draw-x (safe-text-advance-width morph (subseq txt 0 sel-s))))
                   (x2 (+ draw-x (safe-text-advance-width morph (subseq txt 0 sel-e)))))
              (gui-color:set-rgba "light blue")
              (cairo:rectangle x1 3 (- x2 x1) (- h 6))
              (cairo:fill-path))))
        (gui-color:set-rgba "red")
        (cairo:move-to draw-x 15)
        (cairo:show-text txt)
        (when (morph-has-focus-p morph)
          (let ((caret-x (+ draw-x cursor-x)))
            (gui-color:set-rgba "black")
            (cairo:set-line-width 1.5)
            (cairo:move-to caret-x 3)
            (cairo:line-to caret-x (- h 3))
            (cairo:stroke)))))
    (cairo:restore))
  (when (morph-has-focus-p morph)
    (let ((ext (extent morph)))
      (gui-color:set-rgba "blue")
      (cairo:set-line-width 2.0)
      (cairo:rectangle 1 1 (- (x ext) 2) (- (y ext) 2))
      (cairo:stroke))))

;;; ------------ world ---------------------------------------------------------
(defmethod initialize-instance :after ((morph world-morph) &key)
  (reset-morph-id-counter 1)             ; next non-world morph will be id 2
  (setf (id morph) 1
        (owner morph) nil
        (pressed-morph morph) nil
        (location morph) (build-location 0 0)
        (extent morph) (make-instance 'extent
                                      :x (or (car *window-size*) 600)
                                      :y (or (cdr *window-size*) 400)))
  (unless (active-hand morph)
    (setf (active-hand morph)
          (make-instance 'hand-morph
                         :mouse-over-handler (make-instance 'mouse-over-handler)))))

(defmethod render-world ((morph world-morph))
  (let ((ext (extent morph)))
    (draw-box-rect 0 0 (x ext) (y ext) "red"))
  (loop for m in (submorphs morph) do (render m)))

(defmethod resize ((morph world-morph) w h)
  "Resize the world on window resize"
  (if (extent morph)
      (setf (x (extent morph)) w
            (y (extent morph)) h)
      (setf (extent morph) (make-instance 'extent :x w :y h)))
  (dolist (sub (submorphs morph))
    (let ((spec (layout-spec sub)))
      (when spec
        (when (proportional-width spec)
          (setf (x (extent sub)) (round (* w (proportional-width spec)))))
        (when (proportional-height spec)
          (setf (y (extent sub)) (round (* h (proportional-height spec)))))))
    (layout-submorphs sub)))

;;; ---------------focus -------------------------------------------------------
(defgeneric accepts-keyboard-focus-p (morph)
  (:documentation "Return T if MORPH can hold keyboard focus.")
  (:method ((morph morph)) nil)
  (:method ((morph text-model-morph)) t))

(defun world-keyboard-focus (world)
  (when (and world (active-hand world))
    (keyboard-focus (active-hand world))))

(defun (setf world-keyboard-focus) (morph world)
  (when (and world (active-hand world))
    (let ((next-focus (if (and morph (accepts-keyboard-focus-p morph))
                          morph
                          nil)))
      (setf (keyboard-focus (active-hand world)) next-focus))))

(defun morph-has-focus-p (morph)
  (let ((w (morph-target-world morph)))
    (and w (eq (world-keyboard-focus w) morph))))

(defun active-text-morph ()
  (let* ((model (the-model))
         (world (and model (world model)))
         (fm (world-keyboard-focus world)))
    (when (and fm (typep fm 'text-model-morph))
      fm)))
